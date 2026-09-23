// The tradeoffs-trace Pi extension — phase 0 skeleton (design §2.1, §2.2,
// §3.2, §3.3, §9.5 note: no guards in this packet).
//
// Registers `sh`, `submit_phase`, `submit_discovery` and `submit_review` in
// every agent, regardless of role — Pi's `--tools` allowlist (src/core/
// roles.ts's ROLE_TOOLS) decides which are actually callable for a given
// launch, per design §2.1 ("the extension registers all of its tools in
// every agent, and Pi's default set omits `grep`, `find` and `ls`").
//
// At `session_start` it connects to the run socket named by `TT_SOCKET`
// and sends a `hello` carrying `pi.getActiveTools()`, so the conductor (or,
// in phase 0, a test socket server) can assert the tool set *before* ever
// dispatching a prompt (design §2.1's "Tool-set assertion").
//
// Submission tools validate their arguments with the core validator
// (src/core/schema.ts) before ever reaching the socket, so an empty
// required plain-language field (design §3.2) is rejected back to the
// model as a normal tool error it can retry, not silently forwarded.
//
// A submission tool never reports success on its own: with no socket
// connected, or no reply received, it returns an error result. Only the
// conductor's `submit_reply` on the run socket can make it report success.

import { createConnection, type Socket } from "node:net";
import { readFileSync } from "node:fs";
import { randomUUID } from "node:crypto";
import { Type, type TSchema } from "typebox";
import { StringEnum } from "@earendil-works/pi-ai";
import { isToolCallEventType, type ExtensionAPI } from "@earendil-works/pi-coding-agent";

import { guardedShCommand, guardedWritePath, readGuardConfigFromEnv } from "./guards.ts";

import { validate, type JSONSchema } from "../src/core/schema.ts";
import { reviewIngestionIssue } from "../src/core/predicate.ts";
import type { Review } from "../src/core/types.ts";
import {
  JSONLDecoder,
  encodeLine,
  type HelloMessage,
  type NoSubmissionMessage,
  type RunSocketMessage,
  type ShExitMessage,
  type ShOutputMessage,
  type SubmitReply,
} from "../src/core/protocol.ts";
import { DECISION_DISCLOSURE_PARAMS, SUBMIT_DISCOVERY_PARAMS, SUBMIT_PHASE_PARAMS, SUBMIT_REVIEW_PARAMS } from "./param-shapes.ts";

function loadSchema(relPath: string): JSONSchema {
  return JSON.parse(readFileSync(new URL(relPath, import.meta.url), "utf8")) as JSONSchema;
}

/** Pulls one named node out of a loaded schema file's `$defs` and merges
 * the file's own `$defs` back onto it, so `validate()` (which resolves
 * `$ref`s against whatever object it was given as the schema) can still
 * follow `#/$defs/...` references defined alongside it in the same file. */
function subschema(file: JSONSchema, name: string): JSONSchema {
  const defs = file.$defs as Record<string, JSONSchema> | undefined;
  const node = defs?.[name];
  if (!node) throw new Error(`schema has no $defs.${name}`);
  return { ...node, $defs: defs };
}

// The full Review record shape (design §6.3) needs no conductor-assigned
// binding fields at submission time — the reviewer already knows its own
// candidateSha and contractVersion (it is told them; design §2.1's process
// table), so the shared schemas/review.schema.json is used as-is.
const REVIEW_SCHEMA = loadSchema("../schemas/review.schema.json");

// A Decision's binding fields (boundCandidateSha, boundContractVersion) and
// identity fields (id, version, phaseId) are assigned by the conductor —
// for a worker's submit_phase, the candidate does not even exist yet (it is
// produced by the freeze that SUBMIT_PHASE triggers). So submission-time
// validation here checks only the plain-language fields design §3.2
// requires non-empty, via the single source of truth in
// schemas/submission.schema.json (kept identical to decision.schema.json's
// own plain-language fields and Alternative/Recommendation $defs by
// test/contract/submission-schema.test.ts). See README's phase-1 note for
// what the conductor does with these once a candidate exists.
const SUBMISSION_SCHEMA_FILE = loadSchema("../schemas/submission.schema.json");
const SUBMIT_PHASE_SCHEMA = subschema(SUBMISSION_SCHEMA_FILE, "submitPhase");
const SUBMIT_DISCOVERY_SCHEMA = subschema(SUBMISSION_SCHEMA_FILE, "submitDiscovery");

// --- typebox parameter schemas (what the model sees / what Pi enforces
// structurally before `execute` runs). Each is built by mapping over the
// plain-data field lists in ./param-shapes.ts, rather than listing keys a
// second time by hand, so the model-visible shape stays mechanically tied
// to the same source test/contract/submission-schema.test.ts checks. ---

const AlternativeParam = Type.Object({
  option: Type.String({ description: "The alternative considered" }),
  consequence: Type.String({ description: "What choosing it would have cost or changed" }),
});

const RecommendationParam = Type.Object({
  choice: Type.String({ description: "The recommended option" }),
  reason: Type.String({ description: "Why that option is recommended" }),
});

const decisionDisclosureFields: Record<string, TSchema> = {
  choice: Type.String({ description: "One plain sentence naming the choice made" }),
  whyItMatters: Type.String({ description: "Why it matters in terms of the plan's goal, not the code" }),
  alternatives: Type.Array(AlternativeParam, { minItems: 1, description: "At least one alternative, each with its consequence" }),
  recommendation: RecommendationParam,
  classProposal: StringEnum(["detail", "delegated", "reserved"] as const, {
    description: "The worker's or reviewer's proposed classification (design §3.4); only the owner may lower it",
  }),
};
const DecisionParam = Type.Object(
  Object.fromEntries(DECISION_DISCLOSURE_PARAMS.properties.map((key) => [key, decisionDisclosureFields[key]])),
);

const CorrectionStatementParam = Type.Object({
  correctionId: Type.String(),
  status: StringEnum(["honored", "not_honored"] as const),
});

const FindingStatementParam = Type.Object({
  findingId: Type.String(),
  status: StringEnum(["confirm", "withdraw"] as const),
  evidence: Type.Optional(Type.String()),
});

const ContractVersionParam = Type.Object({
  snapshot: Type.Integer(),
  sectionSha256: Type.String(),
});

const BallotParam = Type.Object({
  decisionId: Type.String({ description: "The delegated or reserved decision this ballot votes on" }),
  vote: StringEnum(["approve", "reject"] as const),
  rationale: Type.String({ description: "Why you voted this way" }),
  evidence: Type.Array(Type.String(), { minItems: 1, description: "At least one citation" }),
  contractObjection: Type.Optional(
    Type.Boolean({ description: "Opens a linked contract finding and suspends this vote" }),
  ),
});

const FindingParam = Type.Object({
  kind: StringEnum(["defect", "contract", "integration"] as const),
  severity: StringEnum(["blocking", "advisory"] as const),
  evidence: Type.String({ description: "file:line, scenario, check result or plan clause — required, non-empty" }),
  linkedDecisionId: Type.Optional(Type.String()),
  sameAs: Type.Optional(
    Type.String({ description: "Id of an already-open finding this repeats; you are recorded on it instead of a duplicate" }),
  ),
  reproduction: Type.Optional(
    Type.Object({ command: Type.String({ description: "Command the conductor runs on a fresh disposable checkout" }) }),
  ),
});

const PriorDecisionParam = Type.Object({
  id: Type.String({ description: "The prior decision's id, as listed in the repair prompt" }),
  status: StringEnum(["kept", "changed", "withdrawn"] as const),
  choice: Type.Optional(Type.String({ description: "changed only: the new choice, one plain sentence" })),
  whyItMatters: Type.Optional(Type.String()),
  alternatives: Type.Optional(Type.Array(Type.Object({ option: Type.String(), consequence: Type.String() }))),
  recommendation: Type.Optional(Type.Object({ choice: Type.String(), reason: Type.String() })),
});

const submitPhaseFields: Record<string, TSchema> = {
  decisions: Type.Array(DecisionParam),
  assumptions: Type.Array(Type.String(), { description: "Assumptions made while implementing" }),
  deviations: Type.Array(Type.String(), { description: "Deviations from the plan" }),
  priorDecisions: Type.Optional(
    Type.Array(PriorDecisionParam, {
      description: "Repair attempts only: for each prior decision listed in the prompt, kept, changed (with the new text) or withdrawn",
    }),
  ),
};
const SubmitPhaseParams = Type.Object(Object.fromEntries(SUBMIT_PHASE_PARAMS.properties.map((key) => [key, submitPhaseFields[key]])));

const submitDiscoveryFields: Record<string, TSchema> = {
  discoveries: Type.Array(DecisionParam, { maxItems: 5, description: "At most 5 behavioural choices" }),
};
const SubmitDiscoveryParams = Type.Object(
  Object.fromEntries(SUBMIT_DISCOVERY_PARAMS.properties.map((key) => [key, submitDiscoveryFields[key]])),
);

const submitReviewFields: Record<string, TSchema> = {
  reviewer: StringEnum(["M", "A", "B"] as const),
  phaseId: Type.String(),
  candidateSha: Type.String(),
  contractVersion: ContractVersionParam,
  correctionStatements: Type.Array(CorrectionStatementParam),
  findingStatements: Type.Array(FindingStatementParam),
  ballots: Type.Optional(Type.Array(BallotParam, { description: "One per votable decision (design §5), turn 2 of a real review" })),
  findings: Type.Optional(Type.Array(FindingParam, { description: "Newly raised findings, turn 2 of a real review" })),
  discoveryMatches: Type.Optional(
    Type.Array(Type.Object({ discoveryId: Type.String(), sameAs: Type.String() }), {
      description: "For each of YOUR turn-1 discoveries that is the same choice as another listed record: {discoveryId, sameAs}",
    }),
  ),
};
const SubmitReviewParams = Type.Object(
  Object.fromEntries(SUBMIT_REVIEW_PARAMS.properties.map((key) => [key, submitReviewFields[key]])),
);

function readEnv(name: string): string | undefined {
  const v = process.env[name];
  return v && v.length > 0 ? v : undefined;
}

interface PendingSubmit {
  resolve: (reply: SubmitReply) => void;
}

interface PendingSh {
  onOutput: (msg: ShOutputMessage) => void;
  resolve: (msg: ShExitMessage) => void;
}

class RunSocketClient {
  #socket: Socket | undefined;
  #decoder = new JSONLDecoder();
  #pendingSubmits = new Map<string, PendingSubmit>();
  #pendingSh = new Map<string, PendingSh>();
  #connectError: string | undefined;

  async connect(path: string, timeoutMs = 5000): Promise<void> {
    await new Promise<void>((resolve, reject) => {
      const socket = createConnection(path);
      const timer = setTimeout(() => {
        socket.destroy();
        reject(new Error(`timed out connecting to run socket ${path}`));
      }, timeoutMs);
      socket.once("connect", () => {
        clearTimeout(timer);
        this.#socket = socket;
        resolve();
      });
      socket.once("error", (err) => {
        clearTimeout(timer);
        this.#connectError = String(err);
        reject(err);
      });
      socket.on("data", (chunk) => this.#onData(chunk));
      socket.on("close", () => {
        this.#socket = undefined;
      });
    });
  }

  get connected(): boolean {
    return this.#socket !== undefined && !this.#socket.destroyed;
  }

  get lastError(): string | undefined {
    return this.#connectError;
  }

  #onData(chunk: Buffer): void {
    const messages = this.#decoder.push(chunk) as RunSocketMessage[];
    for (const msg of messages) {
      if (msg.type === "submit_reply") {
        const pending = this.#pendingSubmits.get(msg.id);
        if (pending) {
          this.#pendingSubmits.delete(msg.id);
          pending.resolve(msg);
        }
      } else if (msg.type === "sh_output") {
        this.#pendingSh.get(msg.commandId)?.onOutput(msg);
      } else if (msg.type === "sh_exit") {
        const pending = this.#pendingSh.get(msg.commandId);
        if (pending) {
          this.#pendingSh.delete(msg.commandId);
          pending.resolve(msg);
        }
      }
    }
  }

  send(msg: RunSocketMessage): void {
    if (!this.#socket) throw new Error("run socket is not connected");
    this.#socket.write(encodeLine(msg));
  }

  async submit(tool: "submit_phase" | "submit_discovery" | "submit_review", args: unknown, timeoutMs = 60000): Promise<SubmitReply> {
    const id = randomUUID();
    const reply = new Promise<SubmitReply>((resolve, reject) => {
      const timer = setTimeout(() => {
        this.#pendingSubmits.delete(id);
        reject(new Error("timed out waiting for the conductor's submit reply"));
      }, timeoutMs);
      this.#pendingSubmits.set(id, {
        resolve: (r) => {
          clearTimeout(timer);
          resolve(r);
        },
      });
    });
    this.send({ type: "submit", id, tool, args });
    return reply;
  }

  async runShell(command: string, cwd: string | undefined, onOutput: (msg: ShOutputMessage) => void, timeoutMs = 300000): Promise<ShExitMessage> {
    const id = randomUUID();
    const commandId = randomUUID();
    const exit = new Promise<ShExitMessage>((resolve, reject) => {
      const timer = setTimeout(() => {
        this.#pendingSh.delete(commandId);
        reject(new Error("timed out waiting for the conductor's sh_exit"));
      }, timeoutMs);
      this.#pendingSh.set(commandId, {
        onOutput,
        resolve: (r) => {
          clearTimeout(timer);
          resolve(r);
        },
      });
    });
    this.send({ type: "sh", id, commandId, command, cwd });
    return exit;
  }
}

export default function (pi: ExtensionAPI) {
  const client = new RunSocketClient();
  const guardConfig = readGuardConfigFromEnv();
  let activeTools: string[] = [];
  const role = (readEnv("TT_ROLE") as "worker" | "reviewer" | undefined) ?? "worker";
  const accepted = new Set<string>();
  // A reviewer's turn 2 starts with the first agent_start after its
  // discovery (turn 1) was accepted; before that, turn 1 is still running.
  let reviewTurnStarted = false;
  const remindersUsed = new Map<string, number>();
  const MAX_SETTLE_CONTINUATIONS = 2;

  /** The submission the current turn still owes, or undefined when the turn
   * is satisfied. Worker: submit_phase. Reviewer: submit_discovery in turn
   * 1, then submit_review in turn 2 (design §3.3 two-turn review). */
  function owedSubmission(): string | undefined {
    if (role === "worker") return accepted.has("submit_phase") ? undefined : "submit_phase";
    if (!accepted.has("submit_discovery")) return "submit_discovery";
    if (reviewTurnStarted && !accepted.has("submit_review")) return "submit_review";
    return undefined;
  }

  const REMINDER_TEXT: Record<string, string> = {
    submit_phase:
      "You have not called submit_phase yet. The phase cannot finish without it — call submit_phase with your decisions, assumptions and deviations before finishing.",
    submit_discovery:
      "You have not called submit_discovery yet. List the behavioural choices you see in the diff and call submit_discovery before finishing. Do not call any other submission tool in this turn.",
    submit_review:
      "You have not called submit_review yet. This turn is not finished until you call submit_review with a ballot for every decision listed in the prompt, your findings, and your statements. submit_review is the only submission tool you may use now.",
  };

  pi.on("session_start", async () => {
    activeTools = pi.getActiveTools();
    const socketPath = readEnv("TT_SOCKET");
    if (!socketPath) return; // no conductor: tools will report an error when used
    try {
      await client.connect(socketPath);
      const hello: HelloMessage = {
        type: "hello",
        agentId: readEnv("TT_AGENT_ID") ?? "unknown",
        role,
        tools: activeTools,
        piVersion: readEnv("TT_PI_VERSION"),
      };
      client.send(hello);
    } catch {
      // connection failed — submission/sh tools report this per call.
    }
  });

  // design §9.5: block edit/write outside the worktree, to the phase's
  // protected acceptance files, or under the run directory; block `sh`
  // commands that commit/push or mention the run directory. Workflow
  // guards, not a security boundary — see guardedWritePath/guardedShCommand.
  pi.on("tool_call", (event) => {
    const cwd = readEnv("TT_WORKTREE") ?? process.cwd();
    if (isToolCallEventType("edit", event) || isToolCallEventType("write", event)) {
      const targetPath = (event.input as { path?: string }).path;
      if (typeof targetPath === "string") {
        const reason = guardedWritePath(targetPath, cwd, guardConfig);
        if (reason) return { block: true, reason };
      }
      return undefined;
    }
    if (event.toolName === "sh") {
      const command = (event.input as { command?: string }).command;
      if (typeof command === "string") {
        const reason = guardedShCommand(command, guardConfig);
        if (reason) return { block: true, reason };
      }
    }
    return undefined;
  });

  // design §3.3 item 1: refuse to settle without `submit_phase`, bounded to
  // two continuations, after which the extension reports `no_submission`
  // (a pure protocol addition — see NoSubmissionMessage) so the conductor
  // does not have to wait out the full attempt deadline.
  pi.on("agent_start", async () => {
    if (role === "reviewer" && accepted.has("submit_discovery")) reviewTurnStarted = true;
  });

  pi.on("agent_before_settle", async (event) => {
    const owed = owedSubmission();
    if (!owed) return undefined;
    const used = remindersUsed.get(owed) ?? 0;
    if (used >= MAX_SETTLE_CONTINUATIONS) {
      if (client.connected) {
        const msg: NoSubmissionMessage = { type: "no_submission", agentId: readEnv("TT_AGENT_ID") ?? "unknown" };
        try {
          client.send(msg);
        } catch {
          // conductor will also infer no_submission from agent_settled
          // arriving with no accepted submission — see socket.ts.
        }
      }
      return undefined;
    }
    remindersUsed.set(owed, used + 1);
    return {
      entries: [
        ...event.entries,
        {
          type: "custom_message" as const,
          customType: "tt-submit-reminder",
          content: REMINDER_TEXT[owed],
          display: false,
        },
      ],
      continue: true,
    };
  });

  function validateOrError(schema: JSONSchema, args: unknown): string | undefined {
    const result = validate(schema, args);
    if (result.valid) return undefined;
    return `invalid arguments: ${result.errors.join("; ")}`;
  }

  async function submitTool(tool: "submit_phase" | "submit_discovery" | "submit_review", args: unknown) {
    if (!client.connected) {
      return {
        isError: true,
        content: [
          {
            type: "text" as const,
            text: client.lastError
              ? `cannot submit: not connected to the run socket (${client.lastError})`
              : "cannot submit: no run socket configured (TT_SOCKET unset or not yet connected)",
          },
        ],
      };
    }
    try {
      const reply = await client.submit(tool, args);
      if (!reply.ok) {
        return {
          isError: true,
          content: [{ type: "text" as const, text: reply.reason ?? "submission rejected" }],
        };
      }
      accepted.add(tool);
      return { content: [{ type: "text" as const, text: "submission accepted" }] };
    } catch (err) {
      return {
        isError: true,
        content: [{ type: "text" as const, text: `submission failed: ${String((err as Error)?.message ?? err)}` }],
      };
    }
  }

  pi.registerTool({
    name: "submit_phase",
    label: "Submit Phase",
    description:
      "Submit the phase's decisions, assumptions and deviations. Required before the phase can finish; called at most once per attempt.",
    promptSnippet: "Submit the phase's decisions, assumptions and deviations",
    parameters: SubmitPhaseParams,
    async execute(_toolCallId, params) {
      const error = validateOrError(SUBMIT_PHASE_SCHEMA, params);
      if (error) return { isError: true, content: [{ type: "text", text: error }] };
      return submitTool("submit_phase", params);
    },
  });

  pi.registerTool({
    name: "submit_discovery",
    label: "Submit Discovery",
    description:
      "Submit the behavioral choices you discovered in the diff, before seeing the worker's own disclosure (design §3.3).",
    promptSnippet: "Submit the decisions you discovered in the diff",
    parameters: SubmitDiscoveryParams,
    async execute(_toolCallId, params) {
      const error = validateOrError(SUBMIT_DISCOVERY_SCHEMA, params);
      if (error) return { isError: true, content: [{ type: "text", text: error }] };
      return submitTool("submit_discovery", params);
    },
  });

  pi.registerTool({
    name: "submit_review",
    label: "Submit Review",
    description: "Submit your review: ballots implied by findingStatements/correctionStatements for this candidate.",
    promptSnippet: "Submit your review of the candidate",
    parameters: SubmitReviewParams,
    async execute(_toolCallId, params) {
      const error = validateOrError(REVIEW_SCHEMA, params);
      if (error) return { isError: true, content: [{ type: "text", text: error }] };
      // The same shared ingestion rule reduce() applies for
      // REVIEW_SUBMITTED (F02): a correction stated twice is rejected back
      // to the reviewer here, before it ever reaches the socket.
      const issue = reviewIngestionIssue(params as Review);
      if (issue) return { isError: true, content: [{ type: "text", text: `invalid arguments: ${issue}` }] };
      return submitTool("submit_review", params);
    },
  });

  pi.registerTool({
    name: "sh",
    label: "Shell",
    description: "Run a shell command. The conductor owns and runs every command (design §2.2); this only forwards it.",
    promptSnippet: "Run a shell command",
    parameters: Type.Object({
      command: Type.String(),
      cwd: Type.Optional(Type.String()),
    }),
    async execute(_toolCallId, params) {
      if (!client.connected) {
        return {
          isError: true,
          content: [
            {
              type: "text" as const,
              text: client.lastError
                ? `cannot run command: not connected to the run socket (${client.lastError})`
                : "cannot run command: no run socket configured (TT_SOCKET unset or not yet connected)",
            },
          ],
        };
      }
      const chunks: string[] = [];
      try {
        const waitMs = Number(process.env.TT_SH_WAIT_MS) || 300_000;
        const exit = await client.runShell(params.command, params.cwd, (msg) => chunks.push(msg.chunk), waitMs);
        return {
          isError: exit.code !== 0,
          content: [{ type: "text" as const, text: chunks.join("") || `(no output, exit ${exit.code})` }],
          details: { exitCode: exit.code, signal: exit.signal },
        };
      } catch (err) {
        return {
          isError: true,
          content: [{ type: "text" as const, text: `sh failed: ${String((err as Error)?.message ?? err)}` }],
        };
      }
    },
  });
}
