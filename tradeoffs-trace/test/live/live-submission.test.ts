// Live smoke test (design/plan "mandatory recorded evidence"): real Pi +
// real model (vercel-ai-gateway / deepseek-v4.1-flash), in a disposable
// temp git repo. Runs ONLY with TT_LIVE=1 — otherwise it prints why it is
// skipped and does nothing else. `make live` sets TT_LIVE=1.
//
// For each role: launch with roles.ts's launchArgs, receive `hello`, assert
// the tool set BEFORE ever sending a prompt (refusing to prompt on
// mismatch), then prompt the model to do a trivial task and submit. The
// test's own socket server stands in for the phase-1 conductor: it
// validates every submission with the core schemas (schemas/*.schema.json
// via src/core/schema.ts's validate()) and replies ok/error, so a
// submission with an invalid field comes back to the model as a normal
// tool error it can retry — exactly what a real conductor would do for the
// plain-language fields (design §3.2). A Decision's conductor-assigned
// binding fields (id, version, phaseId, boundCandidateSha,
// boundContractVersion) do not exist yet when the worker submits (the
// candidate itself is produced by the freeze SUBMIT_PHASE triggers), so
// this stand-in synthesizes them the same way a real conductor would
// before validating against the full record schema.
//
// Bounded at 5 minutes per role, then the test aborts and kills the agent.
// Writes one record per role under test/live/records/phase-0/.

import assert from "node:assert/strict";
import { execFileSync, spawn, type ChildProcessWithoutNullStreams } from "node:child_process";
import { mkdtempSync, readFileSync, writeFileSync, mkdirSync } from "node:fs";
import { createServer, type Server, type Socket } from "node:net";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { test } from "node:test";

import { JSONLDecoder, encodeLine, type RunSocketMessage } from "../../src/core/protocol.ts";
import { assertToolSet, defaultExtensionPath, launchArgs, PI_VERSION, ROLE_TOOLS, type Role } from "../../src/core/roles.ts";
import { validate, type JSONSchema } from "../../src/core/schema.ts";

const PROVIDER = "vercel-ai-gateway";
const MODEL = "deepseek/deepseek-v4.1-flash";
const ROLE_BUDGET_MS = 5 * 60 * 1000;

if (process.env.TT_LIVE !== "1") {
  test("live-submission (skipped)", (t) => {
    t.skip("TT_LIVE is not set to '1' — live smoke tests require real Pi, real model credentials and network access; run `make live` to opt in.");
  });
} else {
  runLiveSuite();
}

function loadSchema(name: string): JSONSchema {
  return JSON.parse(readFileSync(new URL(`../../schemas/${name}.schema.json`, import.meta.url), "utf8")) as JSONSchema;
}

const DECISION_SCHEMA = loadSchema("decision");
const REVIEW_SCHEMA = loadSchema("review");
const CV_FIXTURE = { snapshot: 1, sectionSha256: "b".repeat(64) };

interface HelloMsg {
  type: "hello";
  agentId: string;
  role: string;
  tools: string[];
  piVersion?: string;
}

interface ConductorStub {
  server: Server;
  socketPath: string;
  hello: Promise<HelloMsg>;
  submissions: { tool: string; args: unknown; validation: { valid: boolean; errors: string[] } }[];
}

function startConductorStub(phaseId: string, candidateSha: string): ConductorStub {
  const socketPath = join(mkdtempSync(join(tmpdir(), "tt-live-sock-")), "conductor.sock");
  const submissions: ConductorStub["submissions"] = [];
  let resolveHello!: (h: HelloMsg) => void;
  const hello = new Promise<HelloMsg>((resolve) => {
    resolveHello = resolve;
  });
  let decisionSeq = 0;

  function validateSubmission(tool: string, args: unknown): { valid: boolean; errors: string[] } {
    if (tool === "submit_review") {
      return validate(REVIEW_SCHEMA, args);
    }
    // submit_phase / submit_discovery: each item is a plain-language
    // decision the model supplied; synthesize the conductor-assigned
    // fields the same way a real conductor would once a candidate exists,
    // then validate the full record shape.
    const items = tool === "submit_phase" ? (args as { decisions?: unknown[] })?.decisions : (args as { discoveries?: unknown[] })?.discoveries;
    if (!Array.isArray(items)) return { valid: false, errors: [`expected an array of decisions for ${tool}`] };
    const errors: string[] = [];
    for (const item of items) {
      decisionSeq += 1;
      const plain = item as Record<string, unknown>;
      const synthesized = {
        id: `D-${phaseId}-${decisionSeq}`,
        version: 1,
        phaseId,
        source: tool === "submit_phase" ? "worker" : "reviewer-discovered",
        class: plain?.classProposal,
        choice: plain?.choice,
        whyItMatters: plain?.whyItMatters,
        alternatives: plain?.alternatives,
        recommendation: plain?.recommendation,
        boundCandidateSha: candidateSha,
        boundContractVersion: CV_FIXTURE,
      };
      const result = validate(DECISION_SCHEMA, synthesized);
      if (!result.valid) errors.push(...result.errors);
    }
    return { valid: errors.length === 0, errors };
  }

  const server = createServer((socket: Socket) => {
    const decoder = new JSONLDecoder();
    socket.on("data", (chunk) => {
      const messages = decoder.push(chunk) as RunSocketMessage[];
      for (const msg of messages) {
        if (msg.type === "hello") {
          resolveHello(msg as unknown as HelloMsg);
        } else if (msg.type === "submit") {
          const validation = validateSubmission(msg.tool, msg.args);
          submissions.push({ tool: msg.tool, args: msg.args, validation });
          socket.write(
            encodeLine({
              type: "submit_reply",
              id: msg.id,
              ok: validation.valid,
              reason: validation.valid ? undefined : validation.errors.join("; "),
            }),
          );
        } else if (msg.type === "sh") {
          // No shell commands are needed for this smoke test; reply with a
          // trivial success so the tool doesn't hang if the model tries.
          socket.write(encodeLine({ type: "sh_output", commandId: msg.commandId, chunk: "" }));
          socket.write(encodeLine({ type: "sh_exit", commandId: msg.commandId, code: 0 }));
        }
      }
    });
  });
  server.listen(socketPath);
  return { server, socketPath, hello, submissions };
}

function makeRepo(): { dir: string; headSha: string } {
  const dir = mkdtempSync(join(tmpdir(), "tt-live-repo-"));
  execFileSync("git", ["init", "-q"], { cwd: dir });
  execFileSync("git", ["config", "user.email", "tt@example.invalid"], { cwd: dir });
  execFileSync("git", ["config", "user.name", "tradeoffs-trace live smoke"], { cwd: dir });
  writeFileSync(join(dir, "notes.txt"), "one line of notes\n");
  execFileSync("git", ["add", "notes.txt"], { cwd: dir });
  execFileSync("git", ["commit", "-q", "-m", "seed"], { cwd: dir });
  const headSha = execFileSync("git", ["rev-parse", "HEAD"], { cwd: dir }).toString().trim();
  return { dir, headSha };
}

interface RoleRecord {
  piVersion: string;
  provider: string;
  model: string;
  role: Role;
  reportedTools: string[];
  toolAssertion: unknown;
  submissions: { tool: string; args: unknown; valid: boolean; errors: string[] }[];
  eventCounts: Record<string, number>;
  tokenUsage?: unknown;
  durationMs: number;
  outcome: "ok" | "tool-set-mismatch" | "timed-out" | "no-valid-submission" | "error";
  notes?: string;
}

function killGroup(child: ChildProcessWithoutNullStreams): void {
  if (child.pid === undefined) return;
  try {
    process.kill(-child.pid, "SIGKILL");
  } catch {
    // already gone
  }
}

function promptFor(role: Role, phaseId: string, headSha: string): string {
  if (role === "worker") {
    return [
      `You are the worker for phase ${phaseId} of a tiny, throwaway refactor.`,
      "The repository has exactly one file, notes.txt. Make one small, safe one-line edit to notes.txt using the edit or write tool.",
      "Then call submit_phase exactly once. Its arguments: assumptions (an array of strings, may be empty), deviations (an array of strings, may be empty), and decisions (an array with exactly one item) describing the edit you made.",
      "Each decision needs: choice (one plain sentence naming the choice), whyItMatters (why it matters for keeping notes.txt accurate, not implementation detail), alternatives (at least one item, each with option and consequence), recommendation (choice and reason), and classProposal (one of \"detail\", \"delegated\", \"reserved\" — this edit is a \"detail\").",
      "Do not call submit_discovery or submit_review. Call submit_phase only once.",
    ].join("\n");
  }
  return [
    `You are reviewer A for phase ${phaseId}. The repository has exactly one file, notes.txt, at commit ${headSha}. Read it with the read tool.`,
    "First call submit_discovery exactly once with the behavioral choices you see in notes.txt's content, as discoveries (an array). Each discovery needs: choice, whyItMatters, alternatives (at least one, each with option and consequence), recommendation (choice and reason), and classProposal (one of \"detail\", \"delegated\", \"reserved\").",
    `After submit_discovery returns, call submit_review exactly once with: reviewer set to the exact string "A", phaseId set to the exact string "${phaseId}", candidateSha set to the exact string "${headSha}", contractVersion set to exactly {"snapshot": ${CV_FIXTURE.snapshot}, "sectionSha256": "${CV_FIXTURE.sectionSha256}"}, correctionStatements set to an empty array, and findingStatements set to an empty array.`,
    "Do not call submit_phase.",
  ].join("\n");
}

function runOneRole(role: Role): Promise<RoleRecord> {
  return new Promise((resolve) => {
    const startedAt = Date.now();
    const phaseId = "p1";
    const { dir, headSha } = makeRepo();
    const stub = startConductorStub(phaseId, headSha);
    const eventCounts: Record<string, number> = {};
    let tokenUsage: unknown;
    const decoder = new JSONLDecoder();

    const record: RoleRecord = {
      piVersion: PI_VERSION,
      provider: PROVIDER,
      model: MODEL,
      role,
      reportedTools: [],
      toolAssertion: undefined,
      submissions: [],
      eventCounts,
      durationMs: 0,
      outcome: "error",
    };

    function finish(outcome: RoleRecord["outcome"], notes?: string): void {
      record.durationMs = Date.now() - startedAt;
      record.outcome = outcome;
      record.notes = notes;
      record.submissions = stub.submissions.map((s) => ({ tool: s.tool, args: s.args, valid: s.validation.valid, errors: s.validation.errors }));
      record.tokenUsage = tokenUsage;
      stub.server.close();
      resolve(record);
    }

    const args = launchArgs(role, {
      extensionPath: defaultExtensionPath(),
      provider: PROVIDER,
      model: MODEL,
    });
    const child = spawn("pi", args, {
      cwd: dir,
      env: { ...process.env, TT_SOCKET: stub.socketPath, TT_AGENT_ID: `${role}-live`, TT_ROLE: role },
      stdio: ["pipe", "pipe", "pipe"],
      detached: true,
    });

    let settled = false;
    const overallTimer = setTimeout(() => {
      if (settled) return;
      settled = true;
      try {
        child.stdin.write(encodeLine({ type: "abort", id: "timeout-abort" }));
      } catch {
        // ignore
      }
      setTimeout(() => {
        killGroup(child);
        finish("timed-out", `no completion within ${ROLE_BUDGET_MS}ms`);
      }, 5000);
    }, ROLE_BUDGET_MS);

    child.stdout.on("data", (chunk) => {
      const events = decoder.push(chunk) as Array<Record<string, unknown>>;
      for (const ev of events) {
        const type = String(ev.type ?? "unknown");
        eventCounts[type] = (eventCounts[type] ?? 0) + 1;
        if (type === "message_update") {
          const u = (ev as { usage?: unknown }).usage;
          if (u) tokenUsage = u;
        }
        if (type === "agent_settled" && !settled) {
          const requiredTools = role === "worker" ? ["submit_phase"] : ["submit_discovery", "submit_review"];
          const gotAll = requiredTools.every((t) => stub.submissions.some((s) => s.tool === t && s.validation.valid));
          if (gotAll) {
            settled = true;
            clearTimeout(overallTimer);
            killGroup(child);
            finish("ok");
          }
          // else: agent settled without a valid submission of everything
          // required yet — keep waiting for the overall timer; a real
          // conductor would nudge it to continue (agent_before_settle,
          // phase 1). This packet has no such guard.
        }
      }
    });

    let stderrBuf = "";
    child.stderr.on("data", (d) => {
      stderrBuf += d.toString();
    });

    stub.hello
      .then(async (hello) => {
        record.reportedTools = hello.tools;
        const result = assertToolSet(role, hello.tools);
        record.toolAssertion = result;
        if (!result.ok) {
          clearTimeout(overallTimer);
          killGroup(child);
          finish("tool-set-mismatch", `refused to prompt: ${JSON.stringify(result)}`);
          return;
        }
        child.stdin.write(encodeLine({ type: "prompt", id: "live-1", message: promptFor(role, phaseId, headSha) }));
      })
      .catch((err) => {
        clearTimeout(overallTimer);
        killGroup(child);
        finish("error", `hello never arrived: ${String(err)}; stderr: ${stderrBuf}`);
      });

    child.once("exit", () => {
      if (!settled) {
        settled = true;
        clearTimeout(overallTimer);
        const requiredTools = role === "worker" ? ["submit_phase"] : ["submit_discovery", "submit_review"];
        const gotAll = requiredTools.every((t) => stub.submissions.some((s) => s.tool === t && s.validation.valid));
        finish(gotAll ? "ok" : "no-valid-submission", `pi exited early; stderr: ${stderrBuf}`);
      }
    });
  });
}

function runLiveSuite(): void {
  test(
    "live-submission: real Pi + real model, worker and reviewer roles submit schema-valid payloads",
    { timeout: 2 * ROLE_BUDGET_MS + 30000 },
    async () => {
      const recordsDir = new URL("./records/phase-0/", import.meta.url).pathname;
      mkdirSync(recordsDir, { recursive: true });

      for (const role of Object.keys(ROLE_TOOLS) as Role[]) {
        const record = await runOneRole(role);
        const stamp = new Date().toISOString().replace(/[:.]/g, "-");
        writeFileSync(join(recordsDir, `${stamp}-${role}.json`), JSON.stringify(record, null, 2));

        assert.ok(record.toolAssertion && (record.toolAssertion as { ok: boolean }).ok, `${role}: tool assertion must hold before prompting (got ${JSON.stringify(record.toolAssertion)})`);

        const requiredTools = role === "worker" ? ["submit_phase"] : ["submit_discovery", "submit_review"];
        for (const tool of requiredTools) {
          const hasValid = record.submissions.some((s) => s.tool === tool && s.valid);
          assert.ok(
            hasValid,
            `${role}: expected at least one schema-valid ${tool} submission; got outcome=${record.outcome} submissions=${JSON.stringify(record.submissions)} notes=${record.notes}`,
          );
        }
      }
    },
  );
}
