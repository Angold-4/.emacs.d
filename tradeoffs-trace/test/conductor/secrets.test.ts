// Plan 01a (design goal 1, runtime doc §7): a plan declares its secrets by
// name (`#+TT_SECRETS: FAKE_KEY`), the conductor resolves each value from its
// OWN environment, hands it to every agent, and replaces every value with
// `***NAME***` in everything it writes. This is the run-level acceptance
// test: a fake worker echoes `$FAKE_KEY` and also sends a command containing
// the literal value.

import assert from "node:assert/strict";
import { spawn } from "node:child_process";
import * as fs from "node:fs";
import * as path from "node:path";
import { fileURLToPath } from "node:url";
import { randomBytes } from "node:crypto";
import { test } from "node:test";

import { cleanupDir, defaultReviewerHello, defaultWorkerHello, makeRepo, makeRunRoot, setupConductor, waitFor } from "./harness.ts";
import {
  buildContract,
  buildReviewerPrompt,
  buildWorkerPrompt,
  createRun,
  runPaths,
  runReferences,
  type RunPlanFile,
} from "../../src/conductor.ts";
import { EventLog } from "../../src/effects/log.ts";
import { resolveSecrets } from "../../src/effects/secrets.ts";
import type { PhaseState } from "../../src/core/types.ts";

const CLI_PATH = fileURLToPath(new URL("../../src/cli.ts", import.meta.url));

/** `tt <args>` in a child process (cli.ts runs `main()` on import, so it can
 * never be imported by a test). */
function runCli(args: string[], env: NodeJS.ProcessEnv = {}): Promise<{ code: number | null; stdout: string }> {
  return new Promise((resolve, reject) => {
    const child = spawn(process.execPath, [CLI_PATH, ...args], { env: { ...process.env, ...env } });
    let stdout = "";
    child.stdout.on("data", (c) => (stdout += c.toString()));
    child.once("error", reject);
    child.once("exit", (code) => resolve({ code, stdout }));
  });
}

/** Every regular file under DIR. */
function filesUnder(dir: string): string[] {
  const out: string[] = [];
  for (const entry of fs.readdirSync(dir, { withFileTypes: true })) {
    const p = path.join(dir, entry.name);
    if (entry.isDirectory()) out.push(...filesUnder(p));
    else if (entry.isFile()) out.push(p);
  }
  return out;
}

test("secrets: the prompt builders name the declared secrets and say to use $NAME", () => {
  const contract = buildContract({ id: "p1", goal: "g", acceptance: ["a"], checks: ["true"], boundaries: [], reserved: [] });
  const worker = buildWorkerPrompt(contract, undefined, undefined, undefined, [], ["FAKE_KEY", "OTHER_KEY"]);
  assert.match(worker, /declares FAKE_KEY, OTHER_KEY/);
  assert.match(worker, /\$NAME/);
  assert.match(worker, /\$FAKE_KEY, \$OTHER_KEY/);
  assert.match(worker, /never paste a value/);
  assert.ok(!buildWorkerPrompt(contract).includes("Secrets:"), "no declared secrets, no such section");

  const phase = {
    phaseId: "p1",
    candidate: { sha: "abc1234" },
    contract,
    decisions: [],
    findings: [],
    corrections: [],
    ballots: [],
  } as unknown as PhaseState;
  assert.match(buildReviewerPrompt(phase, "M", ["FAKE_KEY"]), /\$FAKE_KEY/);
  assert.ok(!buildReviewerPrompt(phase, "M").includes("Secrets:"));
});

test("secrets: a declared secret never reaches disk, a literal command is refused, and the agents get it", async () => {
  const value = `tt-${randomBytes(12).toString("hex")}`;
  const planValue = "a secret that is only ever a name in the plan";
  process.env.FAKE_KEY = value;
  process.env.FAKE_PLAN_ONLY = planValue;
  const scriptsDir = fs.mkdtempSync("/tmp/tt-secret-logs-");
  const workerPromptLog = path.join(scriptsDir, "worker.prompts");
  const reviewerPromptLog = path.join(scriptsDir, "reviewer.prompts");

  let setup: Awaited<ReturnType<typeof setupConductor>> | undefined;
  try {
    setup = await setupConductor({
      checks: ["true"],
      secrets: ["FAKE_KEY"],
      workerScript: () => ({
        hello: defaultWorkerHello(),
        steps: [
          // The conductor's own environment, not the agent's, is what a `sh`
          // command here inherits — so this echo is what a pasted-value run
          // looked like in the measured evidence (runtime doc §7).
          { kind: "call-sh", command: "echo $FAKE_KEY" },
          // What THIS agent process's environment carries.
          { kind: "emit-env", name: "FAKE_KEY" },
          // The value pasted into a command: the guard must refuse it and
          // name the variable to use instead.
          { kind: "call-sh", command: `echo ${value} > literal.txt` },
          { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
        ],
      }),
      reviewerScriptFor: (reviewer, state) => ({
        hello: defaultReviewerHello(),
        steps: [
          {
            kind: "call-submit",
            tool: "submit_review",
            args: {
              reviewer,
              phaseId: state.phase.phaseId,
              candidateSha: state.phase.candidate?.sha,
              contractVersion: state.phase.contract.contractVersion,
              correctionStatements: [],
              findingStatements: [],
            },
          },
        ],
      }),
      extraWorkerEnv: { FAKE_PI_PROMPT_LOG: workerPromptLog },
      extraReviewerEnv: () => ({ FAKE_PI_PROMPT_LOG: reviewerPromptLog }),
      deadlines: { abortGraceMs: 500, termGraceMs: 500, helloTimeoutMs: 5_000, reviewMs: 10_000 },
    });

    await setup.conductor.start();
    await waitFor(() => setup.conductor.state.phase.phase === "DONE", 90_000);
    const runDir = setup.runDir;

    // 1. No occurrence of the value in any file under the run directory.
    const offenders: string[] = [];
    for (const file of filesUnder(runDir)) {
      if (fs.readFileSync(file).includes(value)) offenders.push(path.relative(runDir, file));
    }
    assert.deepEqual(offenders, [], "the value must not survive anywhere under the run directory");

    // 2. The literal-value command was refused, naming the variable to use.
    const streams = fs
      .readdirSync(runPaths(runDir).stream)
      .map((f) => fs.readFileSync(path.join(runPaths(runDir).stream, f), "utf8"))
      .join("\n");
    assert.match(streams, /contains the value of the secret FAKE_KEY/);
    assert.match(streams, /contains the value of the secret FAKE_KEY[\s\S]{0,200}\$FAKE_KEY/);
    assert.ok(!fs.existsSync(path.join(runPaths(runDir).worktree, "literal.txt")), "the refused command never ran");

    // 3. The value is in the agent's environment (and redacted in the stream).
    assert.match(streams, /FAKE_KEY=\*\*\*FAKE_KEY\*\*\*/);
    // …and the echoed output was redacted rather than lost.
    assert.match(streams, /echo \$FAKE_KEY/);
    assert.match(streams, /\*\*\*FAKE_KEY\*\*\*/);

    // 4. Both prompts name the secret and say to reference it by name.
    for (const log of [workerPromptLog, reviewerPromptLog]) {
      const prompts = fs.readFileSync(log, "utf8");
      assert.match(prompts, /declares FAKE_KEY/);
      assert.match(prompts, /\$FAKE_KEY/);
      assert.match(prompts, /never paste a value/);
      assert.ok(!prompts.includes(value), "a value must never reach a prompt");
      // A secret the plan does not declare is neither named nor passed on.
      assert.ok(!prompts.includes("FAKE_PLAN_ONLY"));
    }
  } finally {
    await setup?.conductor.stop();
    if (setup) {
      cleanupDir(setup.runRoot);
      cleanupDir(setup.scriptsDir);
      cleanupDir(setup.repo.dir);
    }
    fs.rmSync(scriptsDir, { recursive: true, force: true });
    delete process.env.FAKE_KEY;
    delete process.env.FAKE_PLAN_ONLY;
  }
});

test("secrets: a reference document that quotes a key is copied into refs/ redacted", () => {
  const repo = makeRepo();
  const root = makeRunRoot();
  const docs = fs.mkdtempSync("/tmp/tt-secret-refs-");
  const value = `tt-${randomBytes(12).toString("hex")}`;
  process.env.FAKE_KEY = value;
  try {
    const src = path.join(docs, "01_ref_vendor.md");
    fs.writeFileSync(src, `# vendor\nPYTH_ACCESS_TOKEN='${value}' is the key\n`);
    // A doc saved as UTF-16 — and mostly non-Latin-1 (an English heading plus
    // CJK), which has NUL bytes but no every-other-byte pattern, so only the
    // BOM identifies it (finding B-10) — plus a genuinely binary artefact: the
    // first two are searched and copied (masked where they quote the key,
    // copied as-is where they do not), while a file whose bytes cannot be
    // searched is never handed to every agent (finding M-4).
    const utf16Text = `\ufeffVendor notes / 日本語の参考文書: key ${value} end`;
    const utf16 = path.join(docs, "02_ref_utf16.md");
    fs.writeFileSync(utf16, Buffer.from(utf16Text, "utf16le"));
    const cleanText = "\ufeffVendor notes / 日本語の参考文書（鍵は引用しない）";
    const cleanUtf16 = path.join(docs, "04_ref_clean_utf16.md");
    fs.writeFileSync(cleanUtf16, Buffer.from(cleanText, "utf16le"));
    const binary = path.join(docs, "03_ref_binary.bin");
    fs.writeFileSync(binary, Buffer.from([0x00, 0x01, 0x02, 0x00, 0xfe, 0xff]));
    const plan: RunPlanFile = {
      title: "refs",
      repo: repo.dir,
      integrationBranch: "main",
      checks: ["true"],
      secrets: ["FAKE_KEY"],
      references: [src, utf16, cleanUtf16, binary],
      phases: [{ id: "p1", goal: "g", acceptance: ["a"], checks: ["true"], boundaries: [], reserved: [] }],
    };
    const runDir = createRun(root, plan);
    const refs = runReferences(runDir);
    const copy = refs.find((r) => r.endsWith("01_ref_vendor.md"))!;
    const text = fs.readFileSync(copy, "utf8");
    assert.match(text, /is the key/);
    assert.match(text, /\*\*\*FAKE_KEY\*\*\*/);
    assert.ok(!text.includes(value), "the copy in refs/ must not hold the value");
    const utf16Copy = refs.find((r) => r.endsWith("02_ref_utf16.md"))!;
    assert.equal(
      fs.readFileSync(utf16Copy, "utf16le"),
      utf16Text.split(value).join("***FAKE_KEY***"),
      "a non-Latin-1 UTF-16 document is masked in place, not skipped",
    );
    const cleanCopy = refs.find((r) => r.endsWith("04_ref_clean_utf16.md"));
    assert.ok(cleanCopy, "a UTF-16 document that quotes nothing is still searched, so it is copied");
    assert.equal(fs.readFileSync(cleanCopy!, "utf16le"), cleanText);
    assert.ok(!refs.some((r) => r.endsWith("03_ref_binary.bin")), "an unsearchable binary doc is not copied");
    const missingNote = fs.readFileSync(path.join(runDir, "refs", "MISSING.txt"), "utf8");
    assert.match(missingNote, /03_ref_binary\.bin \(binary, not copied/);
    assert.ok(!missingNote.includes("04_ref_clean_utf16"), "a clean UTF-16 doc is neither missing nor refused");
    // The source documents themselves are the owner's, and untouched.
    assert.ok(fs.readFileSync(src, "utf8").includes(value));
  } finally {
    delete process.env.FAKE_KEY;
    cleanupDir(root);
    cleanupDir(repo.dir);
    fs.rmSync(docs, { recursive: true, force: true });
  }
});

test("secrets: a declared but unset secret withholds the reference documents instead of copying them raw", () => {
  const repo = makeRepo();
  const root = makeRunRoot();
  const docs = fs.mkdtempSync("/tmp/tt-secret-refs-unset-");
  const value = `tt-${randomBytes(12).toString("hex")}`;
  const previous = process.env.FAKE_KEY;
  delete process.env.FAKE_KEY; // the plan declares it; this shell does not export it
  try {
    const src = path.join(docs, "01_ref_vendor.md");
    fs.writeFileSync(src, `# vendor\nPYTH_ACCESS_TOKEN='${value}' is the key\n`);
    const plan: RunPlanFile = {
      title: "refs",
      repo: repo.dir,
      integrationBranch: "main",
      checks: ["true"],
      secrets: ["FAKE_KEY"],
      references: [src],
      phases: [{ id: "p1", goal: "g", acceptance: ["a"], checks: ["true"], boundaries: [], reserved: [] }],
    };
    const runDir = createRun(root, plan);
    // Nothing under the run directory may hold the value: with the declared
    // value unknown there is nothing to mask, so the document is not copied at
    // all and is named, with the reason, in MISSING.txt (finding M-13).
    assert.ok(!runReferences(runDir).some((r) => r.endsWith("01_ref_vendor.md")), "no raw copy is written");
    const note = fs.readFileSync(path.join(runDir, "refs", "MISSING.txt"), "utf8");
    assert.match(note, /01_ref_vendor\.md \(not copied: FAKE_KEY could not be masked/);
    for (const file of filesUnder(runDir)) {
      assert.ok(!fs.readFileSync(file).includes(value), `${path.relative(runDir, file)} holds the value`);
    }
    // A plan that declares nothing keeps its references exactly as before.
    const plain = createRun(root, { ...plan, secrets: undefined, title: "plain" });
    assert.ok(runReferences(plain).some((r) => r.endsWith("01_ref_vendor.md")), "no declaration, no withholding");
  } finally {
    if (previous !== undefined) process.env.FAKE_KEY = previous;
    cleanupDir(root);
    cleanupDir(repo.dir);
    fs.rmSync(docs, { recursive: true, force: true });
  }
});

test("secrets: a value the worker discloses reaches neither the conductor's state nor the next prompt", async () => {
  const value = `tt-${randomBytes(12).toString("hex")}`;
  process.env.FAKE_KEY = value;
  const scriptsDir = fs.mkdtempSync("/tmp/tt-secret-state-");
  const promptLog = path.join(scriptsDir, "worker.prompts");
  let setup: Awaited<ReturnType<typeof setupConductor>> | undefined;
  try {
    setup = await setupConductor({
      // Every candidate fails its checks, so the phase repairs: attempt 2's
      // prompt is built from the conductor's in-memory state (the repair
      // request lists attempt 1's disclosed decisions verbatim).
      checks: ["false"],
      secrets: ["FAKE_KEY"],
      workerScriptForAttempt: (attempt) => ({
        hello: defaultWorkerHello(),
        steps: [
          {
            kind: "call-submit",
            tool: "submit_phase",
            args: {
              decisions:
                attempt === 1
                  ? [
                      {
                        choice: `Read the vendor key ${value} from the environment`,
                        whyItMatters: `the key ${value} must not be pasted into code`,
                        alternatives: [{ option: `hard-code ${value}`, consequence: "the key is in the repository" }],
                        recommendation: { choice: "read it from the environment", reason: "the value never enters the tree" },
                        classProposal: "delegated",
                      },
                    ]
                  : [],
              assumptions: [],
              deviations: [],
            },
          },
        ],
      }),
      reviewerScriptFor: (reviewer, state) => ({
        hello: defaultReviewerHello(),
        steps: [
          {
            kind: "call-submit",
            tool: "submit_review",
            args: {
              reviewer,
              phaseId: state.phase.phaseId,
              candidateSha: state.phase.candidate?.sha,
              contractVersion: state.phase.contract.contractVersion,
              correctionStatements: [],
              findingStatements: [],
            },
          },
        ],
      }),
      extraWorkerEnv: { FAKE_PI_PROMPT_LOG: promptLog },
      deadlines: { abortGraceMs: 500, termGraceMs: 500, helloTimeoutMs: 5_000, reviewMs: 10_000, checkMs: 20_000 },
    });
    const conductor = setup.conductor;

    await conductor.start();
    // Attempt 2 exists once the first repair round started.
    await waitFor(() => (conductor.state.phase.attempt.n ?? 1) >= 2, 120_000);
    await waitFor(() => fs.existsSync(promptLog) && fs.readFileSync(promptLog, "utf8").includes("REPAIR"), 120_000);

    // 1. The in-memory state itself holds no value (blocking findings M-3/B-5:
    // the state is what every prompt and view is built from).
    const state = JSON.stringify(conductor.state);
    assert.ok(!state.includes(value), "the conductor's own state must hold the mask, not the value");
    assert.match(state, /\*\*\*FAKE_KEY\*\*\*/, "the disclosed text is still useful, masked");

    // 2. …and the repair prompt built from it carries no value, while the
    // worker is still told which records to keep, change or withdraw.
    const prompts = fs.readFileSync(promptLog, "utf8");
    assert.match(prompts, /REPAIR \(round/);
    assert.match(prompts, /\*\*\*FAKE_KEY\*\*\*/);
    assert.ok(!prompts.includes(value), "a value must never reach a prompt");
  } finally {
    await setup?.conductor.stop();
    if (setup) {
      cleanupDir(setup.runRoot);
      cleanupDir(setup.scriptsDir);
      cleanupDir(setup.repo.dir);
    }
    fs.rmSync(scriptsDir, { recursive: true, force: true });
    delete process.env.FAKE_KEY;
  }
});

test("secrets: a reproduction command carrying the value is refused, not run", async () => {
  const value = `tt-${randomBytes(12).toString("hex")}`;
  process.env.FAKE_KEY = value;
  let setup: Awaited<ReturnType<typeof setupConductor>> | undefined;
  try {
    setup = await setupConductor({
      checks: ["true"],
      secrets: ["FAKE_KEY"],
      workerScript: () => ({
        hello: defaultWorkerHello(),
        steps: [{ kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } }],
      }),
      reviewerScriptFor: (reviewer, state) => ({
        hello: defaultReviewerHello(),
        steps: [
          {
            kind: "call-submit",
            tool: "submit_review",
            args: {
              reviewer,
              phaseId: state.phase.phaseId,
              candidateSha: state.phase.candidate?.sha,
              contractVersion: state.phase.contract.contractVersion,
              correctionStatements: [],
              findingStatements: [],
              findings: [
                {
                  kind: "defect",
                  severity: "advisory",
                  evidence: "a reproduction is offered",
                  // A command the conductor would run — the same literal value
                  // the `sh` guard refuses, submitted through a different door
                  // (blocking finding B-6).
                  reproduction: { command: `echo ${value} >> repro-ran.txt` },
                },
              ],
            },
          },
          {
            kind: "call-submit",
            tool: "submit_review",
            args: {
              reviewer,
              phaseId: state.phase.phaseId,
              candidateSha: state.phase.candidate?.sha,
              contractVersion: state.phase.contract.contractVersion,
              correctionStatements: [],
              findingStatements: [],
            },
          },
        ],
      }),
      deadlines: { abortGraceMs: 500, termGraceMs: 500, helloTimeoutMs: 5_000, reviewMs: 10_000 },
    });

    await setup.conductor.start();
    const conductor = setup.conductor;
    await waitFor(() => conductor.state.phase.phase === "DONE", 120_000);

    // The refusal names the variable to use, and the command never ran.
    const streams = fs
      .readdirSync(runPaths(setup.runDir).stream)
      .map((f) => fs.readFileSync(path.join(runPaths(setup.runDir).stream, f), "utf8"))
      .join("\n");
    assert.match(streams, /contains the value of the secret FAKE_KEY[\s\S]{0,200}\$FAKE_KEY/);
    assert.ok(!fs.existsSync(path.join(runPaths(setup.runDir).worktree, "repro-ran.txt")), "the refused reproduction never ran");
    // No finding carries it either: the submission was rejected as a whole.
    assert.deepEqual(conductor.state.phase.findings, []);
    // …and no file under the run directory holds the value.
    for (const file of filesUnder(setup.runDir)) {
      assert.ok(!fs.readFileSync(file).includes(value), `${path.relative(setup.runDir, file)} holds the value`);
    }
  } finally {
    await setup?.conductor.stop();
    if (setup) {
      cleanupDir(setup.runRoot);
      cleanupDir(setup.scriptsDir);
      cleanupDir(setup.repo.dir);
    }
    delete process.env.FAKE_KEY;
  }
});

test("secrets: plan text and outgoing prompts are redacted at the choke points, and the check still runs", async () => {
  const value = `tt-${randomBytes(12).toString("hex")}`;
  process.env.FAKE_KEY = value;
  const scriptsDir = fs.mkdtempSync("/tmp/tt-secret-plan-");
  const workerPromptLog = path.join(scriptsDir, "worker.prompts");
  const reviewerPromptLog = path.join(scriptsDir, "reviewer.prompts");
  let setup: Awaited<ReturnType<typeof setupConductor>> | undefined;
  try {
    setup = await setupConductor({
      // The plan's own prose carries the value: its goal, and a check line that
      // pastes it inline the way the measured runs did (runtime doc §7).
      goal: `Read the vendor key ${value} from the environment, never paste it`,
      globalChecks: [`test "$FAKE_KEY" = '${value}'`],
      phaseChecks: ["true"],
      secrets: ["FAKE_KEY"],
      workerScript: () => ({
        hello: defaultWorkerHello(),
        steps: [{ kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } }],
      }),
      reviewerScriptFor: (reviewer, state) => ({
        hello: defaultReviewerHello(),
        steps: [
          {
            kind: "call-submit",
            tool: "submit_review",
            args: {
              reviewer,
              phaseId: state.phase.phaseId,
              candidateSha: state.phase.candidate?.sha,
              contractVersion: state.phase.contract.contractVersion,
              correctionStatements: [],
              findingStatements: [],
            },
          },
        ],
      }),
      extraWorkerEnv: { FAKE_PI_PROMPT_LOG: workerPromptLog },
      extraReviewerEnv: () => ({ FAKE_PI_PROMPT_LOG: reviewerPromptLog }),
      deadlines: { abortGraceMs: 500, termGraceMs: 500, helloTimeoutMs: 5_000, reviewMs: 10_000 },
    });

    await setup.conductor.start();
    const conductor = setup.conductor;
    await waitFor(() => conductor.state.phase.phase === "DONE", 120_000);

    // Choke point 1: the run's plan snapshot is written redacted.
    const snapshot = fs.readFileSync(path.join(runPaths(setup.runDir).plan, "v1.json"), "utf8");
    assert.ok(!snapshot.includes(value), "plan/v1.json must not hold the value");
    assert.match(snapshot, /\*\*\*FAKE_KEY\*\*\*/);
    assert.doesNotThrow(() => JSON.parse(snapshot));

    // Choke point 2: every outgoing prompt is redacted before it reaches an
    // agent, for the worker and for the reviewers.
    for (const log of [workerPromptLog, reviewerPromptLog]) {
      const prompts = fs.readFileSync(log, "utf8");
      assert.ok(!prompts.includes(value), "a value must never reach a prompt");
      assert.match(prompts, /\*\*\*FAKE_KEY\*\*\*/, "the plan's prose is still readable, masked");
    }

    // Nothing under the run directory holds it either.
    for (const file of filesUnder(setup.runDir)) {
      assert.ok(!fs.readFileSync(file).includes(value), `${path.relative(setup.runDir, file)} holds the value`);
    }

    // …and the check still ran with the real value from the environment: its
    // command is `test "$FAKE_KEY" = '<value>'`, so it passes only if the
    // conductor's environment carried the key and the command ran unmasked.
    assert.equal(conductor.state.phase.checks?.passed, true, "the inline-value check must still pass");
  } finally {
    await setup?.conductor.stop();
    if (setup) {
      cleanupDir(setup.runRoot);
      cleanupDir(setup.scriptsDir);
      cleanupDir(setup.repo.dir);
    }
    fs.rmSync(scriptsDir, { recursive: true, force: true });
    delete process.env.FAKE_KEY;
  }
});

test("secrets: the live writers mask a value used as a JSON key, and never a number", async () => {
  const value = `tt-${randomBytes(12).toString("hex")}`;
  process.env.FAKE_KEY = value;
  const root = fs.mkdtempSync("/tmp/tt-secret-live-");
  const { maskable } = resolveSecrets(["FAKE_KEY"]);
  let setup: Awaited<ReturnType<typeof setupConductor>> | undefined;
  try {
    // EventLog.append (events.jsonl) — a value used as a JSON key.
    const logPath = path.join(root, "events.jsonl");
    const log = new EventLog(logPath, maskable);
    log.append("event", { type: "X", args: { [value]: "1" }, n: 42 });
    log.close();
    const logged = fs.readFileSync(logPath, "utf8");
    assert.ok(!logged.includes(value), "EventLog must mask a value used as a key");
    const loggedRecord = JSON.parse(logged.trim()) as { event: { args: Record<string, string>; n: number } };
    assert.deepEqual(loggedRecord.event.args, { "***FAKE_KEY***": "1" });
    assert.equal(loggedRecord.event.n, 42, "a number is never touched");

    // The stream writer: a fake-pi event whose args are keyed BY the value.
    setup = await setupConductor({
      checks: ["true"],
      secrets: ["FAKE_KEY"],
      workerScript: () => ({
        hello: defaultWorkerHello(),
        steps: [
          { kind: "emit", event: { type: "custom", args: { [value]: "1" }, n: 42 } },
          { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
        ],
      }),
      reviewerScriptFor: (reviewer, state) => ({
        hello: defaultReviewerHello(),
        steps: [
          {
            kind: "call-submit",
            tool: "submit_review",
            args: {
              reviewer,
              phaseId: state.phase.phaseId,
              candidateSha: state.phase.candidate?.sha,
              contractVersion: state.phase.contract.contractVersion,
              correctionStatements: [],
              findingStatements: [],
            },
          },
        ],
      }),
      deadlines: { abortGraceMs: 500, termGraceMs: 500, helloTimeoutMs: 5_000, reviewMs: 10_000 },
    });
    await setup.conductor.start();
    const conductor = setup.conductor;
    await waitFor(() => conductor.state.phase.phase === "DONE", 120_000);
    const streams = fs
      .readdirSync(runPaths(setup.runDir).stream)
      .map((f) => fs.readFileSync(path.join(runPaths(setup.runDir).stream, f), "utf8"))
      .join("\n");
    assert.ok(!streams.includes(value), "the stream writer must mask a value used as a key");
    assert.match(streams, /\*\*\*FAKE_KEY\*\*\*/);
    for (const line of streams.split("\n")) {
      if (line.length === 0) continue;
      assert.doesNotThrow(() => JSON.parse(line), "every stream line still parses");
    }
  } finally {
    await setup?.conductor.stop();
    if (setup) {
      cleanupDir(setup.runRoot);
      cleanupDir(setup.scriptsDir);
      cleanupDir(setup.repo.dir);
    }
    fs.rmSync(root, { recursive: true, force: true });
    delete process.env.FAKE_KEY;
  }
});

test("secrets: an unset declared secret is recorded and the run still runs", async () => {
  delete process.env.FAKE_KEY;
  let setup: Awaited<ReturnType<typeof setupConductor>> | undefined;
  try {
    setup = await setupConductor({
      checks: ["true"],
      secrets: ["FAKE_KEY"],
      workerScript: () => ({
        hello: defaultWorkerHello(),
        steps: [{ kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } }],
      }),
      reviewerScriptFor: (reviewer, state) => ({
        hello: defaultReviewerHello(),
        steps: [
          {
            kind: "call-submit",
            tool: "submit_review",
            args: {
              reviewer,
              phaseId: state.phase.phaseId,
              candidateSha: state.phase.candidate?.sha,
              contractVersion: state.phase.contract.contractVersion,
              correctionStatements: [],
              findingStatements: [],
            },
          },
        ],
      }),
      deadlines: { abortGraceMs: 500, termGraceMs: 500, helloTimeoutMs: 5_000, reviewMs: 10_000 },
    });

    await setup.conductor.start();
    await waitFor(() => setup.conductor.state.phase.phase === "DONE", 90_000);

    // `tt status` shows the unset declared secret, and the run still ran.
    const status = await runCli(["status", setup.runDir]);
    assert.equal(status.code, 0);
    assert.match(status.stdout, /^secret FAKE_KEY not set$/m, `expected the missing secret in:\n${status.stdout}`);
  } finally {
    await setup?.conductor.stop();
    if (setup) {
      cleanupDir(setup.runRoot);
      cleanupDir(setup.scriptsDir);
      cleanupDir(setup.repo.dir);
    }
  }
});
