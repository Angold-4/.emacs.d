// Plan 06m (A5/A6/R6/R7/C1): `tt deliver` turns accepted phases into one
// commit per layer on its parent, pushes with the ledger lease, opens or
// updates each stacked PR and links the stack; `tt restack` replays the
// delivered stack onto a new base, resolving only declared generated paths
// hunk by hunk. Both change nothing without `--apply`. A real local bare
// remote and a fake `gh` stand in for GitHub.

import assert from "node:assert/strict";
import { execFileSync } from "node:child_process";
import * as fs from "node:fs";
import * as os from "node:os";
import * as path from "node:path";
import { fileURLToPath } from "node:url";
import { test } from "node:test";

import { defaultCmd, deliver, planDelivery, readDeliveryLedger, restack, type Cmd } from "../../src/effects/deliver.ts";
import type { RunPlanFile } from "../../src/conductor.ts";
import type { ProgramFile } from "../../src/core/program.ts";
import type { State } from "../../src/core/types.ts";

function git(args: string[], cwd: string): string {
  return execFileSync("git", args, { cwd, encoding: "utf8", stdio: ["ignore", "pipe", "pipe"] }).trim();
}

function commitFile(repo: string, parent: string, file: string, contents: string, message: string): string {
  const dir = fs.mkdtempSync(path.join("/tmp", "tt-deliver-tree-"));
  try {
    // Build the tree with a temporary index so no worktree is needed.
    const env = { ...process.env, GIT_INDEX_FILE: path.join(dir, "index") };
    execFileSync("git", ["-C", repo, "read-tree", parent], { env, encoding: "utf8" });
    const blob = execFileSync("git", ["-C", repo, "hash-object", "-w", "--stdin"], { input: contents, encoding: "utf8" }).trim();
    execFileSync("git", ["-C", repo, "update-index", "--add", "--cacheinfo", `100644,${blob},${file}`], { env, encoding: "utf8" });
    const tree = execFileSync("git", ["-C", repo, "write-tree"], { env, encoding: "utf8" }).trim();
    return execFileSync("git", ["-C", repo, "-c", "user.name=t", "-c", "user.email=t@t", "commit-tree", tree, "-p", parent, "-m", message], { encoding: "utf8" }).trim();
  } finally {
    fs.rmSync(dir, { recursive: true, force: true });
  }
}

function plan(repo: string, title: string, extra: Partial<RunPlanFile> = {}): RunPlanFile {
  return {
    title,
    repo,
    integrationBranch: "main",
    checks: ["true"],
    phases: [{ id: "p1", goal: `goal ${title}`, acceptance: ["it works"], checks: ["true"], boundaries: [], reserved: [] }],
    ...extra,
  };
}

function writeProgram(dir: string, program: ProgramFile): void {
  fs.mkdirSync(dir, { recursive: true });
  fs.writeFileSync(path.join(dir, "program.json"), JSON.stringify(program, null, 2));
}

/** Mark every node started and DONE in the program's own event log, so
 * `foldProgram` sees the accepted layers the delivery reads. */
function markDone(programDir: string, ids: readonly string[]): void {
  const lines: string[] = [];
  for (const id of ids) {
    lines.push(JSON.stringify({ ts: new Date().toISOString(), event: { type: "NODE_STARTED", node: id, runId: `run-${id}`, branch: `main--${id}`, base: "main" } }));
    lines.push(JSON.stringify({ ts: new Date().toISOString(), event: { type: "NODE_STATUS", node: id, status: "done" } }));
  }
  fs.writeFileSync(path.join(programDir, "events.jsonl"), `${lines.join("\n")}\n`);
}

/** A stateful fake `gh`: `pr view <branch>` succeeds once a PR exists for
 * the branch; `pr create`/`pr edit` record the argv and the stdin body per
 * branch; a `fail-<branch>` marker makes create/edit for that branch fail.
 * Every call is appended to the log. */
function fakeGh(root: string): { gh: string; log: string; prs: string; body: (branch: string) => string; args: (branch: string) => string } {
  const prs = path.join(root, "prs");
  const log = path.join(root, "gh.log");
  const gh = path.join(root, "fake-gh");
  fs.mkdirSync(prs, { recursive: true });
  fs.writeFileSync(
    gh,
    [
      "#!/bin/sh",
      `echo "$@" >> ${log}`,
      `P=${prs}`,
      'if [ "$1" = "pr" ]; then',
      '  case "$2" in',
      '    view) [ -f "$P/$3.open" ] && exit 0; exit 1;;',
      '    create) head=""; prev=""; for a in "$@"; do [ "$prev" = "--head" ] && head="$a"; prev="$a"; done',
      '      if [ -f "$P/fail-$head" ]; then cat > /dev/null; echo "fake gh failure" >&2; exit 1; fi',
      '      cat > "$P/$head.body"; echo "$@" > "$P/$head.args"; touch "$P/$head.open"; exit 0;;',
      '    edit) if [ -f "$P/fail-$3" ]; then cat > /dev/null; echo "fake gh failure" >&2; exit 1; fi',
      '      cat > "$P/$3.body"; echo "$@" > "$P/$3.args"; exit 0;;',
      "  esac",
      "fi",
      "exit 0",
      "",
    ].join("\n"),
  );
  fs.chmodSync(gh, 0o755);
  return {
    gh,
    log,
    prs,
    body: (branch) => fs.readFileSync(path.join(prs, `${branch}.body`), "utf8"),
    args: (branch) => fs.readFileSync(path.join(prs, `${branch}.args`), "utf8").trim(),
  };
}

/** The `--force-with-lease=<branch>:<sha>` value of every push in CALLS. */
function pushLeases(calls: ReadonlyArray<{ file: string; args: string[] }>): Record<string, string> {
  const out: Record<string, string> = {};
  for (const c of calls) {
    if (c.file !== "git" || !c.args.includes("push")) continue;
    const lease = c.args.find((a) => a.startsWith("--force-with-lease="));
    if (!lease) continue;
    const [branch, sha] = lease.slice("--force-with-lease=".length).split(":");
    out[branch] = sha;
  }
  return out;
}

/** A DONE run state with an accepted candidate, for the injected reader. */
function doneState(candidateSha: string, decisions: unknown[] = []): State {
  return {
    run: "RUN_ACTIVE",
    phase: {
      phaseId: "p1",
      phase: "DONE",
      runId: "run",
      candidate: { sha: candidateSha, contractVersion: { snapshot: 1, sectionSha256: "x" } },
      checks: { candidateSha, passed: true },
      probe: { candidateSha, head: candidateSha, probedI: candidateSha, passed: true },
      messages: [],
      findings: [],
      decisions,
      ownerRequests: [],
      ownerInputs: [],
      corrections: [],
      overrides: [],
      reviews: {},
      contract: { contractVersion: { snapshot: 1, sectionSha256: "x" }, checks: ["true"], acceptance: ["it works"] },
    },
  } as unknown as State;
}

test("plan 06m: tt deliver --apply commits each accepted layer on its parent, pushes with ledger leases and links the stack", async () => {
  const root = fs.mkdtempSync(path.join("/tmp", "tt-deliver-"));
  const repo = path.join(root, "repo");
  const remote = path.join(root, "remote.git");
  const programDir = path.join(root, "programs", "prog");
  const runRoot = path.join(root, "runs");
  const bin = path.join(root, "bin");
  fs.mkdirSync(repo, { recursive: true });
  fs.mkdirSync(runRoot, { recursive: true });
  fs.mkdirSync(bin, { recursive: true });
  try {
    git(["init", "-q", "-b", "main"], repo);
    fs.writeFileSync(path.join(repo, "base.txt"), "base\n");
    git(["add", "-A"], repo);
    git(["-c", "user.name=t", "-c", "user.email=t@t", "commit", "-q", "-m", "base"], repo);
    const base = git(["rev-parse", "HEAD"], repo);
    execFileSync("git", ["init", "-q", "--bare", remote]);
    git(["remote", "add", "origin", remote], repo);
    git(["push", "-q", "origin", "main"], repo);

    const a = commitFile(repo, base, "a.txt", "a\n", "a");
    const b = commitFile(repo, a, "b.txt", "b\n", "b");
    const c = commitFile(repo, b, "c.txt", "c\n", "c");
    const treeOf = (sha: string) => git(["rev-parse", `${sha}^{tree}`], repo);

    const program: ProgramFile = {
      title: "stack",
      maxParallel: 1,
      entries: [
        { id: "a", after: [], plan: plan(repo, "layer a", { labels: ["tt", "auto"] }) },
        { id: "b", after: ["a"], plan: plan(repo, "layer b") },
        { id: "c", after: ["b"], plan: plan(repo, "layer c") },
      ],
    };
    writeProgram(programDir, program);
    markDone(programDir, ["a", "b", "c"]);
    for (const id of ["a", "b", "c"] as const) fs.mkdirSync(path.join(runRoot, `run-${id}`), { recursive: true });
    const candidates: Record<string, string> = { "run-a": a, "run-b": b, "run-c": c };
    const decisionsOf: Record<string, unknown[]> = {
      "run-b": [{ id: "D-7", class: "delegated", choice: "cap the retry loop at 64", status: "settled" }],
    };
    const readRun = (_runDir: string, _root: string) => ({ state: doneState(candidates[path.basename(_runDir)], decisionsOf[path.basename(_runDir)] ?? []) });

    const fake = fakeGh(root);
    const gh = fake.gh;
    const ghLog = fake.log;

    // Without --apply nothing changes.
    const dry = deliver({ programDir, runRoot, apply: false, gh, readRun });
    assert.equal(dry.applied, false);
    assert.ok(dry.messages.join("\n").includes("gh stack link --base main"));
    assert.ok(!fs.existsSync(ghLog), "no gh call without --apply");
    assert.ok(!git(["ls-remote", "--heads", remote], repo).includes("main--"), "nothing pushed without --apply");

    // With --apply: one commit per layer, each push carries the ledger lease.
    const calls: Array<{ file: string; args: string[] }> = [];
    const cmd: Cmd = (file, args, opts) => {
      calls.push({ file, args });
      return defaultCmd(file, args, opts);
    };
    const applied = deliver({ programDir, runRoot, apply: true, gh, cmd, readRun });
    assert.equal(applied.ok, true, applied.messages.join("\n"));
    const ledger = readDeliveryLedger(programDir);
    assert.equal(Object.keys(ledger.layers).length, 3);
    for (const id of ["a", "b", "c"]) {
      const entry = ledger.layers[id];
      assert.ok(entry, `layer ${id} is in the ledger`);
      const remoteSha = git(["--git-dir", remote, "rev-parse", `refs/heads/main--${id}`]);
      assert.equal(remoteSha, entry.sha, `layer ${id} pushed`);
      assert.equal(git(["rev-parse", `${entry.sha}^{tree}`], repo), treeOf(candidates[`run-${id}`]), `layer ${id} tree equals the accepted tree`);
    }
    // The parent of layer b's commit is layer a's delivered commit.
    assert.equal(git(["rev-parse", `${ledger.layers.b.sha}^`], repo), ledger.layers.a.sha, "b is committed on a");
    assert.equal(git(["rev-parse", `${ledger.layers.c.sha}^`], repo), ledger.layers.b.sha, "c is committed on b");
    // Each push carries the lease from the ledger (empty on the first push).
    const pushes = calls.filter((c) => c.args[0] === "push" || (c.args[0] === "-C" && c.args.includes("push")));
    for (const p of pushes) {
      const lease = p.args.find((a) => a.startsWith("--force-with-lease="));
      assert.ok(lease, `push carries a lease: ${p.args.join(" ")}`);
    }
    assert.ok(pushes[0].args.some((a) => a === "--force-with-lease=main--a:"), "the first push expects an empty ref");
    // Every push is leased to the ledger's recorded SHA: empty on a first
    // delivery (the branch must not exist yet).
    assert.deepEqual(pushLeases(calls), { "main--a": "", "main--b": "", "main--c": "" });
    // Plan 06m (A5, contract-92): each PR's base is its parent layer (the
    // root base for the first), and EVERY body has the whole-stack graph —
    // all three layers, this PR marked — and the trade-offs table, no footer.
    const expectedBase: Record<string, string> = { "main--a": "main", "main--b": "main--a", "main--c": "main--b" };
    for (const branch of ["main--a", "main--b", "main--c"]) {
      assert.match(fake.args(branch), new RegExp(`--base ${expectedBase[branch]} --head ${branch} `), `${branch}'s PR base is its parent layer`);
      const body = fake.body(branch);
      assert.match(body, /## Stack/);
      assert.match(body, /^base: main$/m, `${branch}'s graph starts at the root base`);
      for (const other of ["main--a", "main--b", "main--c"]) assert.ok(body.includes(`└─ ${other} (`), `${branch}'s body graph names ${other}`);
      assert.match(body, new RegExp(`└─ ${branch} \\([a-z]\\)  ← this PR`), `${branch}'s body marks its own layer`);
      assert.match(body, /## Trade-offs/);
      assert.doesNotMatch(body, /generated by|Generated-by|tradeoffs-trace/i, "no generated-by footer");
    }
    assert.match(fake.body("main--b"), /\| D-7 \| delegated \| cap the retry loop at 64 \|/, "the trade-offs table lists the layer's decisions");
    // The graph is a chain: b under a, c under b.
    const lines = fake.body("main--c").split("\n");
    const indentOf = (branch: string) => lines.find((l) => l.includes(`└─ ${branch} (`))!.indexOf("└─");
    assert.ok(indentOf("main--a") < indentOf("main--b") && indentOf("main--b") < indentOf("main--c"), "each layer sits under its parent");
    const ghCalls = fs.readFileSync(ghLog, "utf8");
    assert.match(ghCalls, /--base main --head main--a/);
    assert.match(ghCalls, /--base main--a --head main--b/);
    assert.match(ghCalls, /--label tt --label auto/, "the plan's labels are applied");
    assert.match(ghCalls, /stack link --base main/, "the stack is linked with the root base");

    // A second run changes nothing: no push, no PR edit.
    const before = git(["ls-remote", "--heads", remote], repo);
    const ghCallsBefore = fs.readFileSync(ghLog, "utf8");
    const again = deliver({ programDir, runRoot, apply: true, gh, cmd, readRun });
    assert.equal(again.ok, true);
    assert.equal(git(["ls-remote", "--heads", remote], repo), before, "a rerun pushes nothing");
    assert.deepEqual(readDeliveryLedger(programDir), ledger, "the ledger is unchanged");
    assert.doesNotMatch(fs.readFileSync(ghLog, "utf8").slice(ghCallsBefore.length), /^pr (create|edit)/m, "a rerun opens or edits no PR");
  } finally {
    fs.rmSync(root, { recursive: true, force: true });
  }
});

test("plan 06m: tt deliver reconciles every PR when a layer joins the stack, and a retried reconciliation after a restack onto a new branch keeps the ledger parents", async () => {
  const root = fs.mkdtempSync(path.join("/tmp", "tt-deliver-grow-"));
  const repo = path.join(root, "repo");
  const remote = path.join(root, "remote.git");
  const programDir = path.join(root, "programs", "prog");
  const runRoot = path.join(root, "runs");
  fs.mkdirSync(repo, { recursive: true });
  fs.mkdirSync(runRoot, { recursive: true });
  try {
    git(["init", "-q", "-b", "main"], repo);
    fs.writeFileSync(path.join(repo, "base.txt"), "base\n");
    git(["add", "-A"], repo);
    git(["-c", "user.name=t", "-c", "user.email=t@t", "commit", "-q", "-m", "base"], repo);
    const base = git(["rev-parse", "HEAD"], repo);
    execFileSync("git", ["init", "-q", "--bare", remote]);
    git(["remote", "add", "origin", remote], repo);
    git(["push", "-q", "origin", "main"], repo);
    const a = commitFile(repo, base, "a.txt", "a\n", "a");
    const b = commitFile(repo, a, "b.txt", "b\n", "b");
    const c = commitFile(repo, b, "c.txt", "c\n", "c");
    const program: ProgramFile = {
      title: "stack",
      maxParallel: 1,
      entries: [
        { id: "a", after: [], plan: plan(repo, "layer a") },
        { id: "b", after: ["a"], plan: plan(repo, "layer b") },
        { id: "c", after: ["b"], plan: plan(repo, "layer c") },
      ],
    };
    writeProgram(programDir, program);
    for (const id of ["a", "b", "c"]) fs.mkdirSync(path.join(runRoot, `run-${id}`), { recursive: true });
    const candidates: Record<string, string> = { "run-a": a, "run-b": b, "run-c": c };
    const readRun = (runDir: string) => ({ state: doneState(candidates[path.basename(runDir)]) });
    const fake = fakeGh(root);
    const gh = fake.gh;

    // Two layers accepted and delivered.
    markDone(programDir, ["a", "b"]);
    const first = deliver({ programDir, runRoot, apply: true, gh, readRun });
    assert.equal(first.ok, true, first.messages.join("\n"));
    const l1 = readDeliveryLedger(programDir);
    assert.equal(l1.layers.a.parent, base, "the root layer's recorded parent is the base commit");
    assert.equal(l1.layers.b.parent, l1.layers.a.sha, "b's recorded parent is a's delivered commit");
    assert.doesNotMatch(fake.body("main--a"), /main--c/);

    // The third layer is accepted: it is delivered on b, and the existing PRs
    // are reconciled to the whole stack (their graph now names c) without
    // recommitting or pushing them.
    markDone(programDir, ["a", "b", "c"]);
    const calls: Array<{ file: string; args: string[] }> = [];
    const cmd: Cmd = (file, args, opts) => {
      calls.push({ file, args });
      return defaultCmd(file, args, opts);
    };
    const grown = deliver({ programDir, runRoot, apply: true, gh, cmd, readRun });
    assert.equal(grown.ok, true, grown.messages.join("\n"));
    const l2 = readDeliveryLedger(programDir);
    assert.equal(l2.layers.a.sha, l1.layers.a.sha, "a is not recommitted");
    assert.equal(l2.layers.b.sha, l1.layers.b.sha, "b is not recommitted");
    assert.deepEqual(pushLeases(calls), { "main--c": "" }, "only the new layer is pushed");
    for (const branch of ["main--a", "main--b", "main--c"]) {
      assert.ok(fake.body(branch).includes("└─ main--c (c)"), `${branch}'s PR body shows the whole stack, c included`);
    }
    assert.match(fake.args("main--a"), /^pr edit main--a --base main /, "a's PR is edited, base still the root base");
    assert.match(fake.args("main--c"), /--base main--b --head main--c /, "c's PR base is its parent layer");

    // Restack onto a NEW branch while c's PR reconciliation is failing.
    const newBase = commitFile(repo, base, "base.txt", "base v2\n", "base v2");
    git(["branch", "release", newBase], repo);
    fs.writeFileSync(path.join(fake.prs, "fail-main--c"), "");
    fs.writeFileSync(path.join(fake.prs, "fail-main--a"), "");
    const restacked = restack({ programDir, runRoot, onto: "release", apply: true, readRun });
    assert.equal(restacked.ok, true, restacked.messages.join("\n"));
    const l3 = readDeliveryLedger(programDir);
    assert.equal(l3.base, "release", "the ledger records the new root base branch");
    assert.equal(l3.layers.a.parent, newBase, "a now sits on the new base");
    assert.equal(git(["rev-parse", `${l3.layers.b.sha}^`], repo), l3.layers.a.sha);
    assert.equal(git(["rev-parse", `${l3.layers.c.sha}^`], repo), l3.layers.b.sha);
    const failed = deliver({ programDir, runRoot, apply: true, gh, readRun });
    assert.equal(failed.ok, false, "a failing PR reconciliation is not a success");
    assert.deepEqual(
      Object.fromEntries(Object.entries(readDeliveryLedger(programDir).layers).map(([k, v]) => [k, v.sha])),
      Object.fromEntries(Object.entries(l3.layers).map(([k, v]) => [k, v.sha])),
      "a failed reconciliation re-parents nothing",
    );
    // The reconciliation is retried: every layer keeps the restacked commit
    // its ledger parent names — the root is never re-parented onto the old
    // base — and the root PR's base moves to the new branch.
    fs.rmSync(path.join(fake.prs, "fail-main--c"));
    fs.rmSync(path.join(fake.prs, "fail-main--a"));
    const retryCalls: Array<{ file: string; args: string[] }> = [];
    const retryCmd: Cmd = (file, args, opts) => {
      retryCalls.push({ file, args });
      return defaultCmd(file, args, opts);
    };
    const retried = deliver({ programDir, runRoot, apply: true, gh, cmd: retryCmd, readRun });
    assert.equal(retried.ok, true, retried.messages.join("\n"));
    const l4 = readDeliveryLedger(programDir);
    for (const id of ["a", "b", "c"]) assert.equal(l4.layers[id].sha, l3.layers[id].sha, `${id} keeps its restacked commit`);
    assert.deepEqual(pushLeases(retryCalls), {}, "the retry pushes nothing");
    assert.equal(git(["--git-dir", remote, "rev-parse", "refs/heads/main--a^"]), newBase, "the remote root layer sits on the new base");
    assert.match(fake.args("main--a"), /^pr edit main--a --base release /, "the root PR's base is the new branch");
    assert.match(fake.args("main--c"), /^pr edit main--c --base main--b /, "c's PR (its body changed) keeps its parent layer as base");
    assert.match(fake.body("main--a"), /^base: release$/m, "the graph names the new root base");
    assert.match(fs.readFileSync(fake.log, "utf8"), /stack link --base release\n$/, "the stack is linked with the new root base");
  } finally {
    fs.rmSync(root, { recursive: true, force: true });
  }
});

test("plan 06m: tt deliver and tt restack change nothing without --apply", async () => {
  const root = fs.mkdtempSync(path.join("/tmp", "tt-restack-dry-"));
  const repo = path.join(root, "repo");
  const remote = path.join(root, "remote.git");
  const programDir = path.join(root, "programs", "prog");
  const runRoot = path.join(root, "runs");
  fs.mkdirSync(repo, { recursive: true });
  fs.mkdirSync(runRoot, { recursive: true });
  try {
    git(["init", "-q", "-b", "main"], repo);
    fs.writeFileSync(path.join(repo, "base.txt"), "base\n");
    git(["add", "-A"], repo);
    git(["-c", "user.name=t", "-c", "user.email=t@t", "commit", "-q", "-m", "base"], repo);
    const base = git(["rev-parse", "HEAD"], repo);
    execFileSync("git", ["init", "-q", "--bare", remote]);
    git(["remote", "add", "origin", remote], repo);
    git(["push", "-q", "origin", "main"], repo);
    const a = commitFile(repo, base, "a.txt", "a\n", "a");
    const program: ProgramFile = {
      title: "stack",
      maxParallel: 1,
      entries: [{ id: "a", after: [], plan: plan(repo, "layer a") }],
    };
    writeProgram(programDir, program);
    markDone(programDir, ["a"]);
    fs.mkdirSync(path.join(runRoot, "run-a"), { recursive: true });
    const readRun = () => ({ state: doneState(a) });
    const before = git(["ls-remote", "--heads", remote], repo);
    const d = deliver({ programDir, runRoot, apply: false, readRun });
    const r = restack({ programDir, runRoot, onto: "main", apply: false, readRun });
    assert.equal(d.applied, false);
    assert.equal(r.applied, false);
    assert.equal(git(["ls-remote", "--heads", remote], repo), before, "nothing pushed");
    assert.ok(!fs.existsSync(path.join(programDir, "delivery.json")), "no ledger without --apply");
    assert.ok(planDelivery({ programDir, runRoot, readRun }).ok);
  } finally {
    fs.rmSync(root, { recursive: true, force: true });
  }
});

test("plan 06m: tt restack stops when a layer's check fails", async () => {
  const root = fs.mkdtempSync(path.join("/tmp", "tt-restack-check-"));
  const repo = path.join(root, "repo");
  const remote = path.join(root, "remote.git");
  const programDir = path.join(root, "programs", "prog");
  const runRoot = path.join(root, "runs");
  fs.mkdirSync(repo, { recursive: true });
  fs.mkdirSync(runRoot, { recursive: true });
  try {
    git(["init", "-q", "-b", "main"], repo);
    fs.writeFileSync(path.join(repo, "base.txt"), "base\n");
    git(["add", "-A"], repo);
    git(["-c", "user.name=t", "-c", "user.email=t@t", "commit", "-q", "-m", "base"], repo);
    const base = git(["rev-parse", "HEAD"], repo);
    execFileSync("git", ["init", "-q", "--bare", remote]);
    git(["remote", "add", "origin", remote], repo);
    git(["push", "-q", "origin", "main"], repo);
    const a = commitFile(repo, base, "a.txt", "a\n", "a");
    const newBase = commitFile(repo, base, "base.txt", "new-base\n", "new base");
    const program: ProgramFile = {
      title: "stack",
      maxParallel: 1,
      entries: [
        {
          id: "a",
          after: [],
          plan: plan(repo, "layer a", {
            phases: [{ id: "p1", goal: "ga", acceptance: ["it works"], checks: ["false"], boundaries: [], reserved: [] }],
          }),
        },
      ],
    };
    writeProgram(programDir, program);
    markDone(programDir, ["a"]);
    fs.mkdirSync(path.join(runRoot, "run-a"), { recursive: true });
    const readRun = () => ({ state: doneState(a) });
    const gh = path.join(root, "gh");
    fs.writeFileSync(gh, "#!/bin/sh\nexit 0\n");
    fs.chmodSync(gh, 0o755);
    const delivered = deliver({ programDir, runRoot, apply: true, gh, readRun });
    assert.equal(delivered.ok, true, delivered.messages.join("\n"));
    const result = restack({ programDir, runRoot, onto: newBase, apply: true, gh, readRun });
    assert.equal(result.ok, false, "a failing layer check stops the restack");
    assert.match(result.messages.join("\n"), /layer check failed/);
  } finally {
    fs.rmSync(root, { recursive: true, force: true });
  }
});

test("plan 06m: tt restack keeps every non-conflicting change, regenerates declared paths per layer and stops on other conflicts", async () => {
  const root = fs.mkdtempSync(path.join("/tmp", "tt-restack-"));
  const repo = path.join(root, "repo");
  const remote = path.join(root, "remote.git");
  const programDir = path.join(root, "programs", "prog");
  const runRoot = path.join(root, "runs");
  fs.mkdirSync(repo, { recursive: true });
  fs.mkdirSync(runRoot, { recursive: true });
  try {
    git(["init", "-q", "-b", "main"], repo);
    fs.writeFileSync(path.join(repo, "base.txt"), "base\n");
    git(["add", "-A"], repo);
    git(["-c", "user.name=t", "-c", "user.email=t@t", "commit", "-q", "-m", "base"], repo);
    const base = git(["rev-parse", "HEAD"], repo);
    execFileSync("git", ["init", "-q", "--bare", remote]);
    git(["remote", "add", "origin", remote], repo);
    git(["push", "-q", "origin", "main"], repo);
    // OD-3(1): the base has a multi-line generated file. L1 adds a file; L2
    // edits TWO hunks of the generated file (one will conflict with the new
    // base, one will not); L3 adds a file the new base also changes (an
    // undeclared conflict).
    fs.writeFileSync(path.join(repo, "gen.txt"), "l1\nl2\nl3\nl4\nl5\n");
    git(["add", "-A"], repo);
    git(["-c", "user.name=t", "-c", "user.email=t@t", "commit", "-q", "-m", "base gen"], repo);
    const baseGen = git(["rev-parse", "HEAD"], repo);
    const a = commitFile(repo, baseGen, "a.txt", "a\n", "a");
    const b = commitFile(repo, a, "gen.txt", "B1\nl2\nl3\nB4\nl5\n", "b");
    const c = commitFile(repo, b, "c.txt", "c\n", "c");
    // The new base changes gen.txt line 1 only, so L3 replays cleanly onto
    // the amended L2.
    const newBase1 = commitFile(repo, baseGen, "gen.txt", "BASE1\nl2\nl3\nl4\nl5\n", "new base gen");

    const generatedPhase = (goal: string) => ({ id: "p1", goal, acceptance: ["it works"], checks: ["true"], boundaries: [], reserved: [], generated: ["gen.txt"], regen: ["printf 'regen\\n' >> gen.txt"] });
    const program: ProgramFile = {
      title: "stack",
      maxParallel: 1,
      entries: [
        { id: "a", after: [], plan: plan(repo, "layer a", { phases: [generatedPhase("ga")] }) },
        { id: "b", after: ["a"], plan: plan(repo, "layer b", { phases: [generatedPhase("gb")] }) },
        { id: "c", after: ["b"], plan: plan(repo, "layer c", { phases: [generatedPhase("gc")] }) },
      ],
    };
    writeProgram(programDir, program);
    markDone(programDir, ["a", "b", "c"]);
    fs.mkdirSync(path.join(runRoot, "run-a"), { recursive: true });
    fs.mkdirSync(path.join(runRoot, "run-b"), { recursive: true });
    fs.mkdirSync(path.join(runRoot, "run-c"), { recursive: true });
    const candidates: Record<string, string> = { "run-a": a, "run-b": b, "run-c": c };
    const readRun = (runDir: string) => ({ state: doneState(candidates[path.basename(runDir)]) });
    const gh = path.join(root, "gh");
    fs.writeFileSync(gh, "#!/bin/sh\nexit 0\n");
    fs.chmodSync(gh, 0o755);

    // Deliver first, so the ledger holds the layer SHAs.
    const delivered = deliver({ programDir, runRoot, apply: true, gh, readRun });
    assert.equal(delivered.ok, true, delivered.messages.join("\n"));
    const ledger = readDeliveryLedger(programDir);
    // OD-3(1): each layer's parent is recorded in the ledger at delivery —
    // the root's is the OLD base.
    assert.equal(ledger.layers.a.parent, baseGen, "the root layer was delivered on the old base");
    assert.equal(ledger.layers.b.parent, ledger.layers.a.sha);
    assert.equal(ledger.layers.c.parent, ledger.layers.b.sha);

    const calls: Array<{ file: string; args: string[] }> = [];
    const cmd: Cmd = (file, args, opts) => {
      calls.push({ file, args });
      return defaultCmd(file, args, opts);
    };
    const result = restack({ programDir, runRoot, onto: newBase1, apply: true, gh, cmd, readRun });
    assert.equal(result.ok, true, result.messages.join("\n"));
    // Each replay's merge base is the parent the ledger recorded (the
    // delivered parent), the root's being the old base — never the new one.
    const mergeBases = calls
      .filter((c) => c.file === "git" && c.args.includes("merge-tree") && c.args.includes("-X"))
      .map((c) => c.args[c.args.indexOf("--merge-base") + 1]);
    assert.deepEqual(mergeBases, [baseGen, ledger.layers.a.sha, ledger.layers.b.sha], "merge bases come from the ledger's recorded parents");
    // The pushes carry the ledger leases: each layer's previously delivered SHA.
    assert.deepEqual(pushLeases(calls), { "main--a": ledger.layers.a.sha, "main--b": ledger.layers.b.sha, "main--c": ledger.layers.c.sha });
    const after = readDeliveryLedger(programDir);
    // L2 took the base side for the CONFLICTING hunk (line 1), KEPT its
    // non-conflicting hunk (line 4 -> B4), and was regenerated and amended;
    // then L3 was restacked on the amended L2.
    const bNew = after.layers.b.sha;
    assert.notEqual(bNew, ledger.layers.b.sha, "layer b was restacked onto the new base");
    const gen = git(["show", `${bNew}:gen.txt`], repo);
    assert.match(gen, /^BASE1$/m, "the conflicting hunk took the base side");
    assert.match(gen, /^B4$/m, "layer b's non-conflicting hunk was kept");
    assert.match(gen, /^regen$/m, "the declared generated path was regenerated");
    assert.notEqual(after.layers.c.sha, ledger.layers.c.sha, "layer c was restacked onto the amended b");
    assert.equal(git(["rev-parse", `${after.layers.c.sha}^`], repo), bNew, "L3's parent is the amended L2");
    assert.equal(git(["show", `${after.layers.c.sha}:c.txt`], repo), "c", "L3's content is preserved");
    // L1 sits on the new base and L2 (amended: ONE commit, regenerated) on
    // the new L1; the ledger records the new parents.
    assert.equal(git(["rev-parse", `${after.layers.a.sha}^`], repo), newBase1, "L1's parent is the new base");
    assert.equal(git(["rev-parse", `${bNew}^`], repo), after.layers.a.sha, "the amended L2 is one commit on the new L1");
    assert.deepEqual(
      [after.layers.a.parent, after.layers.b.parent, after.layers.c.parent],
      [newBase1, after.layers.a.sha, bNew],
      "the ledger records each layer's new parent",
    );
    // Only the conflicting hunk changed: the untouched lines are the base's.
    const genLines = gen.split("\n");
    assert.deepEqual(genLines.slice(0, 5), ["BASE1", "l2", "l3", "B4", "l5"], "only the conflicting hunk took the base side");
    for (const id of ["a", "b", "c"]) {
      assert.equal(git(["--git-dir", remote, "rev-parse", `refs/heads/main--${id}`]), after.layers[id].sha, `layer ${id} pushed`);
    }
  } finally {
    fs.rmSync(root, { recursive: true, force: true });
  }
});

test("plan 06m: tt restack stops on a conflict in an undeclared file naming the layer and path", async () => {
  const root = fs.mkdtempSync(path.join("/tmp", "tt-restack-undeclared-"));
  const repo = path.join(root, "repo");
  const remote = path.join(root, "remote.git");
  const programDir = path.join(root, "programs", "prog");
  const runRoot = path.join(root, "runs");
  fs.mkdirSync(repo, { recursive: true });
  fs.mkdirSync(runRoot, { recursive: true });
  try {
    git(["init", "-q", "-b", "main"], repo);
    fs.writeFileSync(path.join(repo, "plain.txt"), "plain\n");
    fs.writeFileSync(path.join(repo, "gen.txt"), "g\n");
    git(["add", "-A"], repo);
    git(["-c", "user.name=t", "-c", "user.email=t@t", "commit", "-q", "-m", "base"], repo);
    const base = git(["rev-parse", "HEAD"], repo);
    execFileSync("git", ["init", "-q", "--bare", remote]);
    git(["remote", "add", "origin", remote], repo);
    git(["push", "-q", "origin", "main"], repo);
    // Three layers; L2 edits plain.txt, which is NOT declared generated, and
    // the new base edits the same line: an undeclared conflict at L2.
    const l1 = commitFile(repo, base, "one.txt", "1\n", "l1");
    const l2 = commitFile(repo, l1, "plain.txt", "plain-l2\n", "l2");
    const l3 = commitFile(repo, l2, "three.txt", "3\n", "l3");
    const newBase = commitFile(repo, base, "plain.txt", "plain-base\n", "new base");
    const phase = (goal: string) => ({ id: "p1", goal, acceptance: ["it works"], checks: ["true"], boundaries: [], reserved: [], generated: ["gen.txt"], regen: ["true"] });
    const program: ProgramFile = {
      title: "stack",
      maxParallel: 1,
      entries: [
        { id: "l1", after: [], plan: plan(repo, "layer l1", { phases: [phase("g1")] }) },
        { id: "l2", after: ["l1"], plan: plan(repo, "layer l2", { phases: [phase("g2")] }) },
        { id: "l3", after: ["l2"], plan: plan(repo, "layer l3", { phases: [phase("g3")] }) },
      ],
    };
    writeProgram(programDir, program);
    markDone(programDir, ["l1", "l2", "l3"]);
    for (const id of ["l1", "l2", "l3"]) fs.mkdirSync(path.join(runRoot, `run-${id}`), { recursive: true });
    const candidates: Record<string, string> = { "run-l1": l1, "run-l2": l2, "run-l3": l3 };
    const readRun = (runDir: string) => ({ state: doneState(candidates[path.basename(runDir)]) });
    const gh = path.join(root, "gh");
    fs.writeFileSync(gh, "#!/bin/sh\nexit 0\n");
    fs.chmodSync(gh, 0o755);
    assert.equal(deliver({ programDir, runRoot, apply: true, gh, readRun }).ok, true);
    const ledgerBefore = readDeliveryLedger(programDir);
    const remoteBefore = git(["ls-remote", "--heads", remote], repo);
    const result = restack({ programDir, runRoot, onto: newBase, apply: true, gh, readRun });
    assert.equal(result.ok, false, "restack stops on the undeclared conflict");
    assert.match(result.messages.join("\n"), /stopped at main--l2 \(l2\): conflict in plain\.txt/, "the stop names the layer and the path");
    // Nothing is pushed for any layer, L1 included, and the ledger is as it was.
    assert.equal(git(["ls-remote", "--heads", remote], repo), remoteBefore, "a stopped restack pushes nothing");
    assert.deepEqual(readDeliveryLedger(programDir), ledgerBefore, "a stopped restack leaves the ledger unchanged");
  } finally {
    fs.rmSync(root, { recursive: true, force: true });
  }
});
