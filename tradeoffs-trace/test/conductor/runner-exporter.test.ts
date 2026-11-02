import assert from "node:assert/strict";
import { execFileSync, spawnSync } from "node:child_process";
import * as fs from "node:fs";
import * as path from "node:path";
import { test } from "node:test";
import { fileURLToPath } from "node:url";

const pkgRoot = fileURLToPath(new URL("../..", import.meta.url));

test("an installed runner carries its own Emacs exporter", async (t) => {
  const dir = fs.mkdtempSync("/tmp/tt-runner-exporter-");
  try {
    const repo = path.join(dir, "repo");
    const pkg = path.join(repo, "tradeoffs-trace");
    const exporter = path.join(repo, "core", "init-tradeoffs-trace.el");
    fs.cpSync(pkgRoot, pkg, { recursive: true });
    fs.mkdirSync(path.dirname(exporter), { recursive: true });
    fs.copyFileSync(path.join(pkgRoot, "..", "core", "init-tradeoffs-trace.el"), exporter);
    const git = (...args: string[]) => execFileSync("git", ["-C", repo, ...args]);
    git("init", "-q");
    git("add", ".");
    git("-c", "user.name=Test", "-c", "user.email=test@example.com", "commit", "-qm", "runner fixture");
    const sha = git("rev-parse", "HEAD").toString().trim();
    const committed = git("show", `${sha}:core/init-tradeoffs-trace.el`);
    // Installation must use the commit, even when the checkout has changed.
    fs.writeFileSync(exporter, "(error \"uncommitted exporter\")\n");
    const root = path.join(dir, "runs");
    execFileSync(process.execPath, [path.join(pkg, "src", "cli.ts"), "runner", "install", sha, "--root", root]);
    const installed = path.join(root, "runner", sha, "core", "init-tradeoffs-trace.el");
    assert.ok(fs.existsSync(installed), "the installed runner must include core/init-tradeoffs-trace.el");
    assert.deepEqual(fs.readFileSync(installed), committed);

    const emacs = spawnSync("emacs", ["--version"]);
    if (emacs.error) {
      assert.equal((emacs.error as NodeJS.ErrnoException).code, "ENOENT");
      await t.test("standalone Emacs export", { skip: "Emacs is absent from PATH" }, () => {});
      return;
    }
    await t.test("standalone Emacs export", () => {
      const plan = path.join(dir, "node.org");
      fs.writeFileSync(plan, `#+TITLE: tiny node
#+TT_REPO: ${repo}
#+TT_BRANCH: main
#+TT_CHECKS: true
* Phase
  :PROPERTIES:
  :ID: p1
  :CHECKS: true
  :END:
  Goal: export a node
  Acceptance:
  - the node exports
`);
      const source = path.join(dir, "program.org");
      fs.writeFileSync(source, "#+TITLE: tiny program\n#+TT_PROGRAM: 1\n* node\n  :PROPERTIES:\n  :PLAN: node.org\n  :END:\n");
      const out = execFileSync("emacs", ["-Q", "--batch", "-l", installed, "--eval", `(+tt-export-program ${JSON.stringify(source)})`], {
        cwd: dir,
        encoding: "utf8",
      });
      const program = JSON.parse(out);
      assert.equal(program.title, "tiny program");
      assert.equal(program.entries[0].id, "node");
      assert.equal(program.entries[0].plan.phases[0].id, "p1");
    });
  } finally {
    fs.rmSync(dir, { recursive: true, force: true });
  }
});
