// Plan 06j (A1): a phase's checks cannot see what its :BOUNDARIES: let it
// change, or what the repository's CI runs. The coverage lint warns — never
// errors — so a plan still starts, but the owner reads the gap before the run.

import assert from "node:assert/strict";
import * as fs from "node:fs";
import * as os from "node:os";
import * as path from "node:path";
import { test } from "node:test";

import { checkCoverageWarnings, type LintPlanInput } from "../../src/core/plan-lint.ts";
import { readRepoFacts } from "../../src/core/repo-facts.ts";

/** A cargo workspace with crates a, b and c, plus the CI workflow. */
function cargoFixture(ci?: string): string {
  const root = fs.mkdtempSync(path.join(os.tmpdir(), "tt-06j-cargo-"));
  fs.writeFileSync(path.join(root, "Cargo.toml"), '[workspace]\nmembers = ["a", "b", "c"]\nresolver = "2"\n');
  for (const name of ["a", "b", "c"]) {
    fs.mkdirSync(path.join(root, name), { recursive: true });
    fs.writeFileSync(path.join(root, name, "Cargo.toml"), `[package]\nname = "${name}"\nversion = "0.1.0"\n`);
  }
  if (ci !== undefined) {
    fs.mkdirSync(path.join(root, ".github", "workflows"), { recursive: true });
    fs.writeFileSync(path.join(root, ".github", "workflows", "ci.yml"), `name: CI\non: [push]\njobs:\n  test:\n    steps:\n      - ${ci}\n`);
  }
  return root;
}

test("plan 06j: a phase whose boundaries cover a crate its checks never name gets a coverage warning", () => {
  const root = cargoFixture();
  try {
    const plan: LintPlanInput = {
      repo: root,
      phases: [
        {
          id: "p1",
          boundaries: ["a/**", "b/**"],
          checks: ["cargo test -p a"],
        },
      ],
    };
    const findings = checkCoverageWarnings(plan, readRepoFacts(root));
    const namesB = findings.filter((f) => f.rule === "coverage" && f.item.includes("b"));
    assert.equal(namesB.length, 1, `a warning names b: ${JSON.stringify(findings)}`);
    assert.equal(namesB[0].severity, "warning", "coverage is a warning, never an error");
    assert.match(namesB[0].problem, /b/);
    assert.match(namesB[0].fix, /-p b/);

    // Adding `-p b` to the final checks clears it; c is not covered, so it
    // never warned.
    plan.phases![0].finalChecks = ["cargo test -p b"];
    assert.deepEqual(checkCoverageWarnings(plan, readRepoFacts(root)), []);
  } finally {
    fs.rmSync(root, { recursive: true, force: true });
  }
});

test("plan 06j: a repo whose CI runs cargo fmt --all warns a plan that never does", () => {
  const root = cargoFixture("run: cargo fmt --all --check");
  try {
    const plan: LintPlanInput = {
      repo: root,
      phases: [{ id: "p1", boundaries: ["a/**"], checks: ["cargo fmt -p a --check"] }],
    };
    const findings = checkCoverageWarnings(plan, readRepoFacts(root));
    const fmt = findings.filter((f) => f.rule === "coverage" && /cargo fmt --all/.test(f.problem));
    assert.equal(fmt.length, 1, `a warning names cargo fmt --all: ${JSON.stringify(findings)}`);

    // A phase whose final check mirrors CI is not warned for fmt.
    plan.phases![0].finalChecks = ["cargo fmt --all --check"];
    assert.deepEqual(
      checkCoverageWarnings(plan, readRepoFacts(root)).filter((f) => /cargo fmt --all/.test(f.problem)),
      [],
    );
  } finally {
    fs.rmSync(root, { recursive: true, force: true });
  }
});

test("plan 06j: a root crate covered by src/** is warned, and '*.md' does not cover nested packages", () => {
  // (a) a single-package repository: the crate's dir is '.', so any boundary
  // glob inside the repo covers it (A1's covered-crate guarantee).
  const single = fs.mkdtempSync(path.join(os.tmpdir(), "tt-06j-single-"));
  fs.mkdirSync(path.join(single, "src"), { recursive: true });
  fs.writeFileSync(path.join(single, "Cargo.toml"), '[package]\nname = "root"\nversion = "0.1.0"\n');
  try {
    const rootPlan: LintPlanInput = { repo: single, phases: [{ id: "p1", boundaries: ["src/**"], checks: ["true"] }] };
    const warnings = checkCoverageWarnings(rootPlan, readRepoFacts(single));
    assert.equal(warnings.filter((f) => f.rule === "coverage" && f.item.includes("root")).length, 1, JSON.stringify(warnings));
  } finally {
    fs.rmSync(single, { recursive: true, force: true });
  }

  // (b) a prefix-less glob is one path segment (`*.md` is root-only), so it
  // cannot cover a nested crate; `**` can.
  const root = cargoFixture();
  try {
    const mdPlan: LintPlanInput = { repo: root, phases: [{ id: "p1", boundaries: ["*.md"], checks: ["cargo test -p a"] }] };
    assert.deepEqual(
      checkCoverageWarnings(mdPlan, readRepoFacts(root)).filter((f) => f.rule === "coverage"),
      [],
      "*.md must not cover nested crates",
    );
    mdPlan.phases![0].boundaries = ["**"];
    const all = checkCoverageWarnings(mdPlan, readRepoFacts(root)).filter((f) => f.rule === "coverage");
    assert.ok(all.some((f) => f.item.includes("b")) && all.some((f) => f.item.includes("c")), JSON.stringify(all));
    // (c) a wildcard first segment still reaches a nested package:
    // `*/src/**` matches `a/src/...`, so it covers crate a.
    mdPlan.phases![0].boundaries = ["*/src/**"];
    mdPlan.phases![0].checks = ["true"];
    const nested = checkCoverageWarnings(mdPlan, readRepoFacts(root)).filter((f) => f.rule === "coverage");
    assert.ok(nested.some((f) => f.item.includes("a")), JSON.stringify(nested));
  } finally {
    fs.rmSync(root, { recursive: true, force: true });
  }
});

test("plan 06j: a plain cargo fmt --all or --all in another segment never mirrors CI's fmt --all --check", () => {
  const root = cargoFixture("run: cargo fmt --all --check");
  try {
    const plan: LintPlanInput = {
      repo: root,
      phases: [{ id: "p1", boundaries: [], checks: ["cargo fmt -p a --check && cargo clippy --all"] }],
    };
    assert.equal(
      checkCoverageWarnings(plan, readRepoFacts(root)).filter((f) => /cargo fmt --all/.test(f.problem)).length,
      1,
      "--all in a different segment is not the same invocation",
    );
    plan.phases![0].checks = ["cargo fmt --all"];
    assert.equal(
      checkCoverageWarnings(plan, readRepoFacts(root)).filter((f) => /cargo fmt --all/.test(f.problem)).length,
      1,
      "a plain cargo fmt --all reformats and exits 0, so it never catches drift",
    );
    plan.phases![0].checks = ["cargo fmt --all --check"];
    assert.deepEqual(checkCoverageWarnings(plan, readRepoFacts(root)).filter((f) => /cargo fmt --all/.test(f.problem)), []);
  } finally {
    fs.rmSync(root, { recursive: true, force: true });
  }
});

test("plan 06j: a CI run block's separate lines are not one cargo fmt invocation", () => {
  const root = fs.mkdtempSync(path.join(os.tmpdir(), "tt-06j-ciblock-"));
  fs.writeFileSync(path.join(root, "Cargo.toml"), '[workspace]\nmembers = ["a"]\n');
  fs.mkdirSync(path.join(root, "a"), { recursive: true });
  fs.writeFileSync(path.join(root, "a", "Cargo.toml"), '[package]\nname = "a"\nversion = "0.1.0"\n');
  fs.mkdirSync(path.join(root, ".github", "workflows"), { recursive: true });
  fs.writeFileSync(
    path.join(root, ".github", "workflows", "ci.yml"),
    "name: CI\non: [push]\njobs:\n  t:\n    steps:\n      - run: |\n          cargo fmt --check\n          cargo test --all\n",
  );
  try {
    const facts = readRepoFacts(root);
    assert.ok(facts.ciCommands.some((c) => c.includes("cargo fmt --check\ncargo test --all")), JSON.stringify(facts.ciCommands));
    const plan: LintPlanInput = { repo: root, phases: [{ id: "p1", boundaries: [], checks: ["cargo test -p a"] }] };
    assert.deepEqual(
      checkCoverageWarnings(plan, facts).filter((f) => /cargo fmt --all/.test(f.problem)),
      [],
      "fmt --check and test --all are separate lines, not one fmt --all --check",
    );
  } finally {
    fs.rmSync(root, { recursive: true, force: true });
  }
});

test("plan 06j: a CI that gates on cargo fmt --all is matched by a plan running cargo fmt --all --check", () => {
  // OD-13: the CI trigger is any `cargo fmt --all` (a following `git diff
  // --exit-code` is the same gate); the plan only clears it with `--check`.
  const root = cargoFixture("run: cargo fmt --all && git diff --exit-code");
  const root2 = cargoFixture("run: cargo fmt --all --check");
  try {
    const plan: LintPlanInput = { repo: root, phases: [{ id: "p1", boundaries: [], checks: ["cargo fmt -p a --check"] }] };
    assert.equal(
      checkCoverageWarnings(plan, readRepoFacts(root)).filter((f) => /cargo fmt --all/.test(f.problem)).length,
      1,
      "CI gates on fmt --all, the plan does not",
    );
    plan.phases![0].checks = ["cargo fmt --all -- --check"];
    assert.deepEqual(checkCoverageWarnings(plan, readRepoFacts(root)).filter((f) => /cargo fmt --all/.test(f.problem)), []);
    const plan2: LintPlanInput = { repo: root2, phases: [{ id: "p1", boundaries: [], checks: ["cargo fmt --all --check"] }] };
    assert.deepEqual(checkCoverageWarnings(plan2, readRepoFacts(root2)).filter((f) => /cargo fmt --all/.test(f.problem)), []);
  } finally {
    fs.rmSync(root, { recursive: true, force: true });
    fs.rmSync(root2, { recursive: true, force: true });
  }
});

test("plan 06j: coverage facts read a workspace's crates and its workflow commands", () => {
  const root = cargoFixture("run: cargo fmt --all --check");
  try {
    const facts = readRepoFacts(root);
    assert.equal(facts.cargo, true);
    assert.deepEqual(
      facts.packages.map((p) => p.name).sort(),
      ["a", "b", "c"],
    );
    assert.ok(facts.ciCommands.includes("cargo fmt --all --check"), JSON.stringify(facts.ciCommands));
  } finally {
    fs.rmSync(root, { recursive: true, force: true });
  }
});

test("plan 06j: a non-virtual workspace's root [package] is inventoried too", () => {
  // E-76: a root Cargo.toml that is both [package] and [workspace] defines a
  // root crate beside its members; a virtual manifest (no [package]) does not.
  const root = fs.mkdtempSync(path.join(os.tmpdir(), "tt-06j-rootpkg-"));
  fs.mkdirSync(path.join(root, "src"), { recursive: true });
  fs.mkdirSync(path.join(root, "a"), { recursive: true });
  fs.writeFileSync(path.join(root, "Cargo.toml"), '[package]\nname = "root"\nversion = "0.1.0"\n\n[workspace]\nmembers = ["a"]\n');
  fs.writeFileSync(path.join(root, "a", "Cargo.toml"), '[package]\nname = "a"\nversion = "0.1.0"\n');
  try {
    const facts = readRepoFacts(root);
    assert.deepEqual(facts.packages.map((p) => p.name).sort(), ["a", "root"], JSON.stringify(facts.packages));
    const plan: LintPlanInput = { repo: root, phases: [{ id: "p1", boundaries: ["src/**"], checks: ["true"] }] };
    const warned = checkCoverageWarnings(plan, facts).filter((f) => f.rule === "coverage");
    assert.ok(warned.some((f) => f.item.includes("root")), `the root crate is warned: ${JSON.stringify(warned)}`);
    // A virtual manifest (no [package]) keeps today's members-only behaviour,
    // even when another table carries a `name` key.
    fs.writeFileSync(path.join(root, "Cargo.toml"), '[workspace]\nmembers = ["a"]\n\n[workspace.package]\nname = "x"\n');
    const virtual = readRepoFacts(root);
    assert.deepEqual(virtual.packages.map((p) => p.name).sort(), ["a"], JSON.stringify(virtual.packages));
  } finally {
    fs.rmSync(root, { recursive: true, force: true });
  }
});

test("plan 06j: a virtual root with empty members yields no packages", () => {
  // OD-12: without a [package] header the root is never a package, even when
  // `members` is empty — no basename fallback.
  const root = fs.mkdtempSync(path.join(os.tmpdir(), "tt-06j-emptyws-"));
  fs.mkdirSync(path.join(root, "src"), { recursive: true });
  fs.writeFileSync(path.join(root, "Cargo.toml"), "[workspace]\nmembers = []\n");
  try {
    const facts = readRepoFacts(root);
    assert.deepEqual(facts.packages, [], JSON.stringify(facts.packages));
    const plan: LintPlanInput = { repo: root, phases: [{ id: "p1", boundaries: ["src/**"], checks: ["true"] }] };
    assert.deepEqual(checkCoverageWarnings(plan, facts).filter((f) => f.rule === "coverage"), []);
  } finally {
    fs.rmSync(root, { recursive: true, force: true });
  }
});

test("plan 06j: a plan with no repository lints as before", () => {
  const plan: LintPlanInput = { phases: [{ id: "p1", boundaries: ["a/**"], checks: ["cargo test -p a"] }] };
  assert.deepEqual(checkCoverageWarnings(plan, { packages: [], ciCommands: [], cargo: false }), []);
});
