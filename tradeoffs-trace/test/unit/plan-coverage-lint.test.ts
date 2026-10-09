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

test("plan 06j: a plan with no repository lints as before", () => {
  const plan: LintPlanInput = { phases: [{ id: "p1", boundaries: ["a/**"], checks: ["cargo test -p a"] }] };
  assert.deepEqual(checkCoverageWarnings(plan, { packages: [], ciCommands: [], cargo: false }), []);
});
