// Plan 07a (design D1/D2): the domain model, the ingestion refusal, the lint
// rule and the reviewer prompt. The conductor end-to-end lives in
// test/conductor/domain-findings.test.ts.

import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import { test } from "node:test";

import { domainOf, parseDomains, whereKey } from "../../src/core/domains.ts";
import { reviewIngestionIssue } from "../../src/core/predicate.ts";
import { lintPlan } from "../../src/core/plan-lint.ts";
import { parseOrgPlan } from "../../src/core/org-plan.ts";
import { validate } from "../../src/core/schema.ts";
import { buildContract, buildReviewerPrompt } from "../../src/conductor.ts";
import type { PhaseState, Review } from "../../src/core/types.ts";

test("domains: domainOf maps a path to the first declared domain whose glob matches", () => {
  const domains = parseDomains("booking=src/b/*.ts gates=test/**");
  assert.deepEqual(
    domains,
    [
      { name: "booking", globs: ["src/b/*.ts"] },
      { name: "gates", globs: ["test/**"] },
    ],
    "the parsed domains keep names and globs in declaration order",
  );
  assert.equal(domainOf("src/b/x.ts", domains), "booking");
  assert.equal(domainOf("test/a.ts", domains), "gates");
  assert.equal(domainOf("README.md", domains), undefined);
  // A path both globs match resolves to the first declared.
  const overlapping = parseDomains("first=src/** second=src/**");
  assert.equal(domainOf("src/b/x.ts", overlapping), "first");
  assert.equal(domainOf("src/b/x.ts", []), undefined);
});

test("domains: whereKey takes the path and the symbol and ignores the line", () => {
  const a = whereKey("crates/x.rs:120 fn foo");
  assert.equal(a, whereKey("crates/x.rs:98 fn foo"), "the line number is ignored");
  assert.notEqual(a, whereKey("crates/x.rs:120 fn bar"), "a different symbol differs");
  assert.notEqual(a, whereKey("crates/y.rs:120 fn foo"), "a different path differs");
  assert.equal(whereKey("crates/x.rs:120"), "crates/x.rs", "no symbol: the path alone");
});

const DOMAIN_ORG = `#+TITLE: domains
#+TT_REPO: /tmp/repo
#+TT_BRANCH: main
#+TT_CHECKS: true
#+TT_DOMAINS: booking=src/b/*.ts gates=test/**

* P
  :PROPERTIES:
  :ID: p1
  :CHECKS: true
  :END:
** Architecture
*** A1 ok
    :PROPERTIES:
    :ID: A1
    :DOMAIN: booking
    :END:
    text
** Requirements
*** R1 ok
    :PROPERTIES:
    :ID: R1
    :VERIFY: review
    :DOMAIN: gates
    :END:
    text
*** R2 missing
    :PROPERTIES:
    :ID: R2
    :VERIFY: review
    :END:
    text
*** R3 undeclared
    :PROPERTIES:
    :ID: R3
    :VERIFY: review
    :DOMAIN: nope
    :END:
    text
`;

test("tt lint refuses an item without a declared domain in domain mode", () => {
  const plan = parseOrgPlan(DOMAIN_ORG, "/x/PLAN.org");
  // The parser carries the domains and each item's own.
  assert.deepEqual(plan.phases![0].domains, [
    { name: "booking", globs: ["src/b/*.ts"] },
    { name: "gates", globs: ["test/**"] },
  ]);
  assert.equal(plan.phases![0].architecture![0].domain, "booking");
  assert.equal(plan.phases![0].requirements![0].domain, "gates");
  const findings = lintPlan(plan).filter((f) => f.rule === "item-domain");
  assert.equal(findings.length, 2, `exactly the two bad items: ${JSON.stringify(findings)}`);
  const missing = findings.find((f) => /no :DOMAIN:/.test(f.problem))!;
  assert.match(missing.problem, /R2/, "the missing-domain error names the item");
  const undeclared = findings.find((f) => /undeclared domain nope/.test(f.problem))!;
  assert.match(undeclared.problem, /R3/, "the undeclared-domain error names the item");
  assert.ok(!findings.some((f) => /R1/.test(f.problem)), "a declared item is not an error");

  // Without domains neither is an error.
  const plain = parseOrgPlan(DOMAIN_ORG.replace("#+TT_DOMAINS: booking=src/b/*.ts gates=test/**\n", ""), "/x/PLAN.org");
  assert.deepEqual(lintPlan(plain).filter((f) => f.rule === "item-domain"), []);
});

function reviewWith(finding: Record<string, unknown>, blockers: Array<Record<string, unknown>> = []): Review {
  return {
    reviewer: "M",
    phaseId: "p1",
    candidateSha: "c1",
    contractVersion: { snapshot: 1, sectionSha256: "a".repeat(64) },
    correctionStatements: [],
    findingStatements: [],
    findings: [finding],
    blockers,
  } as unknown as Review;
}

const DOMAINS = parseDomains("booking=src/b/*.ts gates=test/**");

test("a domain-mode finding missing domain, item or doneWhen is refused naming the field", () => {
  const complete = { kind: "defect", severity: "advisory", evidence: "e", domain: "booking", item: "R1", where: "src/b/x.ts:1 fn", expected: "x", doneWhen: "true" };
  assert.equal(reviewIngestionIssue(reviewWith(complete), DOMAINS), undefined, "a complete point is accepted");
  for (const field of ["domain", "item", "doneWhen"] as const) {
    const finding = { ...complete, [field]: undefined };
    const issue = reviewIngestionIssue(reviewWith(finding), DOMAINS);
    assert.ok(issue && issue.includes(field), `the refusal names ${field}: ${issue}`);
  }
  // Outside domain mode the same incomplete finding is fine.
  assert.equal(reviewIngestionIssue(reviewWith({ kind: "defect", severity: "advisory", evidence: "e" })), undefined);
});

test("a domain-mode finding with an undeclared domain is refused", () => {
  const issue = reviewIngestionIssue(
    reviewWith({ kind: "defect", severity: "advisory", evidence: "e", domain: "nope", item: "R1", doneWhen: "true" }),
    DOMAINS,
  );
  assert.ok(issue && issue.includes("nope"), `the refusal names the undeclared domain: ${issue}`);
});

test("a domain-mode blocker missing doneWhen is refused; outside domain mode nothing is", () => {
  const issue = reviewIngestionIssue(
    reviewWith({ kind: "defect", severity: "advisory", evidence: "e" }, [{ kind: "defect", evidence: "b", domain: "booking", item: "R1" }]),
    DOMAINS,
  );
  assert.ok(issue && issue.includes("doneWhen"), `the blocker refusal names doneWhen: ${issue}`);
  assert.equal(reviewIngestionIssue(reviewWith({ kind: "defect", severity: "advisory", evidence: "e" }, [{ kind: "defect", evidence: "b" }])), undefined);
});

function phaseState(domains: string | undefined): PhaseState {
  const contract = buildContract({
    id: "p1",
    goal: "g",
    acceptance: ["a"],
    checks: ["true"],
    boundaries: [],
    reserved: [],
    ...(domains ? { domains: parseDomains(domains) } : {}),
  });
  return { phaseId: "p1", candidate: { sha: "abc1234" }, contract, decisions: [], findings: [], corrections: [], ballots: [] } as unknown as PhaseState;
}

test("in domain mode the reviewer prompt lists the domains and the point fields", () => {
  const prompt = buildReviewerPrompt(phaseState("booking=src/b/*.ts gates=test/**"), "M");
  assert.match(prompt, /booking: src\/b\/\*\.ts/);
  assert.match(prompt, /gates: test\/\*\*/);
  for (const field of ["domain", "item", "where", "expected", "doneWhen"]) {
    assert.ok(prompt.includes(field), `the prompt names the point field ${field}`);
  }
  // Outside domain mode the prompt carries no domain section at all.
  const plain = buildReviewerPrompt(phaseState(undefined), "M");
  assert.ok(!/domain mode/i.test(plain), "no domain section outside domain mode");
  assert.ok(!plain.includes("booking"), "no domain names outside domain mode");
});

test("the plan schema accepts domains and an item domain", () => {
  const planSchema = JSON.parse(readFileSync(new URL("../../schemas/plan.schema.json", import.meta.url), "utf8"));
  const plan = {
    title: "t",
    checks: ["true"],
    phases: [
      {
        id: "p1",
        goal: "g",
        checks: ["true"],
        boundaries: [],
        reserved: [],
        provisional: false,
        domains: [{ name: "booking", globs: ["src/**"] }],
        architecture: [{ id: "A1", title: "A", text: "t", tags: [], domain: "booking" }],
        requirements: [{ id: "R1", title: "R", text: "t", arch: [], verify: ["review"], domain: "booking" }],
        constraints: [],
      },
    ],
  };
  const result = validate(planSchema, plan);
  assert.equal(result.valid, true, result.errors.join("; "));
});
