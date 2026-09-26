// Plan 01c: a plan linter that runs before any run starts.
//
// Two failure modes cost the measured program f8ecf5e3 whole rounds and a late
// owner ruling (runtime doc §4):
//
//  1. an acceptance item whose actor is the **owner** or a human — no worker
//     can satisfy it. "the owner records a live run" (13j) parked the phase and
//     reviewers then blocked it for parking.
//  2. an acceptance item that depends on a **future** the gate cannot see —
//     "the recorded live-run SHA is the current rebased tip" (13i) is
//     unsatisfiable, because the rebased tip does not exist until the
//     conductor commits the candidate.
//
// A third, softer failure mode only warns: a comparison against a contracted
// limit with no stated tolerance ("p99 ≤ its contracted interval") turns every
// vendor that misses by a millisecond into a gap.
//
// This module is pure: no I/O, no clock, no environment. The CLI (`tt lint`,
// `tt start`, `tt program start`) owns it; Emacs mirrors it by shelling out to
// `tt lint`, so there is exactly one implementation of the rules. The line
// numbers come from the Org source: Emacs records them on `acceptanceLines`
// when it parses the plan, and a hand-written JSON plan simply gets no line.

export type LintSeverity = "error" | "warning";

export type LintRule = "owner-actor" | "human-actor" | "future-dependency" | "no-tolerance";

export interface LintFinding {
  severity: LintSeverity;
  rule: LintRule;
  /** The phase the item belongs to; a program prefixes "<entry>/". */
  phaseId: string;
  /** The acceptance item exactly as written. */
  item: string;
  /** 1-based line in `sourceFile`, when the plan carried one. */
  line?: number;
  /** The Org file the item came from, when the plan carried one. */
  sourceFile?: string;
  /** One sentence naming the problem. */
  problem: string;
  /** One sentence saying how to rewrite the item. */
  fix: string;
}

/** The slice of a plan the linter needs. `RunPlanFile`/`RunPlanPhase` are
 * structurally assignable to it, so the CLI can pass a parsed plan directly. */
export interface LintPhaseInput {
  id?: string;
  acceptance?: string[];
  /** 1-based source lines of `acceptance`, parallel to it (Emacs records
   * these; absent for a hand-written JSON plan). */
  acceptanceLines?: number[];
}

export interface LintPlanInput {
  /** The Org file this JSON came from, when known. */
  sourceFile?: string;
  phases?: LintPhaseInput[];
}

export interface LintProgramInput {
  entries?: Array<{ id?: string; plan?: LintPlanInput }>;
}

/** True for the JSON shape `tt program start` reads (a list of plan entries
 * rather than a single plan). */
export function isProgramInput(json: unknown): json is LintProgramInput {
  return !!json && typeof json === "object" && Array.isArray((json as { entries?: unknown }).entries);
}

// An acceptance item is the owner's (or a human's) job when the owner is the
// grammatical subject followed directly by a verb or a modal. The passive and
// possessive forms the real atlas plans use — "the owner live check is
// recorded" (13c-13f, 14a-14e), "the owner's K4 ruling is recorded" (14f) —
// name the owner as a qualifier, not as the actor, and must not error.
const OWNER_ACTOR_RE =
  /\bowner\b\s+(?:(?:must|should|shall|will|needs?\s+to|has\s+to|is\s+to|is\s+expected\s+to)\s+)?(?:records?|runs?|executes?|performs?|deploys?|writes?|creates?|uploads?|provides?|confirms?|reviews?|verifies?|checks?|does|makes?|measures?|sets?\s+up|installs?|exports?|starts?|stops?|launches?|signs?\s+off|produces?|prepares?|collects?|captures?|documents?)\b/i;
const HUMAN_ACTOR_RE = /\b(?:manually|someone|a\s+human|the\s+human|a\s+person|the\s+person|by\s+hand)\b/i;

// A fact that does not exist yet when the gate runs cannot be an acceptance
// item. The phrases are the ones the runtime doc §4 names, plus their
// close synonyms.
const FUTURE_PATTERNS: Array<{ re: RegExp; phrase: string }> = [
  { re: /\bafter\s+(?:the\s+)?merge\b/i, phrase: "after merge" },
  { re: /\bafter\s+merging\b/i, phrase: "after merging" },
  { re: /\bonce\s+merged?\b/i, phrase: "once merged" },
  { re: /\bpost-?merge\b/i, phrase: "post-merge" },
  { re: /\brebased\s+tip\b/i, phrase: "rebased tip" },
  { re: /\bcurrent\s+tip\b/i, phrase: "current tip" },
  { re: /\bafter\s+(?:the\s+)?rebase\b/i, phrase: "after rebase" },
  { re: /\bonce\s+(?:it\s+is\s+)?deployed\b/i, phrase: "once deployed" },
  { re: /\bwhen\s+deployed\b/i, phrase: "when deployed" },
  { re: /\bafter\s+(?:the\s+)?deploy(?:ment)?\b/i, phrase: "after deploy" },
];

// A comparison against a contracted/required limit with no stated tolerance:
// "p99 ≤ its contracted interval". An explicit margin ("plus network jitter",
// "× 1.1", "within 10 %"), or a named fallback ("or the market is a recorded
// gap", "or the phase stops"), removes the warning: the plan then says what a
// miss means.
const COMPARISON_RE =
  /(?:≤|≥|<=|>=|<|>|\bat\s+most\b|\bno\s+more\s+than\b|\bno\s+greater\s+than\b|\bno\s+less\s+than\b|\bless\s+than\b|\bgreater\s+than\b|\bexceeds?\b)/i;
const CONTRACTED_RE = /\b(?:contracted|required|expected|agreed|specified)\b/i;
const TOLERANCE_RE =
  /(?:±|×\s*[0-9.]|\b[0-9.]+\s*×|\bmargin\b|\btolerance\b|\bheadroom\b|\ballowance\b|\bslack\b|\bjitter\b|\bplus\b|\bor\s+more\b|\bor\s+less\b|\bup\s+to\s+\+?\d|\bwithin\s+\d|\bat\s+least\s+\d|\bgrace\b|\brecorded\s+gap\b|\brecorded\s+as\s+a\s+gap\b|\bor\s+stops?\b|\bphase\s+stops\b)/i;

function futurePhrase(item: string): string | undefined {
  for (const p of FUTURE_PATTERNS) {
    if (p.re.test(item)) return p.phrase;
  }
  return undefined;
}

function hasNoTolerance(item: string): boolean {
  return COMPARISON_RE.test(item) && CONTRACTED_RE.test(item) && !TOLERANCE_RE.test(item);
}

function ownerActorFinding(phaseId: string, item: string, line: number | undefined, sourceFile: string | undefined): LintFinding {
  return {
    severity: "error",
    rule: "owner-actor",
    phaseId,
    item,
    line,
    sourceFile,
    problem: "the owner is the actor, so no worker and no reviewer can satisfy it",
    fix: 'move it to an "Owner checklist:" list (shown to the owner, never handed to the worker or a reviewer as acceptance), or rewrite it as an observable result a worker produces',
  };
}

function humanActorFinding(phaseId: string, item: string, line: number | undefined, sourceFile: string | undefined): LintFinding {
  return {
    severity: "error",
    rule: "human-actor",
    phaseId,
    item,
    line,
    sourceFile,
    problem: "a human is the actor, so no worker and no reviewer can satisfy it",
    fix: 'move it to an "Owner checklist:" list, or rewrite it as an observable result a worker or a command produces',
  };
}

/** Lint one plan (all its phases' acceptance items). Pure. */
export function lintPlan(plan: LintPlanInput): LintFinding[] {
  const out: LintFinding[] = [];
  for (const phase of plan.phases ?? []) {
    const phaseId = phase.id ?? "?";
    const acceptance = phase.acceptance ?? [];
    acceptance.forEach((item, i) => {
      const line = phase.acceptanceLines?.[i];
      const sourceFile = plan.sourceFile;
      // An error stops the run, so one per item is enough; warnings are only
      // added for items that are satisfiable as written.
      if (OWNER_ACTOR_RE.test(item)) {
        out.push(ownerActorFinding(phaseId, item, line, sourceFile));
        return;
      }
      if (HUMAN_ACTOR_RE.test(item)) {
        out.push(humanActorFinding(phaseId, item, line, sourceFile));
        return;
      }
      const future = futurePhrase(item);
      if (future) {
        out.push({
          severity: "warning",
          rule: "future-dependency",
          phaseId,
          item,
          line,
          sourceFile,
          problem: `it depends on a future state ("${future}") that does not exist when the checks run and the reviewers judge the candidate`,
          fix: 'state a fact that is true when the run finishes, or move it to an "Owner checklist:" item if it is a later owner action',
        });
      }
      if (hasNoTolerance(item)) {
        out.push({
          severity: "warning",
          rule: "no-tolerance",
          phaseId,
          item,
          line,
          sourceFile,
          problem: "it compares a measured value against a contracted limit with no stated tolerance",
          fix: 'state the allowed margin (for example "p99 ≤ the contracted interval × 1.1", or "within 10 %"), or name the recorded gap a miss becomes',
        });
      }
    });
  }
  return out;
}

/** Lint every entry's plan in a program file; phase ids are prefixed with the
 * entry id so two entries with the same phase id stay tellable apart. */
export function lintProgram(program: LintProgramInput): LintFinding[] {
  const out: LintFinding[] = [];
  for (const entry of program.entries ?? []) {
    for (const finding of lintPlan(entry.plan ?? { phases: [] })) {
      out.push({ ...finding, phaseId: entry.id ? `${entry.id}/${finding.phaseId}` : finding.phaseId });
    }
  }
  return out;
}

export function hasLintErrors(findings: readonly LintFinding[]): boolean {
  return findings.some((f) => f.severity === "error");
}

/** One finding as text: `file:line: severity: problem: "item"` plus an
 * indented `fix:` line. `file:line:` is the shape `compilation-mode` jumps on. */
export function formatFinding(finding: LintFinding, fallbackFile = "<plan>"): string {
  const where = `${finding.sourceFile ?? fallbackFile}${finding.line !== undefined ? `:${finding.line}` : ""}`;
  return `${where}: ${finding.severity}: [${finding.phaseId}] ${finding.problem}: "${finding.item}"\n  fix: ${finding.fix}`;
}

export function formatFindings(findings: readonly LintFinding[], fallbackFile = "<plan>"): string {
  return findings.map((f) => formatFinding(f, fallbackFile)).join("\n");
}
