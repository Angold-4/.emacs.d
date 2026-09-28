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

export type LintRule = "owner-actor" | "human-actor" | "future-dependency" | "no-tolerance" | "model-declaration";

/** The roles #+TT_MODELS may assign a model to. */
const MODEL_ROLES = new Set(["worker", "reviewer", "evaluator", "panel", "curator"]);

/** The seat names `reviewer.N` and `panel.N` accept (design §2.1). */
const REVIEWER_SEATS = new Set(["M", "A", "B"]);
const PANEL_SEATS = new Set(["1", "2", "3"]);

/** The slice of #+TT_MODELS the linter reads. `models` is what Emacs parsed
 * (`{ worker?: { provider?, model }, reviewerSeats?: { M?: … }, … }`), with
 * unknown keys kept so the linter can name them; `modelsRepeated` lists the
 * keys the keyword named more than once (a JSON object cannot carry a
 * duplicate key); `modelsLine` is the 1-based line of the keyword, for
 * `file:line:`. */
export interface LintRoleModel {
  provider?: string;
  model?: string;
}

/** A parsed `models` map. Kept open (`unknown` values) so an unknown role or
 * seat the parser preserved still reaches the linter. */
export interface LintModels {
  [key: string]: unknown;
}

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
  models?: LintModels;
  modelsLine?: number;
  modelsRepeated?: string[];
  /** Roles this entry inherited from a program-level #+TT_MODELS. They are
   * already checked once at the program level; rechecking them per entry
   * would report the program's line against the entry's own file. */
  modelsFromProgram?: string[];
}

export interface LintProgramInput {
  /** The Org program file, so a program-level finding names it rather than
   * the temporary JSON copy Emacs deletes. */
  sourceFile?: string;
  entries?: Array<{ id?: string; plan?: LintPlanInput }>;
  /** A program file may also declare #+TT_MODELS; Emacs records the same
   * three fields at the program level so a bad default is reported once. */
  models?: LintModels;
  modelsLine?: number;
  modelsRepeated?: string[];
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

function modelFinding(plan: LintPlanInput, item: string, problem: string, fix: string): LintFinding {
  return {
    severity: "error",
    rule: "model-declaration",
    // A model declaration is plan-wide, not phase-scoped; the "models" label
    // keeps `formatFinding`'s shape and reads as `e1/models` in a program.
    phaseId: "models",
    item,
    line: plan.modelsLine,
    sourceFile: plan.sourceFile,
    problem,
    fix,
  };
}

/** One declaration inside `models`: a flat role, or a `reviewer.M`/`panel.1`
 * seat. `key` is the spelling the linter reports (`worker`, `reviewer.M`). */
interface ModelDecl {
  key: string;
  model: LintRoleModel | undefined;
  kind: "role" | "reviewer-seat" | "panel-seat";
  seat?: string;
}

function asRoleModel(raw: unknown): LintRoleModel | undefined {
  return raw && typeof raw === "object" && !Array.isArray(raw) ? (raw as LintRoleModel) : undefined;
}

/** The seats inside a `reviewerSeats`/`panelSeats` object, unknown names
 * included (the parser keeps them so the linter can report them). */
function seatEntries(raw: unknown): Array<[string, LintRoleModel | undefined]> {
  if (!raw || typeof raw !== "object" || Array.isArray(raw)) return [];
  return Object.entries(raw as Record<string, unknown>).map(([seat, model]) => [seat, asRoleModel(model)] as [string, LintRoleModel | undefined]);
}

/** Every model declaration a parsed `models` map carries, roles and seats. */
function modelDeclarations(models: LintModels | undefined): ModelDecl[] {
  if (!models) return [];
  const out: ModelDecl[] = [];
  for (const [key, raw] of Object.entries(models)) {
    if (key === "reviewerSeats" || key === "panelSeats" || key === "panelFrom") continue;
    out.push({ key, model: asRoleModel(raw), kind: "role" });
  }
  for (const [seat, model] of seatEntries(models.reviewerSeats)) out.push({ key: `reviewer.${seat}`, model, kind: "reviewer-seat", seat });
  for (const [seat, model] of seatEntries(models.panelSeats)) out.push({ key: `panel.${seat}`, model, kind: "panel-seat", seat });
  return out;
}

/** Lint #+TT_MODELS: an unknown role or seat name, a key given twice, a key
 * with no model, or `panel=reviewers` together with an explicit `panel.N` is
 * an error (each would reach `launchArgs` as a broken or ambiguous flag).
 * Absent: no findings, so a plan without the keyword is unchanged. */
export function lintModels(plan: LintPlanInput): LintFinding[] {
  const out: LintFinding[] = [];
  const inherited = new Set(plan.modelsFromProgram ?? []);
  for (const key of plan.modelsRepeated ?? []) {
    if (inherited.has(key)) continue;
    out.push(modelFinding(plan, key, `#+TT_MODELS names ${key} more than once`, `declare each key once: ${key}=<provider>:<model>`));
  }
  for (const decl of modelDeclarations(plan.models)) {
    if (inherited.has(decl.key)) continue;
    if (decl.kind === "role" && !MODEL_ROLES.has(decl.key)) {
      out.push(modelFinding(plan, decl.key, `#+TT_MODELS names the unknown role ${decl.key}`, "use one of worker, reviewer, evaluator, panel, curator"));
      continue;
    }
    if (decl.kind === "reviewer-seat" && !REVIEWER_SEATS.has(decl.seat!)) {
      out.push(modelFinding(plan, decl.key, `#+TT_MODELS names the unknown reviewer seat ${decl.key}`, "use reviewer.M, reviewer.A or reviewer.B"));
      continue;
    }
    if (decl.kind === "panel-seat" && !PANEL_SEATS.has(decl.seat!)) {
      out.push(modelFinding(plan, decl.key, `#+TT_MODELS names the unknown panel seat ${decl.key}`, "use panel.1, panel.2 or panel.3"));
      continue;
    }
    const model = decl.model?.model;
    if (typeof model !== "string" || model.length === 0) {
      out.push(
        modelFinding(plan, `${decl.key}=`, `#+TT_MODELS gives ${decl.key} an empty model`, `write ${decl.key}=<model>, or ${decl.key}=<provider>:<model> when another provider is needed`),
      );
    }
  }
  // panel=reviewers and an explicit panel seat contradict each other: the
  // seat's own model would silently lose to the reviewer's, so the owner
  // cannot tell which ran.
  if (plan.models?.panelFrom !== undefined) {
    const value = plan.models.panelFrom;
    if (value !== "reviewers") {
      if (!inherited.has("panelFrom")) {
        out.push(modelFinding(plan, `panel=${String(value)}`, `#+TT_MODELS sets panel to the unknown value ${String(value)}`, "write panel=reviewers to follow the reviewer seats, or panel=<model> for one model"));
      }
    } else {
      const seats = modelDeclarations(plan.models).filter((d) => d.kind === "panel-seat");
      const allInherited = inherited.has("panelFrom") && seats.length > 0 && seats.every((d) => inherited.has(d.key));
      if (seats.length > 0 && !allInherited) {
        out.push(
          modelFinding(
            plan,
            "panel=reviewers",
            "#+TT_MODELS sets panel=reviewers together with an explicit panel seat",
            "choose one: panel=reviewers for every seat, or panel.N=<model> for the seats you name",
          ),
        );
      }
    }
  }
  return out;
}

/** Lint one plan (all its phases' acceptance items and its #+TT_MODELS). Pure. */
export function lintPlan(plan: LintPlanInput): LintFinding[] {
  const out: LintFinding[] = [...lintModels(plan)];
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
  const out: LintFinding[] = [...lintModels(program)];
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
