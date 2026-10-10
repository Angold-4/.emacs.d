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

import { parseVerify } from "./items.ts";
import { rerunTemplateIssue } from "./test-failures.ts";

export type LintSeverity = "error" | "warning";

export type LintRule =
  | "owner-actor"
  | "human-actor"
  | "future-dependency"
  | "no-tolerance"
  | "model-declaration"
  | "rerun-template"
  // Plan 06g: `#+TT_WORKERS` may only be 1 or 2 in this plan version; more
  // lanes are 06h's.
  | "worker-count"
  // Plan 06b: the structured-plan rules (refs/06_ref_plan_format.md).
  | "item-id"
  | "item-arch"
  | "item-verify"
  | "item-text-loss";

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

/** One structured item (`:ID:` subheading) as the linter reads it. The line
 * fields are recorded by the Emacs parser; a hand-written JSON plan may omit
 * them and the finding simply carries no line. */
export interface LintItemInput {
  id?: string;
  title?: string;
  text?: string;
  /** The exact Org body, when the parser recorded it. `lintItems` reports a
   * parsed `text` that is missing a line of `rawText`. */
  rawText?: string;
  /** Architecture only: the module/area the item lives in. */
  where?: string;
  whereLine?: number;
  tags?: string[];
  /** Requirements only: the architecture ids the item is realised by. */
  arch?: string[];
  /** Requirements and constraints: the raw `:VERIFY:` strings. */
  verify?: string[];
  line?: number;
  archLine?: number;
  verifyLine?: number;
}

/** The slice of a plan the linter needs. `RunPlanFile`/`RunPlanPhase` are
 * structurally assignable to it, so the CLI can pass a parsed plan directly. */
export interface LintPhaseInput {
  id?: string;
  /** The phase's Goal paragraph, when the parser read it. */
  goal?: string;
  acceptance?: string[];
  /** 1-based source lines of `acceptance`, parallel to it (Emacs records
   * these; absent for a hand-written JSON plan). */
  acceptanceLines?: number[];
  /** Plan 06b: the structured items of the phase subtree. Absent on an
   * old-format plan (only `acceptance`), where no item rule fires. */
  architecture?: LintItemInput[];
  requirements?: LintItemInput[];
  constraints?: LintItemInput[];
  /** Plan 06c: the phase's final check (`#+TT_FINAL_CHECKS`, overridden by
   * the phase's `:FINAL_CHECKS:`). Absent leaves the plan as before. */
  finalChecks?: string[];
}

export interface LintPlanInput {
  /** The Org file this JSON came from, when known. */
  sourceFile?: string;
  phases?: LintPhaseInput[];
  models?: LintModels;
  modelsLine?: number;
  /** Plan 05d: the `#+TT_RERUN:` single-test template (`{name}`/`{file}`).
   * Absent means the built-in Node/cargo defaults (or the strict rule). */
  rerun?: string;
  /** Lint-only: the 1-based line of `#+TT_RERUN:` in the source Org file. */
  rerunLine?: number;
  modelsRepeated?: string[];
  /** Plan 06e (A1): the `#+TT_ENV_FILE:` KEY=value file, resolved to an
   * absolute path by Emacs. A declared secret the environment does not set is
   * looked up here second. */
  envFile?: string;
  /** Roles this entry inherited from a program-level #+TT_MODELS. They are
   * already checked once at the program level; rechecking them per entry
   * would report the program's line against the entry's own file. */
  modelsFromProgram?: string[];
  /** Plan 06g: the plan's `#+TT_WORKERS:` — how many lanes one round runs.
   * This plan version allows 1 and 2; 3 or more is 06h's. Absent (or 1) is
   * today's single-candidate loop. */
  workers?: number;
  /** Lint-only: the 1-based line of `#+TT_WORKERS:` in the source Org file. */
  workersLine?: number;
  /** Plan 06g: the plan's `#+TT_ROUNDS:` — how many rounds one phase may
   * spend before it parks on the owner. Absent means the default of 3. */
  rounds?: number;
  /** Lint-only: the 1-based line of `#+TT_ROUNDS:` in the source Org file. */
  roundsLine?: number;
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
  /** Plan 06g: a program file may also declare `#+TT_WORKERS:` and
   * `#+TT_ROUNDS:`; the same rules apply. */
  workers?: number;
  workersLine?: number;
  rounds?: number;
  roundsLine?: number;
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

/** Plan 05d: lint `#+TT_RERUN:` — only `{name}`, `{file}` and `{crate}` are
 * known (each substituted as one shell word), and a template that never names
 * the failing test would run the same command for every one of them. Absent:
 * no findings, so a plan without the keyword is unchanged. */
export function lintRerun(plan: LintPlanInput): LintFinding[] {
  if (typeof plan.rerun !== "string" || plan.rerun.trim().length === 0) return [];
  const issue = rerunTemplateIssue(plan.rerun);
  if (!issue) return [];
  return [
    {
      severity: "error",
      rule: "rerun-template",
      // A rerun template is plan-wide, not phase-scoped; the `rerun` label
      // keeps `formatFinding`'s shape.
      phaseId: "rerun",
      item: plan.rerun,
      line: plan.rerunLine,
      sourceFile: plan.sourceFile,
      problem: `#+TT_RERUN has ${issue}`,
      fix: "write one command that runs one test, using {name} (and {file} only when the runner's output locates the test, {crate} only for a cargo `::` path), for example `node --test --test-name-pattern {name} {file}` or `cargo test -p {crate} --test {file} -- --exact {name}`",
      // A rerun template may name any of the known placeholders; each is
      // substituted as one single-quoted shell word.
    },
  ];
}

/** Lines of `rawText` that the parsed `text` does not carry: the proof that a
 * parser dropped part of an item (a sub-list or a source block cut off at the
 * first blank line, the #41 failure). Property-drawer and blank lines are not
 * item text and are ignored. */
export function lostTextLines(rawText: string | undefined, text: string | undefined): string[] {
  if (typeof rawText !== "string" || rawText.trim().length === 0) return [];
  const parsed = text ?? "";
  const missing: string[] = [];
  for (const raw of rawText.split("\n")) {
    const line = raw.trim();
    if (line.length === 0) continue;
    if (/^:(?:PROPERTIES|END):$/i.test(line)) continue;
    if (/^:[A-Za-z0-9_]+:\s*/.test(line)) continue;
    if (!parsed.includes(line)) missing.push(line);
  }
  return missing;
}

/** Plan 06b: lint a phase's structured items — a missing or duplicate `:ID:`,
 * an `:ARCH:` naming no architecture item, a `test` verify without a name, and
 * text lost in parsing (a line of `rawText` the parsed `text` does not
 * carry). */
export function lintItems(phase: LintPhaseInput): LintFinding[] {
  const out: LintFinding[] = [];
  const phaseId = phase.id ?? "?";
  const architecture = phase.architecture ?? [];
  const requirements = phase.requirements ?? [];
  const constraints = phase.constraints ?? [];
  const all: Array<{ kind: string; item: LintItemInput }> = [
    ...architecture.map((item) => ({ kind: "architecture", item })),
    ...requirements.map((item) => ({ kind: "requirement", item })),
    ...constraints.map((item) => ({ kind: "constraint", item })),
  ];
  const architectureIds = new Set(architecture.map((a) => a.id).filter((id): id is string => typeof id === "string" && id.length > 0));
  const seen = new Map<string, number>();
  const finding = (rule: LintRule, item: LintItemInput, line: number | undefined, problem: string, fix: string): LintFinding => ({
    severity: "error",
    rule,
    phaseId,
    item: (item.text ?? item.title ?? item.id ?? "(item)").split("\n")[0],
    line,
    sourceFile: undefined,
    problem,
    fix,
  });
  for (const { item } of all) {
    const id = item.id;
    if (typeof id !== "string" || id.trim().length === 0) {
      out.push(finding("item-id", item, item.line, "the item has no :ID: property", "give the subheading an :ID: property (for example A1, R2 or C1)"));
    } else if (seen.has(id)) {
      out.push(finding("item-id", item, item.line, `the item id ${id} is already used at line ${seen.get(id)}`, `give each item a unique :ID: (${id} is taken)`));
    } else {
      seen.set(id, item.line ?? 0);
    }
    for (const raw of item.verify ?? []) {
      for (const v of parseVerify(raw)) {
        if (v.kind === "test" && v.name.trim().length === 0) {
          out.push(finding("item-verify", item, item.verifyLine ?? item.line, `a test verify has no name: ${JSON.stringify(raw)}`, 'write test "<the test name>" (the name as the test runner prints it), or drop the test kind'));
        }
      }
    }
    const missing = lostTextLines(item.rawText, item.text);
    if (missing.length > 0) {
      out.push(finding("item-text-loss", item, item.line, `the parsed item text is missing ${missing.length} source line(s), starting with ${JSON.stringify(missing[0])}`, "keep the whole item body in the parsed text: a sub-list or a source block inside an item belongs to that item"));
    }
  }
  for (const item of requirements) {
    for (const arch of item.arch ?? []) {
      if (!architectureIds.has(arch)) {
        out.push(finding("item-arch", item, item.archLine ?? item.line, `:ARCH: names ${arch}, which is no architecture item of this phase`, "name an existing architecture item id (one of " + ([...architectureIds].join(", ") || "none") + "), or add the architecture item"));
      }
    }
  }
  return out;
}

/** Lint one plan (all its phases' acceptance items, its structured items,
 * its #+TT_MODELS and its #+TT_RERUN). Pure. */
export function lintPlan(plan: LintPlanInput): LintFinding[] {
  const out: LintFinding[] = [...lintModels(plan), ...lintRerun(plan), ...lintWorkers(plan)];
  for (const phase of plan.phases ?? []) {
    const phaseId = phase.id ?? "?";
    for (const finding of lintItems(phase)) out.push({ ...finding, sourceFile: plan.sourceFile });
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
  const out: LintFinding[] = [...lintModels(program), ...lintWorkers(program)];
  for (const entry of program.entries ?? []) {
    for (const finding of lintPlan(entry.plan ?? { phases: [] })) {
      out.push({ ...finding, phaseId: entry.id ? `${entry.id}/${finding.phaseId}` : finding.phaseId });
    }
  }
  return out;
}

/** Plan 06g: lint `#+TT_WORKERS:` — this plan version runs one or two lanes;
 * three or more lanes is 06h's phase (`#+TT_WORKERS` made adjustable), so the
 * message names 06h rather than inventing a rule the runtime does not have. A
 * value that is not a whole number is an error too: it would reach the
 * conductor as `NaN` lanes. `#+TT_ROUNDS` only needs to be a positive whole
 * number. */
export function lintWorkers(plan: LintPlanInput): LintFinding[] {
  const out: LintFinding[] = [];
  if (plan.workers !== undefined) {
    const n = plan.workers;
    if (!Number.isInteger(n) || n < 1) {
      out.push({
        severity: "error",
        rule: "worker-count",
        phaseId: "workers",
        item: `#+TT_WORKERS: ${String(n)}`,
        line: plan.workersLine,
        sourceFile: plan.sourceFile,
        problem: `#+TT_WORKERS must be a whole number of lanes, got ${String(n)}`,
        fix: "write #+TT_WORKERS: 1 (today's loop) or #+TT_WORKERS: 2 (two lanes)",
      });
    } else if (n > 2) {
      out.push({
        severity: "error",
        rule: "worker-count",
        phaseId: "workers",
        item: `#+TT_WORKERS: ${n}`,
        line: plan.workersLine,
        sourceFile: plan.sourceFile,
        problem: `#+TT_WORKERS names ${n} lanes; this plan version runs at most 2`,
        fix: "write #+TT_WORKERS: 2, or wait for 06h, the phase that makes the lane count adjustable (3, 4, … lanes)",
      });
    } else if (n === 2) {
      // Plan 06g (C2): two lanes are valid, but the lane round itself lands in
      // 06g2 — until then a `#+TT_WORKERS: 2` plan runs one lane, and the
      // owner is told rather than silently getting half of what they asked.
      out.push({
        severity: "warning",
        rule: "worker-count",
        phaseId: "workers",
        item: `#+TT_WORKERS: 2`,
        line: plan.workersLine,
        sourceFile: plan.sourceFile,
        problem: "the two-lane round arrives in 06g2; this run uses one lane",
        fix: "leave #+TT_WORKERS: 2 (the round lands in 06g2), or write #+TT_WORKERS: 1 for today's single-candidate loop",
      });
    }
  }
  if (plan.rounds !== undefined) {
    const n = plan.rounds;
    if (!Number.isInteger(n) || n < 1) {
      out.push({
        severity: "error",
        rule: "worker-count",
        phaseId: "rounds",
        item: `#+TT_ROUNDS: ${String(n)}`,
        line: plan.roundsLine,
        sourceFile: plan.sourceFile,
        problem: `#+TT_ROUNDS must be a positive whole number of rounds, got ${String(n)}`,
        fix: "write #+TT_ROUNDS: 3 (the default) or the number of rounds this phase may spend",
      });
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
