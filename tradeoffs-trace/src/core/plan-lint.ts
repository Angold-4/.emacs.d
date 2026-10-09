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

import { matchesGlob } from "./boundaries.ts";
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
  // Plan 06j (A1): a phase's checks cannot see what its :BOUNDARIES: let it
  // change, or the repository's CI runs something the plan never does.
  | "coverage"
  // Plan 06g: `#+TT_WORKERS`/`#+TT_ROUNDS` and, from 06h, the reviewer count
  // and seat list.
  | "worker-count"
  // Plan 06h (A4): the reviewer seat list and its leader.
  | "reviewer-seat"
  | "reviewer-leader"
  // Plan 06b: the structured-plan rules (refs/06_ref_plan_format.md).
  | "item-id"
  | "item-arch"
  | "item-verify"
  | "item-text-loss";

/** The roles #+TT_MODELS may assign a model to. */
const MODEL_ROLES = new Set(["worker", "reviewer", "evaluator", "panel", "curator"]);

/** The default seats when the plan declares none. */
const DEFAULT_REVIEWER_SEATS = ["M", "A", "B"];

/** Plan 06h (A4): the smallest reviewer count that is odd and greater than
 * the worker count — 1 or 2 workers need 3, 3 or 4 need 5, 5 or 6 need 7. */
export function smallestReviewerCount(workers: number): number {
  const w = Number.isInteger(workers) && workers >= 1 ? workers : 1;
  return w % 2 === 0 ? w + 1 : w + 2;
}

/** The seats a plan declares, or the default `M A B`. */
function reviewerSeatsOf(plan: LintPlanInput): string[] {
  const declared = plan.seats;
  if (Array.isArray(declared) && declared.length > 0) return declared;
  return [...DEFAULT_REVIEWER_SEATS];
}

/** The lane numbers a plan's worker count names, 1-based. A plan without
 * `#+TT_REVIEWERS` is the 06g2 plan and names at most two lanes. */
function workerLanesOf(plan: LintPlanInput): string[] {
  const hasNewSeats = Array.isArray(plan.seats) && plan.seats.length > 0;
  const n = Number.isInteger(plan.workers) && (plan.workers as number) >= 1 ? (plan.workers as number) : 1;
  // A 06g2 plan (no `#+TT_REVIEWERS`) names at most two lanes, whatever its
  // worker count, so worker.1/worker.2 stay valid and worker.3 is 06h's.
  return Array.from({ length: hasNewSeats ? n : 2 }, (_, i) => String(i + 1));
}

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
  /** Plan 06j (A1): the phase's ordinary checks (`:CHECKS:`), as the
   * coverage rule reads them. */
  checks?: string[];
  /** Plan 06j (A1): the phase's `:BOUNDARIES:` globs. */
  boundaries?: string[];
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
  /** Plan 06h (A4/C2): a phase's own `:REVIEWERS:` override. It must satisfy
   * the same rule as the plan's `#+TT_REVIEWERS`. */
  seats?: string[];
  /** Plan 06h (A4/C2): a phase's own `:LEADER:` override. */
  leader?: string;
  /** Plan 06h: the 1-based line of the phase headline, used as the line for
   * a phase-override finding when the property's own line is not recorded. */
  line?: number;
}

export interface LintPlanInput {
  /** The Org file this JSON came from, when known. */
  sourceFile?: string;
  /** Plan 06j (A1): the absolute path of the git repository the plan runs
   * on (`#+TT_REPO:`). The coverage rule reads it; absent means no coverage
   * facts and no coverage warnings. */
  repo?: string;
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
  /** Plan 06g/06h: the plan's `#+TT_WORKERS:` — how many lanes one round
   * runs. Any positive whole number from 06h on. Absent (or 1) is today's
   * single-candidate loop. */
  workers?: number;
  /** Lint-only: the 1-based line of `#+TT_WORKERS:` in the source Org file. */
  workersLine?: number;
  /** Plan 06g: the plan's `#+TT_ROUNDS:` — how many rounds one phase may
   * spend before it parks on the owner. Absent means the default of 3. */
  rounds?: number;
  /** Lint-only: the 1-based line of `#+TT_ROUNDS:` in the source Org file. */
  roundsLine?: number;
  /** Plan 06h (A1/A4): the plan's `#+TT_REVIEWERS:` — the odd list of
   * reviewer seats. Absent means `M A B`. The JSON field is `seats`, the
   * same one the conductor freezes into the contract. */
  seats?: string[];
  /** Lint-only: the 1-based line of `#+TT_REVIEWERS:`. */
  seatsLine?: number;
  /** Plan 06h (A1/A4): the plan's `#+TT_LEADER:`. Absent means the first
   * seat. */
  leader?: string;
  /** Lint-only: the 1-based line of `#+TT_LEADER:`. */
  leaderLine?: number;
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
  /** Plan 06h (A1/A4): a program file may also declare `#+TT_REVIEWERS:` and
   * `#+TT_LEADER:`; the same rules apply. */
  seats?: string[];
  seatsLine?: number;
  leader?: string;
  leaderLine?: number;
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
  kind: "role" | "reviewer-seat" | "panel-seat" | "worker-seat";
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
    if (key === "reviewerSeats" || key === "panelSeats" || key === "panelFrom" || key === "workerLanes") continue;
    out.push({ key, model: asRoleModel(raw), kind: "role" });
  }
  for (const [seat, model] of seatEntries(models.reviewerSeats)) out.push({ key: `reviewer.${seat}`, model, kind: "reviewer-seat", seat });
  for (const [seat, model] of seatEntries(models.panelSeats)) out.push({ key: `panel.${seat}`, model, kind: "panel-seat", seat });
  // Plan 06g2: a lane's own worker model (`worker.1`, `worker.2`).
  for (const [seat, model] of seatEntries(models.workerLanes)) out.push({ key: `worker.${seat}`, model, kind: "worker-seat", seat });
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
    const seats = reviewerSeatsOf(plan);
    const panelSeats = seats.map((_, i) => String(i + 1));
    const lanes = workerLanesOf(plan);
    const hasNewSeats = Array.isArray(plan.seats) && plan.seats.length > 0;
    if (decl.kind === "reviewer-seat" && !seats.includes(decl.seat!)) {
      out.push(
        modelFinding(
          plan,
          decl.key,
          hasNewSeats
            ? `#+TT_MODELS names the reviewer seat ${decl.key}, which #+TT_REVIEWERS does not declare`
            : `#+TT_MODELS names the unknown reviewer seat ${decl.key}`,
          hasNewSeats
            ? `name one of the declared seats (${seats.map((s) => `reviewer.${s}`).join(", ")}), or add ${decl.seat} to #+TT_REVIEWERS`
            : "use reviewer.M, reviewer.A or reviewer.B",
        ),
      );
      continue;
    }
    if (decl.kind === "panel-seat" && !panelSeats.includes(decl.seat!)) {
      out.push(
        modelFinding(
          plan,
          decl.key,
          hasNewSeats
            ? `#+TT_MODELS names the panel seat ${decl.key}, which the ${seats.length} reviewers do not have`
            : `#+TT_MODELS names the unknown panel seat ${decl.key}`,
          hasNewSeats ? `use one of panel.${panelSeats.join(", panel.")} (one panel seat per reviewer)` : "use panel.1, panel.2 or panel.3",
        ),
      );
      continue;
    }
    if (decl.kind === "worker-seat" && !lanes.includes(decl.seat!)) {
      if (Array.isArray(plan.seats) && plan.seats.length > 0) {
        out.push(
          modelFinding(
            plan,
            decl.key,
            `#+TT_MODELS names the worker lane ${decl.key}, but #+TT_WORKERS declares ${lanes.length}`,
            `use one of worker.${lanes.join(", worker.")}`,
          ),
        );
      } else {
        out.push(
          modelFinding(
            plan,
            decl.key,
            `#+TT_MODELS names the unknown worker lane ${decl.key}`,
            "use worker.1 or worker.2 (this plan version runs one or two lanes; more is 06h's phase)",
          ),
        );
      }
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

/** Plan 06h (A4/C2): lint a phase's own `:REVIEWERS:`/`:LEADER:` override
 * with the same rule as the plan's `#+TT_REVIEWERS`/`#+TT_LEADER`. The
 * effective worker count is the phase's own `:WORKERS:` or the plan's. */
export function lintPhaseSeats(phase: LintPhaseInput, plan: LintPlanInput): LintFinding[] {
  const hasSeats = Array.isArray(phase.seats) && phase.seats.length > 0;
  const hasLeader = typeof phase.leader === "string" && phase.leader.trim().length > 0;
  if (!hasSeats && !hasLeader) return [];
  const out: LintFinding[] = [];
  const phaseId = `${phase.id ?? "?"}/reviewers`;
  const line = phase.line ?? plan.seatsLine;
  const sourceFile = plan.sourceFile;
  const effectiveSeats = hasSeats ? phase.seats! : plan.seats && plan.seats.length > 0 ? plan.seats : [...DEFAULT_REVIEWER_SEATS];
  if (hasSeats) {
    if (phase.seats!.length % 2 === 0) {
      out.push({
        severity: "error",
        rule: "reviewer-seat",
        phaseId,
        item: `:REVIEWERS: ${phase.seats!.join(" ")}`,
        line,
        sourceFile,
        problem: `the phase :REVIEWERS: names ${phase.seats!.length} seats; the count must be odd`,
        fix: `write an odd number of seats (${phase.seats!.length + 1}, or ${Math.max(1, phase.seats!.length - 1)})`,
      });
    }
    const seen = new Set<string>();
    const dupes = new Set<string>();
    for (const s of phase.seats!) {
      if (seen.has(s)) dupes.add(s);
      seen.add(s);
    }
    if (dupes.size > 0) {
      out.push({
        severity: "error",
        rule: "reviewer-seat",
        phaseId,
        item: `:REVIEWERS: ${phase.seats!.join(" ")}`,
        line,
        sourceFile,
        problem: `the phase :REVIEWERS: names ${[...dupes].join(", ")} more than once`,
        fix: "give each seat a unique name",
      });
    }
    const workers = Number.isInteger(phase.workers) && (phase.workers as number) >= 1 ? (phase.workers as number) : Number.isInteger(plan.workers) && (plan.workers as number) >= 1 ? (plan.workers as number) : 1;
    const smallest = smallestReviewerCount(workers);
    if (phase.seats!.length < smallest) {
      out.push({
        severity: "error",
        rule: "worker-count",
        phaseId,
        item: `:REVIEWERS: ${phase.seats!.join(" ")}`,
        line,
        sourceFile,
        problem: `${workers} worker${workers === 1 ? "" : "s"} needs at least ${smallest} reviewers; the phase :REVIEWERS: names ${phase.seats!.length}`,
        fix: `name at least ${smallest} reviewers in the phase :REVIEWERS:`,
      });
    }
  }
  if (hasLeader && !effectiveSeats.includes(phase.leader!)) {
    out.push({
      severity: "error",
      rule: "reviewer-leader",
      phaseId: `${phase.id ?? "?"}/leader`,
      item: `:LEADER: ${phase.leader}`,
      line,
      sourceFile,
      problem: `the phase :LEADER: names ${phase.leader}, which is not one of the reviewer seats (${effectiveSeats.join(", ")})`,
      fix: `name one of the declared seats (${effectiveSeats.join(", ")}), or add ${phase.leader} to the phase :REVIEWERS:`,
    });
  }
  return out;
}

/** Lint one plan (all its phases' acceptance items, its structured items,
 * its #+TT_MODELS and its #+TT_RERUN). Pure. */
export function lintPlan(plan: LintPlanInput): LintFinding[] {
  const out: LintFinding[] = [...lintModels(plan), ...lintRerun(plan), ...lintWorkers(plan)];
  for (const phase of plan.phases ?? []) {
    const phaseId = phase.id ?? "?";
    for (const finding of lintPhaseSeats(phase, plan)) out.push(finding);
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

/** Plan 06g/06h: lint `#+TT_WORKERS:`, `#+TT_ROUNDS:`, `#+TT_REVIEWERS:` and
 * `#+TT_LEADER:`. `#+TT_WORKERS` is any positive whole number of lanes from
 * 06h on; `#+TT_ROUNDS` is a whole number from 1 to 5; the reviewer list must
 * be odd, non-empty and duplicate-free, with at least one reviewer more than
 * the lanes; the leader must name a seat. A value that is not a whole number
 * is an error too: it would reach the conductor as `NaN`.
 *
 * Lint never rounds a count: a non-integer is refused, not silently floored. */
export function lintWorkers(plan: LintPlanInput): LintFinding[] {
  const out: LintFinding[] = [];
  const seats = plan.seats;
  // Plan 06h: a plan that names no `#+TT_REVIEWERS` is the 06g2 plan, so it
  // keeps the two-lane limit; the new keywords are what opt into more lanes.
  const hasNewSeats = Array.isArray(seats) && seats.length > 0;
  const declaredSeats = hasNewSeats ? seats! : [...DEFAULT_REVIEWER_SEATS];
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
        fix: "write #+TT_WORKERS: 1 (today's loop), or any whole number of lanes from 1 on",
      });
    } else if (!hasNewSeats && n > 2) {
      // The 06g2 behaviour, kept for a plan without `#+TT_REVIEWERS`.
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
    } else if (!hasNewSeats && n === 2) {
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
  // Plan 06h (A4): the reviewer list must be non-empty, odd, duplicate-free
  // and greater than the worker count.
  if (Array.isArray(seats)) {
    if (seats.length === 0) {
      out.push({
        severity: "error",
        rule: "reviewer-seat",
        phaseId: "reviewers",
        item: "#+TT_REVIEWERS: (empty)",
        line: plan.seatsLine,
        sourceFile: plan.sourceFile,
        problem: "#+TT_REVIEWERS names no seat",
        fix: "write an odd list of seats, for example #+TT_REVIEWERS: M A B",
      });
    } else {
      if (seats.length % 2 === 0) {
        out.push({
          severity: "error",
          rule: "reviewer-seat",
          phaseId: "reviewers",
          item: `#+TT_REVIEWERS: ${seats.join(" ")}`,
          line: plan.seatsLine,
          sourceFile: plan.sourceFile,
          problem: `#+TT_REVIEWERS names ${seats.length} seats; the count must be odd`,
          fix: `write an odd number of seats (${seats.length + 1}, or ${Math.max(1, seats.length - 1)})`,
        });
      }
      const seen = new Set<string>();
      const dupes = new Set<string>();
      for (const s of seats) {
        if (seen.has(s)) dupes.add(s);
        seen.add(s);
      }
      if (dupes.size > 0) {
        out.push({
          severity: "error",
          rule: "reviewer-seat",
          phaseId: "reviewers",
          item: `#+TT_REVIEWERS: ${seats.join(" ")}`,
          line: plan.seatsLine,
          sourceFile: plan.sourceFile,
          problem: `#+TT_REVIEWERS names ${[...dupes].join(", ")} more than once`,
          fix: "give each seat a unique name",
        });
      }
    }
  }
  const workers = Number.isInteger(plan.workers) && (plan.workers as number) >= 1 ? (plan.workers as number) : 1;
  const smallest = smallestReviewerCount(workers);
  if (hasNewSeats && declaredSeats.length < smallest) {
    out.push({
      severity: "error",
      rule: "worker-count",
      phaseId: "workers",
      item: `#+TT_WORKERS: ${workers}`,
      line: plan.workersLine ?? plan.seatsLine,
      sourceFile: plan.sourceFile,
      problem: `${workers} worker${workers === 1 ? "" : "s"} needs at least ${smallest} reviewers; #+TT_REVIEWERS names ${declaredSeats.length}`,
      fix: `name at least ${smallest} reviewers, for example #+TT_REVIEWERS: ${Array.from({ length: smallest }, (_, i) => String.fromCharCode(65 + i)).join(" ")}`,
    });
  }
  // Plan 06h (A4): the leader must name a seat.
  if (typeof plan.leader === "string" && plan.leader.trim().length > 0 && !declaredSeats.includes(plan.leader)) {
    out.push({
      severity: "error",
      rule: "reviewer-leader",
      phaseId: "leader",
      item: `#+TT_LEADER: ${plan.leader}`,
      line: plan.leaderLine,
      sourceFile: plan.sourceFile,
      problem: `#+TT_LEADER names ${plan.leader}, which is not one of the reviewer seats (${declaredSeats.join(", ")})`,
      fix: `name one of the declared seats (${declaredSeats.join(", ")}), or add ${plan.leader} to #+TT_REVIEWERS`,
    });
  }
  if (plan.rounds !== undefined) {
    const n = plan.rounds;
    if (!Number.isInteger(n) || n < 1 || n > 5) {
      out.push({
        severity: "error",
        rule: "worker-count",
        phaseId: "rounds",
        item: `#+TT_ROUNDS: ${String(n)}`,
        line: plan.roundsLine,
        sourceFile: plan.sourceFile,
        problem: `#+TT_ROUNDS must be a whole number of rounds from 1 to 5, got ${String(n)}`,
        fix: "write #+TT_ROUNDS: 3 (the default), or a whole number from 1 to 5",
      });
    }
  }
  return out;
}

// ---------------------------------------------------------------------------
// Plan 06j (A1): coverage warnings
// ---------------------------------------------------------------------------

/** One package/crate the repository defines. `dir` is repo-relative, with
 * POSIX separators (`.` for a package at the repository root). */
export interface RepoPackage {
  name: string;
  dir: string;
}

/** What `checkCoverageWarnings` reads about the repository. Built by
 * `readRepoFacts` (I/O) and passed in, so this module stays pure. */
export interface RepoFacts {
  /** Every package/crate the repository defines. */
  packages: RepoPackage[];
  /** Every command line the repository's CI workflows run. */
  ciCommands: string[];
  /** True for a cargo repository, whose packages are named by `-p`/
   * `--package`; a non-cargo package is named as a plain token. */
  cargo: boolean;
}

/** An empty fact set: a plan with no repository, or one whose path does not
 * exist, warns about nothing. */
export function emptyRepoFacts(): RepoFacts {
  return { packages: [], ciCommands: [], cargo: false };
}

/** True when `globSeg` matches one path segment (`*` matches within one
 * segment, exactly as core/boundaries.ts's `matchesGlob`). */
function segmentMatches(globSeg: string, dirSeg: string): boolean {
  return globSeg === "**" || matchesGlob(globSeg, dirSeg);
}

/** Can the glob match a path that starts with `dirSegs`? `**` consumes any
 * number of segments; every other segment matches exactly one. This is the
 * one place the "does a boundary reach this package" question is decided: a
 * glob whose first segment is `*` followed by `src/**` reaches package `a`
 * (it matches `a/src/...`), while `*.md` does not reach a nested package
 * (its first segment cannot match two). */
function globCanReachDir(globSegs: readonly string[], dirSegs: readonly string[]): boolean {
  const rec = (gi: number, di: number): boolean => {
    if (di === dirSegs.length) return true;
    if (gi === globSegs.length) return false;
    const seg = globSegs[gi];
    if (seg === "**") return rec(gi + 1, di) || rec(gi, di + 1);
    return segmentMatches(seg, dirSegs[di]) && rec(gi + 1, di + 1);
  };
  return rec(0, 0);
}

/** True when a `:BOUNDARIES:` glob covers a package directory: the glob can
 * match a path inside (or at) the directory, with `*` limited to one path
 * segment (`crates/**` covers `crates/a`; `crates/a/src/**` covers `crates/a`;
 * a `*` followed by `src/**` covers `a`; `*.md` does not cover
 * `packages/b`). */
export function globCoversDir(glob: string, dir: string): boolean {
  const g = glob.trim().replace(/\/+$/, "");
  const d = dir.trim().replace(/\/+$/, "") || ".";
  if (g.length === 0) return false;
  // A root package is the whole repository, so any repo-relative glob covers
  // it: a `src/**` boundary lets the phase change the single root crate.
  if (d === ".") return !g.startsWith("/") && !g.startsWith("..");
  const globSegs = g.split("/").filter((s) => s.length > 0);
  const dirSegs = d.split("/").filter((s) => s.length > 0);
  return globCanReachDir(globSegs, dirSegs);
}

/** True when a command names `name` the way the repository's package manager
 * does: cargo uses `-p`/`--package`; any other package is named as a plain
 * token (e.g. `--workspace a`). */
export function commandNamesPackage(command: string, name: string, cargo: boolean): boolean {
  const tokens = command.split(/\s+/).filter(Boolean);
  for (let i = 0; i < tokens.length; i++) {
    const t = tokens[i];
    if ((t === "-p" || t === "--package") && tokens[i + 1] === name) return true;
    if (t === `-p=${name}` || t === `--package=${name}`) return true;
    if (cargo && t.startsWith("-p") && t.length > 2 && t.slice(2) === name) return true;
  }
  return !cargo && tokens.includes(name);
}

/** True when a command runs `cargo fmt --all --check` in one invocation. A
 * plain `cargo fmt --all` reformats and exits 0, so it never catches the
 * rustfmt drift CI rejects; `--all` in another segment (`cargo fmt -p a
 * --check && cargo clippy --all`) is not the same invocation either. */
export function runsCargoFmtAll(command: string): boolean {
  for (const segment of command.split(/[;&|\n]+/)) {
    if (!/(^|\s)cargo\s+fmt(\s|$)/.test(segment)) continue;
    if (!/(^|\s)--all(\s|$)/.test(segment)) continue;
    if (!/(^|\s)--check(\s|$)/.test(segment)) continue;
    return true;
  }
  return false;
}

/** Plan 06j (A1): the ONE place coverage is judged. Warnings only, never
 * errors. For a cargo repository a phase warns when a crate whose directory
 * its `:BOUNDARIES:` cover is named by no `-p` in its checks or final checks;
 * for any repository a covered package the checks never name warns; and a
 * repository whose CI runs `cargo fmt --all` while no phase check or final
 * check does warns once. */
export function checkCoverageWarnings(plan: LintPlanInput, repo: RepoFacts): LintFinding[] {
  const out: LintFinding[] = [];
  const phases = plan.phases ?? [];
  const commandsOf = (phase: LintPhaseInput): string[] => [...(phase.checks ?? []), ...(phase.finalChecks ?? [])];
  for (const phase of phases) {
    const commands = commandsOf(phase);
    const boundaries = phase.boundaries ?? [];
    for (const pkg of repo.packages) {
      if (!boundaries.some((g) => globCoversDir(g, pkg.dir))) continue;
      if (commands.some((c) => commandNamesPackage(c, pkg.name, repo.cargo))) continue;
      out.push({
        severity: "warning",
        rule: "coverage",
        phaseId: phase.id ?? "?",
        item: `${pkg.name} (${pkg.dir})`,
        line: phase.line,
        sourceFile: plan.sourceFile,
        problem: `the phase's :BOUNDARIES: cover ${pkg.dir}, but no check or final check names the package ${pkg.name}`,
        fix: repo.cargo
          ? `add -p ${pkg.name} to the phase's :CHECKS: or :FINAL_CHECKS:, or narrow :BOUNDARIES:`
          : `name ${pkg.name} in the phase's :CHECKS: or :FINAL_CHECKS:, or narrow :BOUNDARIES:`,
      });
    }
  }
  const ciFmt = repo.ciCommands.find(runsCargoFmtAll);
  if (ciFmt !== undefined && !phases.some((p) => commandsOf(p).some(runsCargoFmtAll))) {
    out.push({
      severity: "warning",
      rule: "coverage",
      phaseId: "coverage",
      item: ciFmt,
      sourceFile: plan.sourceFile,
      problem: "the repository's CI runs `cargo fmt --all`, but no phase check or final check does",
      fix: "add `cargo fmt --all --check` to the phase's :FINAL_CHECKS:, or a plan-wide #+TT_FINAL_CHECKS:",
    });
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
