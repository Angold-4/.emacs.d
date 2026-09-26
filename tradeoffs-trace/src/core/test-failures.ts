// Plan 01e (design D2): pre-existing check failures.
//
// A phase's base (its `integrationHead`) can already fail the phase's own
// checks before the worker touches anything — atlas plan 13's `feat/atlas`
// base had 14 deterministic `exchange-state-machine` test failures, so an
// acceptance item saying "`cargo test --workspace` passes" was unmeetable and
// every worker round paid for the same red output. The conductor runs the
// checks once on the base, records which test names failed there, and — D2's
// default — counts a candidate's failing check as passing when every test name
// its output yields also failed on the base. A check whose output yields no
// test name at all always fails (the strict rule), so a compile error or a
// timeout is never silently excused: a new failure is never hidden.
//
// This module is pure: parsing recorded output, classifying one failing check
// against the base's own failing names, and rendering the record. The
// conductor owns the run that produces the output and the file that keeps the
// result; the view reads that file through `parseBaseline`.

/** One check command's baseline result. */
export interface BaselineCommand {
  /** The command exactly as it was run (plan secrets already resolved). */
  command: string;
  /** Process exit status, or null when the command was killed by a signal. */
  exitCode: number | null;
  signal?: string | null;
  timedOut: boolean;
  durationMs: number;
  /** Failing test names parsed from this command's output (deduped, in order). */
  failures: string[];
  /** Base name of the command's log under `<run>/checks/base/`. */
  log?: string;
}

/** The whole baseline: every effective check command run once on the base. */
export interface Baseline {
  /** The base commit the checks ran on. */
  baseSha: string;
  /** The base commit's **full** tree object id — what "the same base" really
   * means. A program node whose branch is another commit with this same tree
   * reuses the record; `key`'s shortened tree prefix is only a file name, never
   * the identity a reuse decision is based on. */
  tree: string;
  /** `baselineKey` of that base: its tree plus the effective command list. */
  key: string;
  at: string;
  commands: BaselineCommand[];
  /** Every failing test name across `commands`, deduped, in order. */
  failures: string[];
}

/** True iff the command ran to completion and exited non-zero. Only this shape
 * may contribute an excuse: a timeout's output is truncated, a signal death
 * (SIGKILL from the OOM killer, SIGSEGV) never printed its last failure, and a
 * command that exited 0 did not fail at all — in all three cases its
 * `FAILED`-looking lines could not be trusted to name the base's whole failure
 * set. */
export function failedNormally(command: { exitCode: number | null; signal?: string | null; timedOut: boolean }): boolean {
  return command.exitCode !== null && command.exitCode !== 0 && command.signal == null && !command.timedOut;
}

/** A stable, filesystem-safe key for "the same base": the base tree's object
 * id plus a short hash of the effective command list, so two program nodes
 * that start from the same tree with the same checks share one baseline run.
 * The tree alone is not enough — two phases of one program can run different
 * checks from the same base — so callers also verify the command list before
 * reusing a record. */
export function baselineKey(tree: string, commands: readonly string[]): string {
  return `${tree.slice(0, 12)}-${shortHash(commands.join("\n"))}`;
}

/** djb2, hex — a filename-safe digest with no `node:crypto` import, so this
 * module stays a plain, dependency-free pure core module. */
function shortHash(text: string): string {
  let h = 5381;
  for (let i = 0; i < text.length; i++) h = ((h << 5) + h + text.charCodeAt(i)) >>> 0;
  return h.toString(16).padStart(8, "0");
}

/** A line that names a failing test in a recorded check output. Matched per
 * line (see `parseTestFailures`):
 *
 * - cargo:    `test exchange_state_machine::tests::foo ... FAILED`
 * - node:test: `not ok 3 - foo` (the TAP reporter), or the spec reporter's
 *   `✖ foo (1.2ms)` — which node uses by default even when piped
 * - ERT:     `   FAILED  2/3  git-review-test-foo` or `   FAILED  foo`
 *
 * Unrecognized output parses to no names, which is exactly what the strict
 * fallback needs. */
const CARGO_FAILED = /^test (\S+) \.\.\. FAILED$/;
const NODE_TAP_FAILED = /^\s*not ok \d+ - (.+)$/;
const NODE_SPEC_FAILED = /^\s*[✖✗]\s+(.+?)(?:\s+\(\d+(?:\.\d+)?ms\))?$/;
const ERT_FAILED = /^\s*FAILED\s+(?:\d+\/\d+\s+)?(\S+)/;
/** The spec reporter's summary heading (`✖ failing tests:`) is a marker, not
 * a test name. */
const NOT_A_TEST_NAME = /^failing tests?:$/;
/** A TAP line can carry a trailing ` # reason` comment. */
const TAP_COMMENT = /\s+#.*$/;
/** Color codes make every one of the patterns above miss. */
const ANSI = /\u001b\[[0-9;]*m/g;

/** Every failing test name `output` yields, deduped, in first-seen order.
 * An empty array means "nothing parseable" — the caller must then treat the
 * check as strictly failed. */
export function parseTestFailures(output: string): string[] {
  const text = output.replace(ANSI, "");
  const names: string[] = [];
  const seen = new Set<string>();
  const add = (raw: string): void => {
    const name = raw.trim();
    if (name.length === 0 || NOT_A_TEST_NAME.test(name) || seen.has(name)) return;
    seen.add(name);
    names.push(name);
  };
  for (const rawLine of text.split("\n")) {
    const line = rawLine.trimEnd();
    const cargo = line.match(CARGO_FAILED);
    if (cargo) {
      add(cargo[1]);
      continue;
    }
    const tap = line.match(NODE_TAP_FAILED);
    if (tap) {
      add(tap[1].replace(TAP_COMMENT, ""));
      continue;
    }
    const spec = line.match(NODE_SPEC_FAILED);
    if (spec) {
      add(spec[1]);
      continue;
    }
    const ert = line.match(ERT_FAILED);
    if (ert) add(ert[1]);
  }
  return names;
}

export interface CheckFailureVerdict {
  /** Failing test names parsed from this check command's output. */
  parsed: string[];
  /** Parsed names that did NOT fail on the base — the candidate's own blame. */
  newFailures: string[];
  /** True when D2's default may count the failing check as passing: at least
   * one name was parsed, and every parsed name already failed on the base. */
  excused: boolean;
}

/** Classify one failing check command's output against the base's own failing
 * test names (D2's default rule). No parsable name means no excuse. */
export function classifyCheckFailure(
  output: string,
  baseFailures: readonly string[],
  requiredTexts: readonly string[] = [],
): CheckFailureVerdict {
  const parsed = parseTestFailures(output);
  const base = new Set(baseFailures);
  // A test the phase is required to fix is never "pre-existing", even when
  // the base fails it too. Plan 14h: the base failed
  // `gate_is_rerun_when_the_base_moves`, the phase's own directive named it
  // as its job, and the gate excused the candidate's failure of it, so the
  // checks read green while the live gate record was stale.
  const newFailures = parsed.filter((name) => !base.has(name) || testIsNamedIn(name, requiredTexts));
  return { parsed, newFailures, excused: parsed.length > 0 && newFailures.length === 0 };
}

/** True when the test's own name (its last `::`, ` > ` or `/` segment, the
 * identifier a person writes) appears as a whole word in any of `texts`: the
 * phase's goal and acceptance items, and the owner's directives. */
export function testIsNamedIn(name: string, texts: readonly string[]): boolean {
  const leaf = name.split(/::| > |\//).pop()?.trim() ?? "";
  if (leaf.length < 4) return false;
  const escaped = leaf.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
  const re = new RegExp(`(^|[^A-Za-z0-9_])${escaped}([^A-Za-z0-9_]|$)`);
  return texts.some((t) => re.test(t));
}

/** The baseline commands whose failures the prompts may name as pre-existing:
 * the ones that failed normally (see `failedNormally`) and yielded parsable
 * names — exactly the set the gate would excuse, so the prompt's promise and
 * the gate's rule are the same rule. */
export function baselineFailedCommands(commands: readonly BaselineCommand[]): BaselineCommand[] {
  return commands.filter((c) => c.failures.length > 0 && failedNormally(c));
}

/** True iff `baseline` was taken over exactly this ordered command list — the
 * status view's guard, so a record from before a contract amendment (a check
 * changed) is not shown as the current pass's baseline. The conductor's own
 * reuse check is stricter still (same full base tree, masked forms); this one
 * is what a read-only view, which has no repo access, can honestly verify. */
export function baselineCoversCommands(baseline: Baseline | undefined, commands: readonly string[]): boolean {
  if (!baseline || baseline.commands.length !== commands.length) return false;
  return baseline.commands.every((c, i) => c.command === commands[i]);
}

/** Every failing test name across a baseline's commands, deduped, in order. */
export function baselineFailureNames(commands: readonly BaselineCommand[]): string[] {
  const seen = new Set<string>();
  const names: string[] = [];
  for (const command of commands) {
    for (const name of command.failures ?? []) {
      if (seen.has(name)) continue;
      seen.add(name);
      names.push(name);
    }
  }
  return names;
}

/** Shape-check a JSON-parsed baseline. Undefined for anything that is not one
 * (a corrupt or pre-01e file), so callers fall back to the strict rule. */
export function parseBaseline(value: unknown): Baseline | undefined {
  if (value === null || typeof value !== "object") return undefined;
  const raw = value as Record<string, unknown>;
  if (typeof raw.key !== "string" || typeof raw.baseSha !== "string" || !Array.isArray(raw.commands)) return undefined;
  const commands: BaselineCommand[] = [];
  for (const entry of raw.commands) {
    if (entry === null || typeof entry !== "object") return undefined;
    const c = entry as Record<string, unknown>;
    if (typeof c.command !== "string") return undefined;
    commands.push({
      command: c.command,
      exitCode: typeof c.exitCode === "number" ? c.exitCode : null,
      signal: typeof c.signal === "string" ? c.signal : null,
      timedOut: c.timedOut === true,
      durationMs: typeof c.durationMs === "number" ? c.durationMs : 0,
      failures: Array.isArray(c.failures) ? c.failures.filter((f): f is string => typeof f === "string") : [],
      ...(typeof c.log === "string" ? { log: c.log } : {}),
    });
  }
  const stored = Array.isArray(raw.failures) ? raw.failures.filter((f): f is string => typeof f === "string") : undefined;
  return {
    baseSha: raw.baseSha,
    // A record written before the tree field existed has no identity to trust,
    // so it can never be reused (a caller comparing it sees "" ≠ any tree).
    tree: typeof raw.tree === "string" ? raw.tree : "",
    key: raw.key,
    at: typeof raw.at === "string" ? raw.at : "",
    commands,
    failures: stored ?? baselineFailureNames(commands),
  };
}

/** The status line for a base that already fails its own checks, or undefined
 * when the base passes (or no baseline was taken). The `N tests` shape is
 * deliberate — the count is the number of *parsed* names, and a base whose
 * failing output yields none says so, because the checks then stay strict. */
export function baselineStatusLine(baseline: Baseline | undefined): string | undefined {
  if (!baseline) return undefined;
  const failed = baseline.commands.some((c) => c.timedOut || c.exitCode !== 0 || c.signal != null);
  if (!failed) return undefined;
  const names = baseline.failures;
  if (names.length === 0) return "base fails: 0 tests (no test names parsed; checks stay strict)";
  return `base fails: ${names.length} tests: ${names.join(", ")}`;
}
