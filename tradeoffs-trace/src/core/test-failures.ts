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

// Plan 06j (A2/A-8): every parser of check output strips the SAME escapes.
// A numeric-SGR-only regex misses a cursor-control prefix such as
// `ESC[?25l`, which would drop a cargo failure entirely; `stripAnsi`
// (items.ts) is the one implementation.
import { stripAnsi } from "./items.ts";

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
  /** Plan 05d: names that failed this command on the base but PASSED when
   * re-run alone — a base flake, visible and never excusing (finding #25).
   * Never part of `failures`, so a candidate failing one is never excused. */
  flakes?: string[];
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
  /** Plan 05d: every `base flake` across `commands`, deduped, in order. */
  flakes?: string[];
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

/** Plan 05d: which runner a parsed failing name came from. Only the Node test
 * runner and cargo have built-in single-test commands (`singleTestCommand`);
 * ERT names parse but have no default, so a plan that wants them re-run must
 * give a `#+TT_RERUN:` template. */
export type TestRunner = "node" | "cargo" | "ert";

/** One parsed failing test: its name, the runner whose output named it, and
 * the file the output located it in, when it did. For node:test the file comes
 * from the reporter (TAP's `location: …` block or the spec reporter's
 * `test at <file>…` line); for cargo, from the `Running <desc> <path>` line
 * above the test (an integration test's target). It is what `{file}` in a
 * `#+TT_RERUN:` template substitutes. */
export interface ParsedTestFailure {
  name: string;
  runner: TestRunner;
  file?: string;
}

/** Cargo's `Running … (target/<…>/deps/…)` line. It is `Running <kind> <path>`
 * for a unit-test target (`unittests src/lib.rs`) but `Running <path>` for an
 * integration test target (`tests/x.rs`). */
const CARGO_RUNNING = /^\s*Running\s+(.+?)\s+\(target\/[^)]*\/deps\/.*\)\s*$/;

/** TAP's location line (`location: '/tmp/x.test.js:3:1'`). */
const NODE_TAP_LOCATION = /^\s*location: '(.+?):\d+:\d+'\s*$/;
/** The spec reporter's own file line (`test at x.test.js:3:1`). */
const NODE_SPEC_LOCATION = /^\s*test at (.+?):\d+:\d+\s*$/;

/** Every failing test `output` yields, in first-seen order, each tagged with
 * the runner its line came from and (for node:test) the file the reporter
 * located it in, when one was written. Deduped by name. */
export function parseTestFailuresDetailed(output: string): ParsedTestFailure[] {
  const text = stripAnsi(output);
  const lines = text.split("\n").map((l) => l.trimEnd());
  const out: ParsedTestFailure[] = [];
  const byName = new Map<string, ParsedTestFailure>();
  let specFile: string | undefined;
  let cargoFile: string | undefined;
  const add = (raw: string, runner: TestRunner, file?: string): void => {
    const name = raw.trim();
    if (name.length === 0 || NOT_A_TEST_NAME.test(name)) return;
    const existing = byName.get(name);
    if (existing) {
      // The spec reporter lists a failure twice (in the run, then under
      // `failing tests:` after its `test at <file>` line); the later mention
      // is the one that carries the file, so fill it in rather than dropping
      // it with the duplicate.
      if (existing.file === undefined && file !== undefined) existing.file = file;
      return;
    }
    const parsed: ParsedTestFailure = file !== undefined ? { name, runner, file } : { name, runner };
    byName.set(name, parsed);
    out.push(parsed);
  };
  for (let i = 0; i < lines.length; i++) {
    const line = lines[i];
    // A `Running …` line sets the test target every following
    // `test <name> … FAILED` line belongs to. A unit-test target
    // (`unittests src/lib.rs`) is not a `--test` target, so it names no file.
    const running = line.match(CARGO_RUNNING);
    if (running) {
      const parts = running[1].trim().split(/\s+/);
      const path = parts[parts.length - 1];
      const desc = parts.length > 1 ? parts[0] : "";
      cargoFile = desc === "unittests" || !path.endsWith(".rs") ? undefined : path.replace(/^.*\//, "").replace(/\.rs$/, "");
      continue;
    }
    const cargo = line.match(CARGO_FAILED);
    if (cargo) {
      add(cargo[1], "cargo", cargoFile);
      continue;
    }
    // The spec reporter writes `test at <file>:<line>:<col>` just above the
    // `✖ <name>` line it belongs to.
    const specLoc = line.match(NODE_SPEC_LOCATION);
    if (specLoc) {
      specFile = specLoc[1];
      continue;
    }
    const tap = line.match(NODE_TAP_FAILED);
    if (tap) {
      // TAP puts the location inside the test's YAML block, after the
      // `not ok` line; read forward until the next test line.
      let file: string | undefined;
      for (let j = i + 1; j < Math.min(lines.length, i + 16); j++) {
        if (/^\s*(?:not ok|ok) \d+/.test(lines[j])) break;
        const loc = lines[j].match(NODE_TAP_LOCATION);
        if (loc) {
          file = loc[1];
          break;
        }
      }
      add(tap[1].replace(TAP_COMMENT, ""), "node", file);
      continue;
    }
    const spec = line.match(NODE_SPEC_FAILED);
    if (spec) {
      add(spec[1], "node", specFile);
      continue;
    }
    const ert = line.match(ERT_FAILED);
    if (ert) add(ert[1], "ert");
  }
  return out;
}

/** Every failing test name `output` yields, deduped, in first-seen order.
 * An empty array means "nothing parseable" — the caller must then treat the
 * check as strictly failed. */
export function parseTestFailures(output: string): string[] {
  return parseTestFailuresDetailed(output).map((f) => f.name);
}

// ---------------------------------------------------------------------------
// Plan 05d: re-running a newly failing test alone.
//
// A check can fail because the machine was loaded, not because the candidate
// is broken (findings #4, #10, #18, #25, #31, #35). Before a check that names
// new failures may fail the gate, each such test is re-run alone: a test that
// passes when re-run is `load-only`, one that still fails `reproduces alone`.
// The command comes from the plan's `#+TT_RERUN:` template (with `{name}`,
// `{file}` and `{crate}`) or from a built-in default for the Node test runner
// and cargo — the two runners this module parses. With no template and no
// default the strict rule applies: nothing is re-run and the failure stands.

/** One single-quoted shell word. A substituted test name, file or crate must
 * never split on a space or run as shell, so every value goes through this. */
export function shellQuote(value: string): string {
  return `'${value.replace(/'/g, `'\\''`)}'`;
}

/** A literal string as a JavaScript regular expression (Node reads
 * `--test-name-pattern` as one, so an unescaped name with metacharacters —
 * `a failing test ... (plan 14h)` — matches nothing and exits 0). */
export function escapeRegExp(value: string): string {
  return value.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
}

/** Every `{...}` placeholder a `#+TT_RERUN:` template contains. */
export function rerunPlaceholders(template: string): string[] {
  return [...template.matchAll(/\{([^}]*)\}/g)].map((m) => m[1]);
}

/** The placeholders a `#+TT_RERUN:` template may use, and what each resolves
 * to. `{crate}` is the crate the failing test lives in: cargo's own output
 * names its test module path, whose first `::` segment is the crate (a
 * template that cannot resolve one is never run, so a wrong crate can never
 * excuse a failure). */
export const RERUN_PLACEHOLDERS = ["name", "file", "crate"] as const;

/** Why a `#+TT_RERUN:` template cannot be used, or undefined when it is fine.
 * Only the `RERUN_PLACEHOLDERS` are known, and a template that never names the
 * failing test would run the same command for every one of them. */
export function rerunTemplateIssue(template: string): string | undefined {
  for (const placeholder of rerunPlaceholders(template)) {
    if (!(RERUN_PLACEHOLDERS as readonly string[]).includes(placeholder)) {
      return `the unknown placeholder {${placeholder}}`;
    }
  }
  if (!template.includes("{name}")) return "the template does not name the failing test ({name})";
  return undefined;
}

function runnerAndFileFor(output: string, name: string): ParsedTestFailure | undefined {
  return parseTestFailuresDetailed(output).find((f) => f.name === name);
}

/** The crate a cargo test path belongs to: its first `::` segment. Undefined
 * for a name with no `::`, so a `{crate}` template is then never run. */
export function crateOf(name: string): string | undefined {
  const parts = name.split("::");
  return parts.length > 1 && parts[0].length > 0 ? parts[0] : undefined;
}

/** Plan 05d: the single-test command for one failing name: the plan's
 * `#+TT_RERUN:` template when given, otherwise the built-in default for the
 * runner the name's own output line came from. Undefined means "no command is
 * known", and the caller must then keep the strict rule (no re-run).
 *
 * Every substituted value is one single-quoted shell word, and a template
 * that uses a placeholder whose value is unknown (`{file}` without the
 * reporter locating one, `{crate}` without a `::` path) cannot be built — so
 * a half-applied template never runs the wrong test. */
export function singleTestCommand(name: string, output: string, template?: string): string | undefined {
  const named = runnerAndFileFor(output, name);
  const file = named?.file;
  // `{crate}` is the failing test's own module path root (`a::b::c` → `a`); a
  // name with no `::` has none, so a `{crate}` template is then never run.
  const crate = crateOf(name);
  if (template !== undefined && template.trim().length > 0) {
    const values: Record<string, string | undefined> = { name, file, crate };
    for (const placeholder of rerunPlaceholders(template)) {
      if (values[placeholder] === undefined) return undefined;
    }
    // A function replacement, so a `$` in a name is never read as a
    // replacement pattern.
    return template.replace(/\{(name|file|crate)\}/g, (_m, key: string) => shellQuote(values[key]!));
  }
  if (named?.runner === "cargo") return `cargo test -- --exact ${shellQuote(name)}`;
  if (named?.runner === "node") {
    // The file must be known: `node --test --test-name-pattern` exits 0 when
    // nothing matches, so a pattern with no file could report a real failure
    // as a flake. The name is escaped (Node reads the pattern as a regular
    // expression) and single-quoted (names routinely contain spaces).
    if (!file) return undefined;
    return `node --test --test-name-pattern ${shellQuote(escapeRegExp(name))} ${shellQuote(file)}`;
  }
  return undefined;
}

/** The single-test commands for every name in `names`, in order. A name whose
 * command cannot be built stays in the list with `command: undefined`, so the
 * caller records it as `reproduces alone` (unproven flake = real failure)
 * instead of silently dropping it. */
export function rerunCommandsFor(
  output: string,
  names: readonly string[],
  template?: string,
): Array<{ name: string; command?: string }> {
  return names.map((name) => {
    const command = singleTestCommand(name, output, template);
    return command === undefined ? { name } : { name, command };
  });
}

/** One re-run's outcome, as the caller observed it. */
export interface TestRerunOutcome {
  /** Process exit status, or null when the re-run was killed by a signal. */
  exitCode: number | null;
  timedOut: boolean;
  /** The re-run's own output, when the caller kept it: the evidence that the
   * named test actually ran (see `rerunProvesTheTestRan`). */
  output?: string;
}

/** True when a re-run's output shows the NAMED test itself passed. An exit
 * status of 0 alone is not enough, and neither is a runner's summary count:
 *
 * - `node --test --test-name-pattern` exits 0 when its pattern selects no
 *   test, and a describe block whose tests were all filtered prints
 *   `ℹ tests 0`, `ℹ suites 1` and only its own `✔ <suite>` line;
 * - older Node reports filtered tests as `ℹ tests 3` / `ℹ skipped 3`;
 * - TAP writes a skipped test as `ok N - <name> # SKIP`;
 * - a template whose `{name}` is used unescaped inside a regular expression
 *   (`a+b`) can select some OTHER test and exit 0.
 *
 * So the evidence must be the named test's own passing line, in the shape its
 * runner writes: the spec reporter's `✔ <name>`, TAP's `ok N - <name>`
 * (never a `# SKIP`/`# TODO` one), cargo's `test <name> ... ok`, or ERT's
 * `passed  1/1  <name>`. Anything else keeps the strict rule. */
export function rerunProvesTheTestRan(output: string, name: string): boolean {
  const text = stripAnsi(output);
  const literal = escapeRegExp(name);
  return (
    new RegExp(`^\\s*[\\u2714\\u2713]\\s+${literal}(?:\\s+\\(\\d+(?:\\.\\d+)?ms\\))?\\s*$`, "m").test(text) || // node spec
    new RegExp(`^\\s*ok \\d+ - ${literal}\\s*$`, "m").test(text) || // TAP, not skipped/todo
    new RegExp(`^\\s*test ${literal} \\.\\.\\. ok\\s*$`, "m").test(text) || // cargo
    new RegExp(`^\\s*passed\\s+\\d+/\\d+\\s+${literal}\\b`, "m").test(text) // ERT
  );
}

/** A new failing test's classification: `reproduces alone` (a real failure)
 * or `load-only` (it passed alone). A re-run that timed out counts as
 * `reproduces alone` — a truncated run proves nothing, so the strict
 * direction is kept; a name with no command at all is real for the same
 * reason. */
export interface TestClassification {
  name: string;
  /** The single-test command that was re-run, when one could be built. */
  rerunCommand?: string;
  reproducesAlone: boolean;
  loadOnly: boolean;
  failingExitCode: number | null;
  /** The exit status of every re-run that was attempted, in order. */
  rerunExitCodes: Array<number | null>;
  rerunTimedOut: boolean;
}

export function classifyRerun(
  name: string,
  command: string | undefined,
  failingExitCode: number | null,
  reruns: readonly TestRerunOutcome[],
): TestClassification {
  // A re-run only proves a flake when it both exited 0 AND shows the test
  // ran: `node --test --test-name-pattern` (and a cargo filter) exit 0 when
  // nothing matches, which would otherwise excuse a real failure (M-1).
  const loadOnly = reruns.some((r) => !r.timedOut && r.exitCode === 0 && rerunProvesTheTestRan(r.output ?? "", name));
  const base: TestClassification = {
    name,
    ...(command !== undefined ? { rerunCommand: command } : {}),
    reproducesAlone: !loadOnly,
    loadOnly,
    failingExitCode,
    rerunExitCodes: reruns.map((r) => r.exitCode),
    rerunTimedOut: reruns.some((r) => r.timedOut),
  };
  return base;
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

/** Plan 05d: every `base flake` across a baseline's commands, deduped, in
 * order. A base flake is visible and never excuses a candidate's failure of
 * the same test. */
export function baselineFlakeNames(commands: readonly BaselineCommand[]): string[] {
  const seen = new Set<string>();
  const names: string[] = [];
  for (const command of commands) {
    for (const name of command.flakes ?? []) {
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
      ...(Array.isArray(c.flakes) ? { flakes: c.flakes.filter((f): f is string => typeof f === "string") } : {}),
      ...(typeof c.log === "string" ? { log: c.log } : {}),
    });
  }
  const stored = Array.isArray(raw.failures) ? raw.failures.filter((f): f is string => typeof f === "string") : undefined;
  const storedFlakes = Array.isArray(raw.flakes) ? raw.flakes.filter((f): f is string => typeof f === "string") : undefined;
  return {
    baseSha: raw.baseSha,
    // A record written before the tree field existed has no identity to trust,
    // so it can never be reused (a caller comparing it sees "" ≠ any tree).
    tree: typeof raw.tree === "string" ? raw.tree : "",
    key: raw.key,
    at: typeof raw.at === "string" ? raw.at : "",
    commands,
    failures: stored ?? baselineFailureNames(commands),
    ...((storedFlakes ?? baselineFlakeNames(commands)).length > 0 ? { flakes: storedFlakes ?? baselineFlakeNames(commands) } : {}),
  };
}

/** True iff any baseline command exited 126 or 127: the shell could not run
 * it (`command not found` / `not executable`). Such a record is not a base
 * that fails its own tests — it is an environment problem — so plan 05i
 * ignores it everywhere a baseline is read (a record from before the fix,
 * or a sibling's shared copy) and the baseline re-runs in a fixed
 * environment instead of excusing a candidate's checks against it. */
export function baselineHasEnvironmentFailure(baseline: Baseline | undefined): boolean {
  if (!baseline) return false;
  return baseline.commands.some((c) => c.exitCode === 126 || c.exitCode === 127);
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
  const flakes = baseline.flakes ?? baselineFlakeNames(baseline.commands);
  const flakeNote = flakes.length > 0 ? `; base flakes (passed alone, never excusing): ${flakes.join(", ")}` : "";
  if (names.length === 0) return `base fails: 0 tests (no test names parsed; checks stay strict)${flakeNote}`;
  return `base fails: ${names.length} tests: ${names.join(", ")}${flakeNote}`;
}
