// ADMIT's effect half (07 design §3.1 / A1): collect the four facts one start
// decides on — the plan lints, TT_REPO is a git work tree, every declared
// check command parses, and the pinned pi is the one on the PATH this shell
// resolves. Pure decision in `src/core/admit.ts`; this file only runs the
// commands.
//
// The PATH is a parameter, re-read on every call by every caller: a resume
// must resolve the shell it is run from, never the PATH the run recorded at
// its first start (`check-env.json` records what ADMIT saw; it is never the
// source — exex 07f1, 2026-10-28).

import { execFileSync } from "node:child_process";

import {
  AdmitRefusal,
  ADMIT_PINNED_PI,
  admitCommands,
  admitLintErrors,
  admitVerdict,
  type AdmitFacts,
  type AdmitPlanShape,
  type AdmitVerdict,
} from "../core/admit.ts";

/** The first line of a command's stderr, for a one-line reason. */
function firstLine(text: string | undefined): string {
  const line = (text ?? "").split("\n").map((l) => l.trim()).find((l) => l.length > 0);
  return line ?? "";
}

function errorLine(err: unknown): string {
  const e = err as { stderr?: string; message?: string } | undefined;
  const fromStderr = firstLine(typeof e?.stderr === "string" ? e.stderr : undefined);
  if (fromStderr.length > 0) return fromStderr;
  return firstLine(typeof e?.message === "string" ? e.message : String(err));
}

export interface AdmitOptions {
  /** The PATH the shell that runs this command has, re-resolved on every
   * start. Never a recorded one. */
  path: string;
  /** The environment `command -v`, `git` and `bash` see; defaults to this
   * process's own, with PATH replaced by `path`. */
  env?: NodeJS.ProcessEnv;
  /** The pi name ADMIT resolves on `path` and asks for `--version`. `undefined`
   * skips the version check — a conductor or scheduler launched with a test's
   * injected fake-pi binary never touches the real one (the rule the old
   * `assertPiVersion` used, kept so no test depends on the machine's pi). */
  pi?: string;
  /** The version the resolved pi must report; defaults to the pin. */
  pinned?: string;
  /** Where `git` and `bash` run — the plan's repository. */
  cwd?: string;
}

/** Resolve one command name on `path` the way a shell would. Never throws: a
 * name with no match is simply absent. */
function resolveOnPath(name: string, env: NodeJS.ProcessEnv, cwd: string | undefined): string | undefined {
  try {
    const out = execFileSync("/bin/sh", ["-c", 'command -v -- "$1"', "tt-admit", name], {
      encoding: "utf8",
      env,
      ...(cwd !== undefined ? { cwd } : {}),
      stdio: ["ignore", "pipe", "ignore"],
    }).trim();
    return out.length > 0 ? out.split("\n")[0] : undefined;
  } catch {
    return undefined;
  }
}

/** `TT_REPO` as a git work tree, on this start: `git rev-parse
 * --is-inside-work-tree` must say true. A bare repository (or a plain
 * directory, or a missing path) says otherwise, with git's own words. */
function readWorkTree(repo: string, env: NodeJS.ProcessEnv): { ok: boolean; error?: string } {
  if (repo.length === 0) return { ok: false, error: "the plan names no TT_REPO" };
  try {
    const out = execFileSync("git", ["-C", repo, "rev-parse", "--is-inside-work-tree"], {
      encoding: "utf8",
      env,
      stdio: ["ignore", "pipe", "pipe"],
    }).trim();
    if (out === "true") return { ok: true };
    return { ok: false, error: `git reports "${out}", not a work tree` };
  } catch (err) {
    return { ok: false, error: errorLine(err) || `git -C ${repo} rev-parse --is-inside-work-tree failed` };
  }
}

/** `bash -n` on one command: parse only, run nothing. A shell syntax error is
 * a plan defect — on 2026-10-28 (exex 01c3b) a split `AST_GREP` command made
 * every candidate red with a shell syntax error that the run only discovered
 * after a whole build. */
function parseCheck(command: string, env: NodeJS.ProcessEnv, cwd: string | undefined): { ok: boolean; error?: string } {
  try {
    execFileSync("bash", ["-n", "-c", command], {
      encoding: "utf8",
      env,
      ...(cwd !== undefined ? { cwd } : {}),
      stdio: ["ignore", "pipe", "pipe"],
    });
    return { ok: true };
  } catch (err) {
    return { ok: false, error: errorLine(err) || "bash -n refused it" };
  }
}

/** The pinned pi as THIS shell resolves it: the resolved binary, and the
 * version it reports. `skipped` is set only when no real binary is used. */
function readPi(name: string | undefined, pinned: string, env: NodeJS.ProcessEnv, cwd: string | undefined): AdmitFacts["pi"] {
  if (name === undefined) {
    return { name: "pi", pinned, skipped: "a test injected a fake pi binary; no real pi is used" };
  }
  const resolved = resolveOnPath(name, env, cwd);
  if (resolved === undefined) return { name, pinned };
  try {
    const version = execFileSync(resolved, ["--version"], {
      encoding: "utf8",
      env,
      ...(cwd !== undefined ? { cwd } : {}),
      stdio: ["ignore", "pipe", "pipe"],
    }).trim();
    return { name, pinned, path: resolved, version };
  } catch (err) {
    return { name, pinned, path: resolved, error: errorLine(err) || "it exited non-zero" };
  }
}

/** Every fact ADMIT decides on, collected now. Never throws: an unreadable
 * repo or a missing binary is a fact, not an error. */
export function admitFacts(plan: AdmitPlanShape, opts: AdmitOptions): AdmitFacts {
  const env: NodeJS.ProcessEnv = { ...(opts.env ?? process.env), PATH: opts.path };
  const pinned = opts.pinned ?? ADMIT_PINNED_PI;
  const repoPath = typeof (plan as { repo?: unknown }).repo === "string" ? String((plan as { repo?: string }).repo) : "";
  const unparsed: Array<{ command: string; error: string }> = [];
  for (const command of admitCommands(plan)) {
    const result = parseCheck(command, env, opts.cwd);
    if (!result.ok) unparsed.push({ command, error: result.error ?? "bash -n refused it" });
  }
  return {
    lint: admitLintErrors(plan),
    repo: { path: repoPath, ...readWorkTree(repoPath, env) },
    unparsed,
    path: opts.path,
    pi: readPi(opts.pi, pinned, env, opts.cwd),
  };
}

/** ADMIT: the one gate start, resume and a program node's start all run. */
export function admit(plan: AdmitPlanShape, opts: AdmitOptions): AdmitVerdict {
  return admitVerdict(admitFacts(plan, opts));
}

/** `admit`, for a caller that must stop on a refusal: the facts when it
 * passes, an `AdmitRefusal` (carrying kind, reason and facts) when it does
 * not. */
export function admitOrRefuse(plan: AdmitPlanShape, opts: AdmitOptions): AdmitFacts {
  const verdict = admit(plan, opts);
  if (!verdict.ok) throw new AdmitRefusal(verdict.kind, verdict.reason, verdict.facts);
  return verdict.facts;
}

/** The pi name ADMIT's version check resolves for a CLI or scheduler started
 * from this environment: `pi`, unless `TT_TEST_MODE=1` injected a fake binary
 * (which is never the real one). The same rule the conductor applies to its
 * own `piCommand`/`piCommandFor` overrides. */
export function admitPiCommand(env: NodeJS.ProcessEnv = process.env): string | undefined {
  if (env.TT_TEST_MODE === "1" && (env.TT_TEST_PI_COMMAND ?? "").length > 0) return undefined;
  return "pi";
}
