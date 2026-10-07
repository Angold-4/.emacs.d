// 06a finding #24: the process half of `tt models check`. Spawns one Pi print
// process per distinct configured model, in parallel, each bounded by
// `MODELS_CHECK_TIMEOUT_MS`, and classifies the answers with the pure
// `core/models-check.ts`. The caller (`cli.ts`) writes the returned record to
// the run/program directory and refuses to start on a refusal.

import { spawn } from "node:child_process";

import {
  classifyModelProbe,
  MODELS_CHECK_TIMEOUT_MS,
  MODELS_CHECK_PROMPT,
  type ModelGroup,
  type ModelProbeOutcome,
  type ModelProbeResult,
  type ModelsCheck,
} from "../core/models-check.ts";

export interface ModelsCheckOptions {
  /** The `pi` binary, or an injected stand-in for tests. */
  command: string;
  /** Argv prepended before Pi's own flags (the fake-pi script for tests). */
  argsPrefix?: string[];
  timeoutMs?: number;
  /** The environment the probe runs in (defaults to this process's own). */
  env?: NodeJS.ProcessEnv;
  /** Injectable clock, so a test can pin `at`. */
  now?: () => Date;
}

/** Pi's argv for one model: print mode, ephemeral session, the model's own
 * provider/model flags and the tiny prompt. */
export function modelProbeArgv(group: ModelGroup, opts: { command: string; argsPrefix?: string[] }): string[] {
  const argv = [opts.command, ...(opts.argsPrefix ?? []), "--print", "--no-session"];
  if (group.provider) argv.push("--provider", group.provider);
  argv.push("--model", group.model, MODELS_CHECK_PROMPT);
  return argv;
}

/** Run one probe with a hard bound. A probe that never answers is killed and
 * reported as `timedOut`, which classifies as `unreachable`. */
export function probeModel(group: ModelGroup, opts: ModelsCheckOptions): Promise<ModelProbeOutcome> {
  const timeoutMs = opts.timeoutMs ?? MODELS_CHECK_TIMEOUT_MS;
  const [command, ...args] = modelProbeArgv(group, opts);
  return new Promise((resolve) => {
    let output = "";
    let timedOut = false;
    let settled = false;
    const finish = (exitCode: number | null): void => {
      if (settled) return;
      settled = true;
      resolve({ exitCode, timedOut, output });
    };
    const child = spawn(command, args, { stdio: ["ignore", "pipe", "pipe"], env: opts.env ?? process.env });
    const timer = setTimeout(() => {
      timedOut = true;
      try {
        child.kill("SIGKILL");
      } catch {
        // already gone
      }
    }, timeoutMs);
    child.stdout?.on("data", (chunk: Buffer) => {
      output += chunk.toString();
    });
    child.stderr?.on("data", (chunk: Buffer) => {
      output += chunk.toString();
    });
    child.on("error", (err) => {
      clearTimeout(timer);
      output += String(err);
      finish(null);
    });
    child.on("close", (code) => {
      clearTimeout(timer);
      finish(code);
    });
  });
}

/** Probe every group in parallel and assemble the record a run writes. */
export async function runModelsCheck(groups: readonly ModelGroup[], opts: ModelsCheckOptions): Promise<ModelsCheck> {
  const outcomes = await Promise.all(groups.map(async (group) => ({ group, outcome: await probeModel(group, opts) })));
  const probes: ModelProbeResult[] = outcomes.map(({ group, outcome }) => {
    const { status, message } = classifyModelProbe(outcome);
    return {
      key: group.key,
      ...(group.provider ? { provider: group.provider } : {}),
      model: group.model,
      roles: group.roles,
      status,
      ...(message !== undefined ? { message } : {}),
    };
  });
  return { at: (opts.now ?? (() => new Date()))().toISOString(), probes };
}
