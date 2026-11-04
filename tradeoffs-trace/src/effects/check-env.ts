import type { RunPlanFile } from "../conductor.ts";
import { boundDefaultMs } from "../core/bounds.ts";
import { childEnv, runCommand } from "./shell.ts";
import { disposableCheckout } from "./git.ts";

export type CheckEnvironmentRecord = Record<string, string | null>;

/** The environment inputs checks keep across restarts. Secrets stay in the
 * process environment and are never written into this record. */
export function recordCheckEnvironment(plan: RunPlanFile, env: NodeJS.ProcessEnv = process.env): CheckEnvironmentRecord {
  const names = new Set(["PATH", ...(plan.envVars ?? [])]);
  const commands = [
    ...(plan.preflight ?? []), ...plan.checks,
    ...plan.phases.flatMap((p) => [...p.checks, ...(p.finalChecks ?? []), ...(p.gate ? [p.gate] : [])]),
  ];
  for (const command of commands) {
    for (const match of command.matchAll(/\$\{?([A-Za-z_][A-Za-z0-9_]*)\}?/g)) names.add(match[1]);
  }
  for (const name of plan.secrets ?? []) names.delete(name);
  return Object.fromEntries([...names].map((name) => [name, env[name] ?? null]));
}

export function checkEnvironment(record: CheckEnvironmentRecord, env: NodeJS.ProcessEnv = process.env): NodeJS.ProcessEnv {
  const result = childEnv(env);
  for (const [name, value] of Object.entries(record)) {
    if (value === null) delete result[name];
    else result[name] = value;
  }
  return result;
}

/** Run a plan's start-time toolchain guards before any node is admitted. */
export async function runPlanPreflight(plan: RunPlanFile, base: string, record: CheckEnvironmentRecord): Promise<string | undefined> {
  if (!plan.preflight?.length) return undefined;
  const checkout = disposableCheckout(plan.repo, base);
  try {
    const env = checkEnvironment(record);
    for (const command of plan.preflight) {
      const result = await runCommand({
        command, cwd: checkout.dir, env,
        deadlineMs: plan.deadlines?.checkMs ?? boundDefaultMs("check"),
        termGraceMs: plan.deadlines?.termGraceMs,
        onIntent: () => {},
      }).result;
      if (result.exitCode !== 0 || result.timedOut) {
        const tail = result.output.split("\n").filter((line) => line.trim()).slice(-10).join("\n");
        return `preflight \`${command}\` exited ${result.exitCode ?? result.signal ?? "unknown"}: ${tail || (result.timedOut ? "timeout" : "no output")}`;
      }
    }
    return undefined;
  } finally {
    checkout.dispose();
  }
}
