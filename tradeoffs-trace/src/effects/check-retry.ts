import type { RunCommandResult } from "./shell.ts";

/** Absolute one-minute host load, matching record.json's load1. */
export const INFRASTRUCTURE_LOAD_THRESHOLD = 8;
export type InfrastructureCheckResult = RunCommandResult & { infrastructureStall?: string };
export interface InfrastructureRetry {
  load1: number;
  reason: string;
  first: RunCommandResult;
  result?: RunCommandResult;
  retryLoad1?: number;
}

export function infrastructureTimeout(result: RunCommandResult, load1: number): boolean {
  if (!result.timedOut || result.cancelled) return false;
  // A completed compilation followed by a hung test is not still compiling.
  const lines = result.output.trim().split("\n");
  const compiling = lines.findLastIndex((l) => /\b(?:compiling|building)\b/i.test(l));
  const finished = lines.findLastIndex((l) => /^\s*(?:finished\b|running\b|test result:|error\b|fail(?:ed)?\b|test project\b|collected\s+\d+\s+items\b)|^\s*\[\s*(?:RUN|PASSED|FAILED)\b/i.test(l));
  return compiling > finished || load1 > INFRASTRUCTURE_LOAD_THRESHOLD;
}

/** One retry, on the same checkout. The caller persists the retry BEFORE
 * running it. A restart reuses its completion or reports its interrupted
 * attempt; it cannot grant itself another infrastructure retry. */
export async function runCheckWithInfrastructureRetry(opts: {
  command: string;
  run: () => Promise<RunCommandResult>;
  load: () => number;
  stopped: () => boolean;
  prior?: InfrastructureRetry;
  onRetry: (retry: InfrastructureRetry) => void;
  onRetried: (retry: InfrastructureRetry) => void;
}): Promise<InfrastructureCheckResult> {
  const stall = (retry: InfrastructureRetry, interrupted = false): InfrastructureCheckResult => ({
    ...(retry.result ?? retry.first),
    infrastructureStall: `stuck: infrastructure stall in check \`${opts.command}\`: ${interrupted ? "infrastructure retry interrupted" : "timed out again"}; load1 ${retry.load1} -> ${retry.retryLoad1 ?? "unknown"} (threshold ${INFRASTRUCTURE_LOAD_THRESHOLD})`,
  });
  if (opts.prior) {
    if (!opts.prior.result || opts.prior.result.cancelled) return stall(opts.prior, true);
    return opts.prior.result.timedOut ? stall(opts.prior) : opts.prior.result;
  }
  const before = opts.load();
  const first = await opts.run();
  const load1 = Math.max(before, opts.load());
  if (opts.stopped() || !infrastructureTimeout(first, load1)) return first;
  const retry: InfrastructureRetry = { load1, reason: "timeout while compiling or under high host load", first };
  opts.onRetry(retry);
  if (opts.stopped()) return first;
  retry.result = await opts.run();
  retry.retryLoad1 = opts.load();
  opts.onRetried(retry);
  if (opts.stopped() || retry.result.cancelled) return retry.result;
  return retry.result.timedOut ? stall(retry) : retry.result;
}
