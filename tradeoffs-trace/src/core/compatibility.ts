import { stripAnsi } from "./items.ts";
import { parseTestFailuresDetailed, rerunCommandsFor } from "./test-failures.ts";

export interface CompatibilityTest {
  name: string;
  outcome: "passed" | "failed" | "skipped";
  command: string;
  file?: string;
}

/** Preserve individual results, including nextest's real results underneath
 * a wrapper's TAP summary. Never infer every test's result from a group exit. */
export function compatibilityTests(output: string, command: string, rerun?: string): CompatibilityTest[] {
  const tests = new Map<string, CompatibilityTest>();
  const detailed = parseTestFailuresDetailed(output);
  const add = (name: string, outcome: CompatibilityTest["outcome"], file?: string): void => {
    name = name.trim();
    if (!name || /^failing tests?:$/.test(name)) return;
    if (tests.get(name)?.outcome === "failed" && outcome !== "failed") return;
    const failed = detailed.find((f) => f.name === name);
    tests.set(name, { name, outcome, command: outcome === "failed"
      ? rerunCommandsFor(output, [name], rerun)[0]?.command ?? command : command,
      ...((file ?? failed?.file) ? { file: file ?? failed?.file } : {}) });
  };
  for (const line of stripAnsi(output).split("\n")) {
    let m = /^\s*(not )?ok\s+\d+\s+-\s+(.+?)(?:\s+#\s*(.*))?$/.exec(line);
    if (m) { add(m[2], /^(?:SKIP|TODO)\b/i.test(m[3] ?? "") ? "skipped" : m[1] ? "failed" : "passed"); continue; }
    m = /^\s*([✔✓✖✗×])\s+(.+?)(?:\s+\([\d.]+m?s\))?\s*$/.exec(line);
    if (m) { add(m[2], /[✔✓]/.test(m[1]) ? "passed" : "failed"); continue; }
    m = /^\s*test\s+(.+?)\s+\.\.\.\s+(ok|FAILED|ignored)\s*$/.exec(line);
    if (m) { add(m[1], m[2] === "ok" ? "passed" : m[2] === "FAILED" ? "failed" : "skipped"); continue; }
    m = /^\s*(PASS|FAIL|TIMEOUT|SKIP)\s+\[[^\]]*\]\s+(?:\([^)]*\)\s+)?(\S+)\s+(\S+)\s*$/.exec(line);
    if (m) add(`${m[2]} ${m[3]}`, m[1] === "PASS" ? "passed" : m[1] === "SKIP" ? "skipped" : "failed");
  }
  for (const failure of detailed) if (!tests.has(failure.name)) add(failure.name, "failed", failure.file);
  return [...tests.values()];
}

/** Cargo test/nextest can be invoked through arbitrary plan wrappers. The
 * inventory's local PATH adapter applies this only at the actual cargo call. */
export function inventoryCargoArgs(args: readonly string[]): string[] {
  const out = [...args];
  const start = out[0]?.startsWith("+") ? 1 : 0;
  const nextest = out[start] === "nextest" && out[start + 1] === "run";
  if (!nextest && out[start] !== "test") return out;
  const separator = out.indexOf("--");
  const end = separator < 0 ? out.length : separator;
  const head = out.slice(0, end).filter((arg, i, all) =>
    !["--fail-fast", "--ff", "--no-fail-fast", "--nff", "--max-fail"].includes(arg) &&
    !arg.startsWith("--max-fail=") && all[i - 1] !== "--max-fail");
  return [...head, "--no-fail-fast", ...out.slice(end)];
}
