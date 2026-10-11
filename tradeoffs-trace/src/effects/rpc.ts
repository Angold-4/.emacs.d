// Plan 06k2 (A6): cap a tool result before it enters the model's context.
//
// The 2026-10-08 overflow: ONE `sh` result of 3.56 MB — a cargo test panic
// whose assert message Debug-printed ~40k invariant violations on a single
// line — blew the worker's context and the compaction retry could not
// recover. `grep … | head -20` cannot cap a single line, so the cap must be
// applied to the whole result, per line and in bytes, with a marker naming
// the original length. Pure: no I/O, no clock.

/** The default cap, 64 KiB. A plan/runner may override it with
 * `TT_TOOL_RESULT_CAP_BYTES`. */
export const DEFAULT_TOOL_RESULT_CAP_BYTES = 64 * 1024;

/** The configured cap, read fresh (a test can set the env var). */
export function toolResultCapBytes(): number {
  const raw = process.env.TT_TOOL_RESULT_CAP_BYTES;
  const n = raw ? Number(raw) : NaN;
  return Number.isFinite(n) && n > 0 ? Math.floor(n) : DEFAULT_TOOL_RESULT_CAP_BYTES;
}

/** Cap one tool result to `capBytes`, keeping the first and last halves with
 * a marker naming the original byte length in between. A result at or under
 * the cap is returned unchanged, so an ordinary command is byte-identical. */
export function capToolResult(text: string, capBytes: number = toolResultCapBytes()): string {
  const buf = Buffer.from(text, "utf8");
  if (buf.length <= capBytes) return text;
  const half = Math.floor(capBytes / 2);
  // A UTF-8 boundary cut may add a replacement character, so re-check the
  // encoded sizes: the two halves never exceed the cap.
  let head = buf.subarray(0, half).toString("utf8");
  let tail = buf.subarray(buf.length - half).toString("utf8");
  while (Buffer.byteLength(head, "utf8") + Buffer.byteLength(tail, "utf8") > capBytes && (head.length > 0 || tail.length > 0)) {
    if (head.length >= tail.length) head = head.slice(0, -1);
    else tail = tail.slice(1);
  }
  const marker = `\n[... tool result truncated: original ${buf.length} bytes, cap ${capBytes} bytes; first and last kept ...]\n`;
  return `${head}${marker}${tail}`;
}
