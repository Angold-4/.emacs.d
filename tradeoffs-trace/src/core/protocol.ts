// Pure message shapes and strict JSONL framing, shared by:
//   (a) Pi's own RPC protocol (docs: rpc.md) — spoken by fake-pi.ts (as
//       the agent) and by anything driving a real or fake `pi --mode rpc`
//       process from outside;
//   (b) the run socket between the tradeoffs-trace extension (inside a Pi
//       process) and the conductor — spoken by extension/tradeoffs-trace.ts
//       and by fake-pi.ts's scripted `submit`/`sh` steps, and by any test
//       socket server standing in for the conductor.
//
// No process, socket or filesystem access here — this module only encodes
// and decodes strings and defines the vocabulary. rpc.md is explicit that
// RPC framing is strict, LF-only JSONL: "Split records on \n only. Accept
// optional \r\n input by stripping a trailing \r. Do not use generic line
// readers that treat Unicode separators as newlines" — Node's `readline`
// also splits on U+2028/U+2029, which are valid inside JSON strings, so it
// is not protocol-compliant. `JSONLDecoder` below only ever looks for the
// literal byte 0x0A, so a U+2028 inside a JSON string is never mistaken
// for a record boundary.

// ---------------------------------------------------------------------------
// Framing
// ---------------------------------------------------------------------------

/** Encodes one JSONL record: JSON text followed by a single `\n`. Never
 * emits `\r\n` — a peer that sends CRLF is tolerated on decode (see
 * `JSONLDecoder`), but tradeoffs-trace's own writers always emit bare LF. */
export function encodeLine(value: unknown): string {
  return `${JSON.stringify(value)}\n`;
}

export interface DecodeError {
  line: string;
  error: string;
}

/** Incremental LF-delimited JSON decoder. Feed it chunks as they arrive
 * (from a socket or a child process's stdout) with `push()`; it returns
 * every complete record found so far, buffering any trailing partial line
 * for the next call. A chunk boundary landing mid-record, or several
 * records arriving in one chunk, are both handled correctly because
 * splitting happens on the accumulated buffer, not per chunk.
 *
 * A record that is present but fails to `JSON.parse` is reported via
 * `onError` (if given) rather than thrown, so one malformed line does not
 * take down the whole stream; blank lines (after stripping a trailing
 * `\r`) are skipped silently, matching Pi's own tolerance for keep-alive
 * newlines. */
export class JSONLDecoder {
  #buffer = "";

  push(chunk: string | Uint8Array, onError?: (e: DecodeError) => void): unknown[] {
    this.#buffer += typeof chunk === "string" ? chunk : Buffer.from(chunk).toString("utf8");
    const out: unknown[] = [];
    let newlineIndex: number;
    // Only ever split on the literal LF byte — never a generic "line
    // reader" that would also treat U+2028/U+2029 as boundaries.
    while ((newlineIndex = this.#buffer.indexOf("\n")) !== -1) {
      let line = this.#buffer.slice(0, newlineIndex);
      this.#buffer = this.#buffer.slice(newlineIndex + 1);
      if (line.endsWith("\r")) line = line.slice(0, -1);
      if (line.length === 0) continue;
      try {
        out.push(JSON.parse(line));
      } catch (err) {
        onError?.({ line, error: String((err as Error)?.message ?? err) });
      }
    }
    return out;
  }

  /** Whatever has been pushed but not yet terminated by a newline. Used
   * only for diagnostics/tests — a well-behaved peer always terminates its
   * last record. */
  pending(): string {
    return this.#buffer;
  }
}

// ---------------------------------------------------------------------------
// (a) Pi RPC — the subset of commands/events tradeoffs-trace's tests speak,
// per docs/rpc.md. Every command supports an optional `id` for
// request/response correlation (per rpc.md); every response carries it
// back. This is intentionally not the full RPC surface (no compaction,
// model-cycling, etc.) — only what fake-pi.ts and the phase-0 contract
// tests need.
// ---------------------------------------------------------------------------

export interface PiRpcPromptCommand {
  type: "prompt";
  id?: string;
  message: string;
  images?: unknown[];
  streamingBehavior?: "steer" | "followUp";
}
export interface PiRpcSteerCommand {
  type: "steer";
  id?: string;
  message: string;
}
export interface PiRpcAbortCommand {
  type: "abort";
  id?: string;
}
export interface PiRpcGetStateCommand {
  type: "get_state";
  id?: string;
}

export type PiRpcCommand = PiRpcPromptCommand | PiRpcSteerCommand | PiRpcAbortCommand | PiRpcGetStateCommand;

export interface PiRpcResponse {
  type: "response";
  id?: string;
  command: string;
  success: boolean;
  data?: unknown;
  reason?: string;
}

export interface PiRpcAgentStartEvent {
  type: "agent_start";
}
export interface PiRpcAgentEndEvent {
  type: "agent_end";
  messages: unknown[];
  willRetry?: boolean;
}
export interface PiRpcAgentSettledEvent {
  type: "agent_settled";
}
export interface PiRpcMessageUpdateEvent {
  type: "message_update";
  usage?: unknown;
  assistantMessageEvent: unknown;
}
export interface PiRpcToolExecutionStartEvent {
  type: "tool_execution_start";
  toolCallId: string;
  toolName: string;
  args: unknown;
}
export interface PiRpcToolExecutionEndEvent {
  type: "tool_execution_end";
  toolCallId: string;
  toolName: string;
  result: unknown;
  isError: boolean;
}
/** Any other event type (turn_start, message_start, ...): replayed
 * verbatim by fake-pi.ts without a dedicated interface. */
export interface PiRpcOtherEvent {
  type: string;
  [key: string]: unknown;
}

export type PiRpcEvent =
  | PiRpcAgentStartEvent
  | PiRpcAgentEndEvent
  | PiRpcAgentSettledEvent
  | PiRpcMessageUpdateEvent
  | PiRpcToolExecutionStartEvent
  | PiRpcToolExecutionEndEvent
  | PiRpcOtherEvent;

// ---------------------------------------------------------------------------
// (b) The run socket — between the extension (inside a Pi process) and the
// conductor. Every message is one JSONL record via encodeLine/JSONLDecoder.
// ---------------------------------------------------------------------------

/** Sent once, immediately after `session_start`, before anything else. */
export interface HelloMessage {
  type: "hello";
  agentId: string;
  role: "worker" | "reviewer";
  tools: string[];
  piVersion?: string;
}

/** A submission tool call (`submit_phase`, `submit_discovery`,
 * `submit_review`), forwarded verbatim after the extension's own schema
 * validation has passed. `id` correlates the reply. */
export interface SubmitMessage {
  type: "submit";
  id: string;
  tool: "submit_phase" | "submit_discovery" | "submit_review";
  args: unknown;
}

export interface SubmitReply {
  type: "submit_reply";
  id: string;
  ok: boolean;
  reason?: string;
}

/** A shell command the extension's `sh` tool wants run. The conductor owns
 * every shell command (design §2.2) — the extension never runs one itself. */
export interface ShMessage {
  type: "sh";
  id: string;
  commandId: string;
  command: string;
  cwd?: string;
}

export interface ShOutputMessage {
  type: "sh_output";
  commandId: string;
  chunk: string;
  stream?: "stdout" | "stderr";
}

export interface ShExitMessage {
  type: "sh_exit";
  commandId: string;
  code: number | null;
  signal?: string | null;
}

/** Phase 1b addition (pure addition, no change to any existing message or
 * row): sent by the worker's extension when `agent_before_settle` has
 * already requested its two allowed continuations (design §3.3 item 1) and
 * the worker still has not had `submit_phase` accepted. This is the
 * explicit signal design's "report `no_submission` to the conductor" asks
 * for; the conductor also treats an `agent_settled` RPC event with no prior
 * accepted submission as the same condition, so a worker whose extension
 * cannot reach the socket at all does not stall the attempt's deadline. See
 * test/effects/socket-no-submission.test.ts. */
export interface NoSubmissionMessage {
  type: "no_submission";
  agentId: string;
}

export type RunSocketMessage =
  | HelloMessage
  | SubmitMessage
  | SubmitReply
  | ShMessage
  | ShOutputMessage
  | ShExitMessage
  | NoSubmissionMessage;
