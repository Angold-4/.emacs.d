import { isDeepStrictEqual } from "node:util";
import type { LogRecord } from "./effects/log.ts";

// Released runners from the first conductor through 107693a's parent
// logged a refused event before its rejection. Freeze this provenance in
// code: replay must not depend on a reader's git checkout or filesystem.
const logBeforeRejectionRunners = new Set([
  "be6b76296741ff5132cc15ae9cffb31273ac4449",
  "62b5c0902da7886aca864a683d2cddcee57eb6a9",
  "a443de66ba2579c4286d6b48467d19ed3a793ec5",
  "4f652afa0c8bbe8829db78558b4ac06c5a34fd51",
  "1b79f63c79832d07b5994e39ce6daa8cc2397ba0",
  "b0c7a827af1e4766d03b0ac6d98f90685c796723",
  "5947596f0f604efa5b47290e4699ae37c925fbf0",
  "ce3de97ce3423fcd0f41786c781bc95c30239dc6",
  "f00f2122e2791603dc692a70799cc1454a4daebc",
  "456ccc08b67cb648af32c75fbcfb59af28caa90e",
  "d668841744056ff0ee0f9cf6743ee686f7b0c174",
  "309634c040f3613f8591bedbacadbcf0c02b8981",
  "70b14397a3ea39beb97e07ac6d06ec93aa62a518",
  "1feea0879292349c9cb822e863a2687041d05828",
  "fbe02ae4f997ce0932fdfb52b8483b5ce36ed2de",
  "a487a297d4ab17d8a75428f24cfef479922d3888",
]);

/** One accepted-record stream for recovery and timeline projections. Current
 * runners write only successful events, even when a rejected duplicate has
 * exactly the same payload. Legacy suppression requires both provenance and
 * an exact match to the rejection's own attempted event. */
export function* acceptedReplayRecords(records: readonly LogRecord[]): Generator<LogRecord> {
  const init = records.find((r) => r.kind === "init")?.event as { runnerRevision?: string } | undefined;
  // No revision is the historical, pre-provenance log format. Unknown named
  // runners are never assumed legacy.
  const legacy = init?.runnerRevision === undefined || logBeforeRejectionRunners.has(init.runnerRevision);
  for (let i = 0; i < records.length; i++) {
    const record = records[i];
    const next = records[i + 1];
    if (legacy && record.kind === "event" && next?.kind === "rejected" &&
      isDeepStrictEqual(record.event, (next.event as { event?: unknown })?.event)) continue;
    yield record;
  }
}
