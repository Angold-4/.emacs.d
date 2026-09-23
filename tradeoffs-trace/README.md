# tradeoffs-trace

A programmed review pipeline for Pi in Emacs. See
[`../docs/tradeoffs-trace.md`](../docs/tradeoffs-trace.md) for the design and
[`../docs/tradeoffs-trace-plan.md`](../docs/tradeoffs-trace-plan.md) for the
build plan.

This package is plain TypeScript run by Node's native type stripping (Node
≥ 22.19, developed against 25.9.0): **no build step, zero npm dependencies**.
Import modules with explicit `.ts` extensions. The runner is later frozen by
copying a checkout with no `npm install`, so zero dependencies is a hard
constraint, not a style preference.

## Phase 0: executable contracts

`src/core/` is the pure "functional core" described in the design's
"functional core, imperative shell" convention: state transitions, the
acceptance predicate, the vote tally and version binding, all as pure
functions with no process, git or filesystem access. `schemas/` holds the
JSON Schema documents for every record kind; `src/core/schema.ts` is a
minimal validator supporting only the keywords those schemas use (no `ajv`).

`src/core/roles.ts` and `src/core/protocol.ts` extend the pure core with the
launch/role data (design §2.1's tool-allowlist table, `launchArgs`,
`assertToolSet`) and the shared strict-JSONL message protocol used both by
Pi's own RPC mode and by the run socket between the extension and the
conductor. `extension/tradeoffs-trace.ts` is the (still guard-free, design
§9.5 comes later) Pi extension skeleton: it registers `sh`, `submit_phase`,
`submit_discovery` and `submit_review` in every agent, reports
`pi.getActiveTools()` to the run socket at `session_start`, and validates
every submission with the core validator before ever forwarding it.

`schemas/submission.schema.json` is the single source of truth for what a
worker or reviewer can actually supply to `submit_phase`/`submit_discovery`:
its `$defs.decisionDisclosure` holds design §3.2's plain-language fields
(`choice`, `whyItMatters`, `alternatives`, `recommendation`) plus
`classProposal`, kept byte-for-byte identical (for the fields they share) to
`decision.schema.json`'s own definitions — enforced by
`test/contract/submission-schema.test.ts`. `submit_review` needs no such
split and validates directly against `schemas/review.schema.json`, since a
Review carries no conductor-assigned binding fields at all.
`extension/param-shapes.ts` holds the same submission tools' parameter
field lists as plain data (no `typebox` import), so both
`extension/tradeoffs-trace.ts`'s real typebox parameter schemas (built by
mapping over these arrays) and the contract test (which cannot import
`typebox` outside Pi's extension loader) are tied to one list of field
names each.

**What phase 1's conductor must do with a disclosed decision.** A worker's
`submit_phase` (and a reviewer's `submit_discovery`) can only supply the
`decisionDisclosure` shape above — a Decision's identity and binding fields
(`id`, `version`, `phaseId`, `source`, `class`, `boundCandidateSha`,
`boundContractVersion`) don't exist yet at submission time, since the
candidate itself is produced by the freeze that `SUBMIT_PHASE` triggers
(`reduce.ts`'s `SUBMIT_PHASE` case stores `Decision[]` as-is, so a real
conductor must construct the full records, not just relay the tool
arguments). Once `FREEZE_COMPLETED` gives it a `candidateSha`, the
conductor must: assign each disclosed item an `id` and `version: 1`, set
`phaseId` and `source` (`"worker"` for a `submit_phase` decision,
`"reviewer-discovered"` for a `submit_discovery` item), copy `choice` /
`whyItMatters` / `alternatives` / `recommendation` through unchanged, set
`class` from the disclosed `classProposal`, and set `boundCandidateSha` /
`boundContractVersion` to the phase's current candidate and contract
version — then validate the assembled record against
`schemas/decision.schema.json` before it becomes part of phase state. (This
packet's live smoke test's socket-server stub does exactly this, as a
stand-in for that conductor step — see
`test/live/live-submission.test.ts`.)

`test/fake-pi/fake-pi.ts` is a scriptable stand-in for a real Pi process,
speaking the same RPC and run-socket protocols, used by deterministic
tests. `test/contract/role-tool-sets.test.ts` runs against the **real
installed Pi** (no model call). `test/live/live-submission.test.ts` is the
opt-in live smoke test against real Pi and a real model — run it with `make
live` (requires `TT_LIVE=1` and provider credentials); it writes one record
per role under `test/live/records/phase-0/`.

Extensions run inside Pi's own process (loaded via its `jiti`-based
extension loader), so `typebox`, `@earendil-works/pi-ai` and
`@earendil-works/pi-coding-agent` are resolved from Pi's own installation,
not from this package's `node_modules` — tradeoffs-trace's own
`package.json` still declares zero dependencies.

## Running the tests

```sh
make check
# or, equivalently, from anywhere:
node --test 'tradeoffs-trace/test/**/*.test.ts'

# opt-in live smoke test (real Pi + real model):
make live
```

Tests use `node:test` + `node:assert/strict`. There is no `tsc`; correctness
is proven by tests, not by a type-checker pass. `make check` requires `pi`
(pinned to the version in `src/core/roles.ts`'s `PI_VERSION`) on `PATH` for
`role-tool-sets.test.ts`, but sends it no prompt and makes no model call.
