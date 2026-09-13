# Local conversation workflow test

This is an opt-in integration test. Ordinary ERT tests do not contact OrgBrain.
Use a fresh isolated GBrain home, state database and socket, with synthetic
project data. Do not point this test at a production daemon: it approves one
synthetic proposal and rejects another.

The client and daemon must be tested at explicit revisions. For this review,
OrgBrain was `df5263d86deb8b869bac35911a7b7b4f78670c63`, GBrain was
`4deee227be3b9d9ae6322d5f5bd06d3df0088b07`, and the model was the existing
local `qwen3.8-27b-local` endpoint. The test invokes the real Emacs commands,
local CLI transport, daemon socket, job service, model and GBrain.

## Setup and execution

Start an updated OrgBrain daemon with these environment variables pointing at
the same fresh sandbox used by the Emacs process:

- `GBRAIN_HOME`: sandbox GBrain home, initialized with PGLite and no embeddings.
- `ORGBRAIN_RUNTIME_DIR`, `ORGBRAIN_STATE_DB`, `ORGBRAIN_SOCKET`: sandbox paths.
- `ORGBRAIN_ROOT`, `ORGBRAIN_GBRAIN_BUN`, `ORGBRAIN_GBRAIN_CLI`: pinned code/runtime paths.
- `ORGBRAIN_CONVERSATION_SOURCE=dialogue`: a separately registered diary source.
- `ORGBRAIN_CONVERSATION_RETENTION=1`, `ORGBRAIN_AUTO_CONSULT=0`.
- `ORGBRAIN_MODEL_BASE_URL`, `ORGBRAIN_MODEL`: the intended local model.

Seed `projects/atlas` with `PR 850 adds deterministic replay.` and an attributed
historical user turn explaining that sequential execution was kept because
deterministic replay requires stable ordering. This is a sequential workflow,
not a set of independent evaluation trials.

Set `ORGBRAIN_LIVE_SANDBOX` to the sandbox output directory and
`ORGBRAIN_LIVE_COMMAND` to its CLI executable or wrapper. That wrapper must run
the selected checkout's `bin/orgbrain`; `python -m orgbrain` is not an entry point.
Start a separate Emacs with this environment. Load source explicitly to avoid
silently testing stale `.elc` files:

```elisp
(load-file "/path/to/checkout/core/init-orgbrain.el")
(load-file "/path/to/checkout/test/orgbrain-live.el")
(orgbrain-live-run)
(orgbrain-live-repair-regression)
```

The workflow sends an ask, proposes a fact, approves its exact candidate,
starts another conversation, asks about the approved knowledge, proposes and
rejects a second fact, then uses raw recall. The follow-up sends the two request
forms that previously failed during composition. Capture must be verified for
the ordinary conversation sends; a returned memory acknowledgement does not
substitute for a substantive answer. Approval/rejection are graded on their
verified memory outcome, since no substantive answer is requested there.

Inspect `emacs-records.json`, `emacs-repair-records.json` and
`emacs-transcript.txt`. Check the accepted facts after the run: only the seed
and explicitly approved fact may exist; the rejected badge claim must not.
The dialogue source must contain zero accepted facts. Compare production and
agenda fingerprints before and after; this test does not authorize production
retention or deployment.

## Regression checks

From the checkout:

```sh
emacs --batch -Q --eval '(setq user-emacs-directory default-directory load-prefer-newer t)' \
  -L core -L test -l orgbrain-test -f ert-run-tests-batch-and-exit
```

The review adds checks for refusing propose-mode legacy fallback, rendering
failed-job JSON, retaining an exact candidate after failed submission, clearing
it only after a matching disposition, and lowercase generated conversation IDs.

The lowercase rule was found by the real stack test: GBrain canonicalizes
`...T...` to `...t...`, while exact readback used the original path. Jobs could
return successfully while every capture stayed `pending_retry`. New IDs now
remain lowercase, including the project stem. Existing uppercase IDs are not
rewritten: use `gn` to start a new conversation after updating the client.

## Measured result, 2026-09-13

[Recorded outcomes and rejected drafts](orgbrain-live-evidence.json) cover nine
requests through client `7436217` and OrgBrain `df5263d`: eight returned job
results and one failed. This is not nine successful answers or a clean quality
acceptance. The first rationale answer missed historical evidence, approval's
optional composition failed while its knowledge effect verified, and the mixed
remember/advice answer declined the requested trade-off analysis. The failed
cross-conversation ask cited a page slug instead of a numbered ledger row on
both compose attempts; the client displayed the failed receipt and kept input.

Propose, exact approval, rejection and raw recall all reached the actual daemon.
Final accepted knowledge held only the seed and the explicitly approved replay
fact. The rejected badge claim was absent; dialogue contained zero fact rows.
Capture was verified for the new lowercase-ID turns (a failed composition has
no assistant turn to capture). Production GBrain, agenda and protected-file
fingerprints were unchanged. All 116 ERT tests passed; byte-compilation and
whitespace checks passed.

An initial uppercase-ID run and one stale-bytecode launch are retained in local
logs, not counted as passing evidence. The latter is why the instructions above
load source explicitly. Client fixes are pushed; the remaining answer failures
belong to PR #95. No production service was restarted or retention enabled.
