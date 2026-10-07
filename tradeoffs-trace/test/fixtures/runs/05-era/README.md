# 05-era run fixture

A genuine, finished run recorded by the **05j** runner
(`feat/tradeoffs-trace-v1-models--05j`, runnerRevision
`28a599f1fb75daa0897b8dece80ede26d3d28ed8`), checked in so plan 06e's R5 can
prove that a run recorded under an older runner revision is still read as it
was recorded.

It was produced by that revision's own `setupConductor` harness (fake-Pi
worker and reviewers) on a disposable repository, and reached `DONE` with a
candidate. Only the conductor's own files are kept (`meta.json`,
`events.jsonl`, `plan/`, `views/`, `checks/`, `messages.jsonl`,
`ledger.jsonl`); the run's `worktree/`, `candidates/`, `stream/`, `sessions/`,
`refs/` and `inbox/` are omitted.

`meta.json` and the first `init` record name the recording revision, so
`runnerFor` sees an older revision that is not installed under the test's run
root and reads the run with the current runner — the projection must still be
the recorded one (`p1 — DONE`).
