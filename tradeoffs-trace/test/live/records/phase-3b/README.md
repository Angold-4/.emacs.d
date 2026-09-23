# Plan 3b live evidence

Runner 55e707d, started through the Emacs module on a disposable two-file
Node repository, real deepseek worker and reviewers M, A and B. Status,
trace and decision view were rendered by the module's own functions
(`+tt--render-status-from`, `+tt--render-trace`, `+tt--render-decisions`)
during and after the run.

- `643ee336`: DONE in 71 s. Pipeline: implement 20s → freeze 1s → checks 1s
  → probe 0s (reused) → review 49s → DONE. Reviews: M ✓ 1 advisory, A ✓,
  B ✓ 1 advisory (M and B independently found the same misleading error
  message). The trace showed `sum.js +18 −0` under the worker's write and
  the worker's narrow test ran with --test-force-exit.
- `643ee336-timing.txt`: `tt timing` for the run.

Home paths are replaced with `~`.
