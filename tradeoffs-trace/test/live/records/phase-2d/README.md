# Plan 2d live evidence

Runner 859bc48, started through the Emacs module (`+tt-parse-plan`, `tt
start`) on a disposable two-file Node repository, real deepseek worker and
reviewers M, A and B.

- `bfcb0157`: a steer sent through the input buffer (`+tt-input-send`, the
  header read "Sending steers worker attempt 1 now") was recorded
  `delivered` 1 s later and the worker followed it (sum() rejects NaN and
  Infinity). Submit 30.5 s, checks 32.6 s, probe reused, barrier 53.5 s,
  DONE 76.5 s.
- `cda2b8a9`: `tt stop` on an implementing run returned in 1 s, logged the
  stop event and left no process behind.

Home paths are replaced with `~`.
