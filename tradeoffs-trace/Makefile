.PHONY: check test ert live crash

# Node 25 runs TypeScript natively (type stripping) — no build step.
# `cd` first so the quoted glob always resolves the same way regardless of
# the caller's cwd (see README).
check: test ert

# The Emacs front end (core/init-tradeoffs-trace.el) under ERT in batch mode.
ert:
	cd $(CURDIR)/.. && emacs --batch -Q -L core -L test -l test/tradeoffs-trace-test.el -f ert-run-tests-batch-and-exit

# `test/**/*.test.ts` already includes test/crash/crash-suite.test.ts and
# test/live/*.test.ts. Without TT_CRASH_FULL=1, crash-suite.test.ts only
# runs its 3-boundary FAST_SUBSET (ms-scale deadlines — a second or two
# each, see that file's own header comment), not the full 12-boundary
# sweep; both opt-in live tests skip themselves without TT_LIVE=1. So this
# costs only the rest of the suite's own genuinely-slow tests.
test:
	# 180s: this packet's tt-start-happy-path and deadline-every-stage
	# exit-gate tests are both genuinely slow (the former drives a real
	# `tt start`-launched daemon through production DEFAULT_DEADLINES —
	# dominated by the freeze's default 30s abortGraceMs; the latter is
	# 11 sub-tests, several of which wait out a real deadline).
	cd $(CURDIR) && node --test --test-timeout=180000 'test/**/*.test.ts'

# The full crash-suite (every phase-1 TT_CRASH_AT boundary, not just
# `make check`'s 3-boundary FAST_SUBSET), for a reviewer who wants full
# boundary coverage without the rest of `make check`'s own slow tests.
crash:
	cd $(CURDIR) && TT_CRASH_FULL=1 node --test --test-timeout=180000 'test/crash/**/*.test.ts'

# Live smoke tests against the real installed Pi and a real model (opt-in:
# TT_LIVE=1 is required, or each live test skips itself with a printed
# reason). Writes one record per role to test/live/records/phase-0/, a
# record + relativized events.jsonl to test/live/records/phase-1/, and
# (work packet 2a) the same to test/live/records/phase-2/ for the real
# two-turn review protocol (real M/A/B, not stub reviewers) — its own
# 20-minute bound makes this target noticeably slower than before this
# packet. See README's "Live tests" section.
live:
	cd $(CURDIR) && TT_LIVE=1 node --test --test-timeout=1260000 test/live/live-submission.test.ts test/live/live-single-phase.test.ts test/live/live-review.test.ts
