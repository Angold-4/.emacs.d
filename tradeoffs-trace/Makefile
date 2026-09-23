.PHONY: check test live

# Node 25 runs TypeScript natively (type stripping) — no build step.
# `cd` first so the quoted glob always resolves the same way regardless of
# the caller's cwd (see README).
check: test

test:
	cd $(CURDIR) && node --test 'test/**/*.test.ts'

# Live smoke test against the real installed Pi and a real model (opt-in:
# TT_LIVE=1 is required, or test/live/live-submission.test.ts skips itself
# with a printed reason). Writes a record per role to
# test/live/records/phase-0/. See README's "Live tests" section.
live:
	cd $(CURDIR) && TT_LIVE=1 node --test test/live/live-submission.test.ts
