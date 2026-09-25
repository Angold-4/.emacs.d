# atlas plan fixture (plan 01c)

A byte-for-byte copy of the plan files in `~/orgw/work/atlas/indexps/`, taken
2026-09-25. It is the regression fixture for the plan linter
(`src/core/plan-lint.ts`): `test/unit/plan-lint.test.ts` asserts the linter
finds **exactly one** error across all of them — `13j_process_split.org`'s
owner-actor item, "the owner records a live `ips-split` run" — and no false
error anywhere else. The files are plan prose only; they name credential
*variables* but hold no value, so the fixture needs no credentials. Do not
edit them to make a test pass: they are the real plans the linter must accept.
