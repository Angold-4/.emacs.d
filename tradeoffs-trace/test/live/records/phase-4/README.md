# Phase 4 live evidence

Program `ee2b5564` (runner 0f599e4), started through the Emacs module's
program parser (`+tt-parse-program`, then `tt program start`) on a disposable
repository, with a real deepseek worker and real reviewers M, A and B for
every phase.

    a ─┬─► b ─┬─► d        (#+TT_PROGRAM: 2, stacked branches)
       └─► c ─┘

| node | run | started → done (UTC) |
|---|---|---|
| a | 82c0f190 | 17:05:45 → 17:06:47 |
| b | ea2c9038 | 17:06:47 → 17:07:55 (in parallel with c) |
| c | 5c3a1aac | 17:06:47 → 17:08:06 |
| d | a5a8ec75 | 17:08:06 → 17:09:50 (after b and c) |

The whole program took 4m12s. Branches: `main--a` has add; `main--b` has add
and sub; `main--c` has add and mul; `main--d` sits on the scheduler's merge
commit of b and c and has add, sub, mul and the index (`node --test`: 7 of 7).
`main` was not moved.

Home paths are replaced with `~`.
