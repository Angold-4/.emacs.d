# Plan 2c live evidence

Two real runs started through the Emacs module (`+tt-parse-plan` + `tt start`
from the installed runner) on a disposable repository (`/tmp/tt-live-2c`, a
two-file Node project), plan "validate sum() input", with a real deepseek
worker and real deepseek reviewers M, A and B.

| run | runner | worker | freeze | checks | probe | reviews (barrier) | total |
|---|---|---|---|---|---|---|---|
| dc8b6430 | 8af30b1 | 15 s | 32 s | 0.5 s | reused | 35 s | 85 s → DONE |
| be849526 | 8db4b60 | 17 s | 1.5 s | 0.5 s | reused | 31 s | 50 s → DONE |

8db4b60 closes the agent's stdin on termination; before it, every freeze and
reviewer shutdown waited out the 30 s abort grace. For comparison, a round of
dogfood run 4ec5e0f8 (plan 2b, runner 9459e6d) took about 39 minutes.

Home paths in the event logs are replaced with `~`.
