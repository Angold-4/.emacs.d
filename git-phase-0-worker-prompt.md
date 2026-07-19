# Delegation Prompt — Git Review Workbench Phase 0

You are the first implementation worker for a personal Emacs configuration.
Implement **Phase 0 only** from the Git Review Workbench plan. The lead agent
will review your code and evidence; the owner will later test the approved
behavior interactively in Emacs.

## Repository and instructions

- Repository: `/home/ango/.emacs.d`
- Read `/home/ango/.codex/RTK.md` completely before running commands.
- Prefix every shell command with `rtk`, as required by the repository
  instructions.
- Read these files before editing:
  1. `git-plan.md`, especially Phase 0 and the delivery rules.
  2. `docs/git.md`, especially the invariants, test matrix, and performance
     criteria.
  3. `git.md` for the current user-visible workflow.
  4. `.gitignore`, `init.el`, `core/init-git.el`, `core/init-tools.el`, and
     `core/init-evil.el` for current implementation context.
- Inspect `git status --short --branch` before doing anything.

## Existing work that must be preserved

The working tree already contains planning changes owned by the lead:

- modified `core/init-core.el` (unrelated owner work; do not inspect or edit it
  unless needed only to identify an overlap, and do not include it in Phase 0);
- modified `docs/git.md`;
- untracked `git-plan.md`;
- untracked `git-phase-0-worker-prompt.md` may also be present because this
  prompt is part of the planning handoff.

Do not discard, overwrite, stage, commit, or reformat those files. Do not run
`git reset`, `git checkout --`, `git clean`, or any destructive command. Do not
create or switch branches while these planning changes are uncommitted. The
lead will handle branch and commit operations after review.

## Product context

The final product will be a clean, buffer-first GitHub code-review workflow:

```text
Repository status
  -> PR overview
  -> Changes Tree
  -> unified file diff
  -> optional side-by-side detail
  -> real source or Markdown editor
```

Normal navigation will replace the selected Emacs buffer, generated views will
use Evil normal state, diffs will be clean red/green/black buffers, and remote
access will occur only through explicit synchronization. None of that behavior
is implemented in Phase 0. Your task is to establish a trustworthy baseline
and test foundation without changing current Git/Magit/Forge behavior.

## Scope: Phase 0 only

Expected edited or added paths:

- `.gitignore`
- `test/git-review-test.el`
- one or more focused helpers under `test/`, if useful
- `docs/git-baseline.md`

Do not edit production modules under `core/` or change `init.el` in this phase.
If a test cannot be built without a production change, report that limitation
instead of expanding scope.

### 1. Protect Forge state

Add `forge-database.sqlite*` to `.gitignore` in the appropriate generated/cache
section. Do not create, open, migrate, delete, truncate, or copy any actual
Forge database.

### 2. Add repeatable Git fixtures

Create an ERT test foundation that constructs repositories under temporary
directories and cleans them with `unwind-protect` or equivalent.

The small fixture must contain enough history and working state to exercise:

- a modified tracked file;
- a staged modification or addition;
- an untracked file;
- an added file;
- a deleted file;
- a renamed file;
- nested directories.

The large fixture must cheaply create at least 100 changed files so later
phases can measure status, Changes Tree, and diff rendering. Keep generated
fixtures out of the repository and do not rely on the developer's global Git
identity: configure a local test name/email inside each temporary repository.

Use argument-safe Emacs process APIs or direct Git argument lists. Do not build
shell command strings from file paths. Tests must support spaces in temporary
paths.

Add initial tests proving fixture state with machine-readable Git output. The
tests should be deterministic, offline, and safe to run repeatedly.

### 3. Capture the current environment

Create `docs/git-baseline.md` containing:

- date and platform;
- Emacs and Git versions;
- installed Magit, Forge, Evil, and Evil Collection revisions;
- availability/version of `gh`, `delta`, and `difft`;
- the exact commands used to collect the information;
- a clear distinction between measured values and unavailable/not-run values.

Do not install, update, fetch, or rebuild any package. Use the existing local
checkout only. Do not browse the network.

### 4. Record the current behavioral baseline

Record the current behavior before implementation:

- window count before and after opening Magit status;
- window count before and after opening a diff;
- warm Magit status render time;
- warm unified diff render time;
- `magit-refresh` time;
- which external Git, Delta, or Difftastic processes start during those
  operations.

Prefer a repeatable Emacs helper under `test/` for measurement. Any temporary
instrumentation must be test-local and removed with `unwind-protect`; never add
permanent advice or hooks to production configuration.

It is acceptable for GUI/tmux-only measurements to remain explicitly marked
`PENDING OWNER MANUAL RUN` if this environment cannot measure them faithfully.
Do not invent numbers. Still provide exact interactive steps or an Emacs
command the owner can run to fill them in later.

The baseline itself must not contact a remote. Instrument or inspect processes
well enough to demonstrate that no network command was intentionally run.

### 5. Verification

Run and report at least:

```bash
rtk git diff --check
rtk emacs --batch -Q -l init.el --eval '(princ "CONFIG-LOAD-OK\n")'
rtk emacs --batch -Q -l init.el -L test \
  -l test/git-review-test.el -f ert-run-tests-batch-and-exit
```

If a command fails, diagnose and fix in-scope failures. Do not hide failures or
change unrelated configuration to make the output green.

Also verify:

- `git status --short` contains only the protected pre-existing files listed
  above and your Phase 0 paths;
- no generated fixture, database, package output, or secret appears as an
  untracked file;
- no production Git/Magit/Forge behavior changed.

## Acceptance criteria

Phase 0 is complete only when:

1. Forge database files are ignored without touching a database.
2. Small and large temporary fixtures are deterministic and pass ERT.
3. The current tool/package environment is documented accurately.
4. Baseline measurements are reproducible, or manual-only fields have exact
   owner instructions and are honestly marked pending.
5. Configuration load and all new tests pass.
6. The implementation contains no network access, package installation/update,
   production advice, or behavior change.

## Required handoff to the lead reviewer

Return a concise report with:

1. Summary of what you changed.
2. Exact files changed/added.
3. Test commands and complete pass/fail result.
4. Recorded baseline values and pending manual values.
5. Evidence that no network/package update occurred.
6. Any assumptions, limitations, or follow-up risks.
7. Suggested rollback (normally removal of Phase 0 files plus the one
   `.gitignore` entry).

Do not start Phase 1. Do not commit or stage changes. Stop after the Phase 0
handoff so the lead can review the implementation and the owner can perform any
manual Emacs checks.
