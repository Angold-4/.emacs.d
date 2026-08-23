# Delegation Prompt — Git Review Workbench Phase 2

You are the implementation worker for Phase 2 of a personal Emacs Git review
workbench. Implement **Phase 2 only** on the existing PR #5 branch. Phase 0 and
Phase 1 are accepted foundations. The lead agent will review your filesystem
changes and automated evidence; the owner will then use the result in terminal
and graphical Emacs before any later remote/PR phases begin.

## Repository, branch, and instructions

- Repository: `/home/ango/.emacs.d`
- Required branch: `feat/git-review-workbench-phase-1`
- Pull request: `https://github.com/Angold-4/.emacs.d/pull/5`
- Accepted Phase 1 commit: `d81ceab`
- Read `/home/ango/.codex/RTK.md` completely before running commands.
- Prefix every shell command with `rtk`, as required by the repository.
- Inspect `rtk git status --short --branch` before editing. Stop and report if
  the branch differs or if unrelated uncommitted changes are present.
- Do not create or switch branches. PR #5 is intentionally the single PR for
  the Git buffer implementation.
- Do not stage, commit, push, amend, rebase, reset, restore, clean, or modify
  the pull request. The lead owns Git history after review.

Read these files before editing:

1. `git-plan.md`, especially the invariants, delivery rules, and Phase 2.
2. `docs/git.md`, especially Sections 2, 4.6, 4.11, 4.12, 7, and 8.
3. `docs/git-baseline.md` for the recorded performance/window baseline.
4. `git-phase-1-worker-prompt.md` for the accepted Phase 1 interaction contract.
5. `core/init-git.el`, `core/init-git-ui.el`, `core/init-evil.el`, and `init.el`.
6. All existing files under `test/`, especially the Phase 0 fixtures and Phase
   1 navigation tests.
7. The locally installed Magit sources under `straight/repos/magit/lisp/` when
   verifying APIs or section contracts. Do not guess Magit signatures.

Run the existing 23-test suite before editing. It is the Phase 1 regression
gate and must remain green throughout Phase 2.

## Product outcome for this phase

Deliver the first daily-usable, entirely local code-review workflow:

```text
C-c g
  -> choose working tree / staged / commit / branch review
  -> clean unified overview
  -> t opens Changes Tree
  -> walk folders and files
  -> RET opens one file's unified diff
  -> e opens the writable worktree file when valid
  -> q returns to tree
  -> SPC marks the file reviewed
```

The Changes Tree is the clean folder/file page requested by the owner. It must
show the complete changed-file set, support collapsible folders and review
checkmarks, open exact file diffs, and remain fast with 100+ AI-generated
changes.

Phase 2 must work when Forge, `gh`, Delta, and Difftastic are unavailable. It
must not contact a remote. Phase 1's same-window Evil behavior and return stack
remain mandatory.

## Scope: Phase 2 only

Expected paths include:

- `core/init-git.el` for review-target construction and entry commands;
- `core/init-git-ui.el` for local review buffers, Changes Tree, persistence,
  keymaps, and diff faces;
- `test/git-review-fixtures.el` for additional deterministic local fixtures;
- new `test/git-review-phase2.el`;
- `test/git-review-test.el` to load the new suite;
- `.gitignore` only if a required generated path is not already covered.

The existing `.cache/` ignore rule already covers persisted review state.
Never add real state files, fixture repositories, package output, or databases
to the repository.

If `init-git-ui.el` becomes difficult to review, a narrowly focused local
review module may be proposed, but do not invent a new ownership layer without
documenting why it is clearer than the ownership in `git-plan.md`.

## 1. Introduce one explicit review-target value

Define one internal structured value, preferably a `cl-defstruct`, containing
at least:

- local repository root;
- scope: `worktree`, `staged`, `commit`, or `branch`;
- base object/ref and resolved base object ID when applicable;
- head object/ref and resolved head object ID when applicable;
- a stable target-family identity;
- a stable overview buffer identity;
- enough local metadata to reconstruct Git arguments without inspecting
  rendered buffer text.

Commands for working-tree, staged, commit, and branch review must construct
this value and place it buffer-locally on every generated overview, tree, and
file-diff buffer. Opening the same target again must reuse and refresh its
existing buffers instead of producing duplicates.

Target semantics:

- `worktree`: local `HEAD` versus index/worktree, including untracked files;
- `staged`: local `HEAD` versus index;
- `commit`: the selected commit's parent versus the commit; handle a root
  commit using Git's empty-tree object;
- `branch`: prompt for base and head, resolve both locally, and review
  merge-base(base, head) versus head.

Keep symbolic labels for display but use resolved object IDs for correctness.
Worktree and staged buffer identities must remain stable as files change so
refresh reuses the same buffer. Immutable commit/branch identities should
include resolved objects.

Expose all four review commands in `+git-dispatch` with concise labels. Do not
add network synchronization commands.

Staging, unstaging, and discard operations are valid only for worktree/staged
local targets. Immutable commit and branch review buffers must not accidentally
alter the index or working tree; hide, remap, or guard those actions with a
clear `user-error`.

## 2. Build a machine-readable changed-file model

Never derive file identity, status, paths, or line counts by parsing rendered
Magit text.

Use NUL-delimited, argument-safe Git process calls. The required primary data
sources are:

- `git diff --name-status -z`;
- `git diff --numstat -z`;
- `git ls-files --others --exclude-standard -z` for untracked files.

Additional machine-readable local Git commands are allowed when needed to
identify modes, gitlinks/submodules, blobs, or fingerprints. Do not use shell
command strings assembled from paths. Support spaces and Unicode in paths.

Merge the Git results into one file record containing at least:

- current path and old path for rename/copy;
- status: modified, added, deleted, renamed, copied, untracked, binary, or
  submodule/gitlink as applicable;
- additions and deletions, with an explicit binary representation rather than
  pretending binary counts are zero;
- target-specific old/new object information when available;
- a content fingerprint used by review-state persistence.

Handle these cases explicitly and test them:

- modified tracked file;
- staged and unstaged changes;
- untracked/new file;
- deletion;
- rename;
- copy detection;
- binary file;
- submodule/gitlink;
- nested directories;
- spaces and Unicode in paths;
- an empty directory, which Git should omit without breaking the tree;
- 100+ changed files.

Do not launch one Git process per file. Collect metadata and fingerprints in a
bounded number of batched Git calls. Add instrumentation or assertions so a
100-file refresh cannot silently become an N-process-per-file implementation.

## 3. Implement the Changes Tree

Add `+git-changes-tree-mode`, backed by valid `magit-section` objects. Do not
synthesize incomplete sections or manually fake section markers.

Render a compact tree similar to:

```text
Changes  3/7 reviewed  ·  +128 -42

[-] src/                         +112 -38
    [x] api/                      +58 -10
        [x] users.ts       M      +24  -3
        [x] auth.ts        R      +34  -7
    [ ] ui/                       +54 -28
        [ ] panel.tsx      M      +54 -28
[ ] test/
    [ ] review test.el     ?      +16  -0
[ ] assets/logo.bin        BINARY
```

Required behavior:

- directories are collapsible sections;
- folders and files show aggregate additions/deletions;
- files show Git status and reviewed state;
- folders show reviewed, unreviewed, or partial state;
- `SPC` on a file toggles only that file's reviewed metadata;
- `SPC` on a folder applies to all descendants; all-reviewed becomes
  unreviewed, while partial/unreviewed becomes reviewed;
- checkmarks never stage, unstage, discard, modify files, or submit a review;
- `TAB` toggles the folder at point;
- `S-TAB` cycles the tree globally;
- `[f` and `]f` move between file nodes;
- `/`, `n`, and `N` retain Evil path search behavior;
- `RET` on a file opens that file's unified diff in the selected window;
- `o` opens the same file diff in the reusable explicit right window;
- `q` returns through the Phase 1 caller/layout chain;
- `gr` refreshes from local Git and persisted review state only;
- `t` from status, local overview, unified diff, or another applicable review
  buffer opens/reuses the Changes Tree for that buffer's target.

Keep `|` as the existing explicit comparison placeholder/behavior. Do not
implement the later Phase 6 side-by-side renderer or make Difftastic the tree's
default action.

The tree buffer must start in Evil normal state and participate in
`+git-review-buffer-mode` without breaking Phase 1 keys.

Opening the tree may collect file metadata, but must not generate every file's
patch. Folder rendering must be lazy or sufficiently section-driven that
collapsed directories do not create diff buffers or per-file diff work. Add a
test proving that opening a 100-file tree creates no file-diff buffers and no
per-file Git subprocess pattern.

## 4. Reusable exact per-file unified diffs

`RET`/`o` from a tree file node must construct a file-specific diff from the
review target, not from whichever Magit buffer happened to be current.

Required behavior:

- one stable diff buffer per target and path, reused on reopen;
- correct range for worktree, staged, root commit, ordinary commit, and branch;
- exact old/new path treatment for rename and copy;
- deleted files show the old side and remain visitable as a blob;
- untracked files render as a complete all-added Git-produced patch;
- binary and submodule entries show a clear bounded fallback instead of empty
  or misleading text;
- `e` reaches the writable local file only when it exists;
- `q` returns to the tree with point on the same file;
- refresh remains local and preserves the review target.

Use native Magit diff rendering/refinement wherever Magit supports the target.
For untracked or exceptional entries, use complete Git-produced diff input or
fully valid Magit sections. Never reintroduce the disabled hand-built untracked
section with nil/incomplete position markers.

## 5. Persist reviewed state safely

Persist only local review progress under:

```text
~/.emacs.d/.cache/git-review/state/
```

Make the root configurable so tests can use a temporary directory and never
touch the owner's real state.

Persist atomically: write a temporary file in the same directory, finish and
close it, then rename it into place. A failed write must leave the previous
valid generation intact. Malformed state should degrade to all-unreviewed with
a useful message, not break Git review or evaluate arbitrary Lisp.

Review identity must account for:

- repository root for this local-only phase;
- target scope/family;
- base/head object IDs;
- path;
- file-content fingerprint.

When a mutable target or branch head changes:

- a file whose path and fingerprint are unchanged retains reviewed state;
- a changed file becomes unreviewed;
- a new or renamed path begins unreviewed unless its exact persisted key is
  still valid;
- removed files disappear cleanly;
- directory rollups recompute from the current descendants.

Do not key only by the entire target head in a way that loses all progress when
one file changes. Do not query, extend, migrate, or directly manipulate the
Forge SQLite database.

## 6. Deliver the clean native unified diff appearance

Use native Magit rendering and refinement as the default. Apply faces through a
named, repeatable function using the repository's theme-loading approach so
switching themes does not silently erase review readability.

Required appearance in terminal truecolor and graphical Emacs:

- default/context background: black or effectively black;
- context text: readable white/gray;
- added line: dark green background with readable near-white text;
- removed line: dark red background with readable near-white text;
- refined added/removed regions: visibly brighter green/red distinction;
- file headings: clear and prominent without overpowering changed code;
- hunk headings: quieter than file headings but easy to locate;
- selected/section highlight remains visible without destroying diff colors.

Do not invoke Delta or Difftastic during status, tree, unified diff, open, or
refresh. Difftastic remains an optional explicit `D` action only. Do not enable
`magit-delta`. Keep its removal as a separate owner-approved cleanup after the
owner compares the new native faces; report exactly what remains disabled.

Do not spend Phase 2 building the future side-by-side renderer. The ordinary
unified buffer must itself be clean enough for daily review.

## 7. Buffer reuse, offline behavior, and performance

All Phase 1 guarantees remain acceptance requirements:

- ordinary navigation preserves existing window count and selected layout;
- explicit `o` creates/reuses at most one right window;
- `q` restores the caller and layout;
- generated buffers start in Evil normal state;
- `n/N` remain Evil search;
- open, `RET`, `e`, `t`, `SPC`, `q`, and `gr` are offline operations.

Instrument actual public commands, not only lower-level helpers. Fail tests if
the implementation attempts fetch, pull, push, clone, remote update, `gh`,
HTTP(S), Delta, or Difftastic.

On the Phase 0 100-file fixture:

- warm Changes Tree render/refresh median must be under one second;
- warm status/refresh must not regress more than 20% from the Phase 0 method
  without a documented reason;
- report median/min/max with at least five warm samples;
- report process counts and prove they are bounded rather than proportional to
  the number of files.

Do not optimize by caching stale Git state indefinitely. `gr` must accurately
reflect current local files/index while reusing buffers and review metadata.

## Automated tests

Add `test/git-review-phase2.el` and load it from the main suite. Extend existing
fixtures carefully; do not weaken Phase 0 or Phase 1 assertions.

At minimum test:

1. Review-target construction and stable identities for worktree, staged,
   root/ordinary commit, and branch merge-base ranges.
2. Reopening overview/tree/file diff reuses the same buffer.
3. NUL-safe parsing and tree construction for every file case listed above.
4. Folder nesting, aggregate counts, collapse, and reviewed/partial/unreviewed
   rollups.
5. File/folder `SPC` toggles persistence without changing Git status or files.
6. Atomic persistence, failed-write preservation, and malformed-state fallback.
7. Fingerprint invalidation: modify one reviewed file; only that file loses its
   reviewed state.
8. Tree `RET`, `o`, `q`, `t`, navigation, Evil normal state, and search keys.
9. Exact per-file diff behavior for added, modified, removed, rename, copy,
   untracked, binary, and submodule entries.
10. Immutable commit/branch buffers cannot stage, unstage, or discard.
11. No actual public open/tree/diff/refresh command contacts a remote or starts
    Delta/Difftastic.
12. A 100+ file tree starts no per-file diff generation and meets the timing
    and bounded-process requirements.
13. All 23 existing Phase 0/1 tests remain green.
14. Temporary repositories, state directories, buffers, window layouts,
    advice, and hooks are cleaned with `unwind-protect` after success or error.

Tests may use rendered text only to assert presentation or position point; the
production model must never parse rendered text.

## Verification

Run and report at least:

```bash
rtk emacs --batch -Q -l init.el --eval '(princ "CONFIG-LOAD-OK\n")'
rtk emacs --batch -Q -l init.el -L test \
  -l test/git-review-test.el -f ert-run-tests-batch-and-exit
rtk git diff --check -- .gitignore core init.el test
rtk rg -n "[[:blank:]]+$" core test git-phase-2-worker-prompt.md
rtk git status --short --branch
```

Also perform a byte-compilation diagnostic using temporary copies outside the
repository. Report new warnings separately from the already known optional
Difftastic eager macro-expansion warning; do not hide or mislabel it.

Provide the exact performance command/method and the resulting warm samples,
process counts, and external executable counts.

Do not claim graphical/terminal color quality from batch face assertions alone.
Provide an exact owner manual script for both terminal/tmux and graphical Emacs.

## Non-goals and hard boundaries

Do not implement any of the following in Phase 2:

- canonical repository identities or multi-clone sharing;
- `init-git-store.el`, bare mirrors, alternates, or repository registries;
- batch synchronization, timers, background fetch, or an `@` implementation;
- PR/issue dashboards, comments, approvals, merges, or checks;
- Forge database schema changes or direct SQL;
- shared PR review state across clones;
- the Phase 6 side-by-side source renderer;
- automatic Difftastic or Delta rendering;
- package installation, update, rebuild, or lockfile changes;
- unrelated Emacs cleanup or broad keybinding changes.

Do not create a fake remote abstraction in anticipation of later phases. Phase
2 is deliberately local and path-scoped; Phase 3 will introduce canonical
repository/local-context ownership after this UI is accepted.

## Acceptance criteria

Phase 2 is ready for lead review only when:

1. The four local review targets are explicit, correct, and buffer-reused.
2. The Changes Tree is a clean, collapsible folder/file view backed by valid
   Magit sections.
3. Reviewed/partial/unreviewed state persists atomically and invalidates only
   changed files.
4. Tree file actions open exact reusable unified diffs for every required file
   type, including untracked, deletion, binary, and submodule fallbacks.
5. Checkmarks cannot mutate Git state.
6. Native unified diffs are readable black/red/green buffers in terminal and
   GUI, pending the owner's visual approval.
7. Same-window/Evil/return behavior from Phase 1 remains intact.
8. All public navigation and refresh commands are offline and start neither
   Delta nor Difftastic.
9. The 100-file tree is responsive and uses a bounded number of Git processes.
10. Configuration load, the complete ERT suite, compilation diagnostics, and
    scoped whitespace checks are honestly reported.
11. No Phase 3 or later architecture is introduced.

## Required handoff to the lead reviewer

Return a concise but complete report with:

1. User-visible outcome and exact workflow now supported.
2. Exact files added, changed, or removed.
3. Review-target fields, scope/range semantics, and buffer identity rules.
4. Git commands and NUL parsers used for tree construction.
5. Changes Tree model, section hierarchy, keybindings, and lazy-render strategy.
6. Persistence format/location, atomic-write method, and fingerprint behavior.
7. Per-file diff handling for every status/type.
8. Face/color choices and how they survive theme changes.
9. Test commands, total pass/fail counts, and named coverage evidence.
10. Warm timing samples, medians/ranges, process counts, and no-network/no-tool
    evidence.
11. Known risks, private Magit dependencies, or incomplete owner-only checks.
12. Exact terminal and graphical Emacs manual test steps.
13. Suggested rollback limited to Phase 2 paths.

Do not start Phase 3. Do not stage, commit, or push. Stop after the Phase 2
handoff so the lead can review the filesystem implementation before the owner
uses it for daily work.
