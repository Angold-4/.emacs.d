# Git Review Workbench — Phase 0 Baseline

Recorded: **2026-07-15** (UTC date `2026-07-15`)
Platform: **Linux WSL2** `6.6.87.2-microsoft-standard-WSL2` `x86_64` (`uname -srm`)
Host label from `uname -n`: `pangda`

This document freezes the pre-implementation environment and Magit behavior.
Values are labeled **MEASURED** (collected in this Phase 0 worker run) or
**PENDING OWNER MANUAL RUN** (requires an interactive graphical or tmux Emacs
session). No number below is invented.

No package was installed, updated, fetched, or rebuilt while collecting these
values. Package revisions come from the existing local Straight checkouts and
`straight/versions/default.el`.

## 1. Tool and package environment

| Item | Value | Status |
|---|---|---|
| Emacs | `30.2` | MEASURED |
| Git | `2.43.0` | MEASURED |
| Magit | `1288f655733280f3f09616d5b5b7245c8b7dc843` (`1288f65`) | MEASURED |
| Forge | `9628f76740aec9270e9fb31457ff4cb38d9f3f16` (`9628f76`) | MEASURED |
| Evil | `729d9a58b387704011a115c9200614e32da3cefc` (`729d9a5`) | MEASURED |
| Evil Collection | `cd6cdf337ea96abc742a97be61c817788f512413` (`cd6cdf3`) | MEASURED |
| magit-delta (checkout) | `5fc7dbddcfacfe46d3fd876172ad02a9ab6ac616` (`5fc7dbd`) | MEASURED |
| difftastic.el (checkout) | `7db20929cac31687a529943c3d8d5b44fd8d69e2` (`7db2092`) | MEASURED |
| `gh` | `2.96.0 (2026-07-02)` at `/usr/bin/gh` | MEASURED |
| `delta` | not on `PATH` | MEASURED unavailable |
| `difft` | not on `PATH` | MEASURED unavailable |

These are the historical Phase 0 revisions. Current pins remain recorded in
`straight/versions/default.el`; this baseline is not an assertion that package
versions have never changed since measurement.

### Commands used to collect environment data

All shell commands below are shown with the repository-required `rtk` prefix.

```bash
rtk date -u +%Y-%m-%d
rtk uname -srm
rtk emacs --batch --eval '(princ (format "%s\n" emacs-version))'
rtk git --version
rtk gh --version
rtk which delta; rtk which difft
rtk git -C ~/.emacs.d/straight/repos/magit rev-parse HEAD
rtk git -C ~/.emacs.d/straight/repos/forge rev-parse HEAD
rtk git -C ~/.emacs.d/straight/repos/evil rev-parse HEAD
rtk git -C ~/.emacs.d/straight/repos/evil-collection rev-parse HEAD
rtk git -C ~/.emacs.d/straight/repos/magit-delta rev-parse HEAD
rtk git -C ~/.emacs.d/straight/repos/difftastic.el rev-parse HEAD
rtk grep -E '"(magit|forge|evil|evil-collection|magit-delta|difftastic\.el)"' \
  ~/.emacs.d/straight/versions/default.el
```

## 2. Behavioral Magit baseline

### 2.1 Measurement method

Warm timings are collected by `git-review-baseline-measure-repo`:

1. Install the hard network guard.
2. **Prewarm** each surface once under the guard (untimed, not logged): open
   Magit status, refresh once, open unified working-tree diff once.
3. **Sample** each metric **5** times under the same guard with process
   recording enabled.
4. Report **median**, **min**, and **max** (not a single cold/first-open time).

The hard network guard temporarily advises `call-process`, `process-file`,
`call-process-region`, `start-process`, `make-process`, `url-retrieve`,
`url-retrieve-synchronously`, `open-network-stream`, and
`make-network-process`. Remote Git argv after Magit-style global prefixes
(`fetch` / `pull` / `push` / `clone` / `ls-remote` / `remote update`), HTTP(S)
URLs, `curl`/`wget`/`gh`, and Emacs URL/network streams raise an error instead
of contacting the network. Nested process APIs record one child command.
Advice is always removed afterward.

### 2.2 Batch-mode warm timings (MEASURED)

Collected **2026-07-15** with Emacs `--batch`, loading this configuration, on a
temporary large fixture of **100** modified files.

```bash
rtk emacs --batch -Q -l init.el -L test -l test/git-review-baseline.el
```

| Metric | Value | Status |
|---|---|---|
| Warm Magit status | median `0.062s` (min `0.056s`, max `0.070s`, n=5) | MEASURED (batch, prewarmed) |
| Warm `magit-refresh` | median `0.068s` (min `0.061s`, max `0.071s`, n=5) | MEASURED (batch, prewarmed) |
| Warm unified working-tree diff | median `0.026s` (min `0.022s`, max `0.035s`, n=5) | MEASURED (batch, prewarmed) |
| Processes during status samples | `git=315`, `delta=0`, `difft=0`, `other=0` | MEASURED |
| Processes during refresh samples | `git=315`, `delta=0`, `difft=0`, `other=0` | MEASURED |
| Processes during diff samples | `git=315`, `delta=0`, `difft=0`, `other=0` | MEASURED |
| Network guard | `hard` (active for prewarm and samples) | MEASURED |
| Network attempts blocked during measurement | `0` | MEASURED |
| Windows before Magit status | — | PENDING OWNER MANUAL RUN |
| Windows after Magit status | — | PENDING OWNER MANUAL RUN |
| Windows after unified diff | — | PENDING OWNER MANUAL RUN |

Notes:

- These are **prewarmed multi-sample** medians. Do not compare later phases
  against the earlier single-shot figures (`0.069` / `0.069` / `0.022`), which
  were cold/first-open and not statistically stable.
- Batch Emacs has no interactive window layout, so window counts are not
  measured here.
- Prefer the owner manual run below when comparing later phases against
  daily-use interactive latency. Use median-to-median comparison and the
  recorded min/max range when applying the plan’s 20% regression threshold.
- Delta and Difftastic binaries were absent from `PATH`, and no `delta` /
  `difft` child process started during status, refresh, or diff samples.
- The hard network guard remained installed for every timed sample; zero
  blocked attempts means Magit did not attempt a guarded remote/URL path.

### 2.3 Owner interactive run (MEASURED)

Collected **2026-07-15** in terminal Emacs (`graphic-frame: nil`) using the
temporary 100-file fixture. The frame began with two windows.

Run this from an interactive Emacs session (GUI, or terminal Emacs inside
tmux) that has already loaded this configuration. Do not fetch remotes.

Exact steps:

1. Start Emacs the way you normally review code (`emacs` GUI, or
   `emacs -nw` inside tmux).
2. Evaluate:

   ```elisp
   (add-to-list 'load-path (expand-file-name "test" user-emacs-directory))
   (require 'git-review-baseline)
   ```

3. Run:

   ```text
   M-x git-review-baseline-run-on-large-fixture RET
   ```

   This command restores the previous window configuration and kills
   fixture-owned Magit buffers before deleting the temporary repository.

   Or, inside an existing Git repository you care about:

   ```text
   M-x git-review-baseline-run-manual RET
   ```

4. Copy the contents of the `*git-review-baseline*` buffer into the table.

| Metric | Value | Status |
|---|---|---|
| Windows before Magit status | `2` | MEASURED (terminal) |
| Windows after Magit status (`C-x g` / `magit-status`) | `3` | MEASURED (terminal) |
| Windows after unified diff open | `4` | MEASURED (terminal) |
| Warm Magit status | median `0.056s` (min `0.052s`, max `0.075s`, n=5) | MEASURED (terminal, prewarmed) |
| Warm `magit-refresh` / `gr` | median `0.069s` (min `0.065s`, max `0.080s`, n=5) | MEASURED (terminal, prewarmed) |
| Warm unified working-tree diff | median `0.030s` (min `0.024s`, max `0.038s`, n=5) | MEASURED (terminal, prewarmed) |
| Processes during each five-sample set | `git=315`, `delta=0`, `difft=0`, `other=0` | MEASURED |
| Network attempts blocked | `0` | MEASURED |

The `2 -> 3 -> 4` window sequence confirms the current usability defect. Phase
1 must make ordinary status and diff navigation preserve the existing window
count (`2 -> 2 -> 2`); only explicit comparison commands may add a window.

## 3. Hygiene checks recorded with this baseline

| Check | Result | Status |
|---|---|---|
| `forge-database.sqlite*` ignored via `.gitignore` | entry added; database not opened/copied | MEASURED |
| Temporary fixtures cleaned after ERT | parent temp dirs removed | MEASURED (ERT) |
| Fixture Magit buffers cleaned before delete | status/diff buffers killed | MEASURED (ERT) |
| Configuration batch load | `CONFIG-LOAD-OK` | MEASURED |
| ERT `test/git-review-test.el` | 12 passed, 0 unexpected | MEASURED |
| Network/package install during Phase 0 collection | none performed | MEASURED by procedure |

## 4. Distinction summary

- **MEASURED**: Emacs/Git/`gh` versions; Magit/Forge/Evil/Evil Collection /
  magit-delta / difftastic.el revisions; `delta`/`difft` absence; batch
  prewarmed median/min/max Magit timings (n=5); hard network guard active with
  zero blocked attempts during measurement; ERT and config-load results.
- **MEASURED interactively**: terminal window counts before/after Magit status
  and diff; terminal prewarmed median/min/max timings; process and network-guard
  results.

Keep both the batch and terminal-interactive numbers so later phases can compare
like-for-like using medians and ranges.
