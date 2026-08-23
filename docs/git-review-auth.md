# Git Review Authentication

Git and Forge authenticate independently:

- Git fetches and pushes use the repository's SSH remote. This machine routes
  SSH signing through the 1Password SSH agent, so an unlock or approval prompt
  from 1Password during `git fetch` is expected. No GitHub API token is
  involved in that prompt.
- Forge uses a GitHub personal access token (PAT) for HTTPS API requests. This
  configuration retrieves it automatically from macOS Keychain on macOS, or
  from standard Auth Source on Linux and WSL. Forge has no `op` CLI integration.

The pinned Forge version requires a classic GitHub PAT with the `repo`, `user`,
and `read:org` scopes. Create one at <https://github.com/settings/tokens> and
configure the non-secret username expected by Ghub:

```bash
git config --global github.user Angold-4
```

Use your own GitHub login in place of `Angold-4`.

## Current macOS profile

This repository currently uses the SSH remote
`git@github.com:Angold-4/.emacs.d.git`. The existing `~/.ssh/config` routes Git
SSH authentication through the 1Password SSH agent, and that behavior is kept.
An unlock or approval window during `git fetch` or `git push` belongs to SSH.

Forge uses the separate Internet Password item in the login Keychain:

```text
label:   Emacs Forge GitHub token
server:  api.github.com
account: Angold-4^forge
```

The blocked `1password-cli` cask has been removed, and `op` is intentionally
not installed or required. Do not reinstall it for this workflow. The
1Password desktop application and SSH agent remain useful for Git and are not
part of Forge PAT retrieval.

## macOS Keychain

macOS Keychain is the default persistent Forge credential store on macOS. The
token remains in the user's encrypted login Keychain and is retrieved
automatically on later Forge requests and Emacs launches.

### Initial setup from Emacs

Run:

```text
M-x +forge-store-token-in-macos-keychain
```

Paste the PAT at the hidden prompt. Emacs sends it twice through standard
input to satisfy Apple's confirmation prompt; the token is never placed in a
process argument or ordinary file. The resulting Internet Password item is:

```text
label:   Emacs Forge GitHub token
server:  api.github.com
account: GITHUB-USERNAME^forge
```

macOS may ask for Keychain access approval the first time Emacs reads it.
This is a system Keychain authorization prompt. Once access is approved, Forge
retrieves the item without asking for the PAT again.

### Initial setup or token rotation from Terminal

When a PAT expires, create its replacement and run the same command below.
The `-U` flag updates the existing matching item instead of creating a second
credential:

```bash
/usr/bin/security add-internet-password -U \
  -a 'Angold-4^forge' \
  -s 'api.github.com' \
  -l 'Emacs Forge GitHub token' \
  -w
```

Replace `Angold-4` with the value of `git config --global github.user`. At the
two invisible `password data` prompts, paste the same new PAT both times. Do
not write `-w TOKEN`: putting a token directly after `-w` exposes it in shell
history and process arguments.

Confirm the item exists without printing its secret:

```bash
/usr/bin/security find-internet-password \
  -a 'Angold-4^forge' \
  -s 'api.github.com'
```

After updating it, restart Emacs or run:

```text
M-x +forge-clear-token-cache
```

Then run `C-c g f`. A successful sync ends with a message containing
`forge: current`.

## Linux and WSL Auth Source

Linux and WSL use Emacs Auth Source by default. Store this entry in the
GPG-encrypted `~/.authinfo.gpg` file:

```text
machine api.github.com login USERNAME^forge password TOKEN
```

Replace `USERNAME` and `TOKEN` with the GitHub login and PAT. Prefer
`~/.authinfo.gpg`; a plaintext `~/.authinfo` is supported by Emacs but is not
recommended. Keep credential files private, for example:

```bash
chmod 600 ~/.authinfo.gpg
```

When the PAT expires, replace only the token in that entry, save the encrypted
file, run `M-x +forge-clear-token-cache` (or restart Emacs), and run `C-c g f`.

`+forge-allow-auth-source` defaults to non-nil outside macOS. Set it to nil if
a particular Linux or WSL machine must not use persistent Auth Source storage.
This option affects only Forge; other Ghub clients keep their normal behavior.

## Session-only fallback

When no approved persistent store is available, run:

```text
M-x +forge-set-session-token
```

Paste the PAT at the hidden prompt. The value stays only in the current Emacs
process and is reused by explicit `C-c g f` / `C-c g F` synchronization. It is
not added to minibuffer history, the kill ring, messages, process arguments,
customization, or a file by this module. Restart Emacs or run
`M-x +forge-clear-token-cache` to forget it.

1Password may remain the place where a user keeps or copies the PAT, and its
SSH agent may continue signing Git operations. Forge itself does not call
1Password or require the `op` executable.
