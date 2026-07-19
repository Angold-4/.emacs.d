# Git Review Authentication

Git fetches may use SSH, but Forge needs a Github API token for pull-request,
issue, review, and conversation metadata.  The Git review workbench supports
ordinary Emacs Auth Source credentials and an optional 1Password provider.

## 1Password (recommended for managed workstations)

Requirements:

1. Install 1Password CLI (`op`) on the workstation.
2. Connect it to the signed-in 1Password desktop app.
3. Store the Github token in a concealed field on a 1Password item.
4. Copy that field's secret reference.  It has this shape:

   ```text
   op://Employee/GitHub Forge/token
   ```

5. Configure the non-secret Github username expected by Ghub:

   ```bash
   git config --global github.user Angold-4
   ```

   Use your own Github login in place of `Angold-4`. SSH authentication does
   not provide this API username automatically.

Configure only that non-secret reference:

```elisp
(setq +forge-1password-token-references
      '(("api.github.com" . "op://Employee/GitHub Forge/token")))
```

Alternatively, set the reference before starting Emacs:

```bash
export FORGE_GITHUB_TOKEN_OP_REF='op://Employee/GitHub Forge/token'
```

When Ghub cannot find a normal Auth Source token, Emacs runs:

```text
op read <secret-reference>
```

The token is captured from standard output, never placed in process arguments
or messages, and cached only in Emacs memory.  The 1Password desktop app may
request biometric or system authentication the first time.  Restarting Emacs,
or running `M-x +forge-1password-clear-token-cache`, clears the cached token.

Before testing Forge, the workstation can validate the reference without
printing the token:

```bash
op read "$FORGE_GITHUB_TOKEN_OP_REF" >/dev/null
```

Then start Emacs, run `C-c g f`, and wait for a message ending in
`forge: current`. The Git mirror and Forge database remain on local disk
across Emacs restarts; the token itself does not.

Ordinary Auth Source remains authoritative when both providers are configured.
On a managed workstation, remove any temporary `~/.authinfo` entry after the
1Password path is verified.

## Plain or encrypted Auth Source

For machines without 1Password, Ghub also supports the normal Auth Source
entry:

```text
machine api.github.com login USERNAME^forge password TOKEN
```

Use `~/.authinfo.gpg` rather than `~/.authinfo` when persistent encrypted local
storage is acceptable.
