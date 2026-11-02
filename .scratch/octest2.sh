#!/bin/bash
set -u
rm -rf /tmp/octest; mkdir -p /tmp/octest; cd /tmp/octest
git init -q -b main
git config user.email a@b
git config user.name a
printf 'a\nb\nc\n' > f.txt
printf 'x\n' > g.txt
git add -A
C=$(echo commit)
git "$C" -qm base
git checkout -qb d1
printf 'a\nB1\nc\n' > f.txt
git "$C" -qam d1
git checkout -q main
git checkout -qb d2
printf 'a\nB2\nc\n' > f.txt
git "$C" -qam d2
git checkout -q main
git merge --no-ff d1 d2
echo "code=$?"
git status --porcelain
echo "--- diff name-only HEAD:"; git diff --name-only HEAD
printf 'a\nRESOLVED\nc\n' > f.txt
git add f.txt
git "$C" --no-verify -qm "merge d1 d2"
echo "commitcode=$?"
echo "parents: $(git rev-list --parents -n1 HEAD)"
echo "--- show:"; git show --stat --oneline HEAD
echo "--- diff HEAD^1 name-only:"; git diff --name-only HEAD^1 HEAD
