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
git status --short
echo "parents: $(git rev-list --parents -n1 HEAD)"
