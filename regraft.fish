#!/usr/bin/env fish

if test -z "$argv[1]"
  echo "usage: ./graft.fish straight/repos/cmake"
  exit 1
end
set dir $argv[1]
if not test -d $dir
  echo "$dir not exists"
  exit 1
end

pushd $dir
# remove unused remote branch refs
git for-each-ref --format="%(refname)" refs/remotes/ |\
	grep -v HEAD | grep -v (git branch --show-current) |\
	while read ref; git update-ref -d $ref; end
git replace --graft HEAD
git reflog expire --expire=now --all
git gc --aggressive --prune=now
