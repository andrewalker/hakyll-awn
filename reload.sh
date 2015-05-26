#!/bin/bash

# (cabal run server &); pid=$!

cabal run rebuild

events="modify,create,delete,move"
dir=$(dirname $0)
skip="@$dir/site.hi @$dir/site.o @$dir/_site/ @$dir/dist/ @$dir/_cache/ @$dir/.cabal_sandbox @$dir/cabal.sandbox.config @$dir/.git/"

while inotifywait -r -e $events $dir $skip; do cabal run rebuild; done

# kill $pid
