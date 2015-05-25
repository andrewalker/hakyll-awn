#!/bin/bash

# (cabal run server &); pid=$!

cabal run rebuild

events="modify,create,delete,move"
dir=$(basename $0)
skip="@site.hi @site.o @_site/ @dist/ @_cache @.cabal_sandbox @cabal.sandbox.config @.git"

while inotifywait -r -e $events $dir $skip; do cabal run rebuild; done

# kill $pid
