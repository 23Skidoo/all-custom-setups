#!/bin/bash

TMPDIR=/tmp/gen-all-custom

mkdir -p $TMPDIR
cd $TMPDIR
tar xf ~/.cabal/packages/hackage.haskell.org/00-index.tar.gz
find -name '*.cabal' | xargs ag -li '^build-type:.*custom' | cut -f 1,2 -d/ | tr / - | sort | uniq > all-custom.list
cd -
cp $TMPDIR/all-custom.list .
