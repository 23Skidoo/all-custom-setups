#!/bin/bash

while read PKGNAME; do
    URL1="http://hackage.haskell.org/package/$PKGNAME/src/Setup.hs"
    URL2="http://hackage.haskell.org/package/$PKGNAME/src/Setup.lhs"
    DIRNAME=$PKGNAME
    mkdir $DIRNAME
    cd $DIRNAME
    curl -s --fail $URL1 -O || curl -s --fail $URL2 -O
    if [[ ! -f ./Setup.hs && ! -f ./Setup.lhs ]]; then
        echo "Warning: couldn't download the setup script for $PKGNAME!";
    fi
    cd ..
done <all-custom.list
