#!/bin/bash

git submodule update --init --recursive
whichstack=`which stack`
if [ "$whichstack" == "" ]; then
    curl -sSL https://get.haskellstack.org/ | sh
fi
stack setup
stack install split
stack install text
make
# the --extra... options are a workaround needed because the Makefile of softfloat-hs assumes it can globally install itself
stack build --extra-include-dirs softfloat-hs/berkeley-softfloat-3/source/include/ --extra-lib-dirs softfloat-hs/lib/
