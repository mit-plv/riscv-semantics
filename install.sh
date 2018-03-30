#!/bin/bash

git submodule update --init --recursive
whichstack=`which stack`
if [ "$whichstack" == "" ]; then
    curl -sSL https://get.haskellstack.org/ | sh
fi
stack setup; stack install split; stack install text;  stack build
