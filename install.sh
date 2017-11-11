#!/bin/bash

git submodule update --init --recursive
cd clash; git apply ../patchClash.apply 
cd ..
stack setup; stack install split; stack install text;  stack build
