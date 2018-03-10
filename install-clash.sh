#!/bin/bash

cp installclash.yml stack.yml
git submodule update --recursive
stack build
git checkout stack.yml

