#!/bin/bash

cp installclash.yml stack.yaml
git submodule update --recursive
stack build
git checkout stack.yaml

