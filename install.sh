#!/bin/bash

git submodule update --init --recursive
stack setup; stack install split; stack install text;  stack build
