#!/bin/sh

cd src
cloc . --by-file --include-ext=hs --quiet --csv --hide-rate | python3 ../loc.py
