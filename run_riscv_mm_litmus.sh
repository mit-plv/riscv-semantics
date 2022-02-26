#!/bin/bash

for f in $(ls test/litmus/*.litmus); do
  echo $f ;
  /usr/bin/time stack exec riscv-mm $f > $f.log 2>&1 ;
  rm $f.S
  rm $f.o
  rm $f.exe
done