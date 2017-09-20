# riscv-semantic

This project started using stack:
https://docs.haskellstack.org/en/stable/README/

This project assume you have a riscv32 gcc compiler.
(Depending on the version you have you may need to adjust test/Makefile)

After installing stack:

make

If your riscv32 gcc compiler is not set up,  wepushed a compiled version of thuemorse.

To run (interpret) the simulator on a compiled program:

stack exec riscv-semantics test/tests/thuemorse.hex
