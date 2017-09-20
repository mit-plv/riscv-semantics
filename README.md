# riscv-semantic

This project started using stack (In some distributions, stack has not been packaged for a long time, it is recommended to follow the install directions):
https://docs.haskellstack.org/en/stable/README/

This project assume you have a riscv32 gcc compiler.
(Depending on the version you have you may need to adjust test/Makefile)

After installing stack:

make

If your riscv32 gcc compiler is not set up,  wepushed a compiled version of thuemorse.

To run (interpret) the simulator on a compiled program:

stack exec riscv-semantics test/tests/thuemorse.hex
