# riscv-semantics  [![Build Status](https://travis-ci.org/mit-plv/riscv-semantics.svg?branch=master)]

Here lies a formal specification of the RISC-V ISA, written in Haskell.


# How to read this spec

1. Read the reading guide: [./hardware-hacker.md](https://github.com/mit-plv/riscv-semantics/blob/master/hardware-hacker.md)
2. Read the first half of [./exceptions.md](https://github.com/mit-plv/riscv-semantics/blob/master/exceptions.md)
3. Dive into the [./src/Spec](https://github.com/mit-plv/riscv-semantics/tree/master/src/Spec) folder


# Installation Guide

## Install Stack

This project uses stack to manage the Haskell compiler and the project's dependencies.
Even though there is a package for stack that can be installed using `apt` (in Ubuntu et al.), it won't work due to known bugs in the version in the repository.
To install stack, follow [its official directions](https://docs.haskellstack.org/en/stable/README/) or run the following code in a Unix-based machine:

    $ curl -sSL https://get.haskellstack.org/ | sh

## Compile the Project

There is a Makefile with recipes to compile the project, the elf2hex utility, and all the tests.
Building the tests requires a recent version of `riscv-none-embed-gcc` in your path.
To build everything, run:

    $ ./install.sh
    $ make

If you do not have the compiler in your path, this command will fail. To install the compiler and add it to your path:

    $ ./install_riscv_gcc.sh
    $ . setup.sh

## Simulating an Example Program

To simulate an example program, run the command

    $ stack exec riscv-semantics test/build/thuemorse64

This invocation should produce the output

    01101001100101101001011001101001100101100110100101101001100101101001011001101001011010011001011001101001100101101001011001101001

## Run riscv-tests

To run the default 64-bit executable simulator on riscv-tests:

   $ stack exec riscv-semantics-tests

# Generation of Verilog (work in progress)

	$ ./install-clash.sh
	$ ./make-circuit.sh

The output will be in `src/verilog`.

To get rid of intermediate files created in the `src` folder:

	$ ./clean-circuit.sh
