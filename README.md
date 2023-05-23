# riscv-semantics  [![Build Status](https://travis-ci.org/mit-plv/riscv-semantics.svg?branch=master)]

Here lies a formal specification of [the RISC-V ISA](https://riscv.org), written in [Haskell](https://www.haskell.org/).
It is meant to serve many audiences.

First, it should be readable as documentation of the ISA, for, say, hardware and compiler engineers with no background in formal verification or functional programming.
The next section suggests how such readers should get started.

Second, it should be executable as an oracle for test cases and so on.
See the later sections here for build-and-run instructions.

Finally, people with sufficient Haskell background should be able to understand subtleties of the semantics and even extend with new features.
More documentation for that class of users may be forthcoming!


# How to Read this Spec

1. Read [the reading guide](READING.md).
2. Read the first half of [our explanation of exception handling in the semantics](exceptions.md).
3. Dive into [the Spec directory](src/Spec).


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

    $ ./make-circuit.sh

The output will be in `src/verilog`.

To get rid of intermediate files created in the `src` folder:

    $ ./clean-circuit.sh
