# riscv-semantic  [![Build Status](https://travis-ci.org/mit-plv/riscv-semantics.svg?branch=master)]

riscv-semantics is a formal specification of the RISC-V ISA written in Haskell.

### Getting Started

1. Install Stack

This project uses stack to manage the Haskell compiler and the project's dependencies.
Even though there is a package for stack that can be installed using apt-get, this will not work due to known bugs in the version in the repository.
To install stack, follow the directions found here https://docs.haskellstack.org/en/stable/README/ or run the following code in a Unix-based machine:

    $ curl -sSL https://get.haskellstack.org/ | sh

2. Compile the Project

There is a makefile with recipes to compile the project, the elf2hex utility, and all the tests.
Building the tests requires a recent version of `riscv-none-embed-gcc` in your path.
To build everything, run:

    $ ./install.sh
    $ make

If you do not have the compiler in your path, this command will fail. To install the compiler and add it to your path:

    $ ./install_riscv_gcc.sh
    $ . setup.sh


3. Simulating an Example Program

To simulate an example program, run the command

    $ stack exec riscv-semantics test/build/thuemorse64

This should produce the output

    01101001100101101001011001101001100101100110100101101001100101101001011001101001011010011001011001101001100101101001011001101001


### Generation of verilog (work in progress)

	$ ./install-clash.sh
	$ ./make-circuit.sh

The output will be in src/verilog/...

To get rid of intermediate files create in the src folder:
	$ ./clean-circuit.sh
