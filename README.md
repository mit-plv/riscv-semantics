# riscv-semantic

riscv-semantics is a formal specification of the RISC-V ISA written in Haskell.

### Getting Started

1. Install Stack

This project uses stack to manage the Haskell compiler and the project's dependencies.
Even though there is a package for stack that can be installed using apt-get, this will not work due to known bugs in the version in the repository.
To install stack, follow the directions found here https://docs.haskellstack.org/en/stable/README/ or run the following code in a Unix-based machine:

    $ curl -sSL https://get.haskellstack.org/ | sh

2. Compile the Project

There is a makefile with recipes to compile the project, the elf2hex utility, and all the tests.
Building the tests requires a recent version of `riscv32-unknown-elf-gcc` in your path.
To build everything, run:

    $ make

If you do not have the compiler in your path, this command will fail saying

    make[1]: riscv32-unknown-elf-gcc: Command not found

This repo has a pre-compiled test `thuemorse.hex` if you don't have the RISC-V compiler.

3. Simulating an Example Program

To simulate an example program, run the command

    $ stack exec riscv-semantics test/tests/thuemorse.hex

This should produce the output

    01101001100101101001011001101001100101100110100101101001100101101001011001101001011010011001011001101001100101101001011001101001


4. Building the parser of execute to JSON:

   $ stack install aeson; stack install generic-aeson; stack install happy; cd src/; happy ParseToJson.y 


