# Introduction

The source code of this project can (and is intended to be) used as a definitive
reference for the behavior of a compliant RISC-V machine. To that end, we hope
hardware engineers will find it useful as a highly precise description of the
specification, especially with regards to decoding and executing instructions.

The reason that this is such a precise description is, of course, that it's also
a computer program that implements the spec. The benefit is precision and
confidence: unlike a specification in, say, English, there can be no ambiguity
in how this describes the specification, because the computer will not allow it.
And unlike a specification in English, one in Haskell can be run directly
against test suites.

The flip side of this is that there's a great deal of code necessary to make
this actually runnable, much of which you, dear reader, will not care about in
the least. The purpose of this document is to serve as a reading guide for the
kind of reader that cares a great deal about the behavior mandated by the RISC-V
specification, but doesn't care much about how our project actually runs a
virtual RISC-V machine.

# Relevant source files

All source files are in the src/ directory. Within that, there are three
directories: Platform, Spec, and Utility. Utility entirely consists of
infrastructure code, and can be ignored entirely. Platform consists of specific
implementations of virtual machines (i.e. "how does setPC work?"). If you're
designing hardware, you already know how to set the program counter, and the way
you set the program counter is very different from how we set the program
counter; therefore, you probably don't care much about Platform either.

That leaves Spec, which has a bunch of things you might be interested in.

The first is Decode.hs, which lays out all of the instructions and their
parameters, and describes how instructions are extracted from words.

Next, there are a series of files that describe the execution of
different RISC-V instruction subsets. These are all files of the form Execute*.hs (ExecuteI.hs, ExecuteM.hs, ExecuteF.hs...).

Finally, there are a few files that concern the semantics of reading and writing
CSRs. CSR.hs lists the CSRs that exist, and the mapping between CSR number and
name (e.g. 0x300 is mstatus). CSRField.hs lists the CSR fields that exist, and
the behavior of those fields with regards to writing (WARL, WLRL, read-only, or
read-write). Most interestingly, CSRSpec.hs describes the actual behavior of
reading and writing CSRs, which is sometimes fairly involved (as compared to
writing an ordinary register). (Note that the implementation of CSR-related
instructions is in ExecuteCSR.hs, not CSRSpec.hs.)

There are several other files in Spec which bear mentioning: VirtualMemory.hs
describes how virtual memory works, Spec.hs describes how a cycle (of
fetch-execute-commit) works, and Machine.hs defines all of the basic operations
available (e.g. setPC) and a few helper functions. (These are generally more
Haskell-y and less friendly than Execute*.hs.)

# Things to ignore

Within a given file, there's still a little bit of code that won't matter to
you. You can consider it boilerplate and generally ignore it. Of course, this
holds more for more straightforward files.

Using ExecuteI.hs as an example, this code includes:

- The comment-ish thing on the very first line ("{-# LANGUAGE ... #-}").
- All of the module-related code following that ("module ...", "import ...").
- And the type signature for execute ("execute :: forall ...").
- And finally, integer conversion functions, which usually start with "from" or
  include "To": "fromIntegral", "fromImm", "regToInt8", etc. These are currently
  scattered throughout the code to deal with the type system, and are don't
  represent any actual bits changing (just being reinterpreted).

You don't need to understand the details of any of this code to understand the
semantics being described by the execute function.

This approach will work fine for things like Execute*.hs or CSRSpec.hs, where
you likely just want to know the behavior of executing a particular instruction
or reading/writing a particular CSR; it will work decreasingly well as the
complexity of the code increases (e.g. VirtualMemory.hs, Machine.hs).

# Example

It will be illustrative to look at the implementation of a particular
instruction. Take ADD, for example:

    execute (Add rd rs1 rs2) = do
      x <- getRegister rs1
      y <- getRegister rs2
      setRegister rd (x + y)

This is intended to be readable without any knowledge of Haskell. `execute (Add
rd rs1 rs2) =` says that we're describing the behavior of `execute` for ADD
instructions, which are parameratized by `rd`, `rs1`, and `rs2` - the
destination register and input registers. The names are arbitrary (they could
have been `foo`, `bar`, and `baz`), but we maintain the naming conventions of
the English specification whenever possible. `do` groups a series of actions
together. `x <- getRegister rs1` does what it looks like: it gets the value of
register `rs1` and assigns it to `x`. `setRegister rd (x + y)` sets register
`rd` to the value `x + y`, where `+` is built in to the language and, as you
would expect, performs addition.

# Misc

Most operators will look familiar, but a few bear explanation. `.|.` and `.&.`
perform bitwise OR and AND, respectively. The predicate "not equal" is
represented by `/=` (rather than `!=`, which you might be used to).

`bitSlice` returns a "slice" of bits from an integer. `bitSlice n a b` returns
the bits in `n` starting from bit `a` up to (but not including) bit `b`; it's
equivalent to `n[a:b-1]` in Verilog, or the notation of the spec.

`when <condition> <action>` performs an action only when `<condition>` is true;
otherwise, it does nothing. Sometimes we use helper functions that change
machine state, such as `raiseExceptionWithInfo`. If you like, you can take this
as a basic operation, but if you're curious or want to check your implementation
of exceptions, the source is in Machine.hs. Finally, if you see `getPlatform`
anywhere, that indicates we're about to rely on platform-specific behavior which
is not fully defined by the spec; you can think of this as asking the platform
what to do.

# Remarks

The inner workings of our semantics are fairly involved, but one of our primary
goals is having very readable code when it comes to the description of things
like instruction behavior. If you encounter something that seems impenetrable,
please let us know so that we can address it either in the code or the
documentation. Thanks!
