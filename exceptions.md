# RISC-V exceptions

Raising an exception involves two things:

- Modifying a bunch of state (namely numerous CSRs, potentially including
  `mtval`, `mepc`, `mcause`, etc.)
- 'Bailing out' of a cycle early, such that nothing that an instruction would do
  after an exception is raised actually happens.

Updating all the relevant state a little tedious, especially since determining
that right set of CSRs to modify (machine-mode or supervisor-mode) involves
consulting `medeleg` or `mideleg` and the current privilege mode. We provide the
helper functions `raiseException` and `raiseExceptionWithInfo` to alleviate
this.

Cutting the current cycle short is a bit involved. The details are covered in a
[later section](#the-nitty-gritty), but suffice it to say here that ending the
cycle early (via exception) is like returning from a function early: anything
prior to the exception actually happens, and nothing after the exception happens
at all. The next section elaborates on this.

# Practical consequences

In hardware implementations, it's often the case that the effects of an
instruction (such as writing to memory) don't actually go into effect or become
permanent until the instruction is 'committed' or 'retired', in order to support
out-of-order execution. This is not the case in our project; as soon as an
instruction issues the command `setRegister rd 42`, register `rd` really is set
to 42. If that instruction goes on to raise an exception, it will still be the
case that `rd` contains 42. But in most cases, RISC-V instructions don't have
their usual side effects when they raise exceptions, and so this behavior is
rarely desired. Avoiding it requires care when describing instructions.
Generally, this entails putting anything that could raise an exception up front,
before setting registers, writing to memory, etc.

So: what functions can raise an exception?

`raiseException` and `raiseExceptionWithInfo`, of course, and anything that call them:

- `translate` from `VirtualMemory.hs`
- `checkPermissions`, which is specific to `ExecuteCSR.hs`
- `setCSR` and `getCSR`.

`translate` tends to be simple enough to get right, as you'll usually call it
prior to performing a store anyway. Likewise, it makes sense that
`checkPermissions` always goes at the start of a CSR-related instruction, and
it's not used outside of `ExecuteCSR.hs`. Just remember to be careful when
calling `setCSR` and `getCSR`, and when raising an exception manually: unless
you intentionally want to change state in a way that survives an exception,
always make those calls *before* performing any writes or stores.

If you just want to know how to read or write code that involves RISC-V
exceptions, feel free to stop reading. The remainder of this document deals with
our implementation of RISC-V exceptions and how it might change in the future.

# The nitty gritty

tl;dr: exceptions work via the `MaybeT` monad transformer.

The interface for a RISC-V machine is specified by the `RiscvMachine` typeclass, which is defined in `Machine.hs`. Part of this definition looks like this:

    class (Monad p, MachineWidth t) => RiscvMachine p t | p -> t where
      getRegister :: Register -> p t
      setRegister :: Register -> t -> p ()
      ...
      endCycle :: forall z. p z
      ...

The first line indicates that essentially one thing parameterizes a
`RiscvMachine`: some monad `p` (which also implies some `MachineWidth t`). A
particular instance of the `RiscvMachine` typeclass can be defined by specifying
a specific `Monad` and `MachineWidth`, and then defining functions
(`getRegister`, `setRegister`, etc.) that satisfy the type signatures given in
the typeclass.

As an example, I might have a monad called `MState` that models the state of a
simple machine (with registers, memory, and so on). I can define an instance of
`RiscvMachine` using this monad like `instance RiscvMachine MState Int64 where
...`, in which case `p` is `MState` and `t` is `Int64`. Substituting these in to
the typeclass, the definition of `getRegister` I provide had better have the
type signature `Register -> MState Int64`, meaning a function that takes a
`Register` and returns an `Int64` wrapped in the `MState` monad.

With this in mind, the signature for `endCycle` might appear slightly
mysterious. For all types `z`, `endCycle` returns a `z` wrapped in `MState`.
So... for `z` as an `Int`, it returns `MState Int`. For `z` as a `String`, it
returns an `MState String`. This may seem a little strange (an `Int` isn't a
`String`, so a function returning an `Int` isn't a function that returns a
`String`), but looking at the implementation of `endCycle` should clear things
up.

Conspicuously, an implementation of `endCycle` is missing from `Minimal64`, but
there is one given in `Machine.hs`:

    instance (RiscvMachine p t) => RiscvMachine (MaybeT p) t where
      getRegister r = lift (getRegister r)
      setRegister r v = lift (setRegister r v)
      ...
      endCycle = MaybeT (return Nothing)
      ...

This instance demonstrates something powerful about our spec: composability.
This instance takes some existing instance of a `RiscvMachine` and builds on it,
adding in the functionality of the `Maybe` monad. If you're unfamiliar, in the
`Maybe` monad, functions can return something or they can return `Nothing`, a
special value. This is sort of like how methods in Java can always return some
valid object or `null`. But in the `Maybe` monad, a computation halts as soon as
something returns `Nothing`, and this precisely captures the 'early-return'
behavior we want upon encountering an exception.

So, `endCycle` returns `Nothing` (halting the computation), and all other
functions do what they would normally do, just 'lifted' into the new
Maybe-infused form of the monad. This matches the type signature for `endCycle`,
too: `Nothing` could be a `Maybe Int`, a `Maybe String`, or a `Maybe` anything
else.

We use this kind of augmentation repeatedly in our code, sometimes to encode the
semantics of a core feature (like RISC-V exceptions), and sometimes to add neat
features (like memory-mapped I/O) to existing `RiscvMachine` instances.

# Future plans

Thus far, we believe being careful when ordering operations is sufficient to
handle everything in the spec. However, if some hypothetical extension comes up
that makes correct behavior with regard to exceptions more complex, it might be
useful to have some construction that approximates the hardware practice of
buffering state changes before they are finalized by a commit. That is, "do
everything in this block, unless something raises an exception, in which case
don't do any of it".

This could look something like:

    execute (MyFancyInstruction rd rs1 rs2) = do
      -- Block 1
      transact do
        asdf <- getRegister rs2
        setRegister rs1 (asdf * 2)
        setCSR MyFancyCSR (asdf + 1)
      -- Block 2
      transact do
        setRegister rs2 42
        setCSR MyOtherCSR 117

In this example, either everything in block 1 happens, or `setCSR` raises an
exception and we bail from the execution (and nothing happens to `rs1`). Then,
if we didn't bail, either everything in block 2 happens, or `setCSR` raises an
exception and we bail from the execution: in this case, nothing in block 2
happens (`rs2` isn't changed), but everything in block 1 already happened.

We believe this should be relatively straightforward to implement by
constructing a 'phony' instance of `RiscvMachine`, but haven't found it
necessary yet.
