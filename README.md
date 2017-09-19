# riscv-semantic


'''
make
'''

To run (interpret) the simulator on a compiled program:

'''
runhaskell Run.hs test/tests/thuemorse.hex
'''

to compile the simulator (better performances):

'''
ghc --make -O2 -optc-O3 -funfolding-use-threshold=30 Run.hs
'''

Then you can run a program with:
'''
./Run test/tests/thuemorse.hex
'''
