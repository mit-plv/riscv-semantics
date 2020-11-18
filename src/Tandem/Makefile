RISCV=/home/bthom/git/riscv-isa-sim/build
CXXFLAGS=-std=c++11 -I$(RISCV) -I/home/bthom/git/riscv-isa-sim/riscv -I/home/bthom/git/riscv-isa-sim/softfloat
Tandem: Tandem.o ElfFile.o
	$(CXX) -L$(RISCV) $^ -lriscv -o $@
