RISCV=/home/bthom/git/multiriscv
CXXFLAGS=-std=c++11 -I$(RISCV)/include/spike
Tandem: Tandem.o ElfFile.o
	$(CXX) -L$(RISCV)/lib $^ -lriscv -o $@
