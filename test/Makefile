.PHONY: all clean

SOURCES=$(wildcard tests/*.c)
TESTS=$(basename $(SOURCES))
HEX=$(addsuffix .hex,$(TESTS))
ELF2HEX=../elf2hex

RISCVCC=riscv32-unknown-linux-gnu-gcc -march=rv32im -DRISCV -nostdlib -nostartfiles

all: $(HEX)

$(ELF2HEX)/elf2hex:
	$(MAKE) -C $(ELF2HEX)

init.o: init.S
	$(RISCVCC) -c init.S

mmio.o: mmio.c
	$(RISCVCC) -c mmio.c

%.hex: $(ELF2HEX)/elf2hex %.c init.o mmio.o link.ld
	$(RISCVCC) -c $*.c -o intermediate.o
	$(RISCVCC) -o $* -Tmmio.ld intermediate.o init.o mmio.o
	$(ELF2HEX)/elf2hex $*
	rm intermediate.o $*

clean:
	rm -f intermediate.o init.o mmio.o $(TESTS) $(HEX)
