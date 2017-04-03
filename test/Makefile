.PHONY: all clean

TESTS=add sub mul
HEX=$(addsuffix .hex,$(TESTS))
ELF2HEX=../elf2hex

RISCVCC=riscv32-unknown-linux-gnu-gcc -march=rv32im -DRISCV -nostdlib -nostartfiles

all: $(HEX)

$(ELF2HEX)/elf2hex:
	$(MAKE) -C $(ELF2HEX)

init.o: init.S
	$(RISCVCC) -c init.S

%.hex: $(ELF2HEX)/elf2hex %.c init.o link.ld
	$(RISCVCC) -c $*.c -o intermediate.o
	$(RISCVCC) -o $* -Tlink.ld intermediate.o init.o
	$(ELF2HEX)/elf2hex $*
	rm intermediate.o $*

clean:
	rm -f intermediate.o init.o $(TESTS) $(HEX)
