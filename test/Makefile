.PHONY: all clean

SOURCES=$(wildcard tests/*.c)
TESTS=$(basename $(SOURCES))
HEX32=$(addsuffix 32.hex,$(TESTS))
HEX64=$(addsuffix 64.hex,$(TESTS))
ELF2HEX=../elf2hex

RISCVCC64=riscv64-unknown-linux-gnu-gcc -march=RV64IMFD -DRISCV -nostdlib -nostartfiles
RISCVCC32=riscv32-unknown-elf-gcc -march=rv32im -DRISCV -nostdlib -nostartfiles


all: $(HEX32) $(HEX64)

$(ELF2HEX)/elf2hex:
	$(MAKE) -C $(ELF2HEX)

init32.o: init.S
	$(RISCVCC32) -c init.S -o init32.o

init64.o: init.S
	$(RISCVCC64) -c init.S -o init64.o

mmio32.o: mmio.c
	$(RISCVCC32) -c mmio.c -o mmio32.o

mmio64.o: mmio.c
	$(RISCVCC64) -c mmio.c -o mmio64.o

%32.hex: $(ELF2HEX)/elf2hex %.c init32.o mmio32.o link.ld
	$(RISCVCC32) -c $*.c -o intermediate32.o
	$(RISCVCC32) -o $*32 -Tmmio32.ld intermediate32.o init32.o mmio32.o
	$(ELF2HEX)/elf2hex $*32
	rm intermediate32.o $*32

%64.hex: $(ELF2HEX)/elf2hex %.c init64.o mmio64.o link.ld
	$(RISCVCC64) -c $*.c -o intermediate64.o
	$(RISCVCC64) -o $*64 -Tmmio64.ld intermediate64.o init64.o mmio64.o
	$(ELF2HEX)/elf2hex $*64
	rm intermediate64.o $*64


clean:
	rm -f intermediate32.o init32.o mmio32.o $(TESTS) $(HEX32)
	rm -f intermediate64.o init64.o mmio64.o $(TESTS) $(HEX64)
