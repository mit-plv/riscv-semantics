.PHONY: all clean

SRCDIR=src
BUILDDIR=build

SOURCES=$(notdir $(wildcard $(SRCDIR)/*.c))
TESTS=$(basename $(SOURCES))
ELF=$(addprefix build/,$(TESTS))
ELF32=$(addsuffix 32,$(ELF))
ELF64=$(addsuffix 64,$(ELF))
HEX32=$(addsuffix .hex,$(ELF32))
HEX64=$(addsuffix .hex,$(ELF64))
ELF2HEX=../elf2hex

RISCVCC64=riscv-none-embed-gcc -march=rv64im -mabi=lp64 -static -nostdlib -nostartfiles -mcmodel=medany
RISCVCC32=riscv-none-embed-gcc -march=rv32im -mabi=ilp32 -static -nostdlib -nostartfiles -mcmodel=medany

all: $(HEX32) $(HEX64)
all64: $(HEX64)

$(ELF2HEX)/elf2hex:
	$(MAKE) -C $(ELF2HEX)

mp: mp.S
	$(RISCVCC64) -c mp.S -o $(BUILDDIR)/mp64.o
	$(RISCVCC64) -o $(BUILDDIR)/mp64 $(BUILDDIR)/mp64.o
sb: sbData.S
	$(RISCVCC64) -c sbData.S -o $(BUILDDIR)/sbData64.o
	$(RISCVCC64) -o $(BUILDDIR)/sbData64 $(BUILDDIR)/sbData64.o


init32.o: init.S
	$(RISCVCC32) -c init.S -o init32.o

init64.o: init.S
	$(RISCVCC64) -c init.S -o init64.o

mmio32.o: mmio.c
	$(RISCVCC32) -c mmio.c -o mmio32.o

mmio64.o: mmio.c
	$(RISCVCC64) -c mmio.c -o mmio64.o

$(BUILDDIR)/%32.hex: $(ELF2HEX)/elf2hex $(SRCDIR)/%.c init32.o mmio32.o mmio.ld
	mkdir -p $(BUILDDIR)
	$(RISCVCC32) -c $(SRCDIR)/$*.c -o intermediate32.o
	$(RISCVCC32) -o $(BUILDDIR)/$*32 -Tmmio.ld intermediate32.o init32.o mmio32.o
	$(ELF2HEX)/elf2hex $(BUILDDIR)/$*32 0 16G $(BUILDDIR)/$*32.hex
	rm intermediate32.o

$(BUILDDIR)/%64.hex: $(ELF2HEX)/elf2hex $(SRCDIR)/%.c init64.o mmio64.o mmio.ld
	mkdir -p $(BUILDDIR)
	$(RISCVCC64) -c $(SRCDIR)/$*.c -o intermediate64.o
	$(RISCVCC64) -o $(BUILDDIR)/$*64 -Tmmio.ld intermediate64.o init64.o mmio64.o
	$(ELF2HEX)/elf2hex $(BUILDDIR)/$*64 0 16G $(BUILDDIR)/$*64.hex
	rm intermediate64.o


clean:
	rm -f intermediate32.o init32.o mmio32.o
	rm -f intermediate64.o init64.o mmio64.o
	rm -rf build
