.PHONY: all clean

SRCDIR=src
BUILDDIR=build

RISCVCC64=riscv-none-embed-gcc -march=rv64im -mabi=lp64 -static -nostdlib -nostartfiles -mcmodel=medany
RISCVCC32=riscv-none-embed-gcc -march=rv32im -mabi=ilp32 -static -nostdlib -nostartfiles -mcmodel=medany


gcd: gcd.S
	$(RISCVCC32) gcd.S -Tlink.ld -o gcd
clean:
	rm gcd

