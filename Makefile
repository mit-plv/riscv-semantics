.PHONY: elf2hex test riscv-tests clean all

all: elf2hex test riscv-tests

elf2hex:
	$(MAKE) -C elf2hex

test:
	$(MAKE) -C test

riscv-tests:
	RISCV_PREFIX=riscv-none-embed- $(MAKE) -C riscv-tests/isa rv64mi rv64si rv64ui

clean:
	rm -rf .stack-work/
	$(MAKE) -C elf2hex clean
	$(MAKE) -C test clean
	$(MAKE) -C riscv-tests/isa clean
