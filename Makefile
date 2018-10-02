.PHONY: elf2hex test softfloat-hs riscv-tests clean all

all: elf2hex test softfloat-hs riscv-tests

elf2hex:
	$(MAKE) -C elf2hex

test:
	$(MAKE) -C test

softfloat-hs:
	TMPDIR=$$(mktemp -d); \
	echo '$$@' > $$TMPDIR/sudo; \
	chmod +x $$TMPDIR/sudo; \
	PATH=$$TMPDIR:$$PATH $(MAKE) -C softfloat-hs INCLUDEPATH=include; \
	rm -r $$TMPDIR

riscv-tests:
	RISCV_PREFIX=riscv-none-embed- $(MAKE) -C riscv-tests/isa rv64mi rv64si rv64ui rv64uf

clean:
	rm -rf .stack-work/
	$(MAKE) -C elf2hex clean
	$(MAKE) -C test clean
	$(MAKE) -C softfloat-hs clean INCLUDEPATH=include
	$(MAKE) -C riscv-tests/isa clean
