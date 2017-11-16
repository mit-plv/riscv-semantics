.PHONY: elf2hex test clean all

all: elf2hex test

elf2hex:
	$(MAKE) -C elf2hex

test:
	$(MAKE) -C test

clean:
	rm -rf .stack-work/
	$(MAKE) -C elf2hex clean
	$(MAKE) -C test clean
	rm src/*.o src/*.hi
