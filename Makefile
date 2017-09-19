.PHONY: elf2hex test clean all

all: Decode.hs elf2hex test

Decode.hs: src/gen.hs src/Decode_base.hs
	cd src; runhaskell gen.hs

elf2hex:
	$(MAKE) -C elf2hex

test:
	$(MAKE) -C test

clean:
	rm -f src/Decode.hs
	$(MAKE) -C elf2hex clean
	$(MAKE) -C test clean
	rm src/*.o src/*.hi
