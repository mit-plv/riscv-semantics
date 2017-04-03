.PHONY: elf2hex test clean all

all: Decode.hs elf2hex test

Decode.hs: gen.hs Decode_base.hs
	runhaskell gen.hs

elf2hex:
	$(MAKE) -C elf2hex

test:
	$(MAKE) -C test

clean:
	rm -f Decode.hs
	$(MAKE) -C elf2hex clean
	$(MAKE) -C test clean
