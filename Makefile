.PHONY: elf2hex clean all

all: Decode.hs elf2hex

Decode.hs: gen.hs Decode_base.hs
	runhaskell gen.hs

elf2hex:
	$(MAKE) -C elf2hex all

clean:
	rm -f Decode.hs;  $(MAKE) -C elf2hex clean
