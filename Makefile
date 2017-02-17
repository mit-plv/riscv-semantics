Decode.hs: gen.hs Decode_base.hs
	runhaskell gen.hs

all: Decode.hs

clean:
	rm Decode.hs
