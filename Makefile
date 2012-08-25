GHC := ghc
BIN := snake
SRC := $(shell find . -name '*.hs')

$(BIN): $(SRC)
	$(GHC) -o $@ $^

clean:
	find . -name snake -o -name '*.hi' -o -name '*.o' | xargs rm -fv
