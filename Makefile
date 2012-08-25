GHC := ghc
BIN := snake
SRC := Main.hs Snake.hs

$(BIN): $(SRC)
	$(GHC) -o snake Main.hs

clean:
	rm -f $(BIN) *.hi *.o
