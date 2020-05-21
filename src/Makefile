.PHONY: all clean

all: TestParser

TestParser: haskell/*.hs
	ghc --make $^ -o $@

clean:
	-rm -f haskell/*.hi haskell/*.o