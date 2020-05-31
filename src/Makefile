.PHONY: all clean debugger

all: TestParser

debugger: TestType

TestType: test/*.hs
	ghc --make $^ -o $@

TestParser: haskell/*.hs
	ghc --make $^ -o $@

clean:
	rm -f haskell/*.hi haskell/*.o test/*.hi test/*.o