.PHONY: all tac type clean

all: TestTac TestType clean

tac: TestTac
type: TestType

TestTac: test/TestTac.hs haskell/*.hs
	ghc --make $^ -o $@

TestType: test/TestType.hs haskell/*.hs
	ghc --make $^ -o $@

clean:
	rm -f haskell/*.hi haskell/*.o test/*.hi test/*.o