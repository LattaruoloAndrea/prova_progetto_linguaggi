.PHONY: demo clean

Main: Main.hs src/haskell/*.hs
	ghc --make -O2 $^ -o $@

demo: Main
	./Main --demo

clean:
	rm -f src/haskell/*.hi src/haskell/*.o src/test/*.hi src/test/*.o