Goat: Goat.hs GoatAST.hs PrettyGoat.hs CodeGen.hs
	ghc Goat.hs

clean:
	rm -f *.o *.hi
	rm -f Goat 
