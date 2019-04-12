Goat: Goat.hs GoatAST.hs PrettyGoat.hs
	ghc Goat.hs

clean:
	rm -f *.o *.hi
	rm -f Goat 
