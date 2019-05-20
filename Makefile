Goat: Goat.hs GoatAST.hs PrettyGoat.hs GoatParser.hs SymTable.hs Analyse.hs CodePrint.hs
	ghc Goat.hs

clean:
	rm -f *.o *.hi
	rm -f Goat 
