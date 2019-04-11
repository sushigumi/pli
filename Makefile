Goat: Goat.hs GoatAST.hs
	ghc Goat.hs

GoatLexer: GoatLexer.x
	alex GoatLexer.x
	ghc -o GoatLexer GoatLexer.hs

clean:
	rm -f *.o *.hi
	rm -f Goat GoatLexer GoatLexer.hs
