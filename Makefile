GoatLexer: GoatLexer.x
	alex GoatLexer.x
	ghc -o GoatLexer GoatLexer.hs

clean:
	rm -f *.o *.hi
	rm -f KidParser GoatLexer GoatLexer.hs