# GoatLexer: GoatLexer.x
# 	alex GoatLexer.x
# 	ghc -o GoatLexer GoatLexer.hs

Goat: GoatParser.hs GoatAST.hs
	ghc -o Goat GoatParser.hs

test:
	./Goat -p GoatTest/a.gt
	./Goat -a sample.gt
	./Goat -p sample.gt

clean:
	rm -f *.o *.hi
	rm Goat
