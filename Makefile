# GoatLexer: GoatLexer.x
# 	alex GoatLexer.x
# 	ghc -o GoatLexer GoatLexer.hs

GoatParser: GoatParser.hs GoatAST.hs
	ghc -o GoatParser GoatParser.hs

test:
	./GoatParser GoatTest/a.gt
	# ./GoatParser sample.gt

clean:
	rm -f *.o *.hi
	rm GoatParser
