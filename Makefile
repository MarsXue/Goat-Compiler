Goat: GoatParser.hs GoatAST.hs
	ghc -o Goat GoatParser.hs

test:
	./Goat -p GoatTest/a.gt
	./Goat -a GoatTest/sample.gt
	./Goat -p GoatTest/sample.gt

clean:
	rm -f *.o *.hi
	rm Goat
