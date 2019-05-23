Goat: Goat.hs GoatAST.hs GoatParser.hs GoatFormat.hs GoatCompiler.hs
	ghc -o Goat Goat.hs

test:
	./Goat -p GoatTest/a.gt
	./Goat -a GoatTest/sample.gt
	./Goat -p GoatTest/sample.gt

clean:
	rm -f *.o *.hi
	rm Goat
