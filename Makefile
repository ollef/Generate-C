all:
	cabal install
	cabal haddock
clean:
	rm -r dist
