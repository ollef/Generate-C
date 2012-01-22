all:
	cabal install
	cabal haddock
clean:
	$(RM) -r dist
