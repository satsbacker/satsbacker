
all: satsbacker

satsbacker: src/Main.hs
	ghc -Wall -isrc -O2 --make $< -o $@

TAGS:
	hasktags --etags src

.PHONY: satsbacker TAGS
