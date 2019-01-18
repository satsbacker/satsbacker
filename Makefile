
all: satsbacker

satsbacker: src/Main.hs
	ghc -j -Wall -isrc -O --make $< -o $@

TAGS:
	hasktags --etags src

.PHONY: satsbacker TAGS
