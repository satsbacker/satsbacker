
all: satsbacker

satsbacker: src/Main.hs
	ghc -j -Wall -isrc -O2 --make $< -o $@

TAGS:
	hasktags --etags src

.PHONY: satsbacker TAGS
