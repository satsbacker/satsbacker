
all: satsbacker

satsbacker: src/Main.hs
	ghc -Wall -O2 --make $< -o $@

TAGS:
	hasktags --etags src

.PHONY: satsbacker TAGS
