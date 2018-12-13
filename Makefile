
all: bitsbacker

bitsbacker: src/Main.hs
	ghc -Wall -O2 --make $< -o $@

TAGS:
	hasktags --etags src

.PHONY: bitsbacker
