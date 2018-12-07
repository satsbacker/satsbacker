
all: bitsbacker

bitsbacker: src/Main.hs
	ghc -Wall -O2 --make $< -o $@

.PHONY: bitsbacker
