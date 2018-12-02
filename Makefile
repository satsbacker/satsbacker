
all: bitsbacker

bitsbacker: src/Main.hs
	ghc --make $< -o $@

.PHONY: bitsbacker
