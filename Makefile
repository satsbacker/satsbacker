
all: satsbacker

GHCOPS=-j -Wall -isrc -O0

satsbacker: src/Main.hs
	ghc $(GHCOPS) --make $< -o $@

clean:
	rm -f satsbackers

check: tests
	./tests

tests: src/Satsbacker/Test/Main.hs
	ghc $(GHCOPS) --make $< -o $@

TAGS:
	hasktags --etags src

.PHONY: satsbacker TAGS tests
