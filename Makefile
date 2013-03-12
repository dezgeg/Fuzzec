.PHONY: all clean

all:
	cabal build

clean:
	@find . -iname '*.hi' -print0 | xargs -0 rm -v
	@find . -iname '*.o'  -print0 | xargs -0 rm -v
	rm -f CppFuzz
