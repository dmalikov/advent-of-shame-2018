repl:
	idris -i src -p contrib -p effects

build:
	idris --build aoc-2018.ipkg

run: build
	./aoc2018

clean:
	idris --clean aoc-2018.ipkg
