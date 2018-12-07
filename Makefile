repl:
	idris -i src -p contrib -p effects -p lightyear

build:
	idris --build aoc-2018.ipkg

run:
	./aoc2018

clean:
	idris --clean aoc-2018.ipkg
