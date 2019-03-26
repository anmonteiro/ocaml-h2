.PHONY: all build clean test

build:
	esy b dune build @install

all: build

test:
	esy b dune runtest --no-buffer

slow-test:
	esy b dune build @slowtests --no-buffer

install:
	esy b dune install

uninstall:
	esy b dune uninstall

clean:
	rm -rf _build *.install
	esy b dune clean
