# Frontend to dune.

.PHONY: default build js install uninstall test clean

default: build js

build:
	dune build

js:
	dune build src/js/kkmarkdown.js

test:
	dune runtest -f

install:
	dune install

uninstall:
	dune uninstall

clean:
	dune clean
# Optionally, remove all files/folders ignored by git as defined
# in .gitignore (-X).
	git clean -dfXq
