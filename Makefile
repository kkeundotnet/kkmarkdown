.PHONY: default
default: build kkmarkdown.js

.PHONY: build
build:
	dune build

.PHONY: test
test:
	dune runtest -f

.PHONY: install
install:
	dune install

.PHONY: uninstall
uninstall:
	dune uninstall

.PHONY: clean
clean:
	dune clean

kkmarkdown.js: build
	cp -f _build/default/src/js/kkmarkdown.js .
