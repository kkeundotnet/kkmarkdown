.PHONY: default
default: build

.PHONY: build
build:
	dune build @install

.PHONY: test
test:
	dune runtest -f

.PHONY: doc
doc:
	dune build @doc

.PHONY: clean
clean:
	dune clean

.PHONY: fmt
fmt:
	-dune build @fmt
	-dune promote
