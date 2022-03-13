.PHONY: default
default: build

.PHONY: build
build:
	dune build

.PHONY: test
test:
	dune runtest -f

.PHONY: clean
clean:
	dune clean

.PHONY: fmt
fmt:
	-dune build @fmt
	-dune promote
