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
	scripts/gen_syntax_doc.sh
	dune build @doc
	rm -rf docs && cp -r _build/default/_doc/_html docs

.PHONY: clean
clean:
	dune clean

.PHONY: fmt
fmt:
	-dune build @fmt
	-dune promote
