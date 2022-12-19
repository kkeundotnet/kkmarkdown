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

.PHONY: check-doc
check-doc: doc
	@echo -n "Check if there is missing document... " 1>&2
	@if [ -z "$$(git status -s)" ]; then \
	  echo "SUCCEEDED" 1>&2; \
	else \
	  echo "FAILED" 1>&2; \
	  echo "[ERROR] Add a commit for missing documents." 1>&2; \
	  echo "$$(git status -s)" 1>&2; \
	  exit 1; \
	fi

.PHONY: clean
clean:
	dune clean

.PHONY: fmt
fmt:
	-dune build @fmt
	-dune promote
