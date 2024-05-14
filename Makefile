PACKAGES=containers,containers-data

all: build test

build:
	dune build @install -p $(PACKAGES)

test: build
	dune runtest --display=quiet --cache=disabled --no-buffer --force

clean:
	dune clean

doc:
	dune build @doc

examples:
	dune build examples/id_sexp.exe

format:
	@dune build $(DUNE_OPTS) @fmt --auto-promote

format-check:
	@dune build $(DUNE_OPTS) @fmt --display=quiet


VERSION=$(shell awk '/^version:/ {print $$2}' containers.opam)

update_next_tag:
	@echo "update version to $(VERSION)..."
	sed -i "s/NEXT_VERSION/$(VERSION)/g" $(wildcard src/**/*.ml) $(wildcard src/**/*.mli)
	sed -i "s/NEXT_RELEASE/$(VERSION)/g" $(wildcard src/**/*.ml) $(wildcard src/**/*.mli)

WATCH?=@src/check @tests/runtest
watch:
	@dune build $(WATCH) -w

reindent:
	@which ocp-indent || ( echo "require ocp-indent" ; exit 1 )
	@find src '(' -name '*.ml' -or -name '*.mli' ')' -type f -print0 | xargs -0 echo "reindenting: "
	@find src '(' -name '*.ml' -or -name '*.mli' ')' -type f -print0 | xargs -0 ocp-indent -i

.PHONY: all test clean build doc update_next_tag watch examples
