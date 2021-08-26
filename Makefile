PACKAGES=containers,containers-data,containers-thread

all: build test

build:
	dune build @install -p $(PACKAGES)

test: build
	dune runtest --cache=disabled --no-buffer --force

clean:
	dune clean

doc:
	dune build @doc

BENCH_TARGETS=run_benchs.exe run_bench_hash.exe

benchs:
	dune build $(addprefix benchs/, $(BENCH_TARGETS)) --profile=release
	@for i in $(BENCH_TARGETS) ; do ln -sf _build/default/benchs/$$i ; done

examples:
	dune build examples/id_sexp.exe

VERSION=$(shell awk '/^version:/ {print $$2}' containers.opam)

update_next_tag:
	@echo "update version to $(VERSION)..."
	sed -i "s/NEXT_VERSION/$(VERSION)/g" $(wildcard src/**/*.ml) $(wildcard src/**/*.mli)
	sed -i "s/NEXT_RELEASE/$(VERSION)/g" $(wildcard src/**/*.ml) $(wildcard src/**/*.mli)

WATCH?=@all
watch:
	@dune build $(WATCH) -w

reindent:
	@which ocp-indent || ( echo "require ocp-indent" ; exit 1 )
	@find src '(' -name '*.ml' -or -name '*.mli' ')' -type f -print0 | xargs -0 echo "reindenting: "
	@find src '(' -name '*.ml' -or -name '*.mli' ')' -type f -print0 | xargs -0 ocp-indent -i

.PHONY: all benchs test clean build doc update_next_tag watch examples
