PROMOTE=$(if $(shell ocamlc -version |grep '4\.0[012]\.[0-9][0-9]*'), \
	      --ignore-promoted-rules, )

all: build test

build:
	dune build $(PROMOTE) @install

test: build
	dune runtest --no-buffer --force

clean:
	dune clean

doc:
	dune build $(PROMOTE) @doc

BENCH_TARGETS=run_benchs.exe run_bench_hash.exe

benchs:
	dune build $(PROMOTE) $(addprefix benchs/, $(BENCH_TARGETS))

examples:
	dune build examples/id_sexp.exe

VERSION=$(shell awk '/^version:/ {print $$2}' containers.opam)

update_next_tag:
	@echo "update version to $(VERSION)..."
	sed -i "s/NEXT_VERSION/$(VERSION)/g" $(wildcard src/**/*.ml) $(wildcard src/**/*.mli)
	sed -i "s/NEXT_RELEASE/$(VERSION)/g" $(wildcard src/**/*.ml) $(wildcard src/**/*.mli)

release: update_next_tag
	@echo "release version $(VERSION)..."
	git tag -f $(VERSION) ; git push origin :$(VERSION) ; git push origin $(VERSION)
	opam publish prepare https://github.com/c-cube/qcheck/archive/$(VERSION).tar.gz
	@echo "review the release, then type 'opam publish submit qcheck.$(VERSION)/'"

watch:
	while find src/ benchs/ -print0 | xargs -0 inotifywait -e delete_self -e modify ; do \
		echo "============ at `date` ==========" ; \
		make all; \
	done

reindent:
	@which ocp-indent || ( echo "require ocp-indent" ; exit 1 )
	@find src '(' -name '*.ml' -or -name '*.mli' ')' -type f -print0 | xargs -0 echo "reindenting: "
	@find src '(' -name '*.ml' -or -name '*.mli' ')' -type f -print0 | xargs -0 ocp-indent -i

.PHONY: all benchs test clean build doc update_next_tag watch
