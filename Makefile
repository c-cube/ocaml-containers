
all: build test

build:
	jbuilder build @install

test: build
	jbuilder runtest --no-buffer

clean:
	jbuilder clean

doc:
	jbuilder build @doc

BENCH_TARGETS=run_benchs.exe run_bench_hash.exe

benchs:
	jbuilder build $(addprefix benchs/, $(BENCH_TARGETS))

examples:
	jbuilder build examples/id_sexp.exe

VERSION=$(shell awk '/^version:/ {print $$2}' containers.opam)

update_next_tag:
	@echo "update version to $(VERSION)..."
	sed -i "s/NEXT_VERSION/$(VERSION)/g" $(wildcard src/**/*.ml) $(wildcard src/**/*.mli)
	sed -i "s/NEXT_RELEASE/$(VERSION)/g" $(wildcard src/**/*.ml) $(wildcard src/**/*.mli)

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
