
INTERFACE_FILES = $(shell find -name '*.mli')
IMPLEMENTATION_FILES = $(shell find -name '*.ml')

TARGETS_LIB = containers.cmxa containers.cma
TARGET_THREAD_LIB = thread_containers.cmxa thread_containers.cma
TARGET_DOC = containers.docdir/index.html
EXAMPLES = examples/mem_size.native examples/collatz.native examples/crawl.native
OPTIONS = -use-ocamlfind

all: lib lib_thread doc

lib:
	ocamlbuild $(OPTIONS) $(TARGETS_LIB) $(TARGET_DOC)

lib_thread:
	ocamlbuild $(OPTIONS) $(TARGETS_LIB) $(TARGET_THREAD_LIB) $(TARGET_DOC)

doc:
	ocamlbuild $(OPTIONS) $(TARGET_DOC)

examples: all
	ocamlbuild $(OPTIONS) -I . $(EXAMPLES)

tests: lib
	ocamlbuild $(OPTIONS) -package oUnit -I . tests/run_tests.native

bench:
	ocamlbuild $(OPTIONS) -package bench -package unix -I . tests/benchs.native

clean:
	ocamlbuild -clean

tags:
	otags *.ml *.mli

.PHONY: all all_thread clean tests tags examples

