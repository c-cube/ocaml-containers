
INTERFACE_FILES = $(shell find -name '*.mli')
IMPLEMENTATION_FILES = $(shell find -name '*.ml')

TARGETS_LIB = containers.cmxa containers.cma
TARGET_THREAD_LIB = thread_containers.cmxa thread_containers.cma
EXAMPLES = examples/mem_size.native examples/collatz.native
OPTIONS = -use-ocamlfind

all: lib lib_thread

lib:
	ocamlbuild $(OPTIONS) $(TARGETS_LIB)

lib_thread:
	ocamlbuild $(OPTIONS) $(TARGETS_LIB) $(TARGET_THREAD_LIB)

examples: all
	ocamlbuild $(OPTIONS) -I . $(EXAMPLES)

tests:
	ocamlbuild $(OPTIONS) -package oUnit -I . tests/run_tests.native

bench:
	ocamlbuild $(OPTIONS) -package bench -package unix -I . tests/benchs.native

clean:
	ocamlbuild -clean

tags:
	otags *.ml *.mli

.PHONY: all all_thread clean tests tags examples

