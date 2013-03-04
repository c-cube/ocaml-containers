
INTERFACE_FILES = $(shell find -name '*.mli')
IMPLEMENTATION_FILES = $(shell find -name '*.ml')

TARGETS_LIB = containers.cmxa containers.cma
OPTIONS = -use-ocamlfind -package sequence

all:
	ocamlbuild $(OPTIONS) $(TARGETS_LIB)

tests:
	ocamlbuild $(OPTIONS) -package oUnit -I . tests/tests.native

bench:
	ocamlbuild $(OPTIONS) -package bench -package unix -I . tests/benchs.native

clean:
	ocamlbuild -clean

.PHONY: all clean tests

