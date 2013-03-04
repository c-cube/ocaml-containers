
INTERFACE_FILES = $(shell find -name '*.mli')
IMPLEMENTATION_FILES = $(shell find -name '*.ml')

TARGETS_LIB = containers.cmxa containers.cma
OPTIONS = -use-ocamlfind -lib sequence

all:
	ocamlbuild -use-ocamlfind $(TARGETS_LIB)

clean:
	ocamlbuild -clean

.PHONY: all clean


