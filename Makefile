
INTERFACE_FILES = $(shell find src -name '*.mli')
IMPLEMENTATION_FILES = $(shell find src -name '*.ml')

TARGETS_LIB = src/containers.cmxa src/containers.cma
OPTIONS = -use-ocamlfind -lib sequence

all:
	ocamlbuild -use-ocamlfind $(TARGETS_LIB)

clean:
	ocamlbuild -clean

.PHONY: all clean


