
INTERFACE_FILES = $(shell find -name '*.mli')
IMPLEMENTATION_FILES = $(shell find -name '*.ml')

TARGETS_LIB = containers.cmxa containers.cma
TARGETS_DOC = containers.docdir/index.html
EXAMPLES = examples/mem_size.native examples/collatz.native examples/crawl.native

OPTIONS = -use-ocamlfind

ENABLE_THREAD ?= yes
ENABLE_LWT ?= yes

ifeq ($(ENABLE_THREAD), yes)
	OPTIONS += -tag thread
	TARGETS_LIB += thread_containers.cmxa thread_containers.cma
	TARGETS_DOC += thread_containers.docdir/index.html
endif
ifeq ($(ENABLE_LWT), yes)
	OPTIONS += -package lwt -package lwt.unix
	TARGETS_LIB += lwt_containers.cmxa lwt_containers.cma
	TARGETS_DOC += lwt_containers.docdir/index.html
endif

all: lib

lib:
	ocamlbuild $(OPTIONS) $(TARGETS_LIB) $(TARGETS_DOC)

doc:
	ocamlbuild $(OPTIONS) $(TARGETS_DOC)

examples: all
	ocamlbuild $(OPTIONS) -I . $(EXAMPLES)

tests: lib
	ocamlbuild $(OPTIONS) -package oUnit -I . tests/run_tests.native

bench:
	ocamlbuild $(OPTIONS) -package bench -package unix -I . tests/benchs.native

push_doc: doc
	scp -r containers.docdir/* cedeela.fr:~/simon/root/software/containers/

clean:
	ocamlbuild -clean

tags:
	otags *.ml *.mli

.PHONY: all clean tests tags examples

