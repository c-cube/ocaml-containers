# OASIS_START
# DO NOT EDIT (digest: a3c674b4239234cbbe53afe090018954)

SETUP = ocaml setup.ml

build: setup.data
	$(SETUP) -build $(BUILDFLAGS)

doc: setup.data build
	$(SETUP) -doc $(DOCFLAGS)

test: setup.data build
	$(SETUP) -test $(TESTFLAGS)

all:
	$(SETUP) -all $(ALLFLAGS)

install: setup.data
	$(SETUP) -install $(INSTALLFLAGS)

uninstall: setup.data
	$(SETUP) -uninstall $(UNINSTALLFLAGS)

reinstall: setup.data
	$(SETUP) -reinstall $(REINSTALLFLAGS)

clean:
	$(SETUP) -clean $(CLEANFLAGS)

distclean:
	$(SETUP) -distclean $(DISTCLEANFLAGS)

setup.data:
	$(SETUP) -configure $(CONFIGUREFLAGS)

configure:
	$(SETUP) -configure $(CONFIGUREFLAGS)

.PHONY: build doc test all install uninstall reinstall clean distclean configure

# OASIS_STOP

EXAMPLES = examples/mem_size.native examples/collatz.native \
	examples/bencode_write.native # examples/crawl.native
OPTIONS = -use-ocamlfind

examples: all
	ocamlbuild $(OPTIONS) -package unix -I . $(EXAMPLES)

push_doc: doc
	scp -r containers.docdir/* cedeela.fr:~/simon/root/software/containers/

DONTTEST=myocamlbuild.ml setup.ml
QTESTABLE=$(filter-out $(DONTTEST), \
	$(wildcard core/*.ml) $(wildcard core/*.mli) \
	$(wildcard misc/*.ml) $(wildcard misc/*.mli) \
	$(wildcard string/*.ml) $(wildcard string/*.mli) \
	)

qtest-clean:
	@rm -rf qtest/

qtest: qtest-clean build
	@mkdir -p qtest
	@qtest extract -o qtest/qtest_all.ml $(QTESTABLE) 2> /dev/null
	@ocamlbuild $(OPTIONS) -pkg oUnit,QTest2Lib \
		-I core -I misc -I string \
		qtest/qtest_all.native
	@echo
	./qtest_all.native

push-stable: all
	git checkout stable && git merge master && oasis setup && \
	git commit -a 'oasis'

test-all: test qtest

tags:
	otags *.ml *.mli

.PHONY: examples push_doc tags qtest
