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
	scp -r containers_string.docdir/* cedeela.fr:~/simon/root/software/containers/string/
	scp -r containers_misc.docdir/* cedeela.fr:~/simon/root/software/containers/misc/

DONTTEST=myocamlbuild.ml setup.ml
QTESTABLE=$(filter-out $(DONTTEST), \
	$(wildcard core/*.ml) $(wildcard core/*.mli) \
	$(wildcard misc/*.ml) $(wildcard misc/*.mli) \
	$(wildcard string/*.ml) $(wildcard string/*.mli) \
	)

qtest-clean:
	@rm -rf qtest/

qtest-build: qtest-clean build
	@mkdir -p qtest
	@qtest extract -o qtest/qtest_all.ml $(QTESTABLE) 2> /dev/null
	@ocamlbuild $(OPTIONS) -pkg oUnit,QTest2Lib \
		-I core -I misc -I string \
		qtest/qtest_all.native

qtest: qtest-build
	@echo 
	./qtest_all.native

push-stable:
	git checkout stable
	git merge master -m 'merge from master'
	oasis setup
	git commit -a -m 'oasis files'
	git push origin
	git checkout master

clean-generated:
	rm **/*.{mldylib,mlpack,mllib} myocamlbuild.ml -f

run-test: build qtest-build
	./qtest_all.native
	./run_tests.native

test-all: run-test qtest

tags:
	otags *.ml *.mli

VERSION=$(shell awk '/^Version:/ {print $$2}' _oasis)

update_next_tag:
	@echo "update version to $(VERSION)..."
	sed -i "s/NEXT_VERSION/$(VERSION)/g" **/*.ml **/*.mli
	sed -i "s/NEXT_RELEASE/$(VERSION)/g" **/*.ml **/*.mli

.PHONY: examples push_doc tags qtest push-stable clean-generated
