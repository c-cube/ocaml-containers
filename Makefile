# OASIS_START
# DO NOT EDIT (digest: 9a60866e2fa295c5e33a3fe33b8f3a32)

SETUP = ./setup.exe

build: setup.data $(SETUP)
	$(SETUP) -build $(BUILDFLAGS)

doc: setup.data $(SETUP) build
	$(SETUP) -doc $(DOCFLAGS)

test: setup.data $(SETUP) build
	$(SETUP) -test $(TESTFLAGS)

all: $(SETUP)
	$(SETUP) -all $(ALLFLAGS)

install: setup.data $(SETUP)
	$(SETUP) -install $(INSTALLFLAGS)

uninstall: setup.data $(SETUP)
	$(SETUP) -uninstall $(UNINSTALLFLAGS)

reinstall: setup.data $(SETUP)
	$(SETUP) -reinstall $(REINSTALLFLAGS)

clean: $(SETUP)
	$(SETUP) -clean $(CLEANFLAGS)

distclean: $(SETUP)
	$(SETUP) -distclean $(DISTCLEANFLAGS)
	$(RM) $(SETUP)

setup.data: $(SETUP)
	$(SETUP) -configure $(CONFIGUREFLAGS)

configure: $(SETUP)
	$(SETUP) -configure $(CONFIGUREFLAGS)

setup.exe: setup.ml
	ocamlfind ocamlopt -o $@ -linkpkg -package oasis.dynrun $< || ocamlfind ocamlc -o $@ -linkpkg -package oasis.dynrun $< || true
	$(RM) setup.cmi setup.cmo setup.cmx setup.o

.PHONY: build doc test all install uninstall reinstall clean distclean configure

# OASIS_STOP

EXAMPLES = examples/mem_size.native examples/collatz.native \
	examples/bencode_write.native # examples/crawl.native
OPTIONS = -use-ocamlfind -I _build

examples: all
	ocamlbuild $(OPTIONS) -package unix -I . $(EXAMPLES)

push_doc: doc
	scp -r containers.docdir/* cedeela.fr:~/simon/root/software/containers/
	scp -r containers_string.docdir/* cedeela.fr:~/simon/root/software/containers/string/
	scp -r containers_advanced.docdir/* cedeela.fr:~/simon/root/software/containers/advanced
	scp -r containers_misc.docdir/* cedeela.fr:~/simon/root/software/containers/misc/

DONTTEST=myocamlbuild.ml setup.ml $(wildcard **/*.cppo*)
QTESTABLE=$(filter-out $(DONTTEST), \
	$(wildcard core/*.ml) $(wildcard core/*.mli) \
	$(wildcard core/*.cppo.ml) $(wildcard core/*.cppo.mli) \
	$(wildcard misc/*.ml) $(wildcard misc/*.mli) \
	$(wildcard string/*.ml) $(wildcard string/*.mli) \
	)

qtest-clean:
	@rm -rf qtest/

QTEST_PREAMBLE='open CCFun;; '

#qtest-build: qtest-clean build
#	@mkdir -p qtest
#	@qtest extract --preamble $(QTEST_PREAMBLE) \
#		-o qtest/qtest_all.ml \
#		$(QTESTABLE) 2> /dev/null
#	@ocamlbuild $(OPTIONS) -pkg oUnit,QTest2Lib,ocamlbuildlib \
#		-I core -I misc -I string \
#		qtest/qtest_all.native

qtest-gen: qtest-clean
	@mkdir -p qtest
	@qtest extract --preamble $(QTEST_PREAMBLE) \
		-o qtest/run_qtest.cppo.ml \
		$(QTESTABLE) 2> /dev/null

push-stable:
	git checkout stable
	git merge master -m 'merge from master'
	oasis setup
	git commit -a -m 'oasis files'
	git push origin
	git checkout master

clean-generated:
	rm **/*.{mldylib,mlpack,mllib} myocamlbuild.ml -f

run-test: build
	./run_qtest.native
	./run_tests.native

test-all: run-test

tags:
	otags *.ml *.mli

VERSION=$(shell awk '/^Version:/ {print $$2}' _oasis)

update_next_tag:
	@echo "update version to $(VERSION)..."
	sed -i "s/NEXT_VERSION/$(VERSION)/g" **/*.ml **/*.mli
	sed -i "s/NEXT_RELEASE/$(VERSION)/g" **/*.ml **/*.mli

udpate_sequence:
	git subtree pull --prefix sequence sequence stable --squash

.PHONY: examples push_doc tags qtest update_sequence update_next_tag
