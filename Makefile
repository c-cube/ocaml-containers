# OASIS_START
# DO NOT EDIT (digest: 4c293511860bb966e727ba6f0ecc8197)

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

setup.exe: setup.ml _oasis
	ocamlfind ocamlopt -o $@ -linkpkg -package oasis.dynrun setup.ml || ocamlfind ocamlc -o $@ -linkpkg -package oasis.dynrun setup.ml || true
	$(RM) setup.cmi setup.cmo setup.cmx setup.o

.PHONY: build doc test all install uninstall reinstall clean distclean configure

# OASIS_STOP

EXAMPLES = examples/mem_size.native examples/collatz.native \
	examples/bencode_write.native # examples/crawl.native
OPTIONS = -use-ocamlfind -I _build

examples: all
	ocamlbuild $(OPTIONS) -package unix -I . $(EXAMPLES)

push_doc: doc
	rsync -tavu containers.docdir/* cedeela.fr:~/simon/root/software/containers/

push_doc_gh: doc
	git checkout gh-pages && \
	  rm -rf dev/ && \
	  mkdir -p dev && \
	  cp -r containers.docdir/* dev/ && \
	  git add --all dev

DONTTEST=myocamlbuild.ml setup.ml $(wildcard src/**/*.cppo.*) $(wildcard src/**/*Labels*)
QTESTABLE=$(filter-out $(DONTTEST), \
	$(wildcard src/core/*.ml) \
	$(wildcard src/core/*.mli) \
	$(wildcard src/data/*.ml) \
	$(wildcard src/data/*.mli) \
	$(wildcard src/string/*.ml) \
	$(wildcard src/string/*.mli) \
	$(wildcard src/unix/*.ml) \
	$(wildcard src/unix/*.mli) \
	$(wildcard src/sexp/*.ml) \
	$(wildcard src/sexp/*.mli) \
	$(wildcard src/iter/*.ml) \
	$(wildcard src/iter/*.mli) \
	$(wildcard src/bigarray/*.ml) \
	$(wildcard src/bigarray/*.mli) \
	$(wildcard src/threads/*.ml) \
	$(wildcard src/threads/*.mli) \
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

qtest-gen:
	@mkdir -p qtest
	@if which qtest > /dev/null ; then \
		qtest extract --preamble $(QTEST_PREAMBLE) \
			-o qtest/run_qtest.ml \
			$(QTESTABLE) 2> /dev/null ; \
	else touch qtest/run_qtest.ml ; \
	fi

push-stable:
	git checkout stable
	git merge master -m 'merge from master'
	oasis setup
	git commit -a -m 'oasis files'
	git push origin
	git checkout master

clean-generated:
	rm **/*.{mldylib,mlpack,mllib} myocamlbuild.ml -f

tags:
	otags *.ml *.mli

VERSION=$(shell awk '/^Version:/ {print $$2}' _oasis)

update_next_tag:
	@echo "update version to $(VERSION)..."
	zsh -c 'sed -i "s/NEXT_VERSION/$(VERSION)/g" **/*.ml **/*.mli'
	zsh -c 'sed -i "s/NEXT_RELEASE/$(VERSION)/g" **/*.ml **/*.mli'

devel:
	./configure --enable-bench --enable-tests --enable-unix \
		--enable-thread
	make all

watch:
	while find src/ benchs/ -print0 | xargs -0 inotifywait -e delete_self -e modify ; do \
		echo "============ at `date` ==========" ; \
		make all; \
	done

reindent:
	@which ocp-indent || ( echo "require ocp-indent" ; exit 1 )
	@find src '(' -name '*.ml' -or -name '*.mli' ')' -type f -print0 | xargs -0 echo "reindenting: "
	@find src '(' -name '*.ml' -or -name '*.mli' ')' -type f -print0 | xargs -0 ocp-indent -i

.PHONY: examples push_doc tags qtest-gen qtest-clean devel update_next_tag
