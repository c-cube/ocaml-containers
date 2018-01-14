

QTEST_PREAMBLE='open CCFun;;'
DONTTEST=$(wildcard ../src/**/*.cppo.*) $(wildcard ../src/**/*Labels*)
QTESTABLE=$(filter-out $(DONTTEST), \
	$(wildcard ../src/core/*.ml) \
	$(wildcard ../src/core/*.mli) \
	$(wildcard ../src/data/*.ml) \
	$(wildcard ../src/data/*.mli) \
	$(wildcard ../src/string/*.ml) \
	$(wildcard ../src/string/*.mli) \
	$(wildcard ../src/unix/*.ml) \
	$(wildcard ../src/unix/*.mli) \
	$(wildcard ../src/sexp/*.ml) \
	$(wildcard ../src/sexp/*.mli) \
	$(wildcard ../src/iter/*.ml) \
	$(wildcard ../src/iter/*.mli) \
	$(wildcard ../src/bigarray/*.ml) \
	$(wildcard ../src/bigarray/*.mli) \
	$(wildcard ../src/threads/*.ml) \
	$(wildcard ../src/threads/*.mli) \
	)

qtest-gen:
	@if which qtest > /dev/null ; then \
	  	echo "generate qtest"; \
		qtest extract --preamble $(QTEST_PREAMBLE) \
			-o run_qtest.ml \
			$(QTESTABLE) 2> /dev/null ; \
	else touch qtest/run_qtest.ml ; \
	fi
