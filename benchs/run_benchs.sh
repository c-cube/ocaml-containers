#!/bin/sh

exec dune exec --profile=release benchs/run_benchs.exe -- $@
