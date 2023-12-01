#!/bin/sh

OPTS="--profile=release --display=quiet"
exec dune exec $OPTS -- benchs/run_benchs_hash.exe $@
