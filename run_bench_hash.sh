#!/bin/sh

OPTS="--profile=release --display=quiet"
exec dune exec $OPTS -- ./benchs/run_bench_hash.exe $@
