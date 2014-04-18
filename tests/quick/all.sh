#!/usr/bin/env bash

for i in tests/quick/*.ml ; do
    echo -n "${i}..."
    $i
done
