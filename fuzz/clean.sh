#!/bin/bash

script_dir=$(dirname $(readlink -f "$0"))

rm -r "$script_dir"/../fuzz-*-input
rm -r "$script_dir"/../fuzz-*-output
