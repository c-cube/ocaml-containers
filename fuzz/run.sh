#!/bin/bash

script_dir=$(dirname $(readlink -f "$0"))

skip_build=$2

if [[ "$skip_build" != "skip_build" ]]; then
  echo "Building"

  dune build @all
fi

if [[ "$1" == "" ]]; then
  echo "Please enter a fuzzing test to run"
  exit 1
fi

name=$(echo "$1" | sed 's/\.exe$//' | sed 's/\.ml$//')

echo "Creating input directory"

input_dir="$script_dir"/../"fuzz-""$name""-input"

output_dir="$script_dir"/../"fuzz-""$name""-output"

mkdir -p "$input_dir"

echo "abcd" > "$input_dir"/dummy

if [ -d "$output_dir" ]; then
  afl-fuzz -t 1000 -i -            -o "$output_dir" "$script_dir"/../_build/default/fuzz/"$name".exe @@
else
  mkdir -p "$output_dir"

  afl-fuzz -t 1000 -i "$input_dir" -o "$output_dir" "$script_dir"/../_build/default/fuzz/"$name".exe @@
fi

