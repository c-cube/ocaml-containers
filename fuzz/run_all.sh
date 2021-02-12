#!/bin/bash

cpu_count=$(grep -c ^processor /proc/cpuinfo)

simul_test_count=$[cpu_count-1]

test_timeout="10m"

script_dir=$(dirname $(readlink -f "$0"))

log_dir="$script_dir"/../fuzz-logs

echo "Building"

dune build @all

echo ""

start_date=$(date "+%Y-%m-%d %H:%M")
start_time=$(date "+%s")

names=()

i=0
for file in "$script_dir"/../_build/default/fuzz/*.exe; do
  name=$(basename $file | sed 's/\.exe$//')
  names[$i]=$name
  i=$[i+1]
done

test_count=${#names[@]}

echo "Fuzzing tests available:"

for name in ${names[@]}; do
  echo "- "$name
done

echo ""
echo "Fuzzing start time:" $start_date
echo ""

echo "Starting $test_count tests"
echo ""

mkdir -p "$log_dir"

i=0
while (( $i < $test_count )); do
  if (( $test_count - $i >= $simul_test_count )); then
    tests_to_run=$simul_test_count
  else
    tests_to_run=$[test_count - i]
  fi

  echo "Running $tests_to_run tests in parallel"

  for (( c=0; c < $tests_to_run; c++ )); do
    name=${names[$i]}
    if [[ "$name" != "" ]]; then
      echo "  Starting $name"

      (AFL_NO_UI=1 timeout "$test_timeout" "$script_dir"/run.sh "$name" skip_build > "$log_dir"/"$name".log) &

      i=$[i+1]
    fi
  done

  echo "Waiting for $test_timeout"

  sleep $test_timeout

  echo "Terminating tests"

  pkill afl-fuzz

  sleep 5

  echo ""
  echo "$[test_count - i] / $test_count tests remaining"
  echo ""
done

end_date=$(date "+%Y-%m-%d %H:%M")
end_time=$(date "+%s")

echo ""
echo "Test end:" $end_date

echo ""

echo "Time elapsed:" $[(end_time - start_time) / 60] "minutes"

test_fail_count=0
tests_failed=()

for name in ${names[@]}; do
  output_dir="$script_dir"/../"fuzz-""$name""-output"

  crashes_dir="$output_dir"/crashes

  if [ -z "$(ls -A $crashes_dir)" ]; then
    # crashes dir is empty
    :
  else
    # crashes dir is not empty
    test_fail_count=$[$test_fail_count + 1]
    tests_failed+=("$name")
  fi
done

echo "========================================"

if [[ $test_fail_count == 0 ]]; then
    echo "All $test_count tests passed"
    exit_code=0
else
    echo "$test_fail_count tests failed"
    echo ""
    echo "List of tests failed :"
    for t in ${tests_failed[@]}; do
      echo "    "$t
    done
    exit_code=1
fi

