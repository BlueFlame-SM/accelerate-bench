#!/bin/bash

# Check if a benchmark name is provided
if [ $# -ne 1 ]; then
    echo "Usage: $0 <benchmark_name>"
    exit 1
fi

bench_name="$1"
bench_time="$(date +"%Y-%m-%d_%H:%M:%S")"

# Arguments for speed benchmarking
speed_args=("--flag" "accelerate-bench:-memory-bench" "--flag" "accelerate:-memory-counter")

# Arguments for memory benchmarking
memory_args=("--flag" "accelerate-bench:memory-bench" "--flag" "accelerate:memory-counter")


# Perform speed benchmarking
mkdir -p "speed-bench_results"
stack build "${speed_args[@]}"
stack exec accelerate-bench-exe | stdbuf -oL tee "speed-bench_results/${bench_name}_${bench_time}.txt"


# Perform memory benchmarking
mkdir -p "memory-bench_results"
stack build "${memory_args[@]}"
stack exec accelerate-bench-exe | stdbuf -oL tee "memory-bench_results/${bench_name}_${bench_time}.txt"
