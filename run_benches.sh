#!/bin/bash

echo "Run benchmarks"

# File containing paths to benchmarks (one per line)
file_list="benchmarks.txt"

# Check if the file exists
if [ ! -f "$file_list" ]; then
    echo "Benchmark file $file_list not found."
    exit 1
fi

# Read the benchmark paths into an array
declare -a benchmarks
while IFS= read -r line; do
    if [[ ! $line =~ ^# ]]; then
        benchmarks+=("$line")
    fi
done < "$file_list"

# Initialize a variable to track if any command fails
all_success=true

# Loop through each benchmark file path in the list
for file in "${benchmarks[@]}"; do
    # Call effekt.sh for the current file
    echo "run $file"
    effekt.sh "$file" > tmp
    grep -q "error" tmp
    hasError=$?
    # Check the exit code of the effekt.sh call
    if [ $hasError -eq 0 ]; then
        # If any call fails, set the flag to false
        all_success=false
        echo "benchmark failed: $file"
        cat tmp
    fi
done

# Check if all calls were successful
if [ "$all_success" = true ]; then
    echo "success"
    exit 0
else
    exit 1
fi
