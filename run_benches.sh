#!/bin/bash

echo "Run benchmarks"

# List of files
file_list=(
    "src/list.effekt"
    "src/mandelbrot.effekt"
    "src/permute.effekt"
    # Add more file names as needed
)

# Initialize a variable to track if any command fails
all_success=true

# Loop through each file in the list
for file in "${file_list[@]}"; do
    # Call effekt.sh for the current file
    effekt.sh "$file" >> tmp
    res="$?"
    echo "$file => $res"
    # Check the exit code of the effekt.sh call
    if [ "$res" -ne 0 ]; then
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
