#!/bin/bash

MAX_NESTED_RECORD_SIZE=20

for i in $(seq 1 $MAX_NESTED_RECORD_SIZE); do
    python create_nested_record.py $i > ../examples/benchmarks/nested_records/$i.effekt
done

echo "Generated files from 1 to $MAX_NESTED_RECORD_SIZE for nested records"

MAX_LARGE_RECORD_SIZE=20
for i in $(seq 1 $MAX_LARGE_RECORD_SIZE); do
    python create_large_record.py $i > ../examples/benchmarks/large_records/$i.effekt
done

echo "Generated files from 1 to $MAX_LARGE_RECORD_SIZE for large records"
