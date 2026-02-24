#!/bin/bash

NESTED_OUT="../examples/benchmarks/nested_record"
LARGE_OUT="../examples/benchmarks/large_record"

mkdir -p "$NESTED_OUT" "$LARGE_OUT"

MAX_NESTED_RECORD_SIZE=1000

for i in $(seq 1 $MAX_NESTED_RECORD_SIZE); do
    python nested_record/create_return_int_recreate.py $i > "$NESTED_OUT/return_int_recreate_$i.effekt"
    python nested_record/create_return_int_reuse.py $i > "$NESTED_OUT/return_int_reuse_$i.effekt"
    python nested_record/create_return_record_recreate.py $i > "$NESTED_OUT/return_record_recreate_$i.effekt"
    python nested_record/create_return_record_reuse.py $i > "$NESTED_OUT/return_record_reuse_$i.effekt"
done

echo "Generated files from 1 to $MAX_NESTED_RECORD_SIZE for nested records"

MAX_LARGE_RECORD_SIZE=100

for i in $(seq 1 $MAX_LARGE_RECORD_SIZE); do
    python large_record/create_return_int_recreate.py $i > "$LARGE_OUT/return_int_recreate_$i.effekt"
    python large_record/create_return_int_reuse.py $i > "$LARGE_OUT/return_int_reuse_$i.effekt"
    python large_record/create_return_record_recreate.py $i > "$LARGE_OUT/return_record_recreate_$i.effekt"
    python large_record/create_return_record_reuse.py $i > "$LARGE_OUT/return_record_reuse_$i.effekt"
done

echo "Generated files from 1 to $MAX_LARGE_RECORD_SIZE for large records"
