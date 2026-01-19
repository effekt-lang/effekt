#!/bin/bash
set -e

SKIP_COMPILE=0
if [ "$1" = "--skip-compile" ]; then
    SKIP_COMPILE=1
fi

BACKENDS=("llvm")
WARMUP=10
RUNS=50
TARGET_BRANCH="main"
OUTPUT_DIR="benchmark-results"
TIMESTAMP=$(date +%Y%m%d_%H%M%S)
CURRENT_BRANCH=$(git branch --show-current)
CURRENT_BRANCH_SAFE="${CURRENT_BRANCH//\//-}"

cleanup() {
    local current=$(git branch --show-current)
    if [ "$current" != "$CURRENT_BRANCH" ]; then
        echo ""
        echo "Interrupted! Switching back to $CURRENT_BRANCH..."
        git checkout -q "$CURRENT_BRANCH"
    fi
    exit 1
}

trap cleanup SIGINT SIGTERM

if ! command -v hyperfine &> /dev/null; then
    echo "Error: hyperfine is not installed"
    exit 1
fi

if [ "$CURRENT_BRANCH" = "$TARGET_BRANCH" ]; then
    echo "Error: You are currently on the $TARGET_BRANCH branch"
    echo "Please switch to your feature branch first"
    exit 1
fi

mkdir -p "$OUTPUT_DIR"

declare -A BENCHMARKS=(
   ["arity_raising/record_passing"]="25000000"
   ["arity_raising/matrix_determinant"]="2000000"
)

echo "Comparing: $CURRENT_BRANCH vs $TARGET_BRANCH"
echo "Backends: ${BACKENDS[*]}"
echo "Runs: $RUNS, Warmup: $WARMUP"
echo "Skip compilation: $([ $SKIP_COMPILE -eq 1 ] && echo 'yes' || echo 'no')"
echo ""

for backend in "${BACKENDS[@]}"; do
    OUT_CURRENT="out-${CURRENT_BRANCH_SAFE}-${backend}"
    OUT_MAIN="out-main-${backend}"
    mkdir -p "$OUT_CURRENT" "$OUT_MAIN"
done

if [ $SKIP_COMPILE -eq 0 ]; then

    if ! git diff-index --quiet HEAD --; then
        echo "Error: You have uncommitted changes"
        echo "Please commit or stash your changes before running this script"
        exit 1
    fi

    echo "=== Building compiler on $CURRENT_BRANCH ==="
    sbt install
    echo ""

    echo "=== Compiling benchmarks on $CURRENT_BRANCH ==="
    for backend in "${BACKENDS[@]}"; do
        OUT_CURRENT="out-${CURRENT_BRANCH_SAFE}-${backend}"
        echo "Backend: $backend"
        
        for bench_path in "${!BENCHMARKS[@]}"; do
            bench_name=$(basename "$bench_path")
            source_file="examples/benchmarks/${bench_path}.effekt"
            echo "  $bench_name"
            effekt --backend="$backend" --build -o "$OUT_CURRENT" "$source_file"
        done
    done
    echo ""

    echo "=== Building compiler on $TARGET_BRANCH ==="
    git checkout -q "$TARGET_BRANCH"
    sbt install
    echo ""

    echo "=== Compiling benchmarks on $TARGET_BRANCH ==="
    for backend in "${BACKENDS[@]}"; do
        OUT_MAIN="out-main-${backend}"
        echo "Backend: $backend"
        
        for bench_path in "${!BENCHMARKS[@]}"; do
            bench_name=$(basename "$bench_path")
            source_file="examples/benchmarks/${bench_path}.effekt"
            echo "  $bench_name"
            effekt --backend="$backend" --build -o "$OUT_MAIN" "$source_file"
        done
    done
    echo ""

    echo "=== Switching back to $CURRENT_BRANCH ==="
    git checkout -q "$CURRENT_BRANCH"
    echo ""
else
    echo "=== Skipping compilation (using existing binaries) ==="
    echo ""
fi

echo "=== Starting benchmarks ==="
echo ""

for backend in "${BACKENDS[@]}"; do
    echo "=== Benchmarking backend: $backend ==="
    
    OUT_CURRENT="out-${CURRENT_BRANCH_SAFE}-${backend}"
    OUT_MAIN="out-main-${backend}"
    
    comparison_file="${OUTPUT_DIR}/comparison_${backend}_${CURRENT_BRANCH_SAFE}_vs_main_${TIMESTAMP}.md"
    echo "# $CURRENT_BRANCH vs main ($backend)" > "$comparison_file"
    echo "Date: $(date)" >> "$comparison_file"
    echo "Runs: $RUNS, Warmup: $WARMUP" >> "$comparison_file"
    echo "" >> "$comparison_file"
    
    for bench_path in "${!BENCHMARKS[@]}"; do
        bench_name=$(basename "$bench_path")
        params=${BENCHMARKS[$bench_path]}
        
        echo "  $bench_name"
        
        case $backend in
            llvm)
                current_exec="./$OUT_CURRENT/${bench_name}"
                target_exec="./$OUT_MAIN/${bench_name}"
                ;;
            js)
                current_exec="node $OUT_CURRENT/${bench_name}.js"
                target_exec="node $OUT_MAIN/${bench_name}.js"
                ;;
            chez-callcc)
                current_exec="scheme --script $OUT_CURRENT/${bench_name}.ss"
                target_exec="scheme --script $OUT_MAIN/${bench_name}.ss"
                ;;
        esac
        
        echo "## $bench_name" >> "$comparison_file"
        hyperfine \
            --warmup "$WARMUP" \
            --runs "$RUNS" \
            --export-markdown - \
            --command-name "main" "$target_exec $params" \
            --command-name "$CURRENT_BRANCH" "$current_exec $params" \
            2>&1 | tee -a "$comparison_file"
        echo "" >> "$comparison_file"
    done
    
    echo "Results: $comparison_file"
    echo ""
done

echo "Done! Results in: $OUTPUT_DIR/"
