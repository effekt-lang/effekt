#!/bin/bash

# Benchmark comparison script for Effekt
# Compares current branch against main branch

set -e

# Parse command-line arguments
SKIP_COMPILE=0
while [[ $# -gt 0 ]]; do
    case $1 in
        --skip-compile)
            SKIP_COMPILE=1
            shift
            ;;
        *)
            echo "Unknown option: $1"
            echo "Usage: $0 [--skip-compile]"
            exit 1
            ;;
    esac
done

# Configuration (hardcoded)
BACKENDS=("llvm")
WARMUP=5
RUNS=50
TARGET_BRANCH="main"
OUTPUT_DIR="benchmark-results"
TIMESTAMP=$(date +%Y%m%d_%H%M%S)
CURRENT_BRANCH=$(git branch --show-current)
CURRENT_BRANCH_SAFE="${CURRENT_BRANCH//\//-}"

# Colors
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
NC='\033[0m'

if ! command -v hyperfine &> /dev/null; then
    echo "Error: hyperfine is not installed"
    exit 1
fi

# Check if we're on main branch
if [ "$CURRENT_BRANCH" = "$TARGET_BRANCH" ]; then
    echo "Error: You are currently on the $TARGET_BRANCH branch"
    echo "Please switch to your feature branch first"
    exit 1
fi

mkdir -p "$OUTPUT_DIR"

# Define benchmarks with their parameters
declare -A BENCHMARKS=(
    ["arity_raising/record_addition"]="5000000"
    ["arity_raising/nested_records"]="2500000"
    ["arity_raising/record_map"]="10000"
)

echo -e "${BLUE}Comparing: $CURRENT_BRANCH vs $TARGET_BRANCH${NC}"
echo "Backends: ${BACKENDS[*]}"
echo "Runs: $RUNS, Warmup: $WARMUP"
echo "Skip compilation: $([ $SKIP_COMPILE -eq 1 ] && echo 'yes' || echo 'no')"
echo ""

# Create output directories
for backend in "${BACKENDS[@]}"; do
    OUT_CURRENT="out-${CURRENT_BRANCH_SAFE}-${backend}"
    OUT_MAIN="out-main-${backend}"
    mkdir -p "$OUT_CURRENT" "$OUT_MAIN"
done

if [ $SKIP_COMPILE -eq 0 ]; then
    # Build compiler and compile benchmarks on current branch
    echo -e "${YELLOW}=== Building compiler on $CURRENT_BRANCH ===${NC}"
    sbt install
    echo ""

    echo -e "${YELLOW}=== Compiling benchmarks on $CURRENT_BRANCH ===${NC}"
    for backend in "${BACKENDS[@]}"; do
        OUT_CURRENT="out-${CURRENT_BRANCH_SAFE}-${backend}"
        echo -e "${GREEN}Backend: $backend${NC}"
        
        for bench_path in "${!BENCHMARKS[@]}"; do
            bench_name=$(basename "$bench_path")
            source_file="examples/benchmarks/${bench_path}.effekt"
            echo "  $bench_name"
            effekt --backend="$backend" --build -o "$OUT_CURRENT" "$source_file"
        done
    done
    echo ""

    # Stash any uncommitted changes before switching branches
    if ! git diff-index --quiet HEAD --; then
        echo -e "${YELLOW}Stashing uncommitted changes before switching to $TARGET_BRANCH...${NC}"
        git stash push -m "benchmark-compare temporary stash"
        STASHED=1
    else
        STASHED=0
    fi

    # Build compiler and compile benchmarks on main branch
    echo -e "${YELLOW}=== Building compiler on $TARGET_BRANCH ===${NC}"
    git checkout -q "$TARGET_BRANCH"
    sbt install
    echo ""

    echo -e "${YELLOW}=== Compiling benchmarks on $TARGET_BRANCH ===${NC}"
    for backend in "${BACKENDS[@]}"; do
        OUT_MAIN="out-main-${backend}"
        echo -e "${GREEN}Backend: $backend${NC}"
        
        for bench_path in "${!BENCHMARKS[@]}"; do
            bench_name=$(basename "$bench_path")
            source_file="examples/benchmarks/${bench_path}.effekt"
            echo "  $bench_name"
            effekt --backend="$backend" --build -o "$OUT_MAIN" "$source_file"
        done
    done
    echo ""

    # Switch back to current branch
    echo -e "${YELLOW}=== Switching back to $CURRENT_BRANCH ===${NC}"
    git checkout -q "$CURRENT_BRANCH"
    
    # Restore stashed changes if any
    if [ $STASHED -eq 1 ]; then
        echo -e "${YELLOW}Restoring stashed changes...${NC}"
        git stash pop
    fi
    echo ""
else
    echo -e "${YELLOW}=== Skipping compilation (using existing binaries) ===${NC}"
    echo ""
fi

echo -e "${BLUE}=== Starting benchmarks ===${NC}"
echo ""

# Now run benchmarks using pre-compiled binaries
for backend in "${BACKENDS[@]}"; do
    echo -e "${YELLOW}=== Benchmarking backend: $backend ===${NC}"
    
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
        
        echo -e "${GREEN}  $bench_name${NC}"
        
        # Set executable paths based on backend
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
        
        # Run comparison
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
    
    echo -e "${GREEN}Results: $comparison_file${NC}"
    echo ""
done

echo -e "${GREEN}Done! Results in: $OUTPUT_DIR/${NC}"
