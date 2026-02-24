#!/usr/bin/env python3
"""
Compares benchmarks between the current branch and main.
Usage: ./benchmark.py [--skip-compile] [--config benchmark.yml]
"""

import argparse
import os
import shutil
import signal
import subprocess
import sys
import yaml
from datetime import datetime
from pathlib import Path

# Run from the repo root so git/sbt/effekt resolve correctly
REPO_ROOT = Path(__file__).parent.parent

# ── Helpers ───────────────────────────────────────────────────────────────────

def run(cmd, **kwargs):
    """Run a command from the repo root, inheriting stdio so output streams to the terminal."""
    subprocess.run(cmd, check=True, cwd=REPO_ROOT, **kwargs)

def git_current_branch() -> str:
    return subprocess.check_output(
        ["git", "branch", "--show-current"], text=True, cwd=REPO_ROOT
    ).strip()

def bench_exec(backend: str, out_dir: str, bench_name: str, params: str) -> str:
    match backend:
        case "llvm":
            return f"./{out_dir}/{bench_name} {params}"
        case "js":
            return f"node {out_dir}/{bench_name}.js {params}"
        case "chez-callcc":
            return f"scheme --script {out_dir}/{bench_name}.ss {params}"
        case _:
            raise ValueError(f"Unknown backend: {backend}")

# ── Main ──────────────────────────────────────────────────────────────────────

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--skip-compile", action="store_true")
    parser.add_argument("--config", default=Path(__file__).parent / "benchmark.yml",
                        type=Path, help="Path to benchmark config YAML")
    args = parser.parse_args()

    cfg           = yaml.safe_load(args.config.read_text())
    BACKENDS      = cfg["backends"]
    WARMUP        = cfg["warmup"]
    RUNS          = cfg["runs"]
    BRANCH        = cfg["branch"]
    TARGET_BRANCH = cfg["target_branch"]
    OUTPUT_DIR    = REPO_ROOT / cfg.get("output_dir", "benchmark-results")
    # benchmarks: list of {path, n}
    BENCHMARKS    = {b["path"]: str(b["n"]) for b in cfg["benchmarks"]}

    current_branch = git_current_branch()  # saved only to restore at the end
    branch_safe    = BRANCH.replace("/", "-")
    target_safe    = TARGET_BRANCH.replace("/", "-")
    timestamp      = datetime.now().strftime("%Y%m%d_%H%M%S")

    # Restore branch on Ctrl-C
    def on_interrupt(sig, frame):
        if git_current_branch() != current_branch:
            print(f"\nInterrupted! Switching back to {current_branch}...")
            subprocess.run(["git", "checkout", "-q", current_branch])
        sys.exit(1)
    signal.signal(signal.SIGINT,  on_interrupt)
    signal.signal(signal.SIGTERM, on_interrupt)

    if not shutil.which("hyperfine"):
        sys.exit("Error: hyperfine is not installed")

    if BRANCH == TARGET_BRANCH:
        sys.exit(f"Error: branch and target_branch are both '{TARGET_BRANCH}'. They must differ.")

    target_safe = TARGET_BRANCH.replace("/", "-")

    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

    for backend in BACKENDS:
        Path(REPO_ROOT / f"out-{branch_safe}-{backend}").mkdir(exist_ok=True)
        Path(REPO_ROOT / f"out-{target_safe}-{backend}").mkdir(exist_ok=True)

    print(f"Comparing: {BRANCH} vs {TARGET_BRANCH}")
    print(f"Backends:  {', '.join(BACKENDS)}")
    print(f"Runs: {RUNS}, Warmup: {WARMUP}")
    print(f"Skip compilation: {'yes' if args.skip_compile else 'no'}\n")

    if not args.skip_compile:
        result = subprocess.run(["git", "diff-index", "--quiet", "HEAD", "--"], cwd=REPO_ROOT)
        if result.returncode != 0:
            sys.exit("Error: you have uncommitted changes. Commit or stash them first.")

        print(f"=== Building compiler on {BRANCH} ===")
        run(["git", "checkout", "-q", BRANCH])
        run(["sbt", "install"])

        print(f"\n=== Compiling benchmarks on {BRANCH} ===")
        for backend in BACKENDS:
            out_current = f"out-{branch_safe}-{backend}"
            print(f"Backend: {backend}")
            for bench_path in BENCHMARKS:
                bench_name  = Path(bench_path).name
                source_file = f"examples/benchmarks/{bench_path}.effekt"
                print(f"  {bench_name}")
                run(["effekt", f"--backend={backend}", "--build", "-o", out_current, source_file])

        print(f"\n=== Building compiler on {TARGET_BRANCH} ===")
        run(["git", "checkout", "-q", TARGET_BRANCH])
        run(["sbt", "install"])

        print(f"\n=== Compiling benchmarks on {TARGET_BRANCH} ===")
        for backend in BACKENDS:
            out_target = f"out-{target_safe}-{backend}"
            print(f"Backend: {backend}")
            for bench_path in BENCHMARKS:
                bench_name  = Path(bench_path).name
                source_file = f"examples/benchmarks/{bench_path}.effekt"
                print(f"  {bench_name}")
                run(["effekt", f"--backend={backend}", "--build", "-o", out_target, source_file])

        print(f"\n=== Switching back to {current_branch} ===")
        run(["git", "checkout", "-q", current_branch])
    else:
        print("=== Skipping compilation (using existing binaries) ===\n")

    print("=== Starting benchmarks ===\n")

    try:
        for backend in BACKENDS:
            print(f"=== Benchmarking backend: {backend} ===")
            out_current = f"out-{branch_safe}-{backend}"
            out_main    = f"out-{target_safe}-{backend}"
            results_dir = OUTPUT_DIR / f"comparison_{backend}_{branch_safe}_vs_{target_safe}_{timestamp}"
            results_dir.mkdir(parents=True, exist_ok=True)

            for bench_path, n in BENCHMARKS.items():
                bench_name = Path(bench_path).name
                print(f"  {bench_name}")
                result = subprocess.run([
                    "hyperfine",
                    "--warmup",        str(WARMUP),
                    "--runs",          str(RUNS),
                    "--export-json",   str(results_dir / f"{bench_name}.json"),
                    "--command-name",  TARGET_BRANCH,
                    bench_exec(backend, out_main, bench_name, n),
                    "--command-name",  BRANCH,
                    bench_exec(backend, out_current, bench_name, n),
                ], cwd=REPO_ROOT)
                if result.returncode != 0:
                    print(f"  (skipped {bench_name} — hyperfine failed)")

            print(f"Results: {results_dir}/\n")
    finally:
        # Always make sure we end up on the original branch
        if git_current_branch() != current_branch:
            print(f"=== Switching back to {current_branch} ===")
            run(["git", "checkout", "-q", current_branch])

    print(f"Done! Results in: {OUTPUT_DIR}/")


if __name__ == "__main__":
    main()
