#!/usr/bin/env python3
"""
Converts a directory of per-benchmark hyperfine JSON files to CSV.
Usage: ./benchmark_to_csv.py <results_dir/> [output.csv]
  If output.csv is omitted, prints to stdout.
"""

import sys
import re
import csv
import json
import math
from pathlib import Path

if len(sys.argv) < 2:
    print(f"Usage: {sys.argv[0]} <results_dir/> [output.csv]", file=sys.stderr)
    sys.exit(1)

results_dir = Path(sys.argv[1])
output      = sys.argv[2] if len(sys.argv) > 2 else None

results = []

for json_file in results_dir.glob('*.json'):
    entry  = json.loads(json_file.read_text())
    name   = json_file.stem
    by_cmd = {r['command']: r for r in entry['results']}

    if 'main' not in by_cmd or len(by_cmd) < 2:
        continue

    main_r   = by_cmd['main']
    branch_r = next(r for cmd, r in by_cmd.items() if cmd != 'main')

    # hyperfine stores times in seconds; convert to ms
    main_mean = main_r['mean']     * 1000
    main_pm   = main_r['stddev']   * 1000
    br_mean   = branch_r['mean']   * 1000
    br_pm     = branch_r['stddev'] * 1000

    speedup    = main_mean / br_mean
    # error propagation for f = a/b: σ_f/f = sqrt((σ_a/a)² + (σ_b/b)²)
    speedup_pm = speedup * math.sqrt((main_pm / main_mean) ** 2 + (br_pm / br_mean) ** 2)

    results.append((name, speedup, speedup_pm, br_mean, br_pm, main_mean, main_pm))

# Natural sort: split name into text/number chunks so e.g. _2 < _11
def natural_key(row):
    return [int(c) if c.isdigit() else c for c in re.split(r'(\d+)', row[0])]

results.sort(key=natural_key)

header = ['benchmark', 'speedup', 'speedup_pm', 'ar_mean_ms', 'ar_pm_ms', 'main_mean_ms', 'main_pm_ms']

def fmt(v, decimals=2):
    return f"{v:.{decimals}f}"

fh = open(output, 'w', newline='') if output else sys.stdout

writer = csv.writer(fh)
writer.writerow(header)
for name, speedup, speedup_pm, ar_mean, ar_pm, main_mean, main_pm in results:
    writer.writerow([
        name,
        fmt(speedup),
        fmt(speedup_pm) if speedup_pm is not None else '',
        fmt(ar_mean, 1),
        fmt(ar_pm,   1),
        fmt(main_mean, 1),
        fmt(main_pm,   1),
    ])

if output:
    fh.close()
    print(f"Written to {output}", file=sys.stderr)
