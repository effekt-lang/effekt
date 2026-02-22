"""
Plot speedup results from hyperfine benchmark markdown files.

Produces two publication-quality subplots – nested_records and large_records –
with the same visual vocabulary as the learning-curve reference:
  • CI band (fill_between, low alpha)
  • Raw values as a faint thin line
  • Smoothed trend as the main foreground line with markers
"""
from __future__ import annotations

import glob
import re
import time
from pathlib import Path

import matplotlib.pyplot as plt
import matplotlib.ticker as ticker
import numpy as np
import pandas as pd
from tueplots import bundles, figsizes
from tueplots.constants.color import rgb

# ─── Config ──────────────────────────────────────────────────────────────────
BENCHMARK_DIR = Path(__file__).parent.parent / "benchmark-results"
BACKEND = "llvm"

# ─── Helpers ─────────────────────────────────────────────────────────────────
def _to_ms(value: str, unit: str) -> float:
    """Convert a hyperfine time value (value + unit string) to milliseconds."""
    v = float(value.replace(",", ""))
    u = unit.strip()
    if u == "µs":  return v / 1_000
    if u == "ms":  return v
    if u == "s":   return v * 1_000
    raise ValueError(f"Unknown time unit: {u!r}")


# ─── Parser ──────────────────────────────────────────────────────────────────
_TIME_RE    = re.compile(
    r"Time \(mean ± σ\):\s+([\d.,]+)\s+(µs|ms|s)\s+±\s+([\d.,]+)\s+(µs|ms|s)"
)
_SUMMARY_RE = re.compile(
    r"(konradbausch/arity-raising|main) ran\s+([\d.]+) ± ([\d.]+) times faster than",
    re.DOTALL,
)


def parse_file(path: Path) -> pd.DataFrame:
    """Return a DataFrame with one row per successfully benchmarked program."""
    text   = path.read_text()
    rows   = []

    for section in re.split(r"^## ", text, flags=re.MULTILINE)[1:]:
        name = section.splitlines()[0].strip()
        m    = re.match(r"^(nested_records|large_records)_(\d+)$", name)
        if not m:
            continue
        family, n = m.group(1), int(m.group(2))

        if "non-zero exit code" in section or "Error:" in section:
            continue

        times = _TIME_RE.findall(section)
        if len(times) < 2:
            continue

        main_mean = _to_ms(times[0][0], times[0][1])
        main_std  = _to_ms(times[0][2], times[0][3])
        ar_mean   = _to_ms(times[1][0], times[1][1])
        ar_std    = _to_ms(times[1][2], times[1][3])

        sm = _SUMMARY_RE.search(section)
        if sm:
            winner, sp, se = sm.group(1), float(sm.group(2)), float(sm.group(3))
            if winner == "main":          # arity-raising is the *loser*
                sp = 1.0 / sp
                se = se / (sp ** 2)       # propagate 1/x uncertainty
        else:
            sp = main_mean / ar_mean
            se = sp * np.sqrt((main_std / main_mean) ** 2 + (ar_std / ar_mean) ** 2)

        rows.append(dict(
            name=name, family=family, n=n,
            main_mean=main_mean, main_std=main_std,
            ar_mean=ar_mean,   ar_std=ar_std,
            speedup=sp, speedup_err=se,
        ))

    return pd.DataFrame(rows)


def latest_file(backend: str) -> Path:
    pattern = str(
        BENCHMARK_DIR / f"comparison_{backend}_konradbausch-arity-raising_vs_main_*.md"
    )
    files = sorted(glob.glob(pattern))
    if not files:
        raise FileNotFoundError(f"No files matching {pattern}")
    return Path(files[-1])


# ─── Plot ────────────────────────────────────────────────────────────────────
def make_plot(df: pd.DataFrame, outpath: Path | None = None) -> None:
    t0 = time.perf_counter()

    plt.rcParams.update(bundles.icml2022())

    fig, (ax_nested, ax_large) = plt.subplots(
        1, 2,
        figsize=(7.0, 2.6),
        constrained_layout=True,
    )

    panels = [
        ("nested_records", ax_nested, rgb.pn_orange,
         "Record depth $n$",
         "Speedup of arity-raising over main\n(nested records)"),
        ("large_records",  ax_large,  rgb.tue_blue,
         "Number of record fields $n$",
         "Speedup of arity-raising over main\n(large records)"),
    ]

    for family, ax, color, xlabel, title in panels:
        sub = df[df["family"] == family].sort_values("n")
        if sub.empty:
            ax.set_visible(False)
            continue

        x   = sub["n"].to_numpy(dtype=float)
        y   = sub["speedup"].to_numpy()
        lo  = y - sub["speedup_err"].to_numpy()
        hi  = y + sub["speedup_err"].to_numpy()

        # Deviation band
        ax.fill_between(x, lo, hi, alpha=0.18, linewidth=0, color=color, zorder=1)

        # Speedup line
        ax.plot(
            x, y,
            linewidth=1.3,
            marker="o",
            markersize=2.2,
            color=color,
            zorder=3,
            label="arity-raising vs main",
        )

        # Reference line at y = 1 (no speedup)
        ax.axhline(1.0, linewidth=0.6, linestyle="--", color="0.55", zorder=0)

        ax.set_xlabel(xlabel)
        ax.set_ylabel("Speedup (×)")
        ax.set_title(title)
        ax.xaxis.set_minor_locator(ticker.AutoMinorLocator())
        ax.grid(axis="y", which="major", color="0.88", linewidth=0.6)
        ax.margins(x=0.03)
        ax.set_ylim(bottom=0)

    if outpath is not None:
        outpath.parent.mkdir(parents=True, exist_ok=True)
        fig.savefig(outpath, dpi=300, bbox_inches="tight")
        print(f"✓  Saved → {outpath}")

    plt.savefig(
        Path(__file__).parent / "speedup_comparison.png",
        dpi=300, bbox_inches="tight",
    )
    print(f"✓  Saved → {Path(__file__).parent / 'speedup_comparison.png'}")
    plt.show()
    plt.close(fig)
    print(f"[plot_speedup] total time: {time.perf_counter() - t0:.2f}s")


# ─── Entry point ─────────────────────────────────────────────────────────────
if __name__ == "__main__":
    src  = latest_file(BACKEND)
    print(f"Parsing: {src.name}")
    df   = parse_file(src)
    print(df[["name", "speedup", "speedup_err"]].to_string(index=False))
    make_plot(df)
