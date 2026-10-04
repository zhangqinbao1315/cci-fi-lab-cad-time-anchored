#!/usr/bin/env python3
from pathlib import Path
import csv
import numpy as np
import matplotlib.pyplot as plt

ROOT = Path(__file__).resolve().parents[1]
DATA = ROOT / "figure_data"
OUT = ROOT / "generated_figures"
OUT.mkdir(exist_ok=True)

def read_rows(name):
    with open(DATA / name, newline="", encoding="utf-8") as f:
        return list(csv.DictReader(f))

def forest(name, title, xlabel, xlim, stem, figsize=None):
    rows = read_rows(name)
    labels = [r["label"] for r in rows]
    ors = np.array([float(r["or"]) for r in rows])
    lo = np.array([float(r["ci_low"]) for r in rows])
    hi = np.array([float(r["ci_high"]) for r in rows])
    n = len(rows)
    if figsize is None:
        figsize=(8.2, max(4.0, 0.43*n+1.8))
    fig, ax = plt.subplots(figsize=figsize)
    y=np.arange(n)[::-1]
    ax.errorbar(ors, y, xerr=[ors-lo, hi-ors], fmt="o", capsize=3)
    ax.axvline(1.0, linestyle="--", linewidth=1)
    ax.set_yticks(y)
    ax.set_yticklabels(labels)
    ax.set_xlabel(xlabel)
    ax.set_title(title)
    ax.set_xlim(*xlim)
    ax.grid(axis="x", linestyle=":", alpha=0.35)
    fig.tight_layout()
    fig.savefig(OUT/f"{stem}.pdf", bbox_inches="tight")
    fig.savefig(OUT/f"{stem}.png", dpi=300, bbox_inches="tight")
    plt.close(fig)

forest("Figure2_ContinuousOR.csv",
       "Continuous associations with in-hospital mortality",
       "Odds ratio (95% CI)", (0.95,1.68), "Figure2_ContinuousAssociations")
forest("Figure3_eICU_Robustness.csv",
       "eICU-CRD robustness of the continuous FI-Lab association",
       "Odds ratio per 0.1 FI-Lab (95% CI)", (0.98,1.31), "Figure3_eICU_Robustness")
forest("FigureS6_Timing.csv",
       "Strict pre-ICU laboratory-timing sensitivity",
       "Odds ratio per 0.1 FI-Lab (95% CI)", (0.9,2.1), "FigureS6_TimingSensitivity")
forest("FigureS7_Availability.csv",
       "Laboratory-availability sensitivity",
       "Odds ratio per 0.1 FI-Lab (95% CI)", (0.95,1.67), "FigureS7_AvailableItemCount")
forest("FigureS8_Conventional.csv",
       "Independent conventional reference-range sensitivity",
       "Odds ratio per 0.1 FI-Lab (95% CI)", (0.95,1.9), "FigureS8_ConventionalRanges")
forest("FigureS9_Flexible.csv",
       "Flexible age/severity adjustment sensitivity",
       "Odds ratio per 0.1 FI-Lab (95% CI)", (0.95,1.67), "FigureS9_FlexibleAdjustment")
