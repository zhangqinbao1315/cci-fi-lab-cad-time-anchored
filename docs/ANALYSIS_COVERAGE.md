# Current manuscript analysis coverage

This document states what the public archive does and does not contain.

## Executable coverage available in this repository

| Manuscript component | Executable code |
|---|---|
| Locked primary MIMIC-IV/eICU-CRD/SICdb analyses | `analysis/primary_exact_reproduction/` |
| S19-S22 archived timing / availability / conventional-range / flexible-adjustment analyses | `analysis/s19_s22_reconciliation/` |
| S29 threshold-specific eICU diagnosis availability | `analysis/time_anchored_current/` and exact driver under `analysis/time_anchored_exact/` |
| S30 fixed 6-h cohort decomposition | same current/exact time-anchored code |
| Main-text Table 2 true 6/24/36-h landmarks | same current/exact time-anchored code |

## Aggregate-verification-only coverage

The current Source Data workbook contains aggregate results for additional
supplementary sensitivity analyses, including the current S23-S31 set. The
available archived code supplied for this repository does **not** contain a
standalone executable script for every one of those supplementary analyses.

Accordingly:

- this repository does not claim complete executable coverage where the actual
  script is absent;
- missing scripts have not been reconstructed from result tables;
- the aggregate numerical outputs remain publicly auditable in
  `verification/current_submission/Additional_file_2_Source_Data.xlsx`.

This distinction is intentional and protects the provenance of the analysis.
