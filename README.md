# CCI + FI-Lab CAD time-anchored analysis code

Code and aggregate verification outputs for:

**Time-anchored prognostic information from diagnosis-coded comorbidity and laboratory-derived deficit burden in older critically ill patients with coronary artery disease: a multicohort retrospective study**

This repository is the clean public archive for the current manuscript. It was
assembled from the authors' previously public primary-analysis repository plus
the exact code and final aggregate outputs used for the current eICU
time-anchored analyses.

## Repository scope

### Executable code included

1. **Primary exact reproduction**
   - `analysis/primary_exact_reproduction/`
   - exact archived scripts used for the locked MIMIC-IV, eICU-CRD and SICdb
     primary analyses.

2. **S19-S22 reconciliation / sensitivity analyses**
   - `analysis/s19_s22_reconciliation/`
   - exact archived scripts used for the earlier timing, availability,
     conventional-range and flexible-adjustment analyses.

3. **Current eICU time-anchored analyses**
   - `analysis/time_anchored_current/eicu_time_anchored_landmark_current.R`
   - reviewer-facing reproduction script for Supplementary S29, S30 and
     main-text Table 2.
   - `analysis/time_anchored_exact/` contains the exact archived V15 execution
     driver that generated the final S29/S30/true-landmark outputs.

### Aggregate verification included

- `verification/primary_and_s19_s22/`
- `verification/time_anchored/`
- `verification/current_submission/Additional_file_2_Source_Data.xlsx`

No patient-level MIMIC-IV, eICU-CRD or SICdb data are distributed.

## Important coverage note

The available archived code contains executable scripts for the primary
analyses, S19-S22 and the current S29/S30/true-landmark analyses. For several
other supplementary robustness analyses in the current submission, the public
archive contains their aggregate verification outputs in the Source Data
workbook but the available local archive does not contain a standalone
executable script for every analysis. This limitation is stated explicitly in
`docs/ANALYSIS_COVERAGE.md`; no missing script has been reconstructed or
fabricated.

## Current eICU time-anchored definitions

- FI-Lab: locked 12-domain common core
- time-anchored FI-Lab window: -24 h to ICU admission (0 h)
- evaluability: at least 9/12 domains
- CAD(t): structured diagnoses documented by time t
- CCI(t): Quan ICD-9/ICD-10 mapping using diagnoses documented by t
- myocardial-infarction Charlson component excluded
- thresholds / landmarks: 6, 24 and 36 h
- landmark risk sets: Hospital and stricter ICU
- landmark AUCs: apparent within-risk-set discrimination
- paired delta-AUC uncertainty: 1,000 hospital-cluster bootstrap replicates

## Re-running the time-anchored analysis

Restricted/local intermediate files are required and are not distributed:

- `04_eicu_analysis_cohort.csv`
- `062_eicu_diagnosis_tokens_final.csv`
- `063_eicu_core12_lab_long_final.csv`

For a readable reproduction script, see:

`analysis/time_anchored_current/eicu_time_anchored_landmark_current.R`

For the exact archived execution driver used for the final analysis, see:

`analysis/time_anchored_exact/run_exact_time_anchored_analysis.R`

## Data access

See `docs/DATA_ACCESS_AND_RESTRICTIONS.md`.

## FI-Lab domain-selection provenance

See `docs/FI_LAB_DOMAIN_SELECTION_FRAMEWORK.md`. The archive does not preserve
an exhaustive enumerated list of every early candidate laboratory domain, and
this repository does not reconstruct unrecorded excluded candidates.

## Version

Initial release of this clean repository: **v1.0.0**

License: MIT.
