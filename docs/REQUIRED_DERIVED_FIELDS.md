# Required derived patient-level fields

The final public scripts expect locally generated patient-level analysis objects with the following fields.

## MIMIC-IV historical primary input

Required fields:
- `encounter_id` (index hospital admission identifier)
- `icu_id`
- `hospital_mortality`
- `age`
- `gender`
- `ami_flag`
- `fi_lab_core12`
- `oasis_acute_component`
- `cci`

The final code reconstructs the index-admission Charlson MI component from diagnosis codes and derives `cci_locked_no_mi = cci - mi_index`.

## eICU-CRD historical primary input

Required fields:
- `patientunitstayid`
- `hospitalid`
- `hospital_mortality`
- `cci_score`
- `fi_lab`
- `fi_lab_evaluable_r`
- `fi_lab_n_available_r`
- `age5`
- `gender_model`
- `ami_code_flag`
- `aps5`
- `age_numeric`
- `acutephysiologyscore`

Primary inference uses hospital-cluster robust HC0 covariance.

## SICdb historical primary/full input

Required fields:
- `icu_id`
- `hospital_mortality`
- `fi_lab_primary`
- `age`
- `gender`
- `ami`
- `saps3`

No patient-level derived input is included in the public repository.
