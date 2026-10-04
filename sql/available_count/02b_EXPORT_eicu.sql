-- eICU / Navicat export
-- Export result grid as eicu_available_count.csv
SELECT
    patientunitstayid,
    n_available,
    n_abnormal,
    fi_lab_recalc
FROM public.new9_eicu_available_count
ORDER BY patientunitstayid;
