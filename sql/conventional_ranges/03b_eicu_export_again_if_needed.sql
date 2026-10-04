-- eICU / optional re-export for Stage 3
-- Export result grid as eicu_core12_main_selected.csv
SELECT
    patientunitstayid, lab_item, lab_value, offset_min, source_label
FROM public.new9_eicu_main_selected
ORDER BY patientunitstayid, lab_item;
