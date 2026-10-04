-- MIMIC / optional re-export for Stage 3
-- Export result grid as mimic_core12_main_selected.csv
SELECT
    stay_id, lab_item, lab_value, lower_ref, upper_ref, offset_min, source_label
FROM public.new9_mimic_main_selected
ORDER BY stay_id, lab_item;
