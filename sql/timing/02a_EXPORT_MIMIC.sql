-- First create this folder in Windows:
-- C:\New9_export
--
-- Run in the MIMIC database.
-- PostgreSQL COPY writes from the DATABASE SERVER process.
-- If COPY reports permission denied, use the SELECT statements at the bottom
-- and export the result grid from Navicat/DBeaver instead.

COPY (
    SELECT stay_id, lab_item, lab_value, lower_ref, upper_ref, offset_min, source_label
    FROM public.new9_mimic_main_selected
    ORDER BY stay_id, lab_item
)
TO 'C:/New9_export/mimic_core12_main_selected.csv'
WITH (FORMAT CSV, HEADER TRUE, ENCODING 'UTF8');

COPY (
    SELECT stay_id, lab_item, lab_value, lower_ref, upper_ref, offset_min, source_label
    FROM public.new9_mimic_preicu_selected
    ORDER BY stay_id, lab_item
)
TO 'C:/New9_export/mimic_core12_preicu_selected.csv'
WITH (FORMAT CSV, HEADER TRUE, ENCODING 'UTF8');

-- GUI export fallback:
-- SELECT * FROM public.new9_mimic_main_selected ORDER BY stay_id, lab_item;
-- SELECT * FROM public.new9_mimic_preicu_selected ORDER BY stay_id, lab_item;
