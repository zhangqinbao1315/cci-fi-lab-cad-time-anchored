-- First create this folder in Windows:
-- C:\New9_export
--
-- Run in the eICU database.

COPY (
    SELECT patientunitstayid, lab_item, lab_value, offset_min, source_label
    FROM public.new9_eicu_main_selected
    ORDER BY patientunitstayid, lab_item
)
TO 'C:/New9_export/eicu_core12_main_selected.csv'
WITH (FORMAT CSV, HEADER TRUE, ENCODING 'UTF8');

COPY (
    SELECT patientunitstayid, lab_item, lab_value, offset_min, source_label
    FROM public.new9_eicu_preicu_selected
    ORDER BY patientunitstayid, lab_item
)
TO 'C:/New9_export/eicu_core12_preicu_selected.csv'
WITH (FORMAT CSV, HEADER TRUE, ENCODING 'UTF8');

-- GUI export fallback:
-- SELECT * FROM public.new9_eicu_main_selected ORDER BY patientunitstayid, lab_item;
-- SELECT * FROM public.new9_eicu_preicu_selected ORDER BY patientunitstayid, lab_item;
