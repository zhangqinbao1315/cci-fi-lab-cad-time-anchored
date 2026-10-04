-- First create this folder in Windows:
-- C:\New9_export
--
-- Run in the SICdb database.

COPY (
    SELECT icu_id, lab_item, lab_value, offset_min, source_id
    FROM public.new9_sicdb_main_selected
    ORDER BY icu_id, lab_item
)
TO 'C:/New9_export/sicdb_core12_main_selected.csv'
WITH (FORMAT CSV, HEADER TRUE, ENCODING 'UTF8');

COPY (
    SELECT icu_id, lab_item, lab_value, offset_min, source_id
    FROM public.new9_sicdb_preicu_selected
    ORDER BY icu_id, lab_item
)
TO 'C:/New9_export/sicdb_core12_preicu_selected.csv'
WITH (FORMAT CSV, HEADER TRUE, ENCODING 'UTF8');

-- GUI export fallback:
-- SELECT * FROM public.new9_sicdb_main_selected ORDER BY icu_id, lab_item;
-- SELECT * FROM public.new9_sicdb_preicu_selected ORDER BY icu_id, lab_item;
