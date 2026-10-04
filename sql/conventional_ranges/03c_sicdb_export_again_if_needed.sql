-- SICdb / optional re-export for Stage 3
-- Export result grid as sicdb_core12_main_selected.csv
SELECT
    icu_id, lab_item, lab_value, offset_min, source_id
FROM public.new9_sicdb_main_selected
ORDER BY icu_id, lab_item;
