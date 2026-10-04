-- SICdb / Navicat export
-- Export result grid as sicdb_available_count.csv
SELECT
    icu_id,
    n_available,
    n_abnormal,
    fi_lab_recalc
FROM public.new9_sicdb_available_count
ORDER BY icu_id;
