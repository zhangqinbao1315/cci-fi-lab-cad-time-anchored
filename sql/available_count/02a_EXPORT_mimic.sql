-- MIMIC / Navicat export
-- Export result grid as mimic_available_count.csv
SELECT
    stay_id,
    n_available,
    n_abnormal,
    fi_lab_recalc
FROM public.new9_mimic_available_count
ORDER BY stay_id;
