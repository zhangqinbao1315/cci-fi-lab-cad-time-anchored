-- MIMIC available-item count，基于Stage 1 main selected
DROP TABLE IF EXISTS public.new9_mimic_available_count;
CREATE TABLE public.new9_mimic_available_count AS
WITH x AS (
    SELECT
        stay_id,
        CASE
            WHEN lower_ref IS NULL OR upper_ref IS NULL THEN NULL
            WHEN lab_value < lower_ref OR lab_value > upper_ref THEN 1
            ELSE 0
        END AS abnormal
    FROM public.new9_mimic_main_selected
)
SELECT
    stay_id,
    COUNT(abnormal)::integer AS n_available,
    SUM(abnormal)::integer AS n_abnormal,
    CASE
      WHEN COUNT(abnormal)>=9
      THEN SUM(abnormal)::double precision/COUNT(abnormal)
      ELSE NULL
    END AS fi_lab_recalc
FROM x
GROUP BY stay_id;

SELECT n_available, COUNT(*) AS stays
FROM public.new9_mimic_available_count
GROUP BY n_available
ORDER BY n_available;
