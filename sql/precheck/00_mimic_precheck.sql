-- MIMIC-IV PostgreSQL precheck
SELECT
    to_regclass('public.cad_mimic_index_lite') AS cohort_table,
    to_regclass('mimiciv_icu.icustays') AS icustays,
    to_regclass('mimiciv_hosp.labevents') AS labevents,
    to_regclass('mimiciv_hosp.d_labitems') AS d_labitems;

SELECT COUNT(*) AS cohort_rows,
       COUNT(DISTINCT stay_id) AS cohort_stays
FROM public.cad_mimic_index_lite;

SELECT column_name, data_type
FROM information_schema.columns
WHERE table_schema='mimiciv_hosp'
  AND table_name='labevents'
  AND column_name IN
      ('subject_id','hadm_id','itemid','charttime','valuenum',
       'ref_range_lower','ref_range_upper')
ORDER BY column_name;
