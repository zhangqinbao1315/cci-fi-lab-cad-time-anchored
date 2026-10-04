-- eICU PostgreSQL precheck
SELECT
    to_regclass('public.eicu_export_patient_final') AS cohort_table,
    to_regclass('eicu_crd.lab') AS lab_table,
    to_regclass('eicu_crd.patient') AS patient_table;

SELECT COUNT(*) AS cohort_rows,
       COUNT(DISTINCT patientunitstayid) AS cohort_stays
FROM public.eicu_export_patient_final;

SELECT column_name, data_type
FROM information_schema.columns
WHERE table_schema='eicu_crd'
  AND table_name='lab'
  AND column_name IN
      ('labid','patientunitstayid','labname','labresult',
       'labresultoffset','labresultrevisedoffset')
ORDER BY column_name;
