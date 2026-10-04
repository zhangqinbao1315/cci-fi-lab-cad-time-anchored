-- SICdb 1.0.8 PostgreSQL precheck
SELECT
    to_regclass('public.cases') AS cases_table,
    to_regclass('public.laboratory') AS laboratory_table;

SELECT COUNT(*) AS case_rows,
       COUNT(DISTINCT "CaseID") AS unique_cases
FROM public.cases;

SELECT table_name, column_name, data_type
FROM information_schema.columns
WHERE table_schema='public'
  AND table_name IN ('cases','laboratory')
  AND lower(column_name) IN
      ('caseid','icuoffset','laboratoryid','laboratoryvalue','offset')
ORDER BY table_name, column_name;
