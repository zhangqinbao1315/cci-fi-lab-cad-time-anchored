-- ============================================================
-- MIMIC-IV diagnosis export for V6.1 exact primary reproduction
-- ============================================================
--
-- Run this SELECT in Navicat.
-- Then use NAVICAT CLIENT-SIDE EXPORT to CSV.
--
-- Required output filename:
--   mimic_diagnoses_icd_for_v6.csv
--
-- Do NOT use PostgreSQL COPY.
--
-- Standard MIMIC-IV schema:
--   mimiciv_hosp.diagnoses_icd
--
-- If your schema is named "hosp" rather than "mimiciv_hosp",
-- change only the FROM line accordingly.

SELECT
    d.subject_id,
    d.hadm_id,
    d.seq_num,
    UPPER(
        REGEXP_REPLACE(
            TRIM(d.icd_code),
            '[^A-Za-z0-9]',
            '',
            'g'
        )
    ) AS icd_code_clean,
    d.icd_version
FROM mimiciv_hosp.diagnoses_icd AS d
WHERE d.hadm_id IS NOT NULL
  AND d.icd_code IS NOT NULL
  AND TRIM(d.icd_code) <> ''
  AND d.icd_version IN (9, 10)
ORDER BY d.hadm_id, d.seq_num;
