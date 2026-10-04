-- ============================================================
-- eICU timing sensitivity, REVISED
-- PostgreSQL
-- Creates:
--   public.new9_eicu_main_selected   : -1440 to +360 min
--   public.new9_eicu_preicu_selected : -1440 to 0 min
-- ============================================================

DROP TABLE IF EXISTS public.new9_eicu_main_selected;
CREATE TABLE public.new9_eicu_main_selected AS
WITH cohort AS (
    SELECT DISTINCT patientunitstayid
    FROM public.eicu_export_patient_final
),
raw AS (
    SELECT
        l.patientunitstayid,
        l.labname,
        l.labresult::double precision AS lab_value,
        l.labresultoffset::double precision AS offset_min,
        COALESCE(l.labresultrevisedoffset, l.labresultoffset) AS revised_offset,
        l.labid
    FROM eicu_crd.lab AS l
    INNER JOIN cohort AS c
        ON c.patientunitstayid = l.patientunitstayid
    WHERE l.labresult IS NOT NULL
      AND l.labresultoffset BETWEEN -1440 AND 360
),
dedup AS (
    SELECT *
    FROM (
        SELECT
            raw.*,
            ROW_NUMBER() OVER (
                PARTITION BY patientunitstayid, labname, offset_min
                ORDER BY revised_offset DESC, labid DESC
            ) AS rn_revision
        FROM raw
    ) AS x
    WHERE rn_revision = 1
),
mapped AS (
    SELECT
        patientunitstayid,
        CASE
            WHEN lower(trim(labname)) = 'creatinine' THEN 'creatinine'
            WHEN lower(trim(labname)) = 'sodium' THEN 'sodium'
            WHEN lower(trim(labname)) = 'potassium' THEN 'potassium'
            WHEN lower(trim(labname)) = 'chloride' THEN 'chloride'
            WHEN lower(trim(labname)) IN ('bicarbonate','total co2','co2') THEN 'bicarbonate'
            WHEN lower(trim(labname)) = 'glucose' THEN 'glucose'
            WHEN lower(trim(labname)) = 'hgb' THEN 'hemoglobin'
            WHEN lower(trim(labname)) = 'wbc x 1000' THEN 'wbc'
            WHEN lower(trim(labname)) = 'platelets x 1000' THEN 'platelet'
            WHEN lower(trim(labname)) = 'calcium' THEN 'calcium'
            WHEN lower(trim(labname)) = 'pt - inr' THEN 'inr'
            WHEN lower(trim(labname)) = 'magnesium' THEN 'magnesium'
        END AS lab_item,
        lab_value,
        offset_min,
        labname AS source_label,
        CASE
            WHEN lower(trim(labname)) = 'bicarbonate' THEN 1
            WHEN lower(trim(labname)) = 'total co2' THEN 2
            WHEN lower(trim(labname)) = 'co2' THEN 3
            ELSE 1
        END AS source_priority,
        revised_offset,
        labid
    FROM dedup
),
bounded AS (
    SELECT *
    FROM mapped
    WHERE lab_item IS NOT NULL
      AND (
           (lab_item='creatinine'  AND lab_value BETWEEN 0.1 AND 30)
        OR (lab_item='sodium'      AND lab_value BETWEEN 90 AND 200)
        OR (lab_item='potassium'   AND lab_value BETWEEN 1 AND 10)
        OR (lab_item='chloride'    AND lab_value BETWEEN 60 AND 150)
        OR (lab_item='bicarbonate' AND lab_value BETWEEN 5 AND 60)
        OR (lab_item='glucose'     AND lab_value BETWEEN 10 AND 2000)
        OR (lab_item='hemoglobin'  AND lab_value BETWEEN 2 AND 25)
        OR (lab_item='wbc'         AND lab_value BETWEEN 0.1 AND 200)
        OR (lab_item='platelet'    AND lab_value BETWEEN 1 AND 2000)
        OR (lab_item='calcium'     AND lab_value BETWEEN 3 AND 20)
        OR (lab_item='inr'         AND lab_value BETWEEN 0.5 AND 20)
        OR (lab_item='magnesium'   AND lab_value BETWEEN 0.2 AND 10)
      )
),
ranked AS (
    SELECT
        bounded.*,
        ROW_NUMBER() OVER (
            PARTITION BY patientunitstayid, lab_item
            ORDER BY
                ABS(offset_min) ASC,
                CASE WHEN offset_min >= 0 THEN 0 ELSE 1 END ASC,
                source_priority ASC,
                revised_offset DESC,
                labid DESC
        ) AS rn
    FROM bounded
)
SELECT
    patientunitstayid,
    lab_item,
    lab_value,
    offset_min,
    source_label
FROM ranked
WHERE rn = 1;


DROP TABLE IF EXISTS public.new9_eicu_preicu_selected;
CREATE TABLE public.new9_eicu_preicu_selected AS
WITH cohort AS (
    SELECT DISTINCT patientunitstayid
    FROM public.eicu_export_patient_final
),
raw AS (
    SELECT
        l.patientunitstayid,
        l.labname,
        l.labresult::double precision AS lab_value,
        l.labresultoffset::double precision AS offset_min,
        COALESCE(l.labresultrevisedoffset, l.labresultoffset) AS revised_offset,
        l.labid
    FROM eicu_crd.lab AS l
    INNER JOIN cohort AS c
        ON c.patientunitstayid = l.patientunitstayid
    WHERE l.labresult IS NOT NULL
      AND l.labresultoffset BETWEEN -1440 AND 0
),
dedup AS (
    SELECT *
    FROM (
        SELECT
            raw.*,
            ROW_NUMBER() OVER (
                PARTITION BY patientunitstayid, labname, offset_min
                ORDER BY revised_offset DESC, labid DESC
            ) AS rn_revision
        FROM raw
    ) AS x
    WHERE rn_revision = 1
),
mapped AS (
    SELECT
        patientunitstayid,
        CASE
            WHEN lower(trim(labname)) = 'creatinine' THEN 'creatinine'
            WHEN lower(trim(labname)) = 'sodium' THEN 'sodium'
            WHEN lower(trim(labname)) = 'potassium' THEN 'potassium'
            WHEN lower(trim(labname)) = 'chloride' THEN 'chloride'
            WHEN lower(trim(labname)) IN ('bicarbonate','total co2','co2') THEN 'bicarbonate'
            WHEN lower(trim(labname)) = 'glucose' THEN 'glucose'
            WHEN lower(trim(labname)) = 'hgb' THEN 'hemoglobin'
            WHEN lower(trim(labname)) = 'wbc x 1000' THEN 'wbc'
            WHEN lower(trim(labname)) = 'platelets x 1000' THEN 'platelet'
            WHEN lower(trim(labname)) = 'calcium' THEN 'calcium'
            WHEN lower(trim(labname)) = 'pt - inr' THEN 'inr'
            WHEN lower(trim(labname)) = 'magnesium' THEN 'magnesium'
        END AS lab_item,
        lab_value,
        offset_min,
        labname AS source_label,
        CASE
            WHEN lower(trim(labname)) = 'bicarbonate' THEN 1
            WHEN lower(trim(labname)) = 'total co2' THEN 2
            WHEN lower(trim(labname)) = 'co2' THEN 3
            ELSE 1
        END AS source_priority,
        revised_offset,
        labid
    FROM dedup
),
bounded AS (
    SELECT *
    FROM mapped
    WHERE lab_item IS NOT NULL
      AND (
           (lab_item='creatinine'  AND lab_value BETWEEN 0.1 AND 30)
        OR (lab_item='sodium'      AND lab_value BETWEEN 90 AND 200)
        OR (lab_item='potassium'   AND lab_value BETWEEN 1 AND 10)
        OR (lab_item='chloride'    AND lab_value BETWEEN 60 AND 150)
        OR (lab_item='bicarbonate' AND lab_value BETWEEN 5 AND 60)
        OR (lab_item='glucose'     AND lab_value BETWEEN 10 AND 2000)
        OR (lab_item='hemoglobin'  AND lab_value BETWEEN 2 AND 25)
        OR (lab_item='wbc'         AND lab_value BETWEEN 0.1 AND 200)
        OR (lab_item='platelet'    AND lab_value BETWEEN 1 AND 2000)
        OR (lab_item='calcium'     AND lab_value BETWEEN 3 AND 20)
        OR (lab_item='inr'         AND lab_value BETWEEN 0.5 AND 20)
        OR (lab_item='magnesium'   AND lab_value BETWEEN 0.2 AND 10)
      )
),
ranked AS (
    SELECT
        bounded.*,
        ROW_NUMBER() OVER (
            PARTITION BY patientunitstayid, lab_item
            ORDER BY
                ABS(offset_min) ASC,
                source_priority ASC,
                revised_offset DESC,
                labid DESC
        ) AS rn
    FROM bounded
)
SELECT
    patientunitstayid,
    lab_item,
    lab_value,
    offset_min,
    source_label
FROM ranked
WHERE rn = 1;

-- QC
SELECT
    'main_-24h_to_+6h' AS dataset,
    COUNT(*) AS rows,
    COUNT(DISTINCT patientunitstayid) AS stays
FROM public.new9_eicu_main_selected
UNION ALL
SELECT
    'preicu_-24h_to_0h',
    COUNT(*),
    COUNT(DISTINCT patientunitstayid)
FROM public.new9_eicu_preicu_selected;
