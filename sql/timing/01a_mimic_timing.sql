-- ============================================================
-- MIMIC-IV timing sensitivity, REVISED
-- PostgreSQL
-- Creates two ordinary tables only:
--   public.new9_mimic_main_selected   : -24h to +6h
--   public.new9_mimic_preicu_selected : -24h to 0h
-- ============================================================

DROP TABLE IF EXISTS public.new9_mimic_main_selected;
CREATE TABLE public.new9_mimic_main_selected AS
WITH cohort AS (
    SELECT DISTINCT
        c.stay_id,
        i.subject_id,
        i.hadm_id,
        i.intime
    FROM public.cad_mimic_index_lite AS c
    INNER JOIN mimiciv_icu.icustays AS i
        ON i.stay_id = c.stay_id
),
candidate AS (
    SELECT
        c.stay_id,
        CASE
            WHEN d.label = 'Creatinine' THEN 'creatinine'
            WHEN d.label = 'Sodium' THEN 'sodium'
            WHEN d.label = 'Potassium' THEN 'potassium'
            WHEN d.label = 'Chloride' THEN 'chloride'
            WHEN d.label = 'Bicarbonate' THEN 'bicarbonate'
            WHEN d.label = 'Glucose' THEN 'glucose'
            WHEN d.label = 'Hemoglobin' THEN 'hemoglobin'
            WHEN d.label = 'White Blood Cells' THEN 'wbc'
            WHEN d.label = 'Platelet Count' THEN 'platelet'
            WHEN d.label = 'Calcium, Total' THEN 'calcium'
            WHEN d.label = 'INR(PT)' THEN 'inr'
            WHEN d.label = 'Magnesium' THEN 'magnesium'
        END AS lab_item,
        le.valuenum::double precision AS lab_value,
        le.ref_range_lower::double precision AS lower_ref,
        le.ref_range_upper::double precision AS upper_ref,
        EXTRACT(EPOCH FROM (le.charttime - c.intime))/60.0 AS offset_min,
        d.label AS source_label,
        le.charttime,
        le.itemid
    FROM cohort AS c
    INNER JOIN mimiciv_hosp.labevents AS le
        ON le.subject_id = c.subject_id
       AND le.hadm_id = c.hadm_id
    INNER JOIN mimiciv_hosp.d_labitems AS d
        ON d.itemid = le.itemid
    WHERE le.valuenum IS NOT NULL
      AND le.charttime >= c.intime - INTERVAL '24 hour'
      AND le.charttime <= c.intime + INTERVAL '6 hour'
      AND d.label IN (
        'Creatinine',
        'Sodium',
        'Potassium',
        'Chloride',
        'Bicarbonate',
        'Glucose',
        'Hemoglobin',
        'White Blood Cells',
        'Platelet Count',
        'Calcium, Total',
        'INR(PT)',
        'Magnesium'
      )
),
ranked AS (
    SELECT
        candidate.*,
        ROW_NUMBER() OVER (
            PARTITION BY stay_id, lab_item
            ORDER BY
                ABS(offset_min) ASC,
                CASE WHEN offset_min >= 0 THEN 0 ELSE 1 END ASC,
                charttime ASC,
                itemid ASC,
                lab_value ASC
        ) AS rn
    FROM candidate
    WHERE lab_item IS NOT NULL
)
SELECT
    stay_id,
    lab_item,
    lab_value,
    lower_ref,
    upper_ref,
    offset_min,
    source_label
FROM ranked
WHERE rn = 1;


DROP TABLE IF EXISTS public.new9_mimic_preicu_selected;
CREATE TABLE public.new9_mimic_preicu_selected AS
WITH cohort AS (
    SELECT DISTINCT
        c.stay_id,
        i.subject_id,
        i.hadm_id,
        i.intime
    FROM public.cad_mimic_index_lite AS c
    INNER JOIN mimiciv_icu.icustays AS i
        ON i.stay_id = c.stay_id
),
candidate AS (
    SELECT
        c.stay_id,
        CASE
            WHEN d.label = 'Creatinine' THEN 'creatinine'
            WHEN d.label = 'Sodium' THEN 'sodium'
            WHEN d.label = 'Potassium' THEN 'potassium'
            WHEN d.label = 'Chloride' THEN 'chloride'
            WHEN d.label = 'Bicarbonate' THEN 'bicarbonate'
            WHEN d.label = 'Glucose' THEN 'glucose'
            WHEN d.label = 'Hemoglobin' THEN 'hemoglobin'
            WHEN d.label = 'White Blood Cells' THEN 'wbc'
            WHEN d.label = 'Platelet Count' THEN 'platelet'
            WHEN d.label = 'Calcium, Total' THEN 'calcium'
            WHEN d.label = 'INR(PT)' THEN 'inr'
            WHEN d.label = 'Magnesium' THEN 'magnesium'
        END AS lab_item,
        le.valuenum::double precision AS lab_value,
        le.ref_range_lower::double precision AS lower_ref,
        le.ref_range_upper::double precision AS upper_ref,
        EXTRACT(EPOCH FROM (le.charttime - c.intime))/60.0 AS offset_min,
        d.label AS source_label,
        le.charttime,
        le.itemid
    FROM cohort AS c
    INNER JOIN mimiciv_hosp.labevents AS le
        ON le.subject_id = c.subject_id
       AND le.hadm_id = c.hadm_id
    INNER JOIN mimiciv_hosp.d_labitems AS d
        ON d.itemid = le.itemid
    WHERE le.valuenum IS NOT NULL
      AND le.charttime >= c.intime - INTERVAL '24 hour'
      AND le.charttime <= c.intime
      AND d.label IN (
        'Creatinine',
        'Sodium',
        'Potassium',
        'Chloride',
        'Bicarbonate',
        'Glucose',
        'Hemoglobin',
        'White Blood Cells',
        'Platelet Count',
        'Calcium, Total',
        'INR(PT)',
        'Magnesium'
      )
),
ranked AS (
    SELECT
        candidate.*,
        ROW_NUMBER() OVER (
            PARTITION BY stay_id, lab_item
            ORDER BY
                ABS(offset_min) ASC,
                charttime DESC,
                itemid ASC,
                lab_value ASC
        ) AS rn
    FROM candidate
    WHERE lab_item IS NOT NULL
)
SELECT
    stay_id,
    lab_item,
    lab_value,
    lower_ref,
    upper_ref,
    offset_min,
    source_label
FROM ranked
WHERE rn = 1;

-- QC
SELECT
    'main_-24h_to_+6h' AS dataset,
    COUNT(*) AS rows,
    COUNT(DISTINCT stay_id) AS stays
FROM public.new9_mimic_main_selected
UNION ALL
SELECT
    'preicu_-24h_to_0h',
    COUNT(*),
    COUNT(DISTINCT stay_id)
FROM public.new9_mimic_preicu_selected;
