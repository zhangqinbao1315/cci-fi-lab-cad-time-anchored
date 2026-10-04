-- ============================================================
-- SICdb 1.0.8 timing sensitivity, REVISED
-- PostgreSQL
-- ICU relative time = laboratory.Offset - cases.ICUOffset
-- SICdb offsets are treated as seconds.
-- Creates:
--   public.new9_sicdb_main_selected
--   public.new9_sicdb_preicu_selected
-- ============================================================

DROP TABLE IF EXISTS public.new9_sicdb_main_selected;
CREATE TABLE public.new9_sicdb_main_selected AS
WITH candidate AS (
    SELECT
        c."CaseID" AS icu_id,
        CASE
            WHEN l."LaboratoryID" IN (367,368) THEN 'creatinine'
            WHEN l."LaboratoryID" IN (469,686) THEN 'sodium'
            WHEN l."LaboratoryID" IN (463,685) THEN 'potassium'
            WHEN l."LaboratoryID" IN (450,683) THEN 'chloride'
            WHEN l."LaboratoryID" IN (456,666) THEN 'bicarbonate'
            WHEN l."LaboratoryID" IN (348,656) THEN 'glucose'
            WHEN l."LaboratoryID" IN (289,658) THEN 'hemoglobin'
            WHEN l."LaboratoryID" = 301 THEN 'wbc'
            WHEN l."LaboratoryID" = 314 THEN 'platelet'
            WHEN l."LaboratoryID" = 457 THEN 'calcium'
            WHEN l."LaboratoryID" = 3128 THEN 'inr'
            WHEN l."LaboratoryID" = 468 THEN 'magnesium'
        END AS lab_item,
        CASE
            WHEN l."LaboratoryID" = 457
                THEN l."LaboratoryValue"::double precision * 4.0078
            WHEN l."LaboratoryID" = 468
                THEN l."LaboratoryValue"::double precision * 2.4305
            ELSE l."LaboratoryValue"::double precision
        END AS lab_value,
        (l."Offset" - c."ICUOffset")::double precision / 60.0 AS offset_min,
        l."LaboratoryID" AS source_id,
        CASE
            WHEN l."LaboratoryID" IN
                (367,469,463,450,456,348,289,301,314,457,3128,468)
            THEN 1 ELSE 2
        END AS source_priority,
        l."Offset" AS raw_offset
    FROM public.cases AS c
    INNER JOIN public.laboratory AS l
        ON l."CaseID" = c."CaseID"
    WHERE l."LaboratoryValue" IS NOT NULL
      AND (l."Offset" - c."ICUOffset") BETWEEN -86400 AND 21600
      AND l."LaboratoryID" IN (
        367,368,
        469,686,
        463,685,
        450,683,
        456,666,
        348,656,
        289,658,
        301,314,457,3128,468
      )
),
ranked AS (
    SELECT
        candidate.*,
        ROW_NUMBER() OVER (
            PARTITION BY icu_id, lab_item
            ORDER BY
                ABS(offset_min) ASC,
                CASE WHEN offset_min >= 0 THEN 0 ELSE 1 END ASC,
                source_priority ASC,
                raw_offset ASC,
                lab_value ASC
        ) AS rn
    FROM candidate
    WHERE lab_item IS NOT NULL
)
SELECT
    icu_id,
    lab_item,
    lab_value,
    offset_min,
    source_id
FROM ranked
WHERE rn = 1;


DROP TABLE IF EXISTS public.new9_sicdb_preicu_selected;
CREATE TABLE public.new9_sicdb_preicu_selected AS
WITH candidate AS (
    SELECT
        c."CaseID" AS icu_id,
        CASE
            WHEN l."LaboratoryID" IN (367,368) THEN 'creatinine'
            WHEN l."LaboratoryID" IN (469,686) THEN 'sodium'
            WHEN l."LaboratoryID" IN (463,685) THEN 'potassium'
            WHEN l."LaboratoryID" IN (450,683) THEN 'chloride'
            WHEN l."LaboratoryID" IN (456,666) THEN 'bicarbonate'
            WHEN l."LaboratoryID" IN (348,656) THEN 'glucose'
            WHEN l."LaboratoryID" IN (289,658) THEN 'hemoglobin'
            WHEN l."LaboratoryID" = 301 THEN 'wbc'
            WHEN l."LaboratoryID" = 314 THEN 'platelet'
            WHEN l."LaboratoryID" = 457 THEN 'calcium'
            WHEN l."LaboratoryID" = 3128 THEN 'inr'
            WHEN l."LaboratoryID" = 468 THEN 'magnesium'
        END AS lab_item,
        CASE
            WHEN l."LaboratoryID" = 457
                THEN l."LaboratoryValue"::double precision * 4.0078
            WHEN l."LaboratoryID" = 468
                THEN l."LaboratoryValue"::double precision * 2.4305
            ELSE l."LaboratoryValue"::double precision
        END AS lab_value,
        (l."Offset" - c."ICUOffset")::double precision / 60.0 AS offset_min,
        l."LaboratoryID" AS source_id,
        CASE
            WHEN l."LaboratoryID" IN
                (367,469,463,450,456,348,289,301,314,457,3128,468)
            THEN 1 ELSE 2
        END AS source_priority,
        l."Offset" AS raw_offset
    FROM public.cases AS c
    INNER JOIN public.laboratory AS l
        ON l."CaseID" = c."CaseID"
    WHERE l."LaboratoryValue" IS NOT NULL
      AND (l."Offset" - c."ICUOffset") BETWEEN -86400 AND 0
      AND l."LaboratoryID" IN (
        367,368,
        469,686,
        463,685,
        450,683,
        456,666,
        348,656,
        289,658,
        301,314,457,3128,468
      )
),
ranked AS (
    SELECT
        candidate.*,
        ROW_NUMBER() OVER (
            PARTITION BY icu_id, lab_item
            ORDER BY
                ABS(offset_min) ASC,
                source_priority ASC,
                raw_offset DESC,
                lab_value ASC
        ) AS rn
    FROM candidate
    WHERE lab_item IS NOT NULL
)
SELECT
    icu_id,
    lab_item,
    lab_value,
    offset_min,
    source_id
FROM ranked
WHERE rn = 1;

-- QC
SELECT
    'main_-24h_to_+6h' AS dataset,
    COUNT(*) AS rows,
    COUNT(DISTINCT icu_id) AS stays
FROM public.new9_sicdb_main_selected
UNION ALL
SELECT
    'preicu_-24h_to_0h',
    COUNT(*),
    COUNT(DISTINCT icu_id)
FROM public.new9_sicdb_preicu_selected;
