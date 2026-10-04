-- SICdb available-item count，使用锁定外部固定区间
DROP TABLE IF EXISTS public.new9_sicdb_available_count;
CREATE TABLE public.new9_sicdb_available_count AS
WITH ranges(lab_item,lo,hi) AS (
 VALUES
 ('creatinine',0.5,1.2),('sodium',133,145),('potassium',3.3,5.1),
 ('chloride',96,108),('bicarbonate',22,32),('glucose',70,100),
 ('hemoglobin',13.7,17.5),('wbc',4,10),('platelet',150,400),
 ('calcium',8.4,10.3),('inr',0.9,1.1),('magnesium',1.6,2.6)
),
x AS (
 SELECT
   s.icu_id,
   CASE WHEN s.lab_value<r.lo OR s.lab_value>r.hi THEN 1 ELSE 0 END AS abnormal
 FROM public.new9_sicdb_main_selected s
 JOIN ranges r USING(lab_item)
)
SELECT
 icu_id,
 COUNT(*)::integer AS n_available,
 SUM(abnormal)::integer AS n_abnormal,
 CASE WHEN COUNT(*)>=9
      THEN SUM(abnormal)::double precision/COUNT(*)
      ELSE NULL END AS fi_lab_recalc
FROM x
GROUP BY icu_id;

SELECT n_available, COUNT(*) AS stays
FROM public.new9_sicdb_available_count
GROUP BY n_available
ORDER BY n_available;
