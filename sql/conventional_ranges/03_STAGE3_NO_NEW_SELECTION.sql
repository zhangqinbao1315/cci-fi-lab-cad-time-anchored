-- Stage 3 不重新选择实验室值。
-- 目的：只改变 reference-range definition，避免同时改变measurement timing/selection。
--
-- R/03_conventional_ranges_models.R 直接读取：
-- C:/New9_export/mimic_core12_main_selected.csv
-- C:/New9_export/eicu_core12_main_selected.csv
-- C:/New9_export/sicdb_core12_main_selected.csv
--
-- 因此Stage 1的三个main CSV就是Stage 3需要的SQL导出数据。
-- 不需要再建新表。
--
-- 如想在数据库里确认main CSV对应表仍存在：
SELECT 'MIMIC' AS database, COUNT(*) AS rows
FROM public.new9_mimic_main_selected;
-- 注意：这句只在MIMIC数据库运行。
