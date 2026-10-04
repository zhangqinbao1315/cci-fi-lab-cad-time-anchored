source("00_helpers_FINAL.R")
locked <- readRDS(file.path(RESULT_DIR,"LOCKED_PRIMARY_COHORTS.rds"))
m <- locked$MIMIC
e <- locked$EICU
s <- locked$SICDB

# Correct final pre-ICU exports.
mp <- read_csv2(find_export("mimic_core12_preicu_selected.csv",69502))
ep <- read_csv2(find_export("eicu_core12_preicu_selected.csv",54181))
sp <- read_csv2(find_export("sicdb_core12_preicu_selected.csv",124226))

# Restrict to locked eligible cohorts before reconstruction.
mp <- mp %>%
  mutate(id_key=id_chr(stay_id)) %>%
  semi_join(m %>% select(id_key),by="id_key")

ep <- ep %>%
  mutate(id_key=id_chr(patientunitstayid)) %>%
  semi_join(e %>% select(id_key),by="id_key")

sp <- sp %>%
  mutate(id_key=id_chr(icu_id)) %>%
  semi_join(s %>% select(id_key),by="id_key")

mfi <- build_mimic_fi(mp) %>% rename(fi_strict=fi,n_strict=n_available)
efi <- build_fixed_fi(ep,"patientunitstayid") %>%
  rename(fi_strict=fi,n_strict=n_available)
sfi <- build_fixed_fi(sp,"icu_id") %>%
  rename(fi_strict=fi,n_strict=n_available)

mm <- m %>% left_join(mfi %>% select(id_key,fi_strict,n_strict),by="id_key")
ee <- e %>% left_join(efi %>% select(id_key,fi_strict,n_strict),by="id_key")
ss <- s %>% left_join(sfi %>% select(id_key,fi_strict,n_strict),by="id_key")

primary <- readr::read_csv(
  file.path(RESULT_DIR,"PRIMARY_LOCKED_FINAL.csv"),
  show_col_types=FALSE
) %>%
  mutate(analysis="S19_REFERENCE_PRIMARY_LOCKED")

strict <- bind_rows(
  fit_mimic_locked(mm,"fi_strict","S19_strict_-24h_to_0h"),
  fit_eicu_locked(ee,"fi_strict","S19_strict_-24h_to_0h"),
  fit_sicdb_locked(ss,"fi_strict","S19_strict_-24h_to_0h")
)

out <- bind_rows(primary,strict)
save_csv(out,"S19_TIMING_FINAL.csv")

qc <- tibble(
  database=c("MIMIC-IV","eICU-CRD","SICdb"),
  locked_eligible_rows=c(nrow(m),nrow(e),nrow(s)),
  strict_fi_ge9=c(
    sum(!is.na(mm$fi_strict)),
    sum(!is.na(ee$fi_strict)),
    sum(!is.na(ss$fi_strict))
  )
)
save_csv(qc,"S19_TIMING_QC.csv")
print(out)
