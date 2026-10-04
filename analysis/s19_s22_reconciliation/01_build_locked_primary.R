source("00_helpers_FINAL.R")
suppressPackageStartupMessages(library(comorbidity))

# ============================================================
# MIMIC historical full cohort + index-admission MI reconstruction
# ============================================================
m_patient <- file.path(
  SR,"01MIMIC","MIMIC_R","data_derived",
  "mimic_analysis_with_severity.csv"
)
if(!file.exists(m_patient)) stop("Missing historical MIMIC file: ",m_patient)

dx_path <- find_export("mimic_diagnoses_icd_for_v6.csv")
m0 <- read_csv2(m_patient)
dx0 <- read_csv2(dx_path)

need_cols(
  m0,
  c(
    "encounter_id","icu_id","hospital_mortality","age",
    "gender","ami_flag","fi_lab_core12","oasis_acute_component","cci"
  ),
  "historical MIMIC"
)

m <- m0 %>%
  transmute(
    id_key=id_chr(icu_id),
    hadm_id=id_chr(encounter_id),
    hospital_mortality=to_binary(hospital_mortality),
    age=as.numeric(age),
    sex=as.character(gender),
    ami=to_binary(ami_flag),
    fi_primary=as.numeric(fi_lab_core12),
    severity=as.numeric(oasis_acute_component),
    cci_locked=as.numeric(cci)
  )

dx <- dx0 %>%
  transmute(
    hadm_id=id_chr(hadm_id),
    code=as.character(icd_code_clean),
    icd_version=as.integer(icd_version)
  ) %>%
  filter(
    hadm_id %in% m$hadm_id,
    !is.na(code),code!="",
    icd_version %in% c(9L,10L)
  ) %>%
  distinct() %>%
  inner_join(
    m %>% select(id_key,hadm_id) %>% distinct(),
    by="hadm_id"
  ) %>%
  select(id_key,code,icd_version) %>%
  distinct()

map_mi <- function(dat,ver){
  dd <- dat %>% filter(icd_version==ver) %>% select(id_key,code) %>% distinct()
  if(!nrow(dd)) return(tibble(id_key=character(),mi=integer()))
  cm <- comorbidity::comorbidity(
    x=as.data.frame(dd),
    id="id_key",
    code="code",
    map=if(ver==9L) "charlson_icd9_quan" else "charlson_icd10_quan",
    assign0=FALSE
  ) %>% as_tibble()
  if(!("id_key" %in% names(cm))) names(cm)[1] <- "id_key"
  if(!("mi" %in% names(cm))) cm$mi <- 0L
  cm %>% transmute(id_key=id_chr(id_key),mi=as.integer(replace_na(mi,0L)))
}

mi9 <- map_mi(dx,9L) %>% rename(mi9=mi)
mi10 <- map_mi(dx,10L) %>% rename(mi10=mi)

mi <- m %>%
  select(id_key) %>% distinct() %>%
  left_join(mi9,by="id_key") %>%
  left_join(mi10,by="id_key") %>%
  mutate(
    mi9=replace_na(mi9,0L),
    mi10=replace_na(mi10,0L),
    mi_index=pmax(mi9,mi10)
  ) %>%
  select(id_key,mi_index)

m <- m %>%
  left_join(mi,by="id_key") %>%
  mutate(
    mi_index=replace_na(mi_index,0L),
    cci_locked_no_mi=cci_locked-mi_index
  )

# ============================================================
# eICU historical full cohort
# ============================================================
e_path <- file.path(
  SR,"02eICU","eICU_R","结果","04_eicu_analysis_cohort.csv"
)
if(!file.exists(e_path)) stop("Missing historical eICU cohort: ",e_path)
e0 <- read_csv2(e_path)

need_cols(
  e0,
  c(
    "patientunitstayid","hospitalid","hospital_mortality",
    "cci_score","fi_lab","fi_lab_evaluable_r",
    "fi_lab_n_available_r","age5","gender_model",
    "ami_code_flag","aps5","age_numeric","acutephysiologyscore"
  ),
  "historical eICU"
)

e <- e0 %>%
  mutate(
    id_key=id_chr(patientunitstayid),
    fi_primary=as.numeric(fi_lab),
    n_available=as.numeric(fi_lab_n_available_r)
  )

# ============================================================
# SICdb full 1638 historical cohort whose primary 1428 subset
# reproduces the frozen primary result.
# ============================================================
SICDB_FULL_MD5 <- "c0d7ae9bdf76d3a5f537d12407161e38"
s_path <- find_by_md5(
  SICDB_FULL_MD5,
  "SICdb full 1638 historical cohort"
)
s0 <- read_csv2(s_path)

need_cols(
  s0,
  c("icu_id","hospital_mortality","fi_lab_primary","age","gender","ami","saps3"),
  "historical SICdb"
)

s <- s0 %>%
  mutate(
    id_key=id_chr(icu_id),
    fi_primary=as.numeric(fi_lab_primary)
  )

locked <- list(
  MIMIC=m,
  EICU=e,
  SICDB=s,
  paths=list(
    MIMIC=m_patient,
    MIMIC_DIAG=dx_path,
    EICU=e_path,
    SICDB=s_path
  )
)

saveRDS(
  locked,
  file.path(RESULT_DIR,"LOCKED_PRIMARY_COHORTS.rds")
)

# Exact primary models.
primary <- bind_rows(
  fit_mimic_locked(m,"fi_primary","PRIMARY_LOCKED"),
  fit_eicu_locked(
    e %>% filter(fi_lab_evaluable_r==1),
    "fi_primary","PRIMARY_LOCKED"
  ),
  fit_sicdb_locked(s,"fi_primary","PRIMARY_LOCKED")
)

save_csv(primary,"PRIMARY_LOCKED_FINAL.csv")

targets <- tibble::tribble(
  ~database,~n_target,~deaths_target,~hosp_target,~or_target,~lo_target,~hi_target,
  "MIMIC-IV",12688L,1883L,NA_integer_,
  1.293468158229067,1.248657273811780,1.340177687175479,
  "eICU-CRD",5039L,614L,171L,
  1.211438499437693,1.149627920457038,1.276572369029109,
  "SICdb",1428L,55L,NA_integer_,
  1.308183194650221,1.065078168482560,1.606777156275248
)

qc <- primary %>%
  left_join(targets,by="database") %>%
  mutate(
    pass=(
      n==n_target &
      deaths==deaths_target &
      (is.na(hosp_target)|hospitals==hosp_target) &
      abs(or_per_0_1-or_target)<1e-10 &
      abs(ci_low-lo_target)<1e-10 &
      abs(ci_high-hi_target)<1e-10
    )
  )

save_csv(qc,"PRIMARY_LOCK_QC.csv")
print(primary)

if(!all(qc$pass)){
  stop("PRIMARY LOCK FAILED. Do not continue.")
}

cat("\nPRIMARY LOCK PASSED FOR ALL THREE DATABASES.\n")
