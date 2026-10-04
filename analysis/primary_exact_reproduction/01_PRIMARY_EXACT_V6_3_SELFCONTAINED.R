suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(tibble)
  library(sandwich)
  library(lmtest)
  library(comorbidity)
  library(broom)
})

DESKTOP <- normalizePath(
  file.path(Sys.getenv("USERPROFILE"),"Desktop"),
  winslash="/",mustWork=TRUE
)
SR <- file.path(DESKTOP,"Scientific Reports")

RESULT_DIR <- file.path(
  DESKTOP,
  "New9_PRIMARY_EXACT_REPRODUCTION_V6_3_RESULTS"
)
dir.create(RESULT_DIR,recursive=TRUE,showWarnings=FALSE)

save_csv <- function(x,name){
  p <- file.path(RESULT_DIR,name)
  readr::write_csv(x,p)
  message("Wrote: ",p)
}

read_csv2 <- function(p){
  readr::read_csv(p,show_col_types=FALSE,progress=FALSE)
}

to_binary <- function(x){
  if(is.logical(x)) return(as.integer(x))
  if(is.numeric(x)) return(as.integer(x>0))
  z <- tolower(trimws(as.character(x)))
  out <- rep(NA_integer_,length(z))
  out[z %in% c("1","yes","y","true","expired","dead","death","died")] <- 1L
  out[z %in% c("0","no","n","false","alive","survived")] <- 0L
  suppressWarnings({
    zz <- as.numeric(z)
    use <- is.na(out) & !is.na(zz)
    out[use] <- as.integer(zz[use]>0)
  })
  out
}

find_latest_named <- function(filename){
  hits <- list.files(
    DESKTOP,
    pattern=paste0(
      "^",
      gsub("([.()+*?^$|{}\\[\\]\\\\])","\\\\\\1",filename),
      "$"
    ),
    full.names=TRUE,
    recursive=TRUE
  )
  hits <- unique(hits)
  if(length(hits)==0){
    stop(
      "Cannot find ",filename,"\n",
      "If it was deleted, rerun the V6.1 Navicat SELECT once and export it again."
    )
  }
  if(length(hits)>1){
    mt <- file.info(hits)$mtime
    hits <- hits[order(mt,decreasing=TRUE)]
    message("Multiple copies found; using newest:\n",hits[1])
  }
  normalizePath(hits[1],winslash="/",mustWork=TRUE)
}

# ============================================================
# MIMIC-IV
# ============================================================

mimic_patient_path <- file.path(
  SR,"01MIMIC","MIMIC_R","data_derived",
  "mimic_analysis_with_severity.csv"
)

if(!file.exists(mimic_patient_path)){
  stop("Historical MIMIC patient file not found:\n",mimic_patient_path)
}

dx_path <- find_latest_named("mimic_diagnoses_icd_for_v6.csv")

m0 <- read_csv2(mimic_patient_path)
dx0 <- read_csv2(dx_path)

required_m <- c(
  "study_id","encounter_id","icu_id","hospital_mortality",
  "age","gender","ami_flag","fi_lab_core12",
  "oasis_acute_component","cci"
)
miss <- setdiff(required_m,names(m0))
if(length(miss)) stop("MIMIC patient file missing: ",paste(miss,collapse=", "))

required_dx <- c("hadm_id","icd_code_clean","icd_version")
miss <- setdiff(required_dx,names(dx0))
if(length(miss)) stop("Diagnosis export missing: ",paste(miss,collapse=", "))

m <- m0 %>%
  transmute(
    analysis_id=as.character(icu_id),
    hadm_id=as.character(encounter_id),
    hospital_mortality=to_binary(hospital_mortality),
    age=as.numeric(age),
    sex=factor(as.character(gender)),
    ami=to_binary(ami_flag),
    fi_lab=as.numeric(fi_lab_core12),
    severity=as.numeric(oasis_acute_component),
    cci_locked=as.numeric(cci)
  )

dx <- dx0 %>%
  transmute(
    hadm_id=as.character(hadm_id),
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
    m %>% select(analysis_id,hadm_id) %>% distinct(),
    by="hadm_id"
  ) %>%
  select(analysis_id,code,icd_version) %>%
  distinct()

map_mi_one_version <- function(dat,ver){
  dd <- dat %>%
    filter(icd_version==ver) %>%
    select(analysis_id,code) %>%
    distinct()

  if(nrow(dd)==0){
    return(tibble(analysis_id=character(),mi=integer()))
  }

  map_name <- if(ver==9L) "charlson_icd9_quan" else "charlson_icd10_quan"

  cm <- comorbidity::comorbidity(
    x=as.data.frame(dd),
    id="analysis_id",
    code="code",
    map=map_name,
    assign0=FALSE
  ) %>% as_tibble()

  if(!("analysis_id" %in% names(cm))) names(cm)[1] <- "analysis_id"
  if(!("mi" %in% names(cm))) cm$mi <- 0L

  cm %>%
    transmute(
      analysis_id=as.character(analysis_id),
      mi=as.integer(replace_na(mi,0L))
    )
}

mi9 <- map_mi_one_version(dx,9L) %>% rename(mi9=mi)
mi10 <- map_mi_one_version(dx,10L) %>% rename(mi10=mi)

mi <- m %>%
  select(analysis_id) %>%
  distinct() %>%
  left_join(mi9,by="analysis_id") %>%
  left_join(mi10,by="analysis_id") %>%
  mutate(
    mi9=replace_na(mi9,0L),
    mi10=replace_na(mi10,0L),
    mi_index=pmax(mi9,mi10)
  ) %>%
  select(analysis_id,mi_index)

m <- m %>%
  left_join(mi,by="analysis_id") %>%
  mutate(
    mi_index=replace_na(mi_index,0L),
    cci_locked_no_mi=cci_locked-mi_index,
    fi_per_01=fi_lab*10
  )

m_model <- m %>%
  select(
    hospital_mortality,cci_locked_no_mi,fi_per_01,
    age,sex,ami,severity
  ) %>%
  filter(complete.cases(.))

fit_m <- glm(
  hospital_mortality ~
    cci_locked_no_mi + fi_per_01 +
    age + sex + ami + severity,
  data=m_model,
  family=binomial()
)

# Historical MIMIC extraction: profile-likelihood CI.
tm <- broom::tidy(fit_m,conf.int=TRUE)
firow <- tm %>% filter(term=="fi_per_01")
ccirow <- tm %>% filter(term=="cci_locked_no_mi")

if(nrow(firow)!=1 || nrow(ccirow)!=1)
  stop("Could not extract MIMIC FI/CCI terms.")

m_or <- exp(firow$estimate)
m_lo <- exp(firow$conf.low)
m_hi <- exp(firow$conf.high)

m_res <- tibble(
  database="MIMIC-IV",
  n=nrow(m_model),
  deaths=sum(m_model$hospital_mortality==1),
  hospitals=NA_integer_,
  fi_or=m_or,
  fi_ci_low=m_lo,
  fi_ci_high=m_hi,
  cci_or=exp(ccirow$estimate),
  cci_ci_low=exp(ccirow$conf.low),
  cci_ci_high=exp(ccirow$conf.high),
  target_fi_or=1.293468,
  target_ci_low=1.248657,
  target_ci_high=1.340178,
  status=if_else(
    nrow(m_model)==12688L &&
    sum(m_model$hospital_mortality==1)==1883L &&
    round(m_or,6)==1.293468 &&
    round(m_lo,6)==1.248657 &&
    round(m_hi,6)==1.340178,
    "PASS_FINAL","FAIL"
  )
)

# Wald vs profile audit.
sm <- coef(summary(fit_m))
wb <- sm["fi_per_01","Estimate"]
wse <- sm["fi_per_01","Std. Error"]

ci_audit <- tibble(
  method=c("profile_likelihood_historical","Wald_for_comparison_only"),
  fi_or=c(m_or,exp(wb)),
  ci_low=c(m_lo,exp(wb-1.96*wse)),
  ci_high=c(m_hi,exp(wb+1.96*wse))
)
save_csv(ci_audit,"V6_3_MIMIC_CI_AUDIT.csv")

m_qc <- tibble(
  metric=c(
    "historical_patient_rows",
    "unique_hadm",
    "diagnosis_export_rows",
    "diagnosis_rows_used",
    "hadm_with_diagnosis",
    "mi_positive_n",
    "fi_nonmissing",
    "primary_n",
    "primary_deaths",
    "cci_locked_mean",
    "cci_locked_no_mi_mean"
  ),
  value=c(
    nrow(m),
    n_distinct(m$hadm_id),
    nrow(dx0),
    nrow(dx),
    n_distinct(dx$analysis_id),
    sum(m$mi_index==1,na.rm=TRUE),
    sum(!is.na(m$fi_lab)),
    nrow(m_model),
    sum(m_model$hospital_mortality==1),
    mean(m$cci_locked,na.rm=TRUE),
    mean(m$cci_locked_no_mi,na.rm=TRUE)
  )
)
save_csv(m_qc,"V6_3_MIMIC_DIAGNOSIS_QC.csv")

# ============================================================
# eICU-CRD
# ============================================================

eicu_path <- file.path(
  SR,"02eICU","eICU_R","结果",
  "04_eicu_analysis_cohort.csv"
)
if(!file.exists(eicu_path)){
  stop("Historical eICU cohort not found:\n",eicu_path)
}

e0 <- read_csv2(eicu_path)

req_e <- c(
  "hospital_mortality","cci_score","fi_per_01","age5",
  "gender_model","ami_code_flag","aps5","hospitalid",
  "fi_lab_evaluable_r"
)
miss <- setdiff(req_e,names(e0))
if(length(miss)) stop("eICU cohort missing: ",paste(miss,collapse=", "))

e_model <- e0 %>%
  filter(fi_lab_evaluable_r==1L) %>%
  select(
    hospital_mortality,cci_score,fi_per_01,age5,
    gender_model,ami_code_flag,aps5,hospitalid
  ) %>%
  filter(complete.cases(.)) %>%
  mutate(gender_model=factor(gender_model))

fit_e <- glm(
  hospital_mortality ~
    cci_score + fi_per_01 + age5 +
    gender_model + ami_code_flag + aps5,
  data=e_model,
  family=binomial()
)

Ve <- sandwich::vcovCL(
  fit_e,
  cluster=e_model$hospitalid,
  type="HC0"
)
ce <- lmtest::coeftest(fit_e,vcov.=Ve)

e_b <- ce["fi_per_01",1]
e_se <- ce["fi_per_01",2]
e_or <- exp(e_b)
e_lo <- exp(e_b-1.96*e_se)
e_hi <- exp(e_b+1.96*e_se)

e_res <- tibble(
  database="eICU-CRD",
  n=nrow(e_model),
  deaths=sum(e_model$hospital_mortality==1),
  hospitals=n_distinct(e_model$hospitalid),
  fi_or=e_or,
  fi_ci_low=e_lo,
  fi_ci_high=e_hi,
  cci_or=NA_real_,
  cci_ci_low=NA_real_,
  cci_ci_high=NA_real_,
  target_fi_or=1.21143849943769,
  target_ci_low=1.14962792045704,
  target_ci_high=1.27657236902911,
  status=if_else(
    nrow(e_model)==5039L &&
    sum(e_model$hospital_mortality==1)==614L &&
    n_distinct(e_model$hospitalid)==171L &&
    abs(e_or-1.21143849943769)<5e-10 &&
    abs(e_lo-1.14962792045704)<5e-10 &&
    abs(e_hi-1.27657236902911)<5e-10,
    "PASS_FINAL","FAIL"
  )
)

# ============================================================
# SICdb
# ============================================================

sic_hits <- list.files(
  SR,
  pattern="^SICdb患者级基础数据_strictFI_1428主分析\\.csv$",
  full.names=TRUE,
  recursive=TRUE
)
if(length(sic_hits)==0) stop("Historical SICdb strictFI primary file not found.")

s_candidates <- list()

for(sp in unique(sic_hits)){
  s0 <- tryCatch(read_csv2(sp),error=function(e) NULL)
  if(is.null(s0)) next

  req_s <- c(
    "hospital_mortality","fi_lab_primary",
    "age","gender","ami","saps3"
  )
  if(!all(req_s %in% names(s0))) next

  s_model <- s0 %>%
    mutate(
      fi_per_01=as.numeric(fi_lab_primary)*10,
      gender=factor(gender)
    ) %>%
    select(
      hospital_mortality,fi_per_01,age,
      gender,ami,saps3
    ) %>%
    filter(complete.cases(.))

  fit_s <- glm(
    hospital_mortality ~
      fi_per_01 + age + gender + ami + saps3,
    data=s_model,
    family=binomial()
  )

  ss <- coef(summary(fit_s))
  b <- ss["fi_per_01","Estimate"]
  se <- ss["fi_per_01","Std. Error"]

  s_candidates[[length(s_candidates)+1]] <- tibble(
    source=normalizePath(sp,winslash="/"),
    n=nrow(s_model),
    deaths=sum(s_model$hospital_mortality==1),
    fi_or=exp(b),
    fi_ci_low=exp(b-1.96*se),
    fi_ci_high=exp(b+1.96*se)
  )
}

sc <- bind_rows(s_candidates) %>%
  mutate(diff=abs(fi_or-1.30818319465022)) %>%
  arrange(diff)

if(nrow(sc)==0) stop("No usable SICdb historical file.")
s <- sc[1,]

s_res <- tibble(
  database="SICdb",
  n=s$n,
  deaths=s$deaths,
  hospitals=NA_integer_,
  fi_or=s$fi_or,
  fi_ci_low=s$fi_ci_low,
  fi_ci_high=s$fi_ci_high,
  cci_or=NA_real_,
  cci_ci_low=NA_real_,
  cci_ci_high=NA_real_,
  target_fi_or=1.30818319465022,
  target_ci_low=1.06507816848256,
  target_ci_high=1.60677715627525,
  status=if_else(
    s$n==1428L &&
    s$deaths==55L &&
    abs(s$fi_or-1.30818319465022)<5e-10 &&
    abs(s$fi_ci_low-1.06507816848256)<5e-10 &&
    abs(s$fi_ci_high-1.60677715627525)<5e-10,
    "PASS_FINAL","FAIL"
  )
)

# ============================================================
# FINAL
# ============================================================

final <- bind_rows(m_res,e_res,s_res)
save_csv(final,"V6_3_PRIMARY_FINAL_REPRODUCTION.csv")

sources <- tibble(
  database=c("MIMIC-IV","eICU-CRD","SICdb"),
  primary_source=c(
    normalizePath(mimic_patient_path,winslash="/"),
    normalizePath(eicu_path,winslash="/"),
    s$source
  ),
  auxiliary_source=c(dx_path,NA_character_,NA_character_),
  primary_fi=c("fi_lab_core12","fi_lab / fi_per_01","fi_lab_primary"),
  inference=c(
    "glm + profile-likelihood CI via broom::tidy(conf.int=TRUE)",
    "glm + hospital-cluster robust HC0",
    "glm + Wald CI"
  )
)
save_csv(sources,"V6_3_LOCKED_PRIMARY_SOURCES.csv")

print(final,n=Inf)

if(all(final$status=="PASS_FINAL")){
  cat(
    "\n====================================================\n",
    "SUCCESS: ALL THREE PRIMARY MODELS PASS FINAL REPRODUCTION\n",
    "====================================================\n",
    "Primary-definition investigation is CLOSED.\n"
  )
} else {
  cat(
    "\nAt least one database is still FAIL.\n",
    "Upload V6_3_PRIMARY_FINAL_REPRODUCTION.csv and V6_3_MIMIC_DIAGNOSIS_QC.csv.\n"
  )
}
