suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(purrr)
  library(tibble)
  library(sandwich)
  library(lmtest)
  library(broom)
})

DESKTOP <- normalizePath(
  file.path(Sys.getenv("USERPROFILE"),"Desktop"),
  winslash="/",mustWork=TRUE
)
SR <- file.path(DESKTOP,"Scientific Reports")

RESULT_DIR <- file.path(
  DESKTOP,
  "New9_MASTER_RECONCILIATION_V7_RESULTS"
)
dir.create(RESULT_DIR,recursive=TRUE,showWarnings=FALSE)

save_csv <- function(x,name){
  p <- file.path(RESULT_DIR,name)
  readr::write_csv(x,p)
  message("Wrote: ",p)
}

id_chr <- function(x) as.character(x)

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

read_csv2 <- function(p){
  readr::read_csv(p,show_col_types=FALSE,progress=FALSE)
}

need_cols <- function(d,cols,label="data"){
  miss <- setdiff(cols,names(d))
  if(length(miss)) stop(label," missing: ",paste(miss,collapse=", "))
  invisible(TRUE)
}

# ------------------------------------------------------------
# Find exact historical/SQL exports robustly.
# If expected_rows is supplied, prefer exact row-count match.
# Identical-MD5 duplicates are harmless.
# ------------------------------------------------------------
find_export <- function(filename,expected_rows=NULL){
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

  if(length(hits)==0) stop("Required export not found: ",filename)

  info <- lapply(hits,function(p){
    d <- tryCatch(readr::read_csv(p,show_col_types=FALSE,progress=FALSE),
                  error=function(e) NULL)
    tibble(
      path=normalizePath(p,winslash="/",mustWork=FALSE),
      rows=if(is.null(d)) NA_integer_ else nrow(d),
      md5=tryCatch(unname(tools::md5sum(p)),error=function(e) NA_character_),
      mtime=as.character(file.info(p)$mtime)
    )
  }) |> bind_rows()

  save_csv(
    info,
    paste0("INPUT_CANDIDATES_",gsub("[^A-Za-z0-9]+","_",filename),".csv")
  )

  ok <- info
  if(!is.null(expected_rows)){
    hit_rows <- ok %>% filter(rows==expected_rows)
    if(nrow(hit_rows)>0) ok <- hit_rows
  }

  ok <- ok %>% filter(!is.na(rows))
  if(nrow(ok)==0) stop("No readable candidate for ",filename)

  # If remaining contents are identical, choose shortest path.
  if(n_distinct(ok$md5,na.rm=TRUE)==1){
    chosen <- ok$path[which.min(nchar(ok$path))]
  } else {
    # Prefer newest among row-count-matched candidates.
    mt <- file.info(ok$path)$mtime
    chosen <- ok$path[order(mt,decreasing=TRUE)][1]
    warning(
      "Multiple non-identical candidates for ",filename,
      "; using newest matching candidate:\n",chosen
    )
  }

  message("Using ",filename,":\n",chosen)
  chosen
}

find_by_md5 <- function(target_md5,label){
  hits <- list.files(
    DESKTOP,pattern="\\.csv$",full.names=TRUE,
    recursive=TRUE,ignore.case=TRUE
  )
  good <- c()
  for(p in hits){
    md <- tryCatch(unname(tools::md5sum(p)),error=function(e) NA_character_)
    if(!is.na(md) && identical(tolower(md),tolower(target_md5)))
      good <- c(good,normalizePath(p,winslash="/"))
  }
  good <- unique(good)
  if(!length(good)) stop(label," not found by MD5: ",target_md5)
  good[[which.min(nchar(good))]]
}

# ------------------------------------------------------------
# Locked reference intervals for eICU/SICdb primary FI.
# These are the previously frozen MIMIC-median external limits.
# ------------------------------------------------------------
locked_ranges <- tibble::tribble(
  ~lab_item,~lo,~hi,
  "creatinine",0.5,1.2,
  "sodium",133,145,
  "potassium",3.3,5.1,
  "chloride",96,108,
  "bicarbonate",22,32,
  "glucose",70,100,
  "hemoglobin",13.7,17.5,
  "wbc",4,10,
  "platelet",150,400,
  "calcium",8.4,10.3,
  "inr",0.9,1.1,
  "magnesium",1.6,2.6
)

norm_sex <- function(x){
  z <- toupper(trimws(as.character(x)))
  case_when(
    z %in% c("M","MALE","MAN") ~ "Male",
    z %in% c("F","FEMALE","WOMAN") ~ "Female",
    TRUE ~ "Unknown"
  )
}

# ------------------------------------------------------------
# FI reconstruction for timing analyses.
# ------------------------------------------------------------
build_mimic_fi <- function(labs,min_n=9){
  need_cols(
    labs,
    c("stay_id","lab_item","lab_value","lower_ref","upper_ref"),
    "MIMIC long labs"
  )
  labs %>%
    mutate(
      id_key=id_chr(stay_id),
      abnormal=case_when(
        is.na(lab_value)|is.na(lower_ref)|is.na(upper_ref) ~ NA_integer_,
        lab_value<lower_ref | lab_value>upper_ref ~ 1L,
        TRUE ~ 0L
      )
    ) %>%
    group_by(id_key) %>%
    summarise(
      n_available=sum(!is.na(abnormal)),
      n_abnormal=sum(abnormal,na.rm=TRUE),
      fi=if_else(n_available>=min_n,n_abnormal/n_available,NA_real_),
      .groups="drop"
    )
}

build_fixed_fi <- function(labs,id_col,min_n=9){
  need_cols(labs,c(id_col,"lab_item","lab_value"),"external long labs")
  labs %>%
    mutate(id_key=id_chr(.data[[id_col]])) %>%
    inner_join(locked_ranges,by="lab_item") %>%
    mutate(abnormal=if_else(lab_value<lo|lab_value>hi,1L,0L)) %>%
    group_by(id_key) %>%
    summarise(
      n_available=n(),
      n_abnormal=sum(abnormal),
      fi=if_else(n_available>=min_n,n_abnormal/n_available,NA_real_),
      .groups="drop"
    )
}

# ------------------------------------------------------------
# Locked primary fit functions.
# Expected standardized columns:
# id_key, hospital_mortality, fi_primary, age, sex/gender,
# ami, severity, cci (MIMIC/eICU), hospitalid (eICU)
# ------------------------------------------------------------
fit_mimic_locked <- function(d,fi_col="fi_primary",analysis,
                             add_count=FALSE){
  vars <- c(
    "hospital_mortality",fi_col,"cci_locked_no_mi",
    "age","sex","ami","severity"
  )
  if(add_count) vars <- c(vars,"n_available")
  need_cols(d,vars,"MIMIC model")

  x <- d %>%
    filter(if_all(all_of(vars),~!is.na(.x))) %>%
    mutate(
      fi_per_01=.data[[fi_col]]/0.1,
      sex=factor(sex)
    )

  if(add_count){
    x <- x %>% mutate(n_available=factor(n_available))
    f <- hospital_mortality ~
      fi_per_01+n_available+cci_locked_no_mi+
      age+sex+ami+severity
  } else {
    f <- hospital_mortality ~
      fi_per_01+cci_locked_no_mi+age+sex+ami+severity
  }

  m <- glm(f,data=x,family=binomial())

  # Historical MIMIC inference = profile-likelihood CI.
  tt <- broom::tidy(m,conf.int=TRUE)
  rr <- tt %>% filter(term=="fi_per_01")
  if(nrow(rr)!=1) stop("MIMIC FI term extraction failed.")

  tibble(
    database="MIMIC-IV",
    analysis=analysis,
    n=nrow(x),
    deaths=sum(x$hospital_mortality==1),
    hospitals=NA_integer_,
    or_per_0_1=exp(rr$estimate),
    ci_low=exp(rr$conf.low),
    ci_high=exp(rr$conf.high),
    p_value=rr$p.value
  )
}

fit_eicu_locked <- function(d,fi_col="fi_primary",analysis,
                            add_count=FALSE){
  vars <- c(
    "hospital_mortality",fi_col,"cci_score",
    "age5","gender_model","ami_code_flag","aps5","hospitalid"
  )
  if(add_count) vars <- c(vars,"n_available")
  need_cols(d,vars,"eICU model")

  x <- d %>%
    filter(if_all(all_of(vars),~!is.na(.x))) %>%
    mutate(
      fi_per_01=.data[[fi_col]]/0.1,
      gender_model=factor(gender_model)
    )

  if(add_count){
    x <- x %>% mutate(n_available=factor(n_available))
    f <- hospital_mortality ~
      cci_score+fi_per_01+n_available+
      age5+gender_model+ami_code_flag+aps5
  } else {
    f <- hospital_mortality ~
      cci_score+fi_per_01+
      age5+gender_model+ami_code_flag+aps5
  }

  m <- glm(f,data=x,family=binomial())
  V <- sandwich::vcovCL(m,cluster=x$hospitalid,type="HC0")
  ct <- lmtest::coeftest(m,vcov.=V)

  b <- ct["fi_per_01",1]
  se <- ct["fi_per_01",2]

  tibble(
    database="eICU-CRD",
    analysis=analysis,
    n=nrow(x),
    deaths=sum(x$hospital_mortality==1),
    hospitals=n_distinct(x$hospitalid),
    or_per_0_1=exp(b),
    ci_low=exp(b-1.96*se),
    ci_high=exp(b+1.96*se),
    p_value=ct["fi_per_01",4]
  )
}

fit_sicdb_locked <- function(d,fi_col="fi_primary",analysis,
                             add_count=FALSE,add_age=TRUE){
  vars <- c(
    "hospital_mortality",fi_col,"gender","ami","saps3"
  )
  if(add_age) vars <- c(vars,"age")
  if(add_count) vars <- c(vars,"n_available")
  need_cols(d,vars,"SICdb model")

  x <- d %>%
    filter(if_all(all_of(vars),~!is.na(.x))) %>%
    mutate(
      fi_per_01=.data[[fi_col]]/0.1,
      gender=factor(gender)
    )

  rhs <- c("fi_per_01")
  if(add_count){
    x <- x %>% mutate(n_available=factor(n_available))
    rhs <- c(rhs,"n_available")
  }
  if(add_age) rhs <- c(rhs,"age")
  rhs <- c(rhs,"gender","ami","saps3")
  f <- as.formula(paste("hospital_mortality ~",paste(rhs,collapse="+")))

  m <- glm(f,data=x,family=binomial())
  ct <- coef(summary(m))
  b <- ct["fi_per_01","Estimate"]
  se <- ct["fi_per_01","Std. Error"]

  tibble(
    database="SICdb",
    analysis=analysis,
    n=nrow(x),
    deaths=sum(x$hospital_mortality==1),
    hospitals=NA_integer_,
    or_per_0_1=exp(b),
    ci_low=exp(b-1.96*se),
    ci_high=exp(b+1.96*se),
    p_value=ct["fi_per_01","Pr(>|z|)"]
  )
}

fit_locked <- function(d,db,fi_col,analysis,...){
  if(db=="MIMIC-IV")
    return(fit_mimic_locked(d,fi_col,analysis,...))
  if(db=="eICU-CRD")
    return(fit_eicu_locked(d,fi_col,analysis,...))
  if(db=="SICdb")
    return(fit_sicdb_locked(d,fi_col,analysis,...))
  stop("Unknown db")
}

same_result <- function(a,b,tol=1e-11){
  isTRUE(
    a$n==b$n &&
    a$deaths==b$deaths &&
    (is.na(a$hospitals) || is.na(b$hospitals) || a$hospitals==b$hospitals) &&
    abs(a$or_per_0_1-b$or_per_0_1)<tol &&
    abs(a$ci_low-b$ci_low)<tol &&
    abs(a$ci_high-b$ci_high)<tol
  )
}

# ------------------------------------------------------------
# Harrell-style RCS basis used in the previous sensitivity.
# ------------------------------------------------------------
rcs_basis <- function(x,knots,prefix){
  knots <- sort(as.numeric(knots))
  K <- length(knots)
  if(K<3 || length(unique(knots))!=K) stop("Invalid knots: ",prefix)
  tK1 <- knots[K-1]
  tK <- knots[K]
  scale <- (tK-knots[1])^2
  pos3 <- function(z) pmax(z,0)^3
  ans <- lapply(seq_len(K-2),function(j){
    tj <- knots[j]
    (
      pos3(x-tj)
      -pos3(x-tK1)*(tK-tj)/(tK-tK1)
      +pos3(x-tK)*(tK1-tj)/(tK-tK1)
    )/scale
  })
  ans <- as.data.frame(ans)
  names(ans) <- paste0(prefix,"_rcs_nl",seq_len(ncol(ans)))
  ans
}
