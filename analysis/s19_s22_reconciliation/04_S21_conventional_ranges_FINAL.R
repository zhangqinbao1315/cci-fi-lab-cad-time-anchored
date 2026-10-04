source("00_helpers_FINAL.R")
locked <- readRDS(file.path(RESULT_DIR,"LOCKED_PRIMARY_COHORTS.rds"))
m <- locked$MIMIC
e <- locked$EICU
s <- locked$SICDB

rr0 <- readr::read_csv(
  "independent_conventional_reference_ranges.csv",
  show_col_types=FALSE
)

items <- unique(rr0$lab_item)

# Convert selected lab data to a one-row-per-patient conventional FI.
build_conventional_wide <- function(d,id_col,sex_col,exclude_inr=FALSE){
  use_items <- items
  if(exclude_inr) use_items <- setdiff(use_items,"inr")

  need_cols(d,c(id_col,sex_col), "wide conventional input")

  out <- tibble(
    id_key=id_chr(d[[id_col]]),
    sex_norm=norm_sex(d[[sex_col]])
  )

  deficits <- list()

  for(item in use_items){
    if(!(item %in% names(d))){
      deficits[[item]] <- rep(NA_real_,nrow(d))
      next
    }

    value <- suppressWarnings(as.numeric(d[[item]]))
    rri <- rr0 %>% filter(lab_item==item)

    if(any(rri$sex=="Any")){
      r <- rri %>% filter(sex=="Any") %>% slice(1)
      lo <- rep(r$lower,nrow(d))
      hi <- rep(r$upper,nrow(d))
    } else {
      lo <- rep(NA_real_,nrow(d))
      hi <- rep(NA_real_,nrow(d))
      for(sx in c("Male","Female")){
        r <- rri %>% filter(sex==sx)
        if(nrow(r)){
          idx <- out$sex_norm==sx
          lo[idx] <- r$lower[1]
          hi[idx] <- r$upper[1]
        }
      }
      # Unknown sex: sex-specific item is unevaluable, not guessed.
    }

    deficits[[item]] <- ifelse(
      is.na(value)|is.na(lo)|is.na(hi),
      NA_real_,
      as.numeric(value<lo|value>hi)
    )
  }

  mat <- as.data.frame(deficits)
  n_av <- rowSums(!is.na(mat))
  n_ab <- rowSums(mat,na.rm=TRUE)

  out %>%
    transmute(
      id_key,
      n_conv_available=n_av,
      n_conv_abnormal=n_ab,
      fi_conventional=if_else(n_av>=9,n_ab/n_av,NA_real_)
    )
}

build_conventional_long <- function(labs,patient,id_lab,id_patient,sex_col,
                                    exclude_inr=FALSE){
  px <- patient %>%
    transmute(
      id_key=id_chr(.data[[id_patient]]),
      sex_norm=norm_sex(.data[[sex_col]])
    )

  x <- labs %>%
    mutate(id_key=id_chr(.data[[id_lab]])) %>%
    semi_join(px %>% select(id_key),by="id_key") %>%
    inner_join(px,by="id_key")

  if(exclude_inr) x <- x %>% filter(lab_item!="inr")

  # Build interval row-by-row.
  neutral <- rr0 %>%
    filter(sex=="Any") %>%
    select(lab_item,lower,upper)

  specific <- rr0 %>%
    filter(sex!="Any") %>%
    select(lab_item,sex_norm=sex,lower,upper)

  x1 <- x %>%
    left_join(neutral,by="lab_item") %>%
    rename(lower_any=lower,upper_any=upper) %>%
    left_join(specific,by=c("lab_item","sex_norm")) %>%
    mutate(
      lo=coalesce(lower_any,lower),
      hi=coalesce(upper_any,upper),
      abnormal=case_when(
        is.na(lab_value)|is.na(lo)|is.na(hi) ~ NA_real_,
        lab_value<lo|lab_value>hi ~ 1,
        TRUE ~ 0
      )
    )

  x1 %>%
    group_by(id_key) %>%
    summarise(
      n_conv_available=sum(!is.na(abnormal)),
      n_conv_abnormal=sum(abnormal,na.rm=TRUE),
      fi_conventional=if_else(
        n_conv_available>=9,
        n_conv_abnormal/n_conv_available,
        NA_real_
      ),
      .groups="drop"
    )
}

# MIMIC: use historical wide lab values if present; otherwise exact main selected long.
if(all(items %in% names(m))){
  mconv <- build_conventional_wide(
    m %>% mutate(stay_id=id_key),
    "stay_id","sex",FALSE
  )
  m_source <- "historical wide selected values"
} else {
  ml <- read_csv2(find_export("03a_mimic_export_again_if_needed.csv",161702))
  mconv <- build_conventional_long(
    ml,
    m %>% mutate(stay_id=id_key),
    "stay_id","stay_id","sex",FALSE
  )
  m_source <- "main selected long export"
}

# eICU historical cohort contains the exact selected 12 lab values used by the
# frozen primary FI, so use them directly.
econv <- build_conventional_wide(
  e,
  "patientunitstayid","gender",FALSE
)
e_source <- "historical 04_eicu_analysis_cohort selected values"

# SICdb: use wide if available, otherwise main selected long.
sic_items <- setdiff(items,"inr")
if(all(sic_items %in% names(s))){
  sconv <- build_conventional_wide(
    s %>% mutate(icu_id=id_key),
    "icu_id","gender",TRUE
  )
  s_source <- "historical wide selected values; INR excluded"
} else {
  sl <- read_csv2(find_export("03c_sicdb_export_again_if_needed.csv",276946))
  sconv <- build_conventional_long(
    sl,
    s %>% mutate(icu_id=id_key),
    "icu_id","icu_id","gender",TRUE
  )
  s_source <- "main selected long export; INR excluded"
}

mm <- m %>% left_join(mconv,by="id_key")
ee <- e %>% left_join(econv,by="id_key")
ss <- s %>% left_join(sconv,by="id_key")

# Full locked primary references.
primary <- readr::read_csv(
  file.path(RESULT_DIR,"PRIMARY_LOCKED_FINAL.csv"),
  show_col_types=FALSE
) %>%
  mutate(analysis="S21_PRIMARY_locked_full")

# Common-sample comparisons: SAME rows for locked and conventional FI.
m_common <- mm %>%
  filter(!is.na(fi_primary),!is.na(fi_conventional))
e_common <- ee %>%
  filter(
    fi_lab_evaluable_r==1,
    !is.na(fi_primary),
    !is.na(fi_conventional)
  )
s_common <- ss %>%
  filter(!is.na(fi_primary),!is.na(fi_conventional))

common <- bind_rows(
  fit_mimic_locked(
    m_common,"fi_primary","S21_PRIMARY_common_sample"
  ),
  fit_mimic_locked(
    m_common,"fi_conventional","S21_CONVENTIONAL_common_sample"
  ),

  fit_eicu_locked(
    e_common,"fi_primary","S21_PRIMARY_common_sample"
  ),
  fit_eicu_locked(
    e_common,"fi_conventional","S21_CONVENTIONAL_common_sample"
  ),

  fit_sicdb_locked(
    s_common,"fi_primary","S21_PRIMARY_common_sample"
  ),
  fit_sicdb_locked(
    s_common,"fi_conventional","S21_CONVENTIONAL_common_sample"
  )
)

# All conventionally evaluable patients (same covariate model rules).
conv_all <- bind_rows(
  fit_mimic_locked(
    mm %>% filter(!is.na(fi_conventional)),
    "fi_conventional","S21_CONVENTIONAL_all_evaluable"
  ),
  fit_eicu_locked(
    ee %>% filter(!is.na(fi_conventional)),
    "fi_conventional","S21_CONVENTIONAL_all_evaluable"
  ),
  fit_sicdb_locked(
    ss %>% filter(!is.na(fi_conventional)),
    "fi_conventional","S21_CONVENTIONAL_all_evaluable"
  )
)

out <- bind_rows(primary,common,conv_all)
save_csv(out,"S21_CONVENTIONAL_RANGES_FINAL.csv")

qc <- tibble(
  database=c("MIMIC-IV","eICU-CRD","SICdb"),
  eligible_n=c(nrow(m),nrow(e),nrow(s)),
  conventional_ge9=c(
    sum(!is.na(mm$fi_conventional)),
    sum(!is.na(ee$fi_conventional)),
    sum(!is.na(ss$fi_conventional))
  ),
  common_with_locked_primary=c(
    nrow(m_common),
    nrow(e_common),
    nrow(s_common)
  ),
  lab_source=c(m_source,e_source,s_source),
  special_rule=c(
    "none",
    "unknown sex: sex-specific items unevaluable",
    "INR excluded due lower reporting bound at 1.2"
  )
)
save_csv(qc,"S21_CONVENTIONAL_QC.csv")
print(out)
print(qc)
