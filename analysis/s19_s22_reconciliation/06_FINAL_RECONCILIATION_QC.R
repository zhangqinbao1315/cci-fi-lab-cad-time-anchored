source("00_helpers_FINAL.R")

primary <- readr::read_csv(
  file.path(RESULT_DIR,"PRIMARY_LOCKED_FINAL.csv"),
  show_col_types=FALSE
)
s19 <- readr::read_csv(
  file.path(RESULT_DIR,"S19_TIMING_FINAL.csv"),
  show_col_types=FALSE
)
s20 <- readr::read_csv(
  file.path(RESULT_DIR,"S20_AVAILABLE_COUNT_FINAL.csv"),
  show_col_types=FALSE
)
s21 <- readr::read_csv(
  file.path(RESULT_DIR,"S21_CONVENTIONAL_RANGES_FINAL.csv"),
  show_col_types=FALSE
)
s22 <- readr::read_csv(
  file.path(RESULT_DIR,"S22_FLEXIBLE_ADJUSTMENT_FINAL.csv"),
  show_col_types=FALSE
)

checks <- list()

for(db in primary$database){
  p <- primary %>% filter(database==db) %>% slice(1)

  b20 <- s20 %>%
    filter(database==db,analysis=="S20_BASE_locked_primary_ge9") %>%
    slice(1)

  b22 <- s22 %>%
    filter(database==db,analysis=="S22_LINEAR_locked_primary") %>%
    slice(1)

  checks[[length(checks)+1]] <- tibble(
    database=db,
    comparison="PRIMARY vs S20 locked baseline",
    primary_n=p$n,
    comparator_n=b20$n,
    primary_or=p$or_per_0_1,
    comparator_or=b20$or_per_0_1,
    primary_ci_low=p$ci_low,
    comparator_ci_low=b20$ci_low,
    primary_ci_high=p$ci_high,
    comparator_ci_high=b20$ci_high,
    status=if_else(same_result(p,b20),"PASS_EXACT","FAIL")
  )

  checks[[length(checks)+1]] <- tibble(
    database=db,
    comparison="PRIMARY vs S22 linear comparator",
    primary_n=p$n,
    comparator_n=b22$n,
    primary_or=p$or_per_0_1,
    comparator_or=b22$or_per_0_1,
    primary_ci_low=p$ci_low,
    comparator_ci_low=b22$ci_low,
    primary_ci_high=p$ci_high,
    comparator_ci_high=b22$ci_high,
    status=if_else(same_result(p,b22),"PASS_EXACT","FAIL")
  )

  # S21 common sample only has to equal full primary if sample size is identical.
  c21 <- s21 %>%
    filter(database==db,analysis=="S21_PRIMARY_common_sample") %>%
    slice(1)

  stat <- if(c21$n==p$n){
    if(same_result(p,c21)) "PASS_EXACT" else "FAIL"
  } else {
    "PASS_DIFFERENT_COMMON_SAMPLE"
  }

  checks[[length(checks)+1]] <- tibble(
    database=db,
    comparison="PRIMARY vs S21 common-sample primary",
    primary_n=p$n,
    comparator_n=c21$n,
    primary_or=p$or_per_0_1,
    comparator_or=c21$or_per_0_1,
    primary_ci_low=p$ci_low,
    comparator_ci_low=c21$ci_low,
    primary_ci_high=p$ci_high,
    comparator_ci_high=c21$ci_high,
    status=stat
  )
}

qc <- bind_rows(checks) %>%
  mutate(
    delta_n=comparator_n-primary_n,
    delta_or=comparator_or-primary_or,
    delta_ci_low=comparator_ci_low-primary_ci_low,
    delta_ci_high=comparator_ci_high-primary_ci_high
  )

save_csv(qc,"FINAL_RECONCILIATION_QC.csv")

# One long master result file for manuscript/source-data update.
all_results <- bind_rows(
  primary %>% mutate(section="PRIMARY"),
  s19 %>% mutate(section="S19"),
  s20 %>% mutate(section="S20"),
  s21 %>% mutate(section="S21"),
  s22 %>% mutate(section="S22")
)
save_csv(all_results,"FINAL_MASTER_RESULTS_ALL.csv")

# Static provenance declaration.
audit <- tibble(
  issue=c(
    "S20 FI source",
    "S20 available-count source",
    "Sensitivity weights",
    "MIMIC inference",
    "eICU inference",
    "SICdb inference"
  ),
  final_rule=c(
    "locked historical primary FI only; fi_lab_recalc is not used",
    "n_available only",
    "NO weights in S19-S22",
    "profile-likelihood CI",
    "hospital-cluster robust HC0",
    "Wald CI"
  )
)
save_csv(audit,"FINAL_PIPELINE_PROVENANCE_AUDIT.csv")

print(qc,n=Inf)

bad <- qc %>% filter(status=="FAIL")
if(nrow(bad)){
  stop(
    "FINAL RECONCILIATION FAILED. Do not update manuscript. ",
    "Upload FINAL_RECONCILIATION_QC.csv."
  )
}

cat(
  "\n============================================================\n",
  "SUCCESS: FINAL PRIMARY/S20/S22 RECONCILIATION PASSED\n",
  "============================================================\n",
  "The 1.293 vs 1.298 / 1.211 vs 1.213 / 12688 vs 12689 issue is closed.\n",
  "Upload the whole V7 results folder to ChatGPT for manuscript update.\n"
)
