# Reproduce the current eICU time-anchored analyses used in:
# - Supplementary Source Data S29 (threshold-specific diagnosis availability)
# - Supplementary Source Data S30 (fixed 6-h cohort decomposition)
# - Main-text Table 2 (true 6/24/36-h landmark analyses)
#
# Inputs are restricted-access, patient-level intermediate files created by the
# project extraction pipeline and are NOT distributed in this repository:
#   04_eicu_analysis_cohort.csv
#   062_eicu_diagnosis_tokens_final.csv
#   063_eicu_core12_lab_long_final.csv
#
# Usage:
#   Rscript analysis/eicu_time_anchored_landmark_v1_1_0.R \
#     /path/to/eICU/data_raw \
#     /path/to/output
#
# The script uses the locked external FI-Lab ranges committed in:
#   definitions/final_locked_ranges.csv
#
# The myocardial-infarction Charlson component is excluded throughout.

options(stringsAsFactors = FALSE)

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1L) {
  stop("Usage: Rscript analysis/eicu_time_anchored_landmark_v1_1_0.R <data_dir> [output_dir]")
}
DATA_DIR <- normalizePath(args[[1]], winslash = "/", mustWork = TRUE)
OUT <- if (length(args) >= 2L) args[[2]] else file.path(getwd(), "verification_generated", "time_anchored")
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)

script_dir <- local({
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", grep("^--file=", a, value = TRUE))
  if (length(f)) dirname(normalizePath(f[[1]], winslash = "/", mustWork = TRUE)) else normalizePath(getwd(), winslash="/")
})
REPO_ROOT <- normalizePath(file.path(script_dir, ".."), winslash = "/", mustWork = TRUE)
RANGES_FILE <- file.path(REPO_ROOT, "definitions", "final_locked_ranges.csv")

required_pkgs <- c("data.table", "comorbidity", "sandwich", "lmtest")
missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs)) {
  stop("Install required R packages before running: ", paste(missing_pkgs, collapse = ", "))
}

suppressPackageStartupMessages({
  library(data.table)
  library(comorbidity)
  library(sandwich)
  library(lmtest)
})

safe_num <- function(x) suppressWarnings(as.numeric(as.character(x)))
norm_icd <- function(x) gsub("[^A-Z0-9]", "", toupper(trimws(as.character(x))))
is_cad <- function(x) grepl("^41[0-4]", x) | grepl("^I2[0-5]", x)

fast_auc <- function(y, p) {
  ok <- is.finite(y) & is.finite(p)
  y <- y[ok]; p <- p[ok]
  n1 <- sum(y == 1); n0 <- sum(y == 0)
  if (!n1 || !n0) return(NA_real_)
  r <- rank(p, ties.method = "average")
  (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}

cohort_file <- file.path(DATA_DIR, "04_eicu_analysis_cohort.csv")
dx_file <- file.path(DATA_DIR, "062_eicu_diagnosis_tokens_final.csv")
lab_file <- file.path(DATA_DIR, "063_eicu_core12_lab_long_final.csv")

for (p in c(cohort_file, dx_file, lab_file, RANGES_FILE)) {
  if (!file.exists(p)) stop("Required file not found: ", p)
}

cohort <- fread(cohort_file, showProgress = TRUE)
dx <- fread(dx_file, showProgress = TRUE)
lab <- fread(lab_file, showProgress = TRUE)
ranges <- fread(RANGES_FILE)

if (nrow(cohort) != 7445L) {
  stop("QC failure: 04_eicu_analysis_cohort.csv should contain 7,445 eligible patients.")
}

sex_col <- if ("gender_model" %in% names(cohort)) "gender_model" else "gender"
cohort[, patientunitstayid := safe_num(patientunitstayid)]
cohort[, age_model := safe_num(age_numeric)]
cohort[, death := as.integer(safe_num(hospital_mortality))]
cohort[, hospitalid_model := as.character(hospitalid)]
cohort[, hospital_los_days := safe_num(hospital_los_days)]
cohort[, icu_los_days := safe_num(icu_los_days)]
cohort[, sex_male := fifelse(
  tolower(trimws(as.character(get(sex_col)))) == "male", 1L,
  fifelse(tolower(trimws(as.character(get(sex_col)))) == "female", 0L, NA_integer_)
)]

pick_col <- function(nms, exact, regex = NULL) {
  low <- tolower(nms)
  for (e in exact) {
    ix <- which(low == tolower(e))
    if (length(ix)) return(nms[ix[1]])
  }
  if (!is.null(regex)) {
    ix <- grep(regex, low, perl = TRUE)
    if (length(ix)) return(nms[ix[1]])
  }
  stop("Required diagnosis column not found.")
}

idc <- pick_col(names(dx), c("patientunitstayid"))
offc <- pick_col(names(dx), c("diagnosisoffset", "diagnosis_offset", "diagnosisoffset_min"), "diagn.*offset")
ccands <- grep("icd|code|token", names(dx), ignore.case = TRUE, value = TRUE)

score_code_col <- function(nm) {
  x <- dx[[nm]]
  x <- x[!is.na(x)]
  if (!length(x)) return(-Inf)
  z <- norm_icd(head(x, 5000))
  mean(grepl("^[A-Z0-9]{3,8}$", z)) + 2 * mean(is_cad(z))
}
codec <- ccands[which.max(vapply(ccands, score_code_col, numeric(1)))]

dxt <- dx[, .(
  patientunitstayid = safe_num(get(idc)),
  diagnosisoffset = safe_num(get(offc)),
  code = norm_icd(get(codec))
)]
dxt <- dxt[
  is.finite(patientunitstayid) & is.finite(diagnosisoffset) &
  nchar(code) >= 3 & patientunitstayid %in% cohort$patientunitstayid
]

weights <- c(
  mi = 0, chf = 1, pvd = 1, cevd = 1, dementia = 1, cpd = 1,
  rheumd = 1, pud = 1, mld = 1, diab = 1, diabwc = 2, hp = 2,
  rend = 2, canc = 2, msld = 3, metacanc = 6, aids = 6
)
cci_domains <- names(weights)

calc_cci <- function(tok, ids) {
  ids <- unique(ids)
  if (!nrow(tok)) return(data.table(patientunitstayid = ids, cci_t = 0))
  d9 <- unique(tok[grepl("^[0-9]", code), .(id = patientunitstayid, code)])
  d10 <- unique(tok[grepl("^[A-Z]", code), .(id = patientunitstayid, code)])

  mapper <- function(x, map) {
    if (!nrow(x)) return(NULL)
    a <- comorbidity::comorbidity(as.data.frame(x), id = "id", code = "code", map = map, assign0 = TRUE)
    setDT(a); setnames(a, "id", "patientunitstayid")
    a[, intersect(c("patientunitstayid", cci_domains), names(a)), with = FALSE]
  }

  m9 <- mapper(d9, "charlson_icd9_quan")
  m10 <- mapper(d10, "charlson_icd10_quan")
  z <- data.table(patientunitstayid = ids)

  if (!is.null(m9)) {
    cc <- setdiff(names(m9), "patientunitstayid")
    setnames(m9, cc, paste0(cc, "_9"))
    z <- merge(z, m9, by = "patientunitstayid", all.x = TRUE)
  }
  if (!is.null(m10)) {
    cc <- setdiff(names(m10), "patientunitstayid")
    setnames(m10, cc, paste0(cc, "_10"))
    z <- merge(z, m10, by = "patientunitstayid", all.x = TRUE)
  }

  for (d in cci_domains) {
    a <- paste0(d, "_9"); b <- paste0(d, "_10")
    v1 <- if (a %in% names(z)) z[[a]] else rep(0, nrow(z))
    v2 <- if (b %in% names(z)) z[[b]] else rep(0, nrow(z))
    v1[is.na(v1)] <- 0; v2[is.na(v2)] <- 0
    z[[d]] <- pmax(v1, v2)
  }

  z[diabwc == 1, diab := 0]
  z[msld == 1, mld := 0]
  z[metacanc == 1, canc := 0]

  z[, cci_t := 0]
  for (d in cci_domains) z[, cci_t := cci_t + get(d) * weights[[d]]]
  z[, .(patientunitstayid, cci_t)]
}

thresholds <- c(0L, 360L, 1440L, 2160L)
cci_by_t <- list()
cad_by_t <- list()
for (tmin in thresholds) {
  tt <- dxt[diagnosisoffset <= tmin]
  cci_by_t[[as.character(tmin)]] <- calc_cci(tt, cohort$patientunitstayid)
  cad_by_t[[as.character(tmin)]] <- unique(tt[is_cad(code), patientunitstayid])
}
cci_full <- calc_cci(dxt, cohort$patientunitstayid)

# Locked pre-ICU FI-Lab used in S29/S30/Table 2.
required_lab_cols <- c("patientunitstayid", "lab_key", "labresult", "labresultoffset")
if (!all(required_lab_cols %in% names(lab))) {
  stop("063_eicu_core12_lab_long_final.csv is missing required columns: ",
       paste(setdiff(required_lab_cols, names(lab)), collapse = ", "))
}

lab[, patientunitstayid := safe_num(patientunitstayid)]
lab[, domain := tolower(trimws(as.character(lab_key)))]
lab[, value := safe_num(labresult)]
lab[, offset := safe_num(labresultoffset)]
lab <- lab[
  is.finite(patientunitstayid) & is.finite(value) & is.finite(offset) &
  patientunitstayid %in% cohort$patientunitstayid &
  domain %in% ranges$domain &
  offset >= -1440 & offset <= 0
]
setorder(lab, patientunitstayid, domain, -offset)
selected <- lab[, .SD[1L], by = .(patientunitstayid, domain)]
selected <- merge(selected, ranges, by = "domain", all.x = TRUE)
selected[, abnormal := as.integer(value < lower | value > upper)]

FI_DT <- selected[, .(
  n_available = .N,
  fi = sum(abnormal) / .N
), by = patientunitstayid][n_available >= 9, .(patientunitstayid, fi)]

robust_effects <- function(fit, d, model_name, analysis, label) {
  V <- sandwich::vcovCL(fit, cluster = d$hospitalid_model, type = "HC0")
  ct <- lmtest::coeftest(fit, vcov. = V)
  wanted <- intersect(c("cci_t", "fi_per_01"), rownames(ct))
  if (!length(wanted)) return(data.table())
  rbindlist(lapply(wanted, function(term) {
    e <- unname(ct[term, "Estimate"])
    s <- unname(ct[term, "Std. Error"])
    data.table(
      analysis = analysis, label = label, model = model_name, term = term,
      OR = exp(e), CI_low = exp(e - 1.96 * s), CI_high = exp(e + 1.96 * s),
      p_value = unname(ct[term, "Pr(>|z|)"])
    )
  }))
}

fit4 <- function(d, analysis, label) {
  d <- copy(d)
  d[, fi_per_01 := fi / 0.1]
  b <- glm(death ~ age_model + sex_male, data = d, family = binomial())
  c1 <- glm(death ~ age_model + sex_male + cci_t, data = d, family = binomial())
  f1 <- glm(death ~ age_model + sex_male + fi_per_01, data = d, family = binomial())
  u <- glm(death ~ age_model + sex_male + cci_t + fi_per_01, data = d, family = binomial())
  mods <- list(Base = b, Base_CCI = c1, Base_FI = f1, Full = u)

  effects <- rbindlist(lapply(names(mods), function(nm) {
    robust_effects(mods[[nm]], d, nm, analysis, label)
  }), fill = TRUE)

  performance <- rbindlist(lapply(names(mods), function(nm) {
    pr <- predict(mods[[nm]], type = "response")
    data.table(
      analysis = analysis, label = label, model = nm,
      n = nrow(d), deaths = sum(d$death), hospitals = uniqueN(d$hospitalid_model),
      AUC = fast_auc(d$death, pr), Brier = mean((d$death - pr)^2)
    )
  }))

  lr <- rbindlist(list(
    data.table(
      analysis = analysis, label = label, comparison = "FI added to Base+CCI",
      LR = 2 * (as.numeric(logLik(u)) - as.numeric(logLik(c1))), df = 1
    ),
    data.table(
      analysis = analysis, label = label, comparison = "CCI added to Base+FI",
      LR = 2 * (as.numeric(logLik(u)) - as.numeric(logLik(f1))), df = 1
    )
  ))
  lr[, p_value := pchisq(LR, df = df, lower.tail = FALSE)]

  am <- setNames(performance$AUC, performance$model)
  delta <- c(
    fi_over_cci = unname(am["Full"] - am["Base_CCI"]),
    cci_over_fi = unname(am["Full"] - am["Base_FI"])
  )
  list(d = d, effects = effects, performance = performance, lr = lr, delta = delta)
}

BASE <- merge(cohort, FI_DT, by = "patientunitstayid", all.x = TRUE)

# S29: threshold-specific diagnosis availability.
s29_sets <- list(); s29_flow <- list(); s29_fits <- list()
s29_eff <- list(); s29_perf <- list(); s29_lr <- list()
for (tmin in thresholds) {
  d <- merge(BASE, cci_by_t[[as.character(tmin)]], by = "patientunitstayid", all.x = TRUE)
  d[is.na(cci_t), cci_t := 0]
  common <- d[
    patientunitstayid %in% cad_by_t[[as.character(tmin)]] &
    is.finite(fi) & is.finite(age_model) & !is.na(sex_male) &
    !is.na(death) & !is.na(hospitalid_model)
  ]
  s29_sets[[as.character(tmin)]] <- common
  tt <- dxt[diagnosisoffset <= tmin]
  s29_flow[[as.character(tmin)]] <- data.table(
    threshold_min = tmin, time = paste0(tmin / 60, "h"),
    diagnosis_rows = nrow(tt),
    patients_with_any_diagnosis = uniqueN(tt$patientunitstayid),
    patients_with_CAD_by_time = length(cad_by_t[[as.character(tmin)]]),
    n = nrow(common), deaths = sum(common$death),
    hospitals = uniqueN(common$hospitalid_model),
    median_CCI = if (nrow(common)) median(common$cci_t) else NA_real_,
    median_FI = if (nrow(common)) median(common$fi) else NA_real_
  )
  if (tmin > 0) {
    ff <- fit4(common, "S29_TIME_ANCHORED", paste0(tmin / 60, "h"))
    k <- as.character(tmin)
    s29_fits[[k]] <- ff; s29_eff[[k]] <- ff$effects
    s29_perf[[k]] <- ff$performance; s29_lr[[k]] <- ff$lr
  }
}

fwrite(rbindlist(s29_flow), file.path(OUT, "S29_timeanchor_flow.csv"))
fwrite(rbindlist(s29_eff, fill = TRUE), file.path(OUT, "S29_timeanchor_effects.csv"))
fwrite(rbindlist(s29_perf), file.path(OUT, "S29_timeanchor_performance.csv"))
fwrite(rbindlist(s29_lr), file.path(OUT, "S29_timeanchor_LR.csv"))

# S30: fixed 6-h cohort while CCI(t) accrues.
fixed <- copy(s29_sets[["360"]])
s30_flow <- list(); s30_fits <- list()
s30_eff <- list(); s30_perf <- list(); s30_lr <- list()
tlist <- list(
  `0h` = cci_by_t[["0"]],
  `6h` = cci_by_t[["360"]],
  `24h` = cci_by_t[["1440"]],
  `36h` = cci_by_t[["2160"]],
  full = cci_full
)
for (lbl in names(tlist)) {
  dd <- merge(
    fixed[, setdiff(names(fixed), "cci_t"), with = FALSE],
    tlist[[lbl]], by = "patientunitstayid", all.x = TRUE
  )
  dd[is.na(cci_t), cci_t := 0]
  ff <- fit4(dd, "S30_FIXED6H", lbl)
  s30_fits[[lbl]] <- ff; s30_eff[[lbl]] <- ff$effects
  s30_perf[[lbl]] <- ff$performance; s30_lr[[lbl]] <- ff$lr
  s30_flow[[lbl]] <- data.table(
    time = lbl, n = nrow(dd), deaths = sum(dd$death),
    hospitals = uniqueN(dd$hospitalid_model),
    patients_with_CCI_gt0 = sum(dd$cci_t > 0),
    median_CCI = median(dd$cci_t), median_FI = median(dd$fi)
  )
}

fwrite(rbindlist(s30_flow), file.path(OUT, "S30_fixed6h_flow.csv"))
fwrite(rbindlist(s30_eff, fill = TRUE), file.path(OUT, "S30_fixed6h_effects.csv"))
fwrite(rbindlist(s30_perf), file.path(OUT, "S30_fixed6h_performance.csv"))
fwrite(rbindlist(s30_lr), file.path(OUT, "S30_fixed6h_LR.csv"))

# Main-text Table 2: true landmark analyses.
land_flow <- list(); land_fits <- list()
land_eff <- list(); land_perf <- list(); land_lr <- list()
for (tmin in c(360L, 1440L, 2160L)) {
  a <- copy(s29_sets[[as.character(tmin)]])
  cut <- tmin / 1440
  risksets <- list(
    Hospital = a[is.finite(hospital_los_days) & hospital_los_days > cut],
    ICU = a[
      is.finite(hospital_los_days) & is.finite(icu_los_days) &
      hospital_los_days > cut & icu_los_days > cut
    ]
  )
  for (rs in names(risksets)) {
    dd <- risksets[[rs]]
    k <- paste(tmin, rs, sep = "_")
    land_flow[[k]] <- data.table(
      riskset = rs, threshold_min = tmin, threshold_h = tmin / 60,
      availability_n = nrow(a), landmark_n = nrow(dd),
      excluded_before_or_at_landmark = nrow(a) - nrow(dd),
      deaths_after_landmark = sum(dd$death),
      hospitals = uniqueN(dd$hospitalid_model)
    )
    ff <- fit4(dd, "TRUE_LANDMARK", paste0(rs, "_", tmin / 60, "h"))
    land_fits[[k]] <- ff; land_eff[[k]] <- ff$effects
    land_perf[[k]] <- ff$performance; land_lr[[k]] <- ff$lr
  }
}

fwrite(rbindlist(land_flow), file.path(OUT, "Table2_true_landmark_flow.csv"))
fwrite(rbindlist(land_eff, fill = TRUE), file.path(OUT, "Table2_true_landmark_effects.csv"))
fwrite(rbindlist(land_perf), file.path(OUT, "Table2_true_landmark_performance.csv"))
fwrite(rbindlist(land_lr), file.path(OUT, "Table2_true_landmark_LR.csv"))

BOOT_REPS <- 1000L
BOOT_SEED <- 20261001L

boot_one_fit <- function(fitobj, key, analysis_name) {
  d <- copy(fitobj$d)
  y <- as.integer(d$death)
  groups <- split(seq_len(nrow(d)), d$hospitalid_model)
  H <- length(groups)
  xcci <- cbind(1, d$age_model, d$sex_male, d$cci_t)
  xfi <- cbind(1, d$age_model, d$sex_male, d$fi_per_01)
  xfull <- cbind(1, d$age_model, d$sex_male, d$cci_t, d$fi_per_01)

  fit_auc <- function(x, yy) {
    f <- suppressWarnings(glm.fit(x = x, y = yy, family = binomial(), control = glm.control(maxit = 50)))
    cf <- f$coefficients
    if (any(!is.finite(cf))) return(NA_real_)
    fast_auc(yy, plogis(drop(x %*% cf)))
  }

  set.seed(BOOT_SEED + sum(utf8ToInt(paste0(analysis_name, "_", key))))
  out <- matrix(NA_real_, nrow = BOOT_REPS, ncol = 2,
                dimnames = list(NULL, c("fi_over_cci", "cci_over_fi")))

  for (b in seq_len(BOOT_REPS)) {
    g <- sample.int(H, H, replace = TRUE)
    ix <- unlist(groups[g], use.names = FALSE)
    yy <- y[ix]
    a_cci <- tryCatch(fit_auc(xcci[ix,,drop=FALSE], yy), error = function(e) NA_real_)
    a_fi <- tryCatch(fit_auc(xfi[ix,,drop=FALSE], yy), error = function(e) NA_real_)
    a_full <- tryCatch(fit_auc(xfull[ix,,drop=FALSE], yy), error = function(e) NA_real_)
    out[b,] <- c(a_full - a_cci, a_full - a_fi)
  }

  rbindlist(lapply(colnames(out), function(v) {
    x <- out[,v]
    x <- x[is.finite(x)]
    data.table(
      analysis = analysis_name, analysis_key = key, contrast = v,
      point_delta_auc = unname(fitobj$delta[[v]]),
      reps_requested = BOOT_REPS, reps_valid = length(x),
      bootstrap_mean = mean(x), bootstrap_median = median(x),
      CI_low = unname(quantile(x, 0.025)),
      CI_high = unname(quantile(x, 0.975)),
      prop_gt_0 = mean(x > 0)
    )
  }))
}

s29_boot <- rbindlist(lapply(names(s29_fits), function(k) {
  boot_one_fit(s29_fits[[k]], paste0(as.integer(k) / 60, "h"), "S29_TIME_ANCHORED")
}))
s30_boot <- rbindlist(lapply(names(s30_fits), function(k) {
  boot_one_fit(s30_fits[[k]], k, "S30_FIXED6H")
}))
land_boot <- rbindlist(lapply(names(land_fits), function(k) {
  boot_one_fit(land_fits[[k]], k, "TRUE_LANDMARK")
}))

fwrite(s29_boot, file.path(OUT, "S29_timeanchor_deltaAUC_bootstrap.csv"))
fwrite(s30_boot, file.path(OUT, "S30_fixed6h_deltaAUC_bootstrap.csv"))
fwrite(land_boot, file.path(OUT, "Table2_true_landmark_deltaAUC_bootstrap.csv"))

capture.output(sessionInfo(), file = file.path(OUT, "sessionInfo.txt"))
cat("Completed S29, S30 and main-text Table 2 time-anchored analyses.\n")
