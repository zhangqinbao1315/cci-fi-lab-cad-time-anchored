# Exact execution archive for the eICU timing/landmark analysis.
# This driver was used to generate the final S29, S30 and true-landmark outputs.
#
# It auto-searches the local Windows machine for the restricted eICU-derived
# input files. No patient-level data are distributed in this repository.
#
# Recommended use:
#   open this file from the repository root in RStudio and Source it.
#
# Required R packages are installed/checked by the archived driver.

PKG_ROOT <- normalizePath(
  file.path(getwd(), "analysis", "time_anchored_exact"),
  winslash = "/",
  mustWork = TRUE
)

dir.create(file.path(PKG_ROOT, "output"), recursive = TRUE, showWarnings = FALSE)

source(
  file.path(PKG_ROOT, "R", "V15_MAIN.R"),
  local = .GlobalEnv,
  encoding = "UTF-8"
)
