source("00_helpers_FINAL.R")
locked <- readRDS(file.path(RESULT_DIR,"LOCKED_PRIMARY_COHORTS.rds"))
m <- locked$MIMIC
e <- locked$EICU
s <- locked$SICDB

# IMPORTANT:
# available-count exports provide COUNT ONLY.
# fi_lab_recalc is intentionally ignored.
#
# Historical same-name CSV copies may exist. Select the candidate with
# maximal coverage of the LOCKED primary IDs rather than assuming a row count.

find_best_count_export <- function(filename,id_col,locked_primary_ids){
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
  if(!length(hits)) stop("Count export not found: ",filename)

  locked_primary_ids <- unique(id_chr(locked_primary_ids))
  cand <- list()

  for(h in hits){
    d <- tryCatch(read_csv2(h),error=function(e) NULL)
    if(is.null(d) || !all(c(id_col,"n_available") %in% names(d))) next

    ids <- id_chr(d[[id_col]])
    nav <- suppressWarnings(as.numeric(d$n_available))

    cand[[length(cand)+1]] <- tibble(
      path=normalizePath(h,winslash="/",mustWork=FALSE),
      md5=unname(tools::md5sum(h)),
      rows=nrow(d),
      unique_ids=n_distinct(ids),
      locked_id_overlap=sum(locked_primary_ids %in% ids),
      locked_ge9_overlap=sum(
        locked_primary_ids %in% ids[!is.na(nav) & nav>=9]
      ),
      modified=as.character(file.info(h)$mtime)
    )
  }

  tab <- bind_rows(cand)
  if(!nrow(tab)) stop("No valid count candidate for: ",filename)

  tab <- tab %>%
    arrange(
      desc(locked_id_overlap),
      desc(locked_ge9_overlap),
      desc(unique_ids)
    )

  save_csv(
    tab,
    paste0(
      "S20_COUNT_FILE_SELECTION_",
      gsub("[^A-Za-z0-9]+","_",filename),
      ".csv"
    )
  )

  best_score <- tab %>%
    slice(1) %>%
    select(locked_id_overlap,locked_ge9_overlap,unique_ids)

  best <- tab %>%
    filter(
      locked_id_overlap==best_score$locked_id_overlap,
      locked_ge9_overlap==best_score$locked_ge9_overlap,
      unique_ids==best_score$unique_ids
    )

  # Identical content duplicates are harmless.
  if(n_distinct(best$md5)==1){
    chosen <- best$path[which.min(nchar(best$path))]
  } else {
    # For equally covering but non-identical copies, use newest and document it.
    mt <- file.info(best$path)$mtime
    chosen <- best$path[order(mt,decreasing=TRUE)][1]
    warning(
      "Multiple equally covering non-identical count files for ",filename,
      "; using newest:\\n",chosen
    )
  }

  message("Using count file ",filename,":\\n",chosen)
  chosen
}

m_primary_ids <- m$id_key[!is.na(m$fi_primary)]
s_primary_ids <- s$id_key[!is.na(s$fi_primary)]

ma_path <- find_best_count_export(
  "mimic_available_count.csv",
  "stay_id",
  m_primary_ids
)
ma <- read_csv2(ma_path) %>%
  transmute(
    id_key=id_chr(stay_id),
    n_available=as.numeric(n_available)
  )

# eICU count is taken directly from the historical frozen cohort
# because it is exactly the count used to define fi_lab_evaluable_r.
ea <- e %>%
  transmute(
    id_key,
    n_available=as.numeric(fi_lab_n_available_r)
  )

sa_path <- find_best_count_export(
  "sicdb_available_count.csv",
  "icu_id",
  s_primary_ids
)
sa <- read_csv2(sa_path) %>%
  transmute(
    id_key=id_chr(icu_id),
    n_available=as.numeric(n_available)
  )

mm <- m %>% left_join(ma,by="id_key")
ee <- e %>% select(-any_of("n_available")) %>% left_join(ea,by="id_key")
ss <- s %>% left_join(sa,by="id_key")

# Primary model frames / membership.
m_primary <- mm %>%
  filter(!is.na(fi_primary))
e_primary <- ee %>%
  filter(fi_lab_evaluable_r==1,!is.na(fi_primary))
s_primary <- ss %>%
  filter(!is.na(fi_primary))

# Baseline is literally the locked primary.
base <- bind_rows(
  fit_mimic_locked(m_primary,"fi_primary","S20_BASE_locked_primary_ge9"),
  fit_eicu_locked(e_primary,"fi_primary","S20_BASE_locked_primary_ge9"),
  fit_sicdb_locked(s_primary,"fi_primary","S20_BASE_locked_primary_ge9")
)

plus_count <- bind_rows(
  fit_mimic_locked(
    m_primary,"fi_primary","S20_plus_available_item_count",add_count=TRUE
  ),
  fit_eicu_locked(
    e_primary,"fi_primary","S20_plus_available_item_count",add_count=TRUE
  ),
  fit_sicdb_locked(
    s_primary,"fi_primary","S20_plus_available_item_count",add_count=TRUE
  )
)

restricted <- bind_rows(
  lapply(c(10,11),function(k){
    bind_rows(
      fit_mimic_locked(
        m_primary %>% filter(n_available>=k),
        "fi_primary",paste0("S20_require_ge",k)
      ),
      fit_eicu_locked(
        e_primary %>% filter(n_available>=k),
        "fi_primary",paste0("S20_require_ge",k)
      ),
      fit_sicdb_locked(
        s_primary %>% filter(n_available>=k),
        "fi_primary",paste0("S20_require_ge",k)
      )
    )
  }),
  bind_rows(
    fit_mimic_locked(
      m_primary %>% filter(n_available==12),
      "fi_primary","S20_complete12"
    ),
    fit_eicu_locked(
      e_primary %>% filter(n_available==12),
      "fi_primary","S20_complete12"
    ),
    fit_sicdb_locked(
      s_primary %>% filter(n_available==12),
      "fi_primary","S20_complete12"
    )
  )
)

out <- bind_rows(base,plus_count,restricted)
save_csv(out,"S20_AVAILABLE_COUNT_FINAL.csv")

# Explicitly document the old 12689 issue.
qc <- bind_rows(
  tibble(
    database="MIMIC-IV",
    locked_primary_n=sum(!is.na(m$fi_primary)),
    locked_primary_with_count_ge9=sum(!is.na(m$fi_primary)&mm$n_available>=9,na.rm=TRUE),
    sql_ge9_any_id=sum(ma$n_available>=9,na.rm=TRUE),
    locked_primary_missing_count=sum(!is.na(m$fi_primary)&is.na(mm$n_available)),
    fi_source="historical locked fi_primary; fi_lab_recalc NOT USED"
  ),
  tibble(
    database="eICU-CRD",
    locked_primary_n=sum(e$fi_lab_evaluable_r==1,na.rm=TRUE),
    locked_primary_with_count_ge9=sum(
      e$fi_lab_evaluable_r==1 & ee$n_available>=9,na.rm=TRUE
    ),
    sql_ge9_any_id=NA_integer_,
    locked_primary_missing_count=sum(
      e$fi_lab_evaluable_r==1 & is.na(ee$n_available),na.rm=TRUE
    ),
    fi_source="historical locked fi_lab; historical n_available_r"
  ),
  tibble(
    database="SICdb",
    locked_primary_n=sum(!is.na(s$fi_primary)),
    locked_primary_with_count_ge9=sum(
      !is.na(s$fi_primary)&ss$n_available>=9,na.rm=TRUE
    ),
    sql_ge9_any_id=sum(sa$n_available>=9,na.rm=TRUE),
    locked_primary_missing_count=sum(
      !is.na(s$fi_primary)&is.na(ss$n_available)
    ),
    fi_source="historical locked fi_primary; fi_lab_recalc NOT USED"
  )
)
save_csv(qc,"S20_MEMBERSHIP_AND_FI_SOURCE_QC.csv")
print(out)
print(qc)
