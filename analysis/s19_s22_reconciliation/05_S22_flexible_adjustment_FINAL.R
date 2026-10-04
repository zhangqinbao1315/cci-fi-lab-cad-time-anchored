source("00_helpers_FINAL.R")
locked <- readRDS(file.path(RESULT_DIR,"LOCKED_PRIMARY_COHORTS.rds"))
m <- locked$MIMIC
e <- locked$EICU
s <- locked$SICDB

# Exact primary samples.
m1 <- m %>%
  filter(
    complete.cases(
      hospital_mortality,fi_primary,cci_locked_no_mi,
      age,sex,ami,severity
    )
  ) %>%
  mutate(sex=factor(sex),fi_per_01=fi_primary/0.1)

e1 <- e %>%
  filter(
    fi_lab_evaluable_r==1,
    complete.cases(
      hospital_mortality,fi_primary,cci_score,
      age5,gender_model,ami_code_flag,aps5,hospitalid
    )
  ) %>%
  mutate(
    gender_model=factor(gender_model),
    fi_per_01=fi_primary/0.1
  )

s1 <- s %>%
  filter(
    complete.cases(
      hospital_mortality,fi_primary,age,gender,ami,saps3
    )
  ) %>%
  mutate(gender=factor(gender),fi_per_01=fi_primary/0.1)

# Linear comparator is literally the locked primary fit.
linear <- bind_rows(
  fit_mimic_locked(m1,"fi_primary","S22_LINEAR_locked_primary"),
  fit_eicu_locked(e1,"fi_primary","S22_LINEAR_locked_primary"),
  fit_sicdb_locked(s1,"fi_primary","S22_LINEAR_locked_primary")
)

# ---------------- MIMIC 4-knot age + OASIS acute ----------------
ka <- quantile(m1$age,c(.05,.35,.65,.95),na.rm=TRUE,type=7)
ko <- quantile(m1$severity,c(.05,.35,.65,.95),na.rm=TRUE,type=7)
ba <- rcs_basis(m1$age,ka,"age")
bo <- rcs_basis(m1$severity,ko,"sev")
mm <- bind_cols(m1,ba,bo)

fm <- as.formula(paste(
  "hospital_mortality ~ fi_per_01 + cci_locked_no_mi + sex + ami + age +",
  paste(names(ba),collapse="+"),
  "+ severity +",
  paste(names(bo),collapse="+")
))
modm <- glm(fm,data=mm,family=binomial())
tm <- broom::tidy(modm,conf.int=TRUE) %>% filter(term=="fi_per_01")

rm <- tibble(
  database="MIMIC-IV",
  analysis="S22_RCS_age_OASIS",
  n=nrow(mm),
  deaths=sum(mm$hospital_mortality==1),
  hospitals=NA_integer_,
  or_per_0_1=exp(tm$estimate),
  ci_low=exp(tm$conf.low),
  ci_high=exp(tm$conf.high),
  p_value=tm$p.value
)

# ---------------- eICU 4-knot raw age + APS ----------------
# age5/aps5 are just scaled versions of raw age/APS in the same locked rows.
e1 <- e1 %>%
  mutate(
    age_raw=as.numeric(age_numeric),
    aps_raw=as.numeric(acutephysiologyscore)
  )

if(any(e1$aps_raw<0,na.rm=TRUE))
  stop("Unexpected negative APS remained in exact eICU primary sample.")

kea <- quantile(e1$age_raw,c(.05,.35,.65,.95),na.rm=TRUE,type=7)
kap <- quantile(e1$aps_raw,c(.05,.35,.65,.95),na.rm=TRUE,type=7)
bea <- rcs_basis(e1$age_raw,kea,"age")
bap <- rcs_basis(e1$aps_raw,kap,"aps")
ee <- bind_cols(e1,bea,bap)

fe <- as.formula(paste(
  "hospital_mortality ~ fi_per_01 + cci_score + gender_model + ami_code_flag +",
  "age_raw +",paste(names(bea),collapse="+"),
  "+ aps_raw +",paste(names(bap),collapse="+")
))
mode <- glm(fe,data=ee,family=binomial())
Ve <- sandwich::vcovCL(mode,cluster=ee$hospitalid,type="HC0")
ce <- lmtest::coeftest(mode,vcov.=Ve)
b <- ce["fi_per_01",1]
se <- ce["fi_per_01",2]

re <- tibble(
  database="eICU-CRD",
  analysis="S22_RCS_age_APS",
  n=nrow(ee),
  deaths=sum(ee$hospital_mortality==1),
  hospitals=n_distinct(ee$hospitalid),
  or_per_0_1=exp(b),
  ci_low=exp(b-1.96*se),
  ci_high=exp(b+1.96*se),
  p_value=ce["fi_per_01",4]
)

# ---------------- SICdb with/without age and 3-knot SAPS III ----------------
r_noage <- fit_sicdb_locked(
  s1,"fi_primary","S22_SAPSIII_without_separate_age",
  add_age=FALSE
)

ks <- quantile(s1$saps3,c(.10,.50,.90),na.rm=TRUE,type=7)
bs <- rcs_basis(s1$saps3,ks,"saps")
sss <- bind_cols(s1,bs)

fs <- as.formula(paste(
  "hospital_mortality ~ fi_per_01 + age + gender + ami + saps3 +",
  paste(names(bs),collapse="+")
))
mods <- glm(fs,data=sss,family=binomial())
cs <- coef(summary(mods))
b <- cs["fi_per_01","Estimate"]
se <- cs["fi_per_01","Std. Error"]

rs <- tibble(
  database="SICdb",
  analysis="S22_RCS_SAPSIII_plus_age",
  n=nrow(sss),
  deaths=sum(sss$hospital_mortality==1),
  hospitals=NA_integer_,
  or_per_0_1=exp(b),
  ci_low=exp(b-1.96*se),
  ci_high=exp(b+1.96*se),
  p_value=cs["fi_per_01","Pr(>|z|)"]
)

out <- bind_rows(linear,rm,re,r_noage,rs)
save_csv(out,"S22_FLEXIBLE_ADJUSTMENT_FINAL.csv")

knots <- bind_rows(
  tibble(database="MIMIC-IV",variable="age",knot=seq_along(ka),value=as.numeric(ka)),
  tibble(database="MIMIC-IV",variable="oasis_acute_component",knot=seq_along(ko),value=as.numeric(ko)),
  tibble(database="eICU-CRD",variable="age",knot=seq_along(kea),value=as.numeric(kea)),
  tibble(database="eICU-CRD",variable="acute_physiology_score",knot=seq_along(kap),value=as.numeric(kap)),
  tibble(database="SICdb",variable="saps3",knot=seq_along(ks),value=as.numeric(ks))
)
save_csv(knots,"S22_SPLINE_KNOTS_FINAL.csv")

print(out)
print(knots)
