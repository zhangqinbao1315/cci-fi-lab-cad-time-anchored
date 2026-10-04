# V15 corrected analysis freeze

Frozen before inspecting new corrected landmark results.

Historical audit:
- old S29 FI provenance is verified independently from historical CCI implementation;
- provisional FI definitions are not carried forward.

Current FI-Lab:
- fixed current locked 12-domain profile
- -1440 to 0 min
- >=9/12 domains

CAD(t):
- diagnosis documented by landmark t

CCI(t):
- diagnosis tokens documented by landmark t only
- Quan mapping
- myocardial infarction component excluded

Landmarks:
- 6 h
- 24 h
- 36 h

Risk sets:
Hospital:
  hospital_los_days > t/1440

ICU:
  hospital_los_days > t/1440
  AND icu_los_days > t/1440

Models:
Base(age+sex)
Base+CCI
Base+FI
Full

Inference:
hospital-cluster robust HC0
AUC / Brier
nested LR tests
paired Delta AUC
1000 hospital-cluster bootstrap replicates

All prespecified landmarks and both risk sets retained regardless of significance.
