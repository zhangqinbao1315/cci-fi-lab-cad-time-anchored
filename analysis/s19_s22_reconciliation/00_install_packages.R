pkgs <- c(
  "dplyr","readr","tidyr","purrr","tibble",
  "sandwich","lmtest","comorbidity","broom"
)
need <- pkgs[!vapply(pkgs,requireNamespace,logical(1),quietly=TRUE)]
if(length(need)) install.packages(need,dependencies=TRUE)
message("Packages ready: ",paste(pkgs,collapse=", "))
