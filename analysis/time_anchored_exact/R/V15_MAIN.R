options(stringsAsFactors=FALSE)
options(repos=c(CRAN="https://cloud.r-project.org"))

OUT <- file.path(PKG_ROOT,"output")
dir.create(OUT,recursive=TRUE,showWarnings=FALSE)
LOG <- file.path(OUT,"run_console_live.log")
HEART <- file.path(OUT,"HEARTBEAT.txt")
CACHE <- file.path(OUT,"AUTO_PATH_CACHE.csv")
SEARCH_AUDIT <- file.path(OUT,"SEARCH_AUDIT.csv")

log_msg <- function(...) {
  line <- paste0("[",format(Sys.time(),"%Y-%m-%d %H:%M:%S"),"] ",
                 paste0(...,collapse=""))
  cat(line,"\n"); flush.console()
  con <- file(LOG,open="a",encoding="UTF-8")
  writeLines(line,con,useBytes=TRUE)
  close(con)
}
heartbeat <- function(stage) {
  writeLines(c(
    paste0("Last update: ",format(Sys.time(),"%Y-%m-%d %H:%M:%S %Z")),
    paste0("Stage: ",stage)
  ),HEART,useBytes=TRUE)
  log_msg("STAGE: ",stage)
}

# Persist unexpected runtime errors so a future output.zip contains the exact
# failing message even when the RStudio console screenshot is unavailable.
options(error = function() {
  msg <- tryCatch(geterrmessage(), error=function(e) "Unknown R error")
  stamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
  try(
    writeLines(
      c(
        paste0("Time: ", stamp),
        paste0("Error: ", msg),
        "",
        "The analysis stopped before SUCCESS.",
        "Upload the complete output folder; do not modify analysis parameters."
      ),
      file.path(OUT, "FATAL_ERROR.txt"),
      useBytes=TRUE
    ),
    silent=TRUE
  )
  try(
    writeLines(
      c(
        paste0("Last update: ", stamp),
        paste0("Stage: STOP: FATAL ERROR | ", msg)
      ),
      HEART,
      useBytes=TRUE
    ),
    silent=TRUE
  )
  try(log_msg("FATAL ERROR: ", msg), silent=TRUE)
})

log_msg("============================================================")
log_msg("BMC Geriatrics Reviewer-8 COMPLETE AUTO v15 SELF-CONTAINED SELF-CONTAINED")
log_msg("Package root: ",PKG_ROOT)
log_msg("============================================================")
heartbeat("startup")

parse(file=file.path(PKG_ROOT,"R","V15_MAIN.R"))
log_msg("R parse gate: PASSED")

req <- c("data.table","comorbidity","sandwich","lmtest")
miss <- req[!vapply(req,requireNamespace,logical(1),quietly=TRUE)]
if(length(miss)){
  heartbeat("install required R packages")
  if(.Platform$OS.type=="windows")
    try(install.packages(miss,dependencies=TRUE,type="binary"),silent=TRUE)
}
miss <- req[!vapply(req,requireNamespace,logical(1),quietly=TRUE)]
if(length(miss))install.packages(miss,dependencies=TRUE)
miss <- req[!vapply(req,requireNamespace,logical(1),quietly=TRUE)]
if(length(miss))stop("Missing packages: ",paste(miss,collapse=", "))

suppressPackageStartupMessages({
  library(data.table)
  library(comorbidity)
  library(sandwich)
  library(lmtest)
})

norm <- function(x) normalizePath(x,winslash="/",mustWork=FALSE)
safe_num <- function(x) suppressWarnings(as.numeric(as.character(x)))

# Byte-safe ASCII keys for legacy/malformed Windows CSV headers.
# Only ASCII schema tokens are needed (patientunitstayid/lab/offset/value).
ascii_key_one <- function(x) {
  tryCatch({
    rr <- charToRaw(as.character(x))
    ii <- as.integer(rr)
    ii <- ii[ii >= 32L & ii <= 126L]
    if (!length(ii)) return("")
    chartr(
      "ABCDEFGHIJKLMNOPQRSTUVWXYZ",
      "abcdefghijklmnopqrstuvwxyz",
      rawToChar(as.raw(ii))
    )
  }, error = function(e) "")
}
safe_ascii_text <- function(x) {
  vapply(as.character(x), ascii_key_one, character(1), USE.NAMES = FALSE)
}
safe_ascii_names <- function(x) safe_ascii_text(x)

norm_icd <- function(x) gsub("[^A-Z0-9]","",toupper(trimws(as.character(x))))
is_cad <- function(x) grepl("^41[0-4]",x)|grepl("^I2[0-5]",x)

audit <- data.table(
  stage=character(),root=character(),pattern=character(),
  timeout_sec=integer(),hits=integer(),note=character()
)
add_audit <- function(stage,root,pattern,timeout,hits,note=""){
  audit <<- rbind(audit,data.table(
    stage=stage,root=root,pattern=pattern,
    timeout_sec=as.integer(timeout),hits=as.integer(hits),note=note
  ),fill=TRUE)
  fwrite(audit,SEARCH_AUDIT)
}

get_drives <- function(){
  if(.Platform$OS.type!="windows")return(norm(path.expand("~")))
  ps <- "(Get-PSDrive -PSProvider FileSystem | Where-Object {$_.Root -match '^[A-Z]:\\\\$'} | Select-Object -ExpandProperty Root)"
  z <- tryCatch(system2(
    "powershell.exe",args=c("-NoProfile","-ExecutionPolicy","Bypass","-Command",ps),
    stdout=TRUE,stderr=TRUE,timeout=20L
  ),error=function(e)character())
  z<-trimws(z);z<-z[dir.exists(z)]
  unique(norm(z))
}
DRIVES <- get_drives()
UP <- Sys.getenv("USERPROFILE",unset=path.expand("~"))

where_search <- function(root,pattern,timeout=60L,stage="search"){
  if(.Platform$OS.type!="windows"||!dir.exists(root))return(character())
  heartbeat(paste0(stage,": ",pattern," under ",root))
  wr<-gsub("/","\\\\",norm(root))
  ans<-tryCatch(
    suppressWarnings(system2(
      "where.exe",args=c("/r",wr,pattern),
      stdout=TRUE,stderr=TRUE,timeout=timeout
    )),
    error=function(e){
      add_audit(stage,root,pattern,timeout,0,paste0("timeout/error: ",conditionMessage(e)))
      character()
    }
  )
  ans<-trimws(ans)
  ans<-unique(norm(ans[file.exists(ans)]))
  add_audit(stage,root,pattern,timeout,length(ans),"")
  ans
}

# --- exact 04 + 062 ---
roots <- unique(c(
  dirname(PKG_ROOT),dirname(dirname(PKG_ROOT)),
  file.path(UP,"Desktop"),file.path(UP,"Documents"),
  file.path(UP,"Downloads"),DRIVES
))
roots <- roots[dir.exists(roots)]

cohort_hits<-dx_hits<-character()
for(r in roots){
  tlim<-if(r%in%DRIVES)120L else 45L
  if(!length(cohort_hits))
    cohort_hits<-where_search(r,"04_eicu_analysis_cohort.csv",tlim,"find_04")
  if(!length(dx_hits))
    dx_hits<-where_search(r,"062_eicu_diagnosis_tokens_final.csv",tlim,"find_062")
  if(length(cohort_hits)&&length(dx_hits))break
}
if(!length(cohort_hits)||!length(dx_hits))
  stop("Could not auto-locate 04 + 062.",call.=FALSE)

validate_04<-function(p){
  x<-tryCatch(fread(p,nrows=5,showProgress=FALSE),error=function(e)NULL)
  !is.null(x)&&all(c("patientunitstayid","hospitalid","age_numeric",
                     "hospital_mortality","hospital_los_days","icu_los_days")%in%names(x))
}
validate_062<-function(p){
  x<-tryCatch(fread(p,nrows=5,showProgress=FALSE),error=function(e)NULL)
  if(is.null(x))return(FALSE)
  nm<-safe_ascii_names(names(x))
  "patientunitstayid"%in%nm&&any(grepl("diagn.*offset",nm))&&any(grepl("icd|code|token",nm))
}
cohort_hits<-cohort_hits[vapply(cohort_hits,validate_04,logical(1))]
dx_hits<-dx_hits[vapply(dx_hits,validate_062,logical(1))]
if(!length(cohort_hits)||!length(dx_hits))stop("04/062 schema validation failed.")

prefix_len<-function(a,b){
  aa<-strsplit(tolower(norm(a)),"/",fixed=TRUE)[[1]]
  bb<-strsplit(tolower(norm(b)),"/",fixed=TRUE)[[1]]
  n<-min(length(aa),length(bb));k<-0L
  for(i in seq_len(n)){if(identical(aa[i],bb[i]))k<-i else break}
  k
}
pairs<-expand.grid(i=seq_along(cohort_hits),j=seq_along(dx_hits))
pairs$prefix<-mapply(function(i,j)prefix_len(cohort_hits[i],dx_hits[j]),pairs$i,pairs$j)
pairs$score<-pairs$prefix*100+
  ifelse(grepl("eicu",cohort_hits[pairs$i],ignore.case=TRUE),25,0)+
  ifelse(grepl("eicu",dx_hits[pairs$j],ignore.case=TRUE),25,0)-
  ifelse(grepl("demo|sample|test",cohort_hits[pairs$i],ignore.case=TRUE),500,0)
pairs<-pairs[order(-pairs$score),,drop=FALSE]
COHORT_FILE<-cohort_hits[pairs$i[1]]
DX_FILE<-dx_hits[pairs$j[1]]
DATA_RAW_DIR<-dirname(DX_FILE)

# derive BMC root
ancestors<-function(p,n=12L){
  out<-character();x<-norm(p)
  for(i in seq_len(n)){
    out<-c(out,x);y<-norm(dirname(x))
    if(identical(x,y))break
    x<-y
  }
  unique(out)
}
aa<-ancestors(COHORT_FILE)
ix<-which(tolower(basename(aa))=="bmc geriatrics")
BMC_ROOT<-if(length(ix))aa[ix[1]] else norm(dirname(dirname(dirname(COHORT_FILE))))
PROJECT_ROOT<-norm(dirname(DATA_RAW_DIR))

log_msg("Matched 04: ",COHORT_FILE)
log_msg("Matched 062: ",DX_FILE)
log_msg("DATA_RAW_DIR: ",DATA_RAW_DIR)
log_msg("PROJECT_ROOT: ",PROJECT_ROOT)
log_msg("BMC_ROOT: ",BMC_ROOT)

# --- support 061-066 ---
support_names<-c(
  "061_eicu_patient_final.csv",
  "063_eicu_core12_lab_long_final.csv",
  "064_eicu_lab_map_final.csv",
  "065_eicu_project_config_final.csv",
  "066_eicu_run_summary_final.csv"
)
support_rows<-list()
support_paths<-list()

for(nm in support_names){
  p0<-file.path(DATA_RAW_DIR,nm)
  hits<-if(file.exists(p0))norm(p0) else character()
  if(!length(hits)){
    for(rr in unique(c(PROJECT_ROOT,BMC_ROOT,DRIVES))){
      if(!dir.exists(rr))next
      hits<-where_search(rr,nm,ifelse(rr%in%DRIVES,120L,60L),"support")
      if(length(hits))break
    }
  }
  if(length(hits)){
    # prefer closest to 062
    sc<-vapply(hits,function(x)prefix_len(x,DX_FILE),numeric(1))
    p<-hits[which.max(sc)]
    support_paths[[nm]]<-p
    info<-file.info(p)
    hdr<-tryCatch(names(fread(p,nrows=3,showProgress=FALSE)),error=function(e)character())
    support_rows[[nm]]<-data.table(
      filename=nm,path=p,size_bytes=info$size,
      columns=paste(hdr,collapse=" | "),found=TRUE
    )
    log_msg("Support matched: ",nm," -> ",p)
  } else {
    support_rows[[nm]]<-data.table(
      filename=nm,path=NA_character_,size_bytes=NA_real_,
      columns="",found=FALSE
    )
  }
}
fwrite(rbindlist(support_rows,fill=TRUE),file.path(OUT,"SUPPORT_FILE_AUDIT.csv"))

# Read config/run-summary and harvest existing path hints.
extra_roots<-character()
for(nm in c("065_eicu_project_config_final.csv","066_eicu_run_summary_final.csv")){
  p<-support_paths[[nm]]
  if(is.null(p)||!file.exists(p))next
  obj<-tryCatch(fread(p,showProgress=FALSE),error=function(e)NULL)
  if(is.null(obj))next
  vals<-unique(unlist(lapply(obj,as.character),use.names=FALSE))
  vals<-vals[!is.na(vals)&nzchar(vals)]
  for(v in vals){
    candidates<-unique(c(v,file.path(dirname(p),v),file.path(PROJECT_ROOT,v),file.path(BMC_ROOT,v)))
    for(cc in candidates){
      cc2<-norm(cc)
      if(dir.exists(cc2))extra_roots<-c(extra_roots,cc2)
      if(file.exists(cc2))extra_roots<-c(extra_roots,norm(dirname(cc2)))
    }
  }
}
extra_roots<-unique(extra_roots)
if(length(extra_roots))log_msg("Path hints from 065/066: ",paste(extra_roots,collapse=" | "))

# core data
heartbeat("read 04 + 062")
cohort<-fread(COHORT_FILE,showProgress=TRUE)
dx<-fread(DX_FILE,showProgress=TRUE)
if(nrow(cohort)!=7445L)stop("Selected 04 is not 7445 rows.")

sex_col<-if("gender_model"%in%names(cohort))"gender_model" else "gender"
cohort[,patientunitstayid:=safe_num(patientunitstayid)]
cohort[,age_model:=safe_num(age_numeric)]
cohort[,death:=as.integer(safe_num(hospital_mortality))]
cohort[,hospitalid_model:=as.character(hospitalid)]
cohort[,hospital_los_days:=safe_num(hospital_los_days)]
cohort[,icu_los_days:=safe_num(icu_los_days)]
cohort[,sex_male:=fifelse(
  tolower(trimws(as.character(get(sex_col))))=="male",1L,
  fifelse(tolower(trimws(as.character(get(sex_col))))=="female",0L,NA_integer_)
)]

pick<-function(nms,exact,regex=NULL){
  low<-safe_ascii_names(nms)
  for(e in exact){ix<-which(low==tolower(e));if(length(ix))return(nms[ix[1]])}
  if(!is.null(regex)){ix<-grep(regex,low,perl=TRUE);if(length(ix))return(nms[ix[1]])}
  stop("Required diagnosis column not found.")
}
idc<-pick(names(dx),c("patientunitstayid"))
offc<-pick(names(dx),c("diagnosisoffset","diagnosis_offset","diagnosisoffset_min"),"diagn.*offset")
ccands<-grep("icd|code|token",names(dx),ignore.case=TRUE,value=TRUE)
scorec<-function(nm){
  x<-dx[[nm]];x<-x[!is.na(x)]
  if(!length(x))return(-Inf)
  z<-norm_icd(head(x,5000))
  mean(grepl("^[A-Z0-9]{3,8}$",z))+2*mean(is_cad(z))
}
codec<-ccands[which.max(vapply(ccands,scorec,numeric(1)))]
dxt<-dx[,.(patientunitstayid=safe_num(get(idc)),
          diagnosisoffset=safe_num(get(offc)),
          code=norm_icd(get(codec)))]
dxt<-dxt[
  is.finite(patientunitstayid)&is.finite(diagnosisoffset)&
  nchar(code)>=3&patientunitstayid%in%cohort$patientunitstayid
]

weights<-c(mi=0,chf=1,pvd=1,cevd=1,dementia=1,cpd=1,rheumd=1,pud=1,
           mld=1,diab=1,diabwc=2,hp=2,rend=2,canc=2,msld=3,metacanc=6,aids=6)
domains<-names(weights)
calc_cci<-function(tok,ids){
  ids<-unique(ids)
  if(!nrow(tok))return(data.table(patientunitstayid=ids,cci_t=0))
  d9<-unique(tok[grepl("^[0-9]",code),.(id=patientunitstayid,code)])
  d10<-unique(tok[grepl("^[A-Z]",code),.(id=patientunitstayid,code)])
  mapper<-function(x,map){
    if(!nrow(x))return(NULL)
    a<-comorbidity::comorbidity(as.data.frame(x),id="id",code="code",map=map,assign0=TRUE)
    setDT(a);setnames(a,"id","patientunitstayid")
    a[,intersect(c("patientunitstayid",domains),names(a)),with=FALSE]
  }
  m9<-mapper(d9,"charlson_icd9_quan");m10<-mapper(d10,"charlson_icd10_quan")
  z<-data.table(patientunitstayid=ids)
  if(!is.null(m9)){cc<-setdiff(names(m9),"patientunitstayid");setnames(m9,cc,paste0(cc,"_9"));z<-merge(z,m9,by="patientunitstayid",all.x=TRUE)}
  if(!is.null(m10)){cc<-setdiff(names(m10),"patientunitstayid");setnames(m10,cc,paste0(cc,"_10"));z<-merge(z,m10,by="patientunitstayid",all.x=TRUE)}
  for(d in domains){
    a<-paste0(d,"_9");b<-paste0(d,"_10")
    v1<-if(a%in%names(z))z[[a]] else rep(0,nrow(z))
    v2<-if(b%in%names(z))z[[b]] else rep(0,nrow(z))
    v1[is.na(v1)]<-0;v2[is.na(v2)]<-0;z[[d]]<-pmax(v1,v2)
  }
  z[diabwc==1,diab:=0];z[msld==1,mld:=0];z[metacanc==1,canc:=0]
  z[,cci_t:=0]
  for(d in domains)z[,cci_t:=cci_t+get(d)*weights[[d]]]
  z[,.(patientunitstayid,cci_t)]
}
thresholds<-c(0L,360L,1440L,2160L)
cci_by_t<-list();cad_by_t<-list()
for(tmin in thresholds){
  tt<-dxt[diagnosisoffset<=tmin]
  cci_by_t[[as.character(tmin)]]<-calc_cci(tt,cohort$patientunitstayid)
  cad_by_t[[as.character(tmin)]]<-unique(tt[is_cad(code),patientunitstayid])
}
cci_full<-calc_cci(dxt,cohort$patientunitstayid)

expected<-fread(file.path(PKG_ROOT,"definitions","expected_old_S29.csv"))
fast_auc<-function(y,p){
  ok<-is.finite(y)&is.finite(p);y<-y[ok];p<-p[ok]
  n1<-sum(y==1);n0<-sum(y==0)
  if(!n1||!n0)return(NA_real_)
  r<-rank(p,ties.method="average")
  (sum(r[y==1])-n1*(n1+1)/2)/(n1*n0)
}

fingerprint<-function(obj,fi_col,gate_type="none",gate_col=NA_character_,label=""){
  x<-data.table(
    patientunitstayid=safe_num(obj[["patientunitstayid"]]),
    fi=safe_num(obj[[fi_col]])
  )
  if (gate_type == "count") {
    x[, gate := safe_num(obj[[gate_col]]) >= 9]
  } else if (gate_type == "eval") {
    g <- obj[[gate_col]]
    if (is.logical(g)) {
      x[, gate := g]
    } else {
      gn <- safe_num(g)
      if (sum(is.finite(gn)) > 0) {
        x[, gate := gn == 1]
      } else {
        x[, gate := tolower(trimws(as.character(g))) %in%
             c("true", "yes", "y", "evaluable", "eligible")]
      }
    }
  } else {
    x[, gate := TRUE]
  }
  x<-x[is.finite(patientunitstayid)&is.finite(fi)&gate]
  x<-x[!duplicated(patientunitstayid)]
  base<-merge(cohort,x[,.(patientunitstayid,fi)],by="patientunitstayid",all.x=TRUE)
  rows<-list()
  for(tmin in c(360L,1440L,2160L)){
    d<-merge(base,cci_by_t[[as.character(tmin)]],by="patientunitstayid",all.x=TRUE)
    d[is.na(cci_t),cci_t:=0]
    d<-d[
      patientunitstayid%in%cad_by_t[[as.character(tmin)]]&
      is.finite(fi)&is.finite(age_model)&!is.na(sex_male)&
      !is.na(death)&!is.na(hospitalid_model)
    ]
    if(nrow(d)<100)next
    d[,fi_per_01:=fi/0.1]
    b<-glm(death~age_model+sex_male,data=d,family=binomial())
    c1<-glm(death~age_model+sex_male+cci_t,data=d,family=binomial())
    f1<-glm(death~age_model+sex_male+fi_per_01,data=d,family=binomial())
    u<-glm(death~age_model+sex_male+cci_t+fi_per_01,data=d,family=binomial())
    er<-expected[threshold_min==tmin]
    rr<-data.table(
      label=label,threshold_min=tmin,n=nrow(d),deaths=sum(d$death),
      hospitals=uniqueN(d$hospitalid_model),median_fi=median(d$fi),
      base_auc=fast_auc(d$death,predict(b,type="response")),
      base_cci_auc=fast_auc(d$death,predict(c1,type="response")),
      base_fi_auc=fast_auc(d$death,predict(f1,type="response")),
      full_auc=fast_auc(d$death,predict(u,type="response")),
      cci_or_full=exp(coef(u)["cci_t"]),
      fi_or_full=exp(coef(u)["fi_per_01"])
    )
    rr[,exact_old_S29:=
      n==er$n&deaths==er$deaths&hospitals==er$hospitals&
      abs(median_fi-er$median_fi)<1e-10&
      abs(base_auc-er$base_auc)<1e-8&
      abs(base_cci_auc-er$base_cci_auc)<1e-8&
      abs(base_fi_auc-er$base_fi_auc)<1e-8&
      abs(full_auc-er$full_auc)<1e-8&
      abs(cci_or_full-er$cci_or_full)<1e-7&
      abs(fi_or_full-er$fi_or_full)<1e-7]
    rows[[as.character(tmin)]]<-rr
  }
  if(!length(rows))return(NULL)
  rbindlist(rows)
}

# The historical FI source can be verified independently of the historical
# CCI implementation. These are FI/cohort-side quantities only.
fi_side_exact <- function(fp) {
  if (is.null(fp) || nrow(fp) != 3L) return(FALSE)
  ok <- logical(nrow(fp))
  for (i in seq_len(nrow(fp))) {
    r <- fp[i]
    e <- expected[threshold_min == r$threshold_min]
    ok[i] <-
      r$n == e$n &&
      r$deaths == e$deaths &&
      r$hospitals == e$hospitals &&
      abs(r$median_fi - e$median_fi) < 1e-10 &&
      abs(r$base_auc - e$base_auc) < 1e-10 &&
      abs(r$base_fi_auc - e$base_fi_auc) < 1e-10
  }
  all(ok)
}

# --- script forensics ---
heartbeat("deep-scan BMC project scripts")
script_paths<-character()
if(.Platform$OS.type=="windows"&&dir.exists(BMC_ROOT)){
  ps<-paste0(
    "$r='",gsub("'","''",gsub("/","\\\\",BMC_ROOT,fixed=TRUE),fixed=TRUE),"';",
    "Get-ChildItem -LiteralPath $r -Recurse -File -ErrorAction SilentlyContinue | ",
    "Where-Object { $_.Length -lt 5242880 -and $_.Extension -in '.R','.Rmd','.py','.sql','.txt','.md' } | ",
    "Select-Object -ExpandProperty FullName"
  )
  script_paths<-tryCatch(system2(
    "powershell.exe",args=c("-NoProfile","-ExecutionPolicy","Bypass","-Command",ps),
    stdout=TRUE,stderr=TRUE,timeout=120L
  ),error=function(e)character())
}
script_paths<-unique(norm(trimws(script_paths[file.exists(script_paths)])))
terms<-c("S29","time.?anchor","pre.?ICU","cci_early","diagnosisoffset",
         "66792","1\\.42630591695301","fixed.?6.?h","S30")
script_hits<-list()
referenced_data<-character()
for(p in script_paths){
  txt<-tryCatch(paste(readLines(p,warn=FALSE,encoding="UTF-8"),collapse="\n"),
                error=function(e)"")
  if(!nzchar(txt))next
  mh<-vapply(terms,function(t)grepl(t,txt,ignore.case=TRUE,perl=TRUE),logical(1))
  if(any(mh)){
    script_hits[[p]]<-data.table(
      path=p,matched_terms=paste(terms[mh],collapse=" | "),
      match_count=sum(mh)
    )
    # quoted data-file references
    m<-gregexpr("['\\\"]([^'\\\"]+\\.(csv(?:\\.gz)?|rds|parquet))['\\\"]",
                txt,ignore.case=TRUE,perl=TRUE)
    vals<-regmatches(txt,m)[[1]]
    if(length(vals)&&vals[1]!="-1"){
      vals<-gsub("^['\\\"]|['\\\"]$","",vals)
      for(v in vals){
        candidates<-unique(c(
          v,file.path(dirname(p),v),file.path(PROJECT_ROOT,v),
          file.path(BMC_ROOT,v),file.path(DATA_RAW_DIR,v)
        ))
        for(cc in candidates)if(file.exists(cc))referenced_data<-c(referenced_data,norm(cc))
      }
    }
  }
}
if (length(script_hits)) {
  fwrite(
    rbindlist(script_hits),
    file.path(OUT, "SCRIPT_FORENSICS.csv")
  )
} else {
  fwrite(
    data.table(
      path = character(),
      matched_terms = character(),
      match_count = integer()
    ),
    file.path(OUT, "SCRIPT_FORENSICS.csv")
  )
}
referenced_data<-unique(referenced_data)

# --- enumerate moderate data files under BMC project ---
heartbeat("enumerate patient-level data candidates")
data_paths<-character()
if(.Platform$OS.type=="windows"&&dir.exists(BMC_ROOT)){
  ps<-paste0(
    "$r='",gsub("'","''",gsub("/","\\\\",BMC_ROOT,fixed=TRUE),fixed=TRUE),"';",
    "Get-ChildItem -LiteralPath $r -Recurse -File -ErrorAction SilentlyContinue | ",
    "Where-Object { $_.Length -lt 314572800 -and ($_.Name -match '(?i)\\.(csv|csv\\.gz|rds|parquet)$') } | ",
    "Select-Object -ExpandProperty FullName"
  )
  data_paths<-tryCatch(system2(
    "powershell.exe",args=c("-NoProfile","-ExecutionPolicy","Bypass","-Command",ps),
    stdout=TRUE,stderr=TRUE,timeout=150L
  ),error=function(e)character())
}
data_paths<-unique(c(norm(trimws(data_paths[file.exists(data_paths)])),referenced_data))
data_paths<-data_paths[!grepl("BMC_Geriatrics_Draft8_Reviewer8_",data_paths,fixed=TRUE)]

patient_audit<-list()
fp_list<-list()
verified_obj<-NULL
verified_meta<-NULL

read_candidate<-function(p,full=FALSE){
  ext<-safe_ascii_text(basename(p))
  if(grepl("\\.rds$",ext)){
    if(!full)return(tryCatch({
      x<-readRDS(p)
      if(is.data.frame(x))as.data.table(head(x,50))else NULL
    },error=function(e)NULL))
    return(tryCatch({
      x<-readRDS(p)
      if(is.data.frame(x))as.data.table(x)else NULL
    },error=function(e)NULL))
  }
  if(grepl("\\.parquet$",ext))return(NULL)
  tryCatch(
    suppressWarnings(
      fread(p,nrows=if(full)Inf else 50,showProgress=FALSE)
    ),
    error=function(e){
      log_msg("SKIP unreadable patient candidate: ",p,
              " | ",conditionMessage(e))
      NULL
    }
  )
}

for(p in data_paths){
  smp<-read_candidate(p,FALSE)
  if(is.null(smp)||!"patientunitstayid"%in%names(smp))next
  smp_keys <- safe_ascii_names(names(smp))
  fi_ix <- grep(
    "(^fi$|fi_lab|pre.*fi|fi.*pre|strict.*fi|tim.*fi)",
    smp_keys, perl = TRUE
  )
  fi_cols <- names(smp)[fi_ix]
  if(!length(fi_cols))next
  patient_audit[[p]]<-data.table(
    path=p,size_bytes=file.info(p)$size,
    fi_columns=paste(fi_cols,collapse=" | "),
    status="schema_candidate"
  )
  obj<-read_candidate(p,TRUE)
  if(is.null(obj))next
  obj_keys <- safe_ascii_names(names(obj))
  count_cols <- names(obj)[grep("avail|n_item|n_domain|count", obj_keys, perl=TRUE)]
  eval_cols <- names(obj)[grep("evaluable|eligible", obj_keys, perl=TRUE)]
  for(fc in fi_cols){
    vals<-safe_num(obj[[fc]])
    vv<-vals[is.finite(vals)]
    if(length(vv)<100||any(vv<0|vv>1))next
    gates<-list(list(type="none",col=NA_character_))
    for(gc in count_cols)gates<-c(gates,list(list(type="count",col=gc)))
    for(gc in eval_cols)gates<-c(gates,list(list(type="eval",col=gc)))
    for(g in gates){
      lab<-paste0(basename(p),"::",fc,"::",g$type,"::",
                  ifelse(is.na(g$col),"none",g$col))
      fp<-tryCatch(fingerprint(obj,fc,g$type,g$col,lab),error=function(e)NULL)
      if(!is.null(fp)){
        fp_list[[lab]]<-fp
        if(nrow(fp)==3L&&all(fp$exact_old_S29)&&is.null(verified_obj)){
          verified_obj<-obj
          verified_meta<-list(path=p,fi_col=fc,gate_type=g$type,gate_col=g$col,label=lab)
        }
      }
    }
  }
}
if (length(patient_audit)) {
  fwrite(
    rbindlist(patient_audit, fill = TRUE),
    file.path(OUT, "PATIENT_FI_CANDIDATE_AUDIT.csv")
  )
} else {
  fwrite(
    data.table(
      path = character(),
      size_bytes = numeric(),
      fi_columns = character(),
      status = character()
    ),
    file.path(OUT, "PATIENT_FI_CANDIDATE_AUDIT.csv")
  )
}
if(length(fp_list))
  fwrite(rbindlist(fp_list,fill=TRUE),file.path(OUT,"HISTORICAL_FI_FINGERPRINTS.csv"))

# --- event-level lab candidate discovery ---
final_ranges<-fread(file.path(PKG_ROOT,"definitions","final_locked_ranges.csv"))
prov_ranges<-fread(file.path(PKG_ROOT,"definitions","provisional_old_ranges.csv"))
labmap<-fread(file.path(PKG_ROOT,"definitions","lab_mapping.csv"))
domain_set<-final_ranges$domain

lab_candidates<-character()

# known 063
p063<-support_paths[["063_eicu_core12_lab_long_final.csv"]]
if(!is.null(p063)&&file.exists(p063))lab_candidates<-c(lab_candidates,p063)

# BMC moderate files whose name suggests lab
lab_candidates<-c(
  lab_candidates,
  data_paths[grepl("lab|laboratory",basename(data_paths),ignore.case=TRUE)]
)

# wildcard all drives, no manual selection
if(is.null(verified_obj)){
  heartbeat("auto-search all local drives for arbitrary lab-like CSV names")
  for(rr in unique(c(extra_roots,PROJECT_ROOT,BMC_ROOT,DRIVES))){
    if(!dir.exists(rr))next
    tlim<-if(rr%in%DRIVES)150L else 75L
    for(pt in c("*lab*.csv","*lab*.csv.gz","*laboratory*.csv","*laboratory*.csv.gz")){
      z<-where_search(rr,pt,tlim,"lab_wildcard")
      lab_candidates<-c(lab_candidates,z)
    }
    # stop drive expansion once many candidates obtained
    if(length(unique(lab_candidates))>100L)break
  }
}
lab_candidates<-unique(norm(lab_candidates[file.exists(lab_candidates)]))

detect_lab_schema <- function(p) {
  x <- tryCatch(
    suppressWarnings(fread(p, nrows = 100, showProgress = FALSE)),
    error = function(e) {
      log_msg("SKIP unreadable lab candidate: ", p,
              " | ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(x)) return(NULL)

  nms <- names(x)
  low <- safe_ascii_names(nms)

  idix <- which(low == "patientunitstayid")
  if (!length(idix)) return(NULL)

  offix <- grep("offset", low, perl = TRUE)
  nameix <- grep(
    "labname|lab_name|labkey|lab_key|domain|testname|test_name|laboratory",
    low, perl = TRUE
  )
  valix <- grep(
    "labresult$|lab_result$|labvalue$|lab_value$|(^|_)value$|(^|_)result$",
    low, perl = TRUE
  )
  if (!length(offix) || !length(nameix) || !length(valix)) return(NULL)

  off_score <- ifelse(
    grepl("lab.*offset|result.*offset", low[offix], perl = TRUE), 10, 0
  ) + ifelse(grepl("offset$", low[offix], perl = TRUE), 2, 0)

  name_score <- ifelse(
    grepl("^labname$|^lab_name$", low[nameix], perl = TRUE), 10, 0
  ) + ifelse(grepl("domain|laboratory", low[nameix], perl = TRUE), 2, 0)

  val_score <- ifelse(
    grepl("^labresult$|^lab_result$", low[valix], perl = TRUE), 10, 0
  ) + ifelse(grepl("value|result", low[valix], perl = TRUE), 2, 0)

  list(
    id = nms[idix[1]],
    offset = nms[offix[which.max(off_score)]],
    name = nms[nameix[which.max(name_score)]],
    value = nms[valix[which.max(val_score)]]
  )
}

event_audit<-list()
event_fps<-list()
raw_final_fi<-raw_prov_fi<-NULL
raw_status<-NULL
raw_source<-NULL
raw_hist_fp<-NULL
raw_final_fp_confirm<-NULL

ensure_duckdb<-function(){
  need<-c("DBI","duckdb")
  miss<-need[!vapply(need,requireNamespace,logical(1),quietly=TRUE)]
  if(length(miss)){
    if(.Platform$OS.type=="windows")
      try(install.packages(miss,dependencies=TRUE,type="binary"),silent=TRUE)
    miss<-need[!vapply(need,requireNamespace,logical(1),quietly=TRUE)]
    if(length(miss))install.packages(miss,dependencies=TRUE)
  }
  all(vapply(need,requireNamespace,logical(1),quietly=TRUE))
}

make_fi_from_events<-function(ev,ranges){
  s<-merge(copy(ev),ranges,by="domain",all.x=TRUE)
  s[,abnormal:=as.integer(value<lower|value>upper)]
  f<-s[,.(n_available=.N,fi=sum(abnormal)/.N),by=patientunitstayid]
  f[n_available>=9]
}

if(is.null(verified_obj)){
  for(p in lab_candidates){
    sc <- tryCatch(
      detect_lab_schema(p),
      error = function(e) {
        log_msg("SKIP lab candidate after schema error: ", p,
                " | ", conditionMessage(e))
        NULL
      }
    )
    if (is.null(sc)) next
    sz <- file.info(p)$size
    log_msg("Lab-like schema candidate: ", p)

    # Read columns. Moderate files use fread; huge files use DuckDB.
    ev<-NULL
    if(is.finite(sz)&&sz<500*1024^2){
      allx<-tryCatch(fread(p,select=c(sc$id,sc$offset,sc$name,sc$value),
                          showProgress=FALSE),error=function(e)NULL)
      if(!is.null(allx)){
        setnames(allx,c(sc$id,sc$offset,sc$name,sc$value),
                 c("patientunitstayid","offset","lab_name","value"))
        ev<-allx
      }
    } else if(ensure_duckdb()){
      qpath<-gsub("'","''",normalizePath(p,winslash="/",mustWork=TRUE),fixed=TRUE)
      con<-DBI::dbConnect(duckdb::duckdb(),dbdir=":memory:")
      q<-sprintf(
        "SELECT \"%s\" AS patientunitstayid, \"%s\" AS offset, \"%s\" AS lab_name, \"%s\" AS value FROM read_csv_auto('%s', header=true, all_varchar=true) WHERE try_cast(\"%s\" AS DOUBLE) BETWEEN -1440 AND 0",
        sc$id,sc$offset,sc$name,sc$value,qpath,sc$offset
      )
      ev<-tryCatch(as.data.table(DBI::dbGetQuery(con,q)),error=function(e)NULL)
      try(DBI::dbDisconnect(con,shutdown=TRUE),silent=TRUE)
    }
    if(is.null(ev))next

    ev[,patientunitstayid:=safe_num(patientunitstayid)]
    ev[,offset:=safe_num(offset)]
    ev[,value:=safe_num(value)]
    ev<-ev[
      is.finite(patientunitstayid)&is.finite(offset)&is.finite(value)&
      patientunitstayid%in%cohort$patientunitstayid
    ]
    ev<-ev[offset>=-1440&offset<=0]
    if(!nrow(ev))next

    # Map lab_name either already-domain or raw source name.
    ev[,name_clean:=safe_ascii_text(trimws(as.character(lab_name)))]
    direct<-copy(ev[name_clean%in%safe_ascii_text(domain_set)])
    if(nrow(direct)){
      mp<-setNames(domain_set,safe_ascii_text(domain_set))
      direct[,domain:=unname(mp[name_clean])]
      direct[,priority:=1L]
      direct[,qc_low:=-Inf];direct[,qc_high:=Inf]
    }
    raw<-merge(
      ev,labmap,
      by.x="lab_name",by.y="raw_name",
      all=FALSE,allow.cartesian=TRUE
    )
    if(nrow(raw))raw<-raw[value>=qc_low&value<=qc_high]
    mapped<-rbindlist(list(
      if(nrow(direct))direct[,.(patientunitstayid,offset,value,domain,priority)] else NULL,
      if(nrow(raw))raw[,.(patientunitstayid,offset,value,domain,priority)] else NULL
    ),fill=TRUE)
    if(!nrow(mapped))next
    mapped<-unique(mapped)

    mult<-mapped[,.(events=.N),by=.(patientunitstayid,domain)]
    p_multi<-mean(mult$events>1)
    max_events<-max(mult$events)
    event_audit[[p]]<-data.table(
      path=p,size_bytes=sz,mapped_rows=nrow(mapped),
      unique_patient_domain=nrow(mult),
      proportion_patient_domain_with_multiple_events=p_multi,
      max_events_per_patient_domain=max_events
    )

    # select closest to admission among all pre-ICU events
    setorder(mapped,patientunitstayid,domain,-offset,priority)
    sel<-mapped[,.SD[1L],by=.(patientunitstayid,domain)]
    ff<-make_fi_from_events(sel,final_ranges)
    pp<-make_fi_from_events(sel,prov_ranges)
    fpf<-tryCatch(fingerprint(ff,"fi","count","n_available",
                             paste0("EVENT_FINAL::",basename(p))),error=function(e)NULL)
    fpp<-tryCatch(fingerprint(pp,"fi","count","n_available",
                             paste0("EVENT_PROV::",basename(p))),error=function(e)NULL)
    if(!is.null(fpf))event_fps[[paste0(p,"::final")]]<-fpf
    if(!is.null(fpp))event_fps[[paste0(p,"::prov")]]<-fpp

    # Confirm FI provenance separately from the historical CCI implementation.
    if (fi_side_exact(fpf)) {
      raw_final_fi <- ff
      raw_prov_fi <- pp
      raw_status <- "current_locked_fi_confirmed"
      raw_source <- p
      raw_hist_fp <- fpf
      raw_final_fp_confirm <- fpf
      log_msg("FI provenance confirmed with CURRENT LOCKED ranges: ", p)
      break
    }
    if (fi_side_exact(fpp)) {
      raw_final_fi <- ff
      raw_prov_fi <- pp
      raw_status <- "old_provisional_fi_confirmed"
      raw_source <- p
      raw_hist_fp <- fpp
      raw_final_fp_confirm <- fpf
      log_msg("FI provenance confirmed for OLD S29 under PROVISIONAL ranges: ", p)
      break
    }
  }
}

if (length(event_audit)) {
  fwrite(
    rbindlist(event_audit, fill = TRUE),
    file.path(OUT, "LAB_EVENT_CANDIDATE_AUDIT.csv")
  )
} else {
  fwrite(
    data.table(path = character()),
    file.path(OUT, "LAB_EVENT_CANDIDATE_AUDIT.csv")
  )
}
if(length(event_fps))
  fwrite(rbindlist(event_fps,fill=TRUE),file.path(OUT,"LAB_EVENT_FINGERPRINTS.csv"))

# --- provenance / correction decision ---
FI_DT<-NULL
FI_SOURCE<-NULL
REFERENCE_STATUS<-NULL
CORRECTION_MODE<-FALSE
HISTORICAL_FI_DT<-NULL

# A non-provisional historical patient-level artifact that exactly reproduces
# old S29 can be accepted directly if one exists.
if(!is.null(verified_obj)){
  m<-verified_meta
  tmp_fi<-data.table(
    patientunitstayid=safe_num(verified_obj[["patientunitstayid"]]),
    fi=safe_num(verified_obj[[m$fi_col]])
  )
  if (m$gate_type == "count") {
    tmp_fi[, gate := safe_num(verified_obj[[m$gate_col]]) >= 9]
    tmp_fi <- tmp_fi[gate]
  } else if (m$gate_type == "eval") {
    g <- verified_obj[[m$gate_col]]
    gn <- safe_num(g)
    if (sum(is.finite(gn)) > 0) {
      tmp_fi[, gate := gn == 1]
    } else {
      tmp_fi[, gate := safe_ascii_text(trimws(as.character(g))) %in%
               c("true","yes","y","evaluable","eligible")]
    }
    tmp_fi <- tmp_fi[gate]
  }
  tmp_fi<-tmp_fi[,.(patientunitstayid,fi)]

  verified_keys <- safe_ascii_names(names(verified_obj))
  statuscols <- names(verified_obj)[
    grep("reference.*status|fi_reference_status", verified_keys, perl=TRUE)
  ]
  hist_status <- if (length(statuscols)) {
    paste(unique(na.omit(as.character(verified_obj[[statuscols[1]]]))),
          collapse=" | ")
  } else {
    "UNKNOWN"
  }

  if (!grepl("provisional", hist_status, ignore.case=TRUE)) {
    FI_DT <- tmp_fi
    HISTORICAL_FI_DT <- tmp_fi
    FI_SOURCE <- paste0("historical_patient_file:",m$path,"::",m$fi_col)
    REFERENCE_STATUS <- hist_status
  }
}

# V11 forensic output established that the old S29 FI side is reproduced
# exactly by 063 under provisional ranges. That is sufficient to identify
# the historical FI source even though the historical CCI implementation
# differs slightly.
if (is.null(FI_DT) && identical(raw_status,"old_provisional_fi_confirmed")) {
  CORRECTION_MODE <- TRUE
  HISTORICAL_FI_DT <- raw_prov_fi[,.(patientunitstayid,fi)]
  FI_DT <- raw_final_fi[,.(patientunitstayid,fi)]
  FI_SOURCE <- paste0(
    "corrected_preICU_from_event_source_CURRENT_LOCKED:", raw_source
  )
  REFERENCE_STATUS <- "locked_mimic_median_external_v1"

  hist_evidence <- copy(raw_hist_fp)
  hist_evidence[, `:=`(
    old_S29_FI_side_exact = TRUE,
    interpretation = "FI/cohort provenance exact; historical CCI implementation differs"
  )]
  fwrite(hist_evidence,file.path(OUT,"OLD_S29_FI_PROVENANCE_CONFIRMED.csv"))

  cci_disc <- copy(raw_hist_fp)
  cci_disc <- merge(
    cci_disc,
    expected[,.(threshold_min,
                expected_base_cci_auc=base_cci_auc,
                expected_full_auc=full_auc,
                expected_cci_or_full=cci_or_full,
                expected_fi_or_full=fi_or_full)],
    by="threshold_min", all.x=TRUE
  )
  cci_disc[, `:=`(
    diff_base_cci_auc = base_cci_auc - expected_base_cci_auc,
    diff_full_auc = full_auc - expected_full_auc,
    diff_cci_or_full = cci_or_full - expected_cci_or_full,
    diff_fi_or_full = fi_or_full - expected_fi_or_full
  )]
  fwrite(
    cci_disc,
    file.path(OUT,"OLD_S29_CCI_IMPLEMENTATION_DISCREPANCY.csv")
  )

  writeLines(c(
    "AUDIT CONCLUSION",
    "================",
    "The old S29 pre-ICU FI source is identified.",
    paste0("Event source: ", raw_source),
    "",
    "Under the historical PROVISIONAL FI ranges, 6/24/36 h reproduce exactly:",
    "- n",
    "- deaths",
    "- hospitals",
    "- median FI",
    "- Base AUC",
    "- Base+FI AUC",
    "",
    "The remaining discrepancy is confined to CCI-containing models.",
    "Old S29 therefore must not be forced into the current manuscript unchanged.",
    "",
    "V15 regenerates S29, fixed-6h S30 and true landmarks using:",
    "- CURRENT LOCKED pre-ICU FI ranges",
    "- time-anchored Quan CCI from diagnosis tokens",
    "- myocardial infarction component excluded",
    "- all 6/24/36 h thresholds and both risk sets retained."
  ),file.path(OUT,"OLD_S29_MUST_BE_REPLACED_READ_ME.txt"),useBytes=TRUE)

} else if (is.null(FI_DT) &&
           identical(raw_status,"current_locked_fi_confirmed")) {
  FI_DT <- raw_final_fi[,.(patientunitstayid,fi)]
  HISTORICAL_FI_DT <- raw_final_fi[,.(patientunitstayid,fi)]
  FI_SOURCE <- paste0("event_reconstruction_current_locked:",raw_source)
  REFERENCE_STATUS <- "locked_mimic_median_external_v1"

} else if (is.null(FI_DT)) {
  heartbeat("STOP: FI provenance could not be established")
  writeLines(c(
    "V15 could not establish FI-side provenance.",
    "No new landmark was run.",
    "Do not search files manually; upload the complete output folder."
  ),file.path(OUT,"FI_PROVENANCE_FAILED.txt"),useBytes=TRUE)
  stop("FI provenance failed.",call.=FALSE)
}

FI_DT<-FI_DT[is.finite(patientunitstayid)&is.finite(fi)]
FI_DT<-FI_DT[!duplicated(patientunitstayid)]

writeLines(c(
  paste0("FI source: ",FI_SOURCE),
  paste0("Reference status: ",REFERENCE_STATUS),
  paste0("Correction mode: ",CORRECTION_MODE)
),file.path(OUT,"VERIFIED_FI_PROVENANCE.txt"),useBytes=TRUE)

# Historical CCI(t) audit is informative only; it does not block the corrected
# analysis. The corrected reviewer-facing CCI(t) below is the explicit current
# token-based Quan definition.
heartbeat("auto-audit historical CCI candidate files")

cci_candidate_rows <- list()
cci_best_rows <- list()

if (!is.null(HISTORICAL_FI_DT)) {
  hist_base <- merge(
    cohort,
    HISTORICAL_FI_DT[,.(patientunitstayid,fi_hist=fi)],
    by="patientunitstayid",
    all.x=TRUE
  )

  cci_paths <- unique(c(
    data_paths[grepl("cci", safe_ascii_text(data_paths), perl=TRUE)],
    referenced_data[grepl("cci", safe_ascii_text(referenced_data), perl=TRUE)]
  ))

  # Cheap header screen for files whose filename does not contain "cci".
  for (pp in setdiff(data_paths, cci_paths)) {
    sm <- read_candidate(pp, FALSE)
    if (is.null(sm) || !"patientunitstayid" %in% names(sm)) next
    keys <- safe_ascii_names(names(sm))
    if (any(grepl("cci|charlson", keys, perl=TRUE))) {
      cci_paths <- c(cci_paths, pp)
    }
  }
  cci_paths <- unique(cci_paths)

  eval_cci_col <- function(obj,col,tmin,label) {
    z <- data.table(
      patientunitstayid=safe_num(obj[["patientunitstayid"]]),
      cci_candidate=safe_num(obj[[col]])
    )
    z <- z[is.finite(patientunitstayid)&is.finite(cci_candidate)]
    z <- z[!duplicated(patientunitstayid)]
    if(nrow(z)<100L)return(NULL)

    d <- merge(hist_base,z,by="patientunitstayid",all.x=TRUE)
    d <- d[
      patientunitstayid%in%cad_by_t[[as.character(tmin)]]&
      is.finite(fi_hist)&is.finite(cci_candidate)&
      is.finite(age_model)&!is.na(sex_male)&
      !is.na(death)&!is.na(hospitalid_model)
    ]
    if(nrow(d)<100L)return(NULL)

    d[,fi_per_01:=fi_hist/0.1]
    c1<-glm(death~age_model+sex_male+cci_candidate,
            data=d,family=binomial())
    u<-glm(death~age_model+sex_male+cci_candidate+fi_per_01,
           data=d,family=binomial())
    er<-expected[threshold_min==tmin]

    rr<-data.table(
      source_label=label,
      threshold_min=tmin,
      n=nrow(d),deaths=sum(d$death),
      hospitals=uniqueN(d$hospitalid_model),
      base_cci_auc=fast_auc(d$death,predict(c1,type="response")),
      full_auc=fast_auc(d$death,predict(u,type="response")),
      cci_or_full=exp(coef(u)["cci_candidate"]),
      fi_or_full=exp(coef(u)["fi_per_01"]),
      expected_base_cci_auc=er$base_cci_auc,
      expected_full_auc=er$full_auc,
      expected_cci_or_full=er$cci_or_full,
      expected_fi_or_full=er$fi_or_full
    )
    rr[,cci_fingerprint_error:=
      abs(n-er$n)*100+
      abs(deaths-er$deaths)*100+
      abs(hospitals-er$hospitals)*50+
      abs(base_cci_auc-er$base_cci_auc)*1000+
      abs(full_auc-er$full_auc)*1000+
      abs(cci_or_full-er$cci_or_full)*100+
      abs(fi_or_full-er$fi_or_full)*100
    ]
    rr[,exact_historical_cci:=
      n==er$n&deaths==er$deaths&hospitals==er$hospitals&
      abs(base_cci_auc-er$base_cci_auc)<1e-8&
      abs(full_auc-er$full_auc)<1e-8&
      abs(cci_or_full-er$cci_or_full)<1e-7&
      abs(fi_or_full-er$fi_or_full)<1e-7
    ]
    rr
  }

  for(pp in cci_paths){
    obj<-read_candidate(pp,TRUE)
    if(is.null(obj)||!"patientunitstayid"%in%names(obj))next
    keys<-safe_ascii_names(names(obj))
    ix<-grep("cci|charlson",keys,perl=TRUE)
    if(!length(ix))next

    for(jj in ix){
      col<-names(obj)[jj]
      vv<-safe_num(obj[[col]])
      vv<-vv[is.finite(vv)]
      if(length(vv)<100L||any(vv<0|vv>100))next

      for(tm in c(360L,1440L,2160L)){
        lab<-paste0(basename(pp),"::",col)
        rr<-tryCatch(eval_cci_col(obj,col,tm,lab),error=function(e)NULL)
        if(!is.null(rr)){
          cci_candidate_rows[[paste(lab,tm,sep="::")]]<-rr
        }
      }
    }
  }

  if(length(cci_candidate_rows)){
    cci_all<-rbindlist(cci_candidate_rows,fill=TRUE)
    fwrite(
      cci_all,
      file.path(OUT,"HISTORICAL_CCI_CANDIDATE_FINGERPRINTS.csv")
    )
    for(tm in c(360L,1440L,2160L)){
      zz<-cci_all[threshold_min==tm][order(cci_fingerprint_error)]
      if(nrow(zz))cci_best_rows[[as.character(tm)]]<-zz[1]
    }
    if(length(cci_best_rows)){
      fwrite(
        rbindlist(cci_best_rows,fill=TRUE),
        file.path(OUT,"HISTORICAL_CCI_BEST_MATCH_BY_TIME.csv")
      )
    }
  }
}

writeLines(c(
  "CURRENT CORRECTED ANALYSIS DEFINITION",
  "====================================",
  "FI-Lab: current locked 12-domain profile, -24h to 0h, >=9/12.",
  "CAD(t): CAD diagnosis documented by landmark t.",
  "CCI(t): Quan mapping from diagnosis tokens documented by t; MI component excluded.",
  "Landmarks: 6h, 24h, 36h.",
  "Risk sets: Hospital and stricter ICU.",
  "Outcome: final in-hospital mortality among patients still at risk at landmark.",
  "All prespecified thresholds/risk sets retained regardless of P value."
),file.path(OUT,"CURRENT_CORRECTED_ANALYSIS_DEFINITION.txt"),useBytes=TRUE)

# --- modeling ---
robust_effects<-function(fit,d,nm,analysis,label){
  V<-sandwich::vcovCL(fit,cluster=d$hospitalid_model,type="HC0")
  ct<-lmtest::coeftest(fit,vcov.=V)
  wanted<-intersect(c("cci_t","fi_per_01"),rownames(ct))
  if(!length(wanted))return(data.table())
  rbindlist(lapply(wanted,function(term){
    e<-unname(ct[term,"Estimate"]);s<-unname(ct[term,"Std. Error"])
    data.table(analysis=analysis,label=label,model=nm,term=term,
               OR=exp(e),CI_low=exp(e-1.96*s),CI_high=exp(e+1.96*s),
               p_value=unname(ct[term,"Pr(>|z|)"]))
  }))
}
fit4<-function(d,analysis,label){
  d<-copy(d);d[,fi_per_01:=fi/0.1]
  b<-glm(death~age_model+sex_male,data=d,family=binomial())
  c1<-glm(death~age_model+sex_male+cci_t,data=d,family=binomial())
  f1<-glm(death~age_model+sex_male+fi_per_01,data=d,family=binomial())
  u<-glm(death~age_model+sex_male+cci_t+fi_per_01,data=d,family=binomial())
  mods<-list(Base=b,Base_CCI=c1,Base_FI=f1,Full=u)
  eff<-rbindlist(lapply(names(mods),function(nm)
    robust_effects(mods[[nm]],d,nm,analysis,label)),fill=TRUE)
  perf<-rbindlist(lapply(names(mods),function(nm){
    pr<-predict(mods[[nm]],type="response")
    data.table(analysis=analysis,label=label,model=nm,n=nrow(d),
               deaths=sum(d$death),hospitals=uniqueN(d$hospitalid_model),
               AUC=fast_auc(d$death,pr),Brier=mean((d$death-pr)^2))
  }))
  lr<-rbindlist(list(
    data.table(analysis=analysis,label=label,comparison="FI added to Base+CCI",
               LR=2*(as.numeric(logLik(u))-as.numeric(logLik(c1))),df=1),
    data.table(analysis=analysis,label=label,comparison="CCI added to Base+FI",
               LR=2*(as.numeric(logLik(u))-as.numeric(logLik(f1))),df=1)
  ))
  lr[,p_value:=pchisq(LR,df=df,lower.tail=FALSE)]
  am<-setNames(perf$AUC,perf$model)
  delta<-c(fi_over_cci=unname(am["Full"]-am["Base_CCI"]),
           cci_over_fi=unname(am["Full"]-am["Base_FI"]))
  list(d=d,effects=eff,performance=perf,lr=lr,delta=delta)
}

BASE<-merge(cohort,FI_DT,by="patientunitstayid",all.x=TRUE)
s29_sets<-list();s29_eff<-list();s29_perf<-list();s29_lr<-list();s29_fits<-list();s29_flow<-list()

heartbeat("generate verified/corrected S29")
for(tmin in thresholds){
  d<-merge(BASE,cci_by_t[[as.character(tmin)]],by="patientunitstayid",all.x=TRUE)
  d[is.na(cci_t),cci_t:=0]
  common<-d[
    patientunitstayid%in%cad_by_t[[as.character(tmin)]]&
    is.finite(fi)&is.finite(age_model)&!is.na(sex_male)&
    !is.na(death)&!is.na(hospitalid_model)
  ]
  s29_sets[[as.character(tmin)]]<-common
  tt<-dxt[diagnosisoffset<=tmin]
  s29_flow[[as.character(tmin)]]<-data.table(
    threshold_min=tmin,time=paste0(tmin/60,"h"),
    diagnosis_rows=nrow(tt),
    patients_with_any_diagnosis=uniqueN(tt$patientunitstayid),
    patients_with_CAD_by_time=length(cad_by_t[[as.character(tmin)]]),
    n=nrow(common),deaths=sum(common$death),
    hospitals=uniqueN(common$hospitalid_model),
    median_CCI=if(nrow(common))median(common$cci_t)else NA_real_,
    median_FI=if(nrow(common))median(common$fi)else NA_real_
  )
  if(tmin>0){
    ff<-fit4(common,"S29_threshold",paste0(tmin/60,"h"))
    k<-as.character(tmin);s29_fits[[k]]<-ff
    s29_eff[[k]]<-ff$effects;s29_perf[[k]]<-ff$performance;s29_lr[[k]]<-ff$lr
  }
}
fwrite(rbindlist(s29_flow),file.path(OUT,"S29_verified_or_corrected_flow.csv"))
fwrite(rbindlist(s29_eff,fill=TRUE),file.path(OUT,"S29_verified_or_corrected_effects.csv"))
fwrite(rbindlist(s29_perf),file.path(OUT,"S29_verified_or_corrected_performance.csv"))
fwrite(rbindlist(s29_lr),file.path(OUT,"S29_verified_or_corrected_LR.csv"))

# S30
heartbeat("generate fixed-6h S30")
fixed<-copy(s29_sets[["360"]])
s30_eff<-list();s30_perf<-list();s30_lr<-list();s30_flow<-list();s30_fits<-list()
tlist<-list(`0h`=cci_by_t[["0"]],`6h`=cci_by_t[["360"]],
            `24h`=cci_by_t[["1440"]],`36h`=cci_by_t[["2160"]],full=cci_full)
for(lbl in names(tlist)){
  dd<-merge(fixed[,setdiff(names(fixed),"cci_t"),with=FALSE],
            tlist[[lbl]],by="patientunitstayid",all.x=TRUE)
  dd[is.na(cci_t),cci_t:=0]
  ff<-fit4(dd,"S30_fixed6h",lbl)
  s30_fits[[lbl]]<-ff
  s30_eff[[lbl]]<-ff$effects;s30_perf[[lbl]]<-ff$performance;s30_lr[[lbl]]<-ff$lr
  s30_flow[[lbl]]<-data.table(
    time=lbl,n=nrow(dd),deaths=sum(dd$death),hospitals=uniqueN(dd$hospitalid_model),
    patients_with_CCI_gt0=sum(dd$cci_t>0),median_CCI=median(dd$cci_t),median_FI=median(dd$fi)
  )
}
fwrite(rbindlist(s30_flow),file.path(OUT,"S30_fixed6h_flow.csv"))
fwrite(rbindlist(s30_eff,fill=TRUE),file.path(OUT,"S30_fixed6h_effects.csv"))
fwrite(rbindlist(s30_perf),file.path(OUT,"S30_fixed6h_performance.csv"))
fwrite(rbindlist(s30_lr),file.path(OUT,"S30_fixed6h_LR.csv"))

# True landmark
heartbeat("fit true landmarks")
land_fits<-list();land_flow<-list();land_eff<-list();land_perf<-list();land_lr<-list()
for(tmin in c(360L,1440L,2160L)){
  a<-copy(s29_sets[[as.character(tmin)]])
  cut<-tmin/1440
  rr<-list(
    Hospital=a[is.finite(hospital_los_days)&hospital_los_days>cut],
    ICU=a[is.finite(hospital_los_days)&is.finite(icu_los_days)&
          hospital_los_days>cut&icu_los_days>cut]
  )
  for(rs in names(rr)){
    dd<-rr[[rs]];k<-paste(tmin,rs,sep="_")
    land_flow[[k]]<-data.table(
      riskset=rs,threshold_min=tmin,threshold_h=tmin/60,
      availability_n=nrow(a),landmark_n=nrow(dd),
      excluded_before_or_at_landmark=nrow(a)-nrow(dd),
      deaths_after_landmark=sum(dd$death),hospitals=uniqueN(dd$hospitalid_model)
    )
    log_msg("Landmark ",rs," ",tmin/60,"h n=",nrow(dd)," deaths=",sum(dd$death))
    ff<-fit4(dd,"TRUE_LANDMARK",paste0(rs,"_",tmin/60,"h"))
    land_fits[[k]]<-ff;land_eff[[k]]<-ff$effects
    land_perf[[k]]<-ff$performance;land_lr[[k]]<-ff$lr
  }
}
fwrite(rbindlist(land_flow),file.path(OUT,"landmark_cohort_flow.csv"))
fwrite(rbindlist(land_eff,fill=TRUE),file.path(OUT,"landmark_effects.csv"))
fwrite(rbindlist(land_perf),file.path(OUT,"landmark_performance.csv"))
fwrite(rbindlist(land_lr),file.path(OUT,"landmark_LR.csv"))

# Point Delta AUC summary across corrected S29, corrected S30, and true landmark.
point_delta_rows<-list()
for(k in names(s29_fits)){
  point_delta_rows[[paste0("S29_",k)]]<-data.table(
    analysis="S29_CORRECTED",
    analysis_key=paste0(as.integer(k)/60,"h"),
    contrast=names(s29_fits[[k]]$delta),
    point_delta_auc=unname(s29_fits[[k]]$delta)
  )
}
for(k in names(s30_fits)){
  point_delta_rows[[paste0("S30_",k)]]<-data.table(
    analysis="S30_FIXED6H_CORRECTED",
    analysis_key=k,
    contrast=names(s30_fits[[k]]$delta),
    point_delta_auc=unname(s30_fits[[k]]$delta)
  )
}
for(k in names(land_fits)){
  point_delta_rows[[paste0("LM_",k)]]<-data.table(
    analysis="TRUE_LANDMARK",
    analysis_key=k,
    contrast=names(land_fits[[k]]$delta),
    point_delta_auc=unname(land_fits[[k]]$delta)
  )
}
fwrite(
  rbindlist(point_delta_rows,fill=TRUE),
  file.path(OUT,"all_point_deltaAUC.csv")
)

# Fast resumable hospital-cluster bootstrap.

BOOT_REPS<-1000L;BOOT_BATCH<-100L;BOOT_SEED<-20261001L
BOOT_WORKERS<-max(1L,min(6L,parallel::detectCores(logical=TRUE)-1L))

boot_resume<-function(fitobj,key,group){
  d<-copy(fitobj$d)
  tag<-gsub("[^A-Za-z0-9_]+","_",paste0(group,"_",key))
  cp<-file.path(OUT,paste0("bootstrap_checkpoint_",tag,".csv"))
  old<-if(file.exists(cp))tryCatch(fread(cp),error=function(e)data.table())else data.table()
  todo<-setdiff(seq_len(BOOT_REPS),unique(old$replicate))
  if(length(todo)){
    heartbeat(paste0("bootstrap ",tag," remaining ",length(todo)))
    y<-as.integer(d$death)
    xcci<-cbind(1,d$age_model,d$sex_male,d$cci_t)
    xfi<-cbind(1,d$age_model,d$sex_male,d$fi_per_01)
    xfull<-cbind(1,d$age_model,d$sex_male,d$cci_t,d$fi_per_01)
    groups<-split(seq_len(nrow(d)),d$hospitalid_model);H<-length(groups)
    set.seed(BOOT_SEED+sum(utf8ToInt(tag)))
    resamp<-lapply(seq_len(BOOT_REPS),function(i)sample.int(H,H,replace=TRUE))
    aucw<-function(y,p){
      n1<-sum(y==1);n0<-sum(y==0);if(!n1||!n0)return(NA_real_)
      r<-rank(p,ties.method="average")
      (sum(r[y==1])-n1*(n1+1)/2)/(n1*n0)
    }
    fita<-function(x,y){
      f<-suppressWarnings(glm.fit(x=x,y=y,family=binomial(),control=glm.control(maxit=50)))
      cf<-f$coefficients;if(any(!is.finite(cf)))return(NA_real_)
      aucw(y,plogis(drop(x%*%cf)))
    }
    one<-function(b){
      ix<-unlist(groups[resamp[[b]]],use.names=FALSE);yy<-y[ix]
      a1<-tryCatch(fita(xcci[ix,,drop=FALSE],yy),error=function(e)NA_real_)
      a2<-tryCatch(fita(xfi[ix,,drop=FALSE],yy),error=function(e)NA_real_)
      a3<-tryCatch(fita(xfull[ix,,drop=FALSE],yy),error=function(e)NA_real_)
      c(replicate=b,fi_over_cci=a3-a1,cci_over_fi=a3-a2)
    }
    workers<-min(BOOT_WORKERS,length(todo))
    cl<-parallel::makeCluster(workers)
    on.exit(try(parallel::stopCluster(cl),silent=TRUE),add=TRUE)
    parallel::clusterExport(cl,c("y","xcci","xfi","xfull","groups","resamp",
                                 "aucw","fita","one"),envir=environment())
    batches<-split(todo,ceiling(seq_along(todo)/BOOT_BATCH))
    for(bi in seq_along(batches)){
      ids<-batches[[bi]]
      heartbeat(paste0("bootstrap ",tag," batch ",bi,"/",length(batches),
                       " reps ",min(ids),"-",max(ids)))
      aa<-parallel::parLapplyLB(cl,ids,one)
      z<-as.data.table(do.call(rbind,aa));z[,replicate:=as.integer(replicate)]
      fwrite(z,cp,append=file.exists(cp),col.names=!file.exists(cp))
      log_msg("Checkpoint ",tag," through rep ",max(ids))
    }
    parallel::stopCluster(cl)
  }
  all<-fread(cp);all<-all[!duplicated(replicate)]
  if(nrow(all)<BOOT_REPS)stop("Incomplete bootstrap ",tag)
  rbindlist(lapply(c("fi_over_cci","cci_over_fi"),function(v){
    x<-all[[v]];x<-x[is.finite(x)]
    data.table(
      analysis=group,analysis_key=key,contrast=v,
      point_delta_auc=unname(fitobj$delta[[v]]),
      reps_requested=BOOT_REPS,reps_valid=length(x),
      bootstrap_mean=mean(x),bootstrap_median=median(x),
      CI_low=unname(quantile(x,.025)),CI_high=unname(quantile(x,.975)),
      prop_gt_0=mean(x>0)
    )
  }))
}

heartbeat("start/resume corrected S29 + S30 + true-landmark bootstrap")
bs<-list()

for(k in names(s29_fits)){
  bs[[paste0("S29_",k)]]<-boot_resume(
    s29_fits[[k]],paste0(as.integer(k)/60,"h"),"S29_CORRECTED"
  )
}
for(k in names(s30_fits)){
  bs[[paste0("S30_",k)]]<-boot_resume(
    s30_fits[[k]],k,"S30_FIXED6H_CORRECTED"
  )
}
for(k in names(land_fits)){
  bs[[paste0("LM_",k)]]<-boot_resume(
    land_fits[[k]],k,"TRUE_LANDMARK"
  )
}

all_bs<-rbindlist(bs,fill=TRUE)
fwrite(all_bs,file.path(OUT,"all_deltaAUC_bootstrap_summary.csv"))
fwrite(
  all_bs[analysis=="TRUE_LANDMARK"],
  file.path(OUT,"landmark_deltaAUC_bootstrap_summary.csv")
)
fwrite(
  all_bs[analysis=="S29_CORRECTED"],
  file.path(OUT,"S29_corrected_deltaAUC_bootstrap_summary.csv")
)
fwrite(
  all_bs[analysis=="S30_FIXED6H_CORRECTED"],
  file.path(OUT,"S30_corrected_deltaAUC_bootstrap_summary.csv")
)

capture.output(sessionInfo(),file=file.path(OUT,"sessionInfo.txt"))
writeLines(c(
  "COMPLETE AUTO v15 SELF-CONTAINED COMPLETED.",
  paste0("FI source: ",FI_SOURCE),
  paste0("Reference status: ",REFERENCE_STATUS),
  paste0("Correction mode: ",CORRECTION_MODE),
  "",
  "Old S29 FI provenance and historical CCI provenance were evaluated separately.",
  "Superseded provisional FI values were not carried forward into the current analysis.",
  "Corrected S29, corrected fixed-6h S30, and true 6/24/36 h landmarks were generated.",
  "1,000 hospital-cluster bootstrap replicates completed for S29, S30, and all true landmark risk sets."
),file.path(OUT,"RUN_COMPLETED_READ_ME.txt"),useBytes=TRUE)

heartbeat("SUCCESS")
log_msg("SUCCESS — corrected S29/S30 + true landmark complete; upload the entire output folder.")
