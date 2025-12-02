rm(list=ls());gc();source(".Rprofile")

library(survival)
library(ggsurvfit)

source("analysis/decan_survival analysis equations.R")

insulin_df = read_parquet(paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan004/insulin dataset.parquet")) 
metformin_df = read_parquet(paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan004/metformin dataset.parquet")) 
incretin_df = read_parquet(paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan004/incretin dataset.parquet")) 

retinopathy_df = read_parquet(paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan004/retinopathy dataset.parquet"))
neuropathy_df = read_parquet(paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan004/neuropathy dataset.parquet")) 
nephropathy_df = read_parquet(paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan004/nephropathy dataset.parquet"))

## severe ascvd
nstemi_df <- read_parquet(paste0(path_diabetes_subtypes_macrovascular_folder,"/working/dsman002/nstemi dataset.parquet"))
stemi_df <- read_parquet(paste0(path_diabetes_subtypes_macrovascular_folder,"/working/dsman002/stemi dataset.parquet"))
otheracs_df <- read_parquet(paste0(path_diabetes_subtypes_macrovascular_folder,"/working/dsman002/otheracs dataset.parquet"))
stroke_df <- read_parquet(paste0(path_diabetes_subtypes_macrovascular_folder,"/working/dsman002/stroke dataset.parquet"))

## heart failure
hfpef_df <- read_parquet(paste0(path_diabetes_subtypes_macrovascular_folder,"/working/dsman002/hfpef dataset.parquet"))
hfref_df <- read_parquet(paste0(path_diabetes_subtypes_macrovascular_folder,"/working/dsman002/hfref dataset.parquet"))

## other ascvd
tia_df <- read_parquet(paste0(path_diabetes_subtypes_macrovascular_folder,"/working/dsman002/tia dataset.parquet"))
stableangina_df <- read_parquet(paste0(path_diabetes_subtypes_macrovascular_folder,"/working/dsman002/stableangina dataset.parquet"))
chd_df <- read_parquet(paste0(path_diabetes_subtypes_macrovascular_folder,"/working/dsman002/chd dataset.parquet"))
cardiomyopathy_df <- read_parquet(paste0(path_diabetes_subtypes_macrovascular_folder,"/working/dsman002/cardiomyopathy dataset.parquet"))
afib_df <- read_parquet(paste0(path_diabetes_subtypes_macrovascular_folder,"/working/dsman002/afib dataset.parquet"))
pad_df <- read_parquet(paste0(path_diabetes_subtypes_macrovascular_folder,"/working/dsman002/pad dataset.parquet"))

# create composite variables in the ASCVD outcomes 

## severe ASCVD outcomes 

nstemi_df2 <- nstemi_df %>%
  dplyr::select(PatientDurableKey, t, event, female, Dmagediag,subphenotype_category,sidd_category) %>%
  dplyr::rename(t_nstemi = t, event_nstemi = event)

stemi_df2 <- stemi_df %>%
  dplyr::select(PatientDurableKey, t, event, female, Dmagediag,subphenotype_category,sidd_category) %>%
  dplyr::rename(t_stemi = t, event_stemi = event)

otheracs_df2 <- otheracs_df %>%
  dplyr::select(PatientDurableKey, t, event, female, Dmagediag,subphenotype_category,sidd_category) %>%
  dplyr::rename(t_otheracs = t, event_otheracs = event)

stroke_df2 <- stroke_df %>%
  dplyr::select(PatientDurableKey, t, event, female, Dmagediag,subphenotype_category,sidd_category) %>%
  dplyr::rename(t_stroke = t, event_stroke = event)



sv_ascvd_df <- reduce(list(nstemi_df2, stemi_df2, otheracs_df2, stroke_df2), 
                      function(x, y) merge(x, y, by = c("PatientDurableKey", "female", "Dmagediag","subphenotype_category","sidd_category"), all = TRUE))%>% 
  mutate(
    sv_ascvd = if_else(rowSums(cbind(event_nstemi, event_stemi, event_otheracs, event_stroke), na.rm = TRUE) > 0, 1, 0)
  ) %>% 
  mutate(
    time_to_event_composite = if_else(
      sv_ascvd == 1,  # Check if any severe ASCVD event occurred
      pmin(t_nstemi, t_stemi, t_otheracs, t_stroke, na.rm = TRUE),  # Earliest event time
      pmax(t_nstemi, t_stemi, t_otheracs, t_stroke, na.rm = TRUE)   # Latest censoring time
    )
  )%>% 
  dplyr::rename(t= time_to_event_composite, event =sv_ascvd)%>% 
  mutate(subphenotype_category = factor(subphenotype_category,
                                        levels=c("MOD","SIDD","MARD","Unclassified"),
                                        labels=c("MOD","SIDD","MARD","Mixed")))

# check duplciations
dups<-sv_ascvd_df %>%
  count(PatientDurableKey) %>%
  dplyr::filter(n > 1)


## other ASCVD outcomes 

tia_df2 <- tia_df %>%
  dplyr::select(PatientDurableKey, t, event, female, Dmagediag,subphenotype_category,sidd_category) %>%
  dplyr::rename(t_tia = t, event_tia = event)

chd_df2 <- chd_df %>%
  dplyr::select(PatientDurableKey, t, event, female, Dmagediag,subphenotype_category,sidd_category) %>%
  dplyr::rename(t_chd = t, event_chd = event)

cardiomyopathy_df2 <- cardiomyopathy_df %>%
  dplyr::select(PatientDurableKey, t, event, female, Dmagediag,subphenotype_category,sidd_category) %>%
  dplyr::rename(t_cardiomyopathy = t, event_cardiomyopathy = event)

afib_df2 <- afib_df %>%
  dplyr::select(PatientDurableKey, t, event, female, Dmagediag,subphenotype_category,sidd_category) %>%
  dplyr::rename(t_afib = t, event_afib = event)


stableangina_df2 <- stableangina_df %>%
  dplyr::select(PatientDurableKey, t, event, female, Dmagediag,subphenotype_category,sidd_category) %>%
  dplyr::rename(t_stableangina = t, event_stableangina = event)

pad_df2 <- pad_df %>%
  dplyr::select(PatientDurableKey, t, event, female, Dmagediag,subphenotype_category,sidd_category) %>%
  dplyr::rename(t_pad = t, event_pad = event)



other_ascvd_df <- reduce(list(tia_df2, chd_df2, cardiomyopathy_df2, afib_df2,stableangina_df2,pad_df2), 
                         function(x, y) merge(x, y, by = c("PatientDurableKey", "female", "Dmagediag","subphenotype_category","sidd_category"), all = TRUE))%>% 
  mutate(
    other_ascvd = if_else(rowSums(cbind(event_tia, event_chd, event_cardiomyopathy, event_afib,event_stableangina,event_pad), na.rm = TRUE) > 0, 1, 0)
  ) %>% 
  mutate(
    time_to_event_composite = if_else(
      other_ascvd == 1,  # Check if any severe ASCVD event occurred
      pmin(t_tia, t_chd, t_cardiomyopathy, t_afib,t_stableangina,t_pad, na.rm = TRUE),  # Earliest event time
      pmax(t_tia, t_chd, t_cardiomyopathy, t_afib,t_stableangina,t_pad, na.rm = TRUE)   # Latest censoring time
    )
  )%>% 
  dplyr::rename(t= time_to_event_composite, event =other_ascvd)%>% 
  mutate(subphenotype_category = factor(subphenotype_category,
                                        levels=c("MOD","SIDD","MARD","Unclassified"),
                                        labels=c("MOD","SIDD","MARD","Mixed")))

dups<-other_ascvd_df %>%
  count(PatientDurableKey) %>%
  dplyr::filter(n > 1)


# create composite variables in the heart failure outcome 

hfpef_df2 <- hfpef_df %>%
  dplyr::select(PatientDurableKey, t, event, female, Dmagediag,subphenotype_category,sidd_category) %>%
  dplyr::rename(t_hfpef = t, event_hfpef = event)

hfref_df2 <- hfref_df %>%
  dplyr::select(PatientDurableKey, t, event, female, Dmagediag,subphenotype_category,sidd_category) %>%
  dplyr::rename(t_hfref = t, event_hfref = event)

hf_df <- reduce(list(hfref_df2,hfpef_df2), 
                function(x, y) merge(x, y, by = c("PatientDurableKey", "female", "Dmagediag","subphenotype_category","sidd_category"), all = TRUE)) %>% 
  mutate(
    any_hf = if_else(rowSums(cbind(event_hfpef, event_hfref), na.rm = TRUE) > 0, 1, 0)
  ) %>% 
  mutate(
    time_to_event_composite = if_else(
      any_hf == 1,
      pmin(t_hfref,t_hfpef, na.rm = TRUE),  # Earliest event time
      pmax(t_hfref,t_hfpef, na.rm = TRUE)   # Latest censoring time
    )
  ) %>% 
  dplyr::rename(t= time_to_event_composite, event =any_hf) %>% 
  mutate(subphenotype_category = factor(subphenotype_category,
                                        levels=c("MOD","SIDD","MARD","Unclassified"),
                                        labels=c("MOD","SIDD","MARD","Mixed")))
dups<-hf_df %>%
  count(PatientDurableKey) %>%
  dplyr::filter(n > 1)

# MODELS ----------

insulin0 = coxph(as.formula(s0),data=insulin_df)
insulin1 = coxph(as.formula(s1),data=insulin_df)


metformin0 = coxph(as.formula(s0),data=metformin_df)
metformin1 = coxph(as.formula(s1),data=metformin_df)


incretin0 = coxph(as.formula(s0),data=incretin_df)
incretin1 = coxph(as.formula(s1),data=incretin_df)


retinopathy0 = coxph(as.formula(s0),data=retinopathy_df)
retinopathy1 = coxph(as.formula(s1),data=retinopathy_df)


neuropathy0 = coxph(as.formula(s0),data=neuropathy_df)
neuropathy1 = coxph(as.formula(s1),data=neuropathy_df)


nephropathy0 = coxph(as.formula(s0),data=nephropathy_df)
nephropathy1 = coxph(as.formula(s1),data=nephropathy_df)


sv_ascvd0 = coxph(as.formula(s0),data=sv_ascvd_df)
sv_ascvd1 = coxph(as.formula(s1),data=sv_ascvd_df)


other_ascvd0 = coxph(as.formula(s0),data=other_ascvd_df)
other_ascvd1 = coxph(as.formula(s1),data=other_ascvd_df)


hf0 = coxph(as.formula(s0),data=hf_df)
hf1 = coxph(as.formula(s1),data=hf_df)

### Calibration - Use Bootstrap and rms pacakhe 
library(rms)
library(purrr)
library(tibble)
library(readr)
# tell rms about your data
dd <- datadist(retinopathy_df)
options(datadist = "dd")
cox_model <- cph(Surv(t, event) ~ Dmagediag + female + subphenotype_category,
                 data = retinopathy_df,
                 x = TRUE, y = TRUE, surv = TRUE)
val_boot <- validate(cox_model, B = 200, method = "boot")
print(val_boot)

outcome_dfs <- list(
  Retinopathy   = retinopathy_df,
  Neuropathy    = neuropathy_df,
  Nephropathy   = nephropathy_df,
  Severe_ASCVD  = sv_ascvd_df,
  Other_ASCVD   = other_ascvd_df,
  HeartFailure  = hf_df
)


validate_one <- function(df, outcome_name) {
  df <- df %>% dplyr::select(t, event, Dmagediag, female, subphenotype_category)
  
  dd <- datadist(df); options(datadist = "dd")
  
  fit <- cph(Surv(t, event) ~ Dmagediag + female + subphenotype_category,
             data = df, x = TRUE, y = TRUE, surv = TRUE)
  
  # Bootstrap validation (optimism-corrected metrics)
  v <- validate(fit, B = 1000, method = "boot")
  
  # Extract corrected (optimism-adjusted) indices
  dxy_corr   <- as.numeric(v["Dxy",   "index.corrected"])
  slope_corr <- as.numeric(v["Slope", "index.corrected"])
  r2_corr    <- as.numeric(v["R2",    "index.corrected"])
  
  tibble(
    outcome = outcome_name,
    n       = nrow(df),
    events  = sum(df$event, na.rm = TRUE),
    C_index_corrected = (dxy_corr + 1) / 2,  
    Dxy_corrected     = dxy_corr,
    Slope_corrected   = slope_corr,
    R2_corrected      = r2_corr
  )
}

results_tbl <- imap_dfr(outcome_dfs, validate_one)

print(results_tbl)

write_csv(results_tbl, "paper/bootstrap_validation_summary.csv")

# plots 

