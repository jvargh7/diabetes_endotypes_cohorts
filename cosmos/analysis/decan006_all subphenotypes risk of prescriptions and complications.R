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


# ckd_df <- read_parquet(,paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan004/ckd dataset.parquet"))
chd_df <- read_parquet(paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan004/chd dataset.parquet"))
afib_df <- read_parquet(paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan004/afib dataset.parquet"))
stroke_df <- read_parquet(paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan004/stroke dataset.parquet"))
pad_df <- read_parquet(paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan004/pad dataset.parquet"))
hfpef_df <- read_parquet(paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan004/hfpef dataset.parquet"))
hfref_df <- read_parquet(paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan004/hfref dataset.parquet"))

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

# ckd0 = coxph(as.formula(s0),data=ckd_df)
# ckd1 = coxph(as.formula(s1),data=ckd_df)

# MACROVASCULAR -------
chd0 = coxph(as.formula(s0),data=chd_df)
chd1 = coxph(as.formula(s1),data=chd_df)

afib0 = coxph(as.formula(s0),data=afib_df)
afib1 = coxph(as.formula(s1),data=afib_df)

stroke0 = coxph(as.formula(s0),data=stroke_df)
stroke1 = coxph(as.formula(s1),data=stroke_df)

pad0 = coxph(as.formula(s0),data=pad_df)
pad1 = coxph(as.formula(s1),data=pad_df)

hfpef0 = coxph(as.formula(s0),data=hfpef_df)
hfpef1 = coxph(as.formula(s1),data=hfpef_df)

hfref0 = coxph(as.formula(s0),data=hfref_df)
hfref1 = coxph(as.formula(s1),data=hfref_df)

bind_rows(
  broom::tidy(insulin0) %>% mutate(model = "s0",outcome = "Insulin"),
  broom::tidy(insulin1) %>% mutate(model = "s1",outcome = "Insulin"),
  
  broom::tidy(metformin0) %>% mutate(model = "s0",outcome = "Metformin"),
  broom::tidy(metformin1) %>% mutate(model = "s1",outcome = "Metformin"),
  
  broom::tidy(incretin0) %>% mutate(model = "s0",outcome = "Incretin"),
  broom::tidy(incretin1) %>% mutate(model = "s1",outcome = "Incretin"),
  
  broom::tidy(retinopathy0) %>% mutate(model = "s0",outcome = "Retinopathy"),
  broom::tidy(retinopathy1) %>% mutate(model = "s1",outcome = "Retinopathy"),
  
  broom::tidy(neuropathy0) %>% mutate(model = "s0",outcome = "Neuropathy"),
  broom::tidy(neuropathy1) %>% mutate(model = "s1",outcome = "Neuropathy"),
  
  broom::tidy(nephropathy0) %>% mutate(model = "s0",outcome = "Nephropathy"),
  broom::tidy(nephropathy1) %>% mutate(model = "s1",outcome = "Nephropathy"),
  
  # broom::tidy(ckd0) %>% mutate(model = "s0",outcome = "CKD"),
  # broom::tidy(ckd1) %>% mutate(model = "s1",outcome = "CKD"),
  
  broom::tidy(chd0) %>% mutate(model = "s0",outcome = "CHD"),
  broom::tidy(chd1) %>% mutate(model = "s1",outcome = "CHD"),
  
  broom::tidy(afib0) %>% mutate(model = "s0",outcome = "AFib"),
  broom::tidy(afib1) %>% mutate(model = "s1",outcome = "AFib"),
  
  broom::tidy(stroke0) %>% mutate(model = "s0",outcome = "Stroke"),
  broom::tidy(stroke1) %>% mutate(model = "s1",outcome = "Stroke"),
  
  broom::tidy(pad0) %>% mutate(model = "s0",outcome = "PAD"),
  broom::tidy(pad1) %>% mutate(model = "s1",outcome = "PAD"),
  
  broom::tidy(hfpef0) %>% mutate(model = "s0",outcome = "HFpEF"),
  broom::tidy(hfpef1) %>% mutate(model = "s1",outcome = "HFpEF"),
  
  broom::tidy(hfref0) %>% mutate(model = "s0",outcome = "HFrEF"),
  broom::tidy(hfref1) %>% mutate(model = "s1",outcome = "HFrEF")
  
) %>% 
  write_csv(.,"analysis/decan006_coefficients of all subphenotypes risk of prescriptions and complications.csv")

# added severe ASCVD, other ASCVD and HF (OCT 2025)
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


## models for composite macrovascular 


sv_ascvd0 = coxph(as.formula(s0),data=sv_ascvd_df)
sv_ascvd1 = coxph(as.formula(s1),data=sv_ascvd_df)


other_ascvd0 = coxph(as.formula(s0),data=other_ascvd_df)
other_ascvd1 = coxph(as.formula(s1),data=other_ascvd_df)


hf0 = coxph(as.formula(s0),data=hf_df)
hf1 = coxph(as.formula(s1),data=hf_df)

bind_rows(
  broom::tidy(insulin0) %>% mutate(model = "s0",outcome = "Insulin"),
  broom::tidy(insulin1) %>% mutate(model = "s1",outcome = "Insulin"),
  
  broom::tidy(metformin0) %>% mutate(model = "s0",outcome = "Metformin"),
  broom::tidy(metformin1) %>% mutate(model = "s1",outcome = "Metformin"),
  
  broom::tidy(incretin0) %>% mutate(model = "s0",outcome = "Incretin"),
  broom::tidy(incretin1) %>% mutate(model = "s1",outcome = "Incretin"),
  
  broom::tidy(retinopathy0) %>% mutate(model = "s0",outcome = "Retinopathy"),
  broom::tidy(retinopathy1) %>% mutate(model = "s1",outcome = "Retinopathy"),
  
  broom::tidy(neuropathy0) %>% mutate(model = "s0",outcome = "Neuropathy"),
  broom::tidy(neuropathy1) %>% mutate(model = "s1",outcome = "Neuropathy"),
  
  broom::tidy(nephropathy0) %>% mutate(model = "s0",outcome = "Nephropathy"),
  broom::tidy(nephropathy1) %>% mutate(model = "s1",outcome = "Nephropathy"),
  
  broom::tidy(sv_ascvd0) %>% mutate(model = "s0",outcome = "Severe ASCVD"),
  broom::tidy(sv_ascvd1) %>% mutate(model = "s1",outcome = "Severe ASCVD"),
  
  broom::tidy(other_ascvd0) %>% mutate(model = "s0",outcome = "Other ASCVD"),
  broom::tidy(other_ascvd1) %>% mutate(model = "s1",outcome = "Other ASCVD"),
  
  broom::tidy(hf0) %>% mutate(model = "s0",outcome = "Heart Failure"),
  broom::tidy(hf1) %>% mutate(model = "s1",outcome = "Heart Failure")
  
) %>% 
  write_csv(.,"analysis/decan006_coefficients of all subphenotypes risk of prescriptions and complications updated.csv")

