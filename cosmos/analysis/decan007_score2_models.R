rm(list=ls());gc();source(".Rprofile")

library(tidymodels)
library(survival)
library(purrr)

# we will use both SCORE 2 and subtypes to predict the outcomes including both marcro and micro vascular complications
# dataset preparation --------------

## Imputed dataset (all variabales to be used in the subsequent analysis as predictors have been imputed if missing)
imputed_df = read_parquet(paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan001/imputed_score2.parquet")) %>% 
  mutate(
    total_chol_mg = Ldlc + Hdlc + (Tgl / 5),
    # Convert to mmol/L
    total_chol = total_chol_mg / 38.67,
    Hdlc_mmol = Hdlc / 38.67,
    Ldlc_mmol = Ldlc / 38.67,
    Tgl_mmol  = Tgl / 88.57
  )%>%
  mutate(
    #convert HbA1c to mmol/mol
    hba1c_mmolmol = (Hba1c - 2.15) * 10.929
  )%>% 
  # join smoking status
  left_join(
    read_parquet(paste0(path_diabetes_subtypes_untraditional_folder,
                        "/working/smk_bfr.parquet")) %>%
      select(PatientDurableKey, smk_bfr),
    by = "PatientDurableKey"
  ) %>% 
  # recode smoking variable: Current vs Other to match ScorE2 
  mutate (
    smk_bfr = case_when(
    !is.na(smk_bfr) & smk_bfr == "Current" ~ "Current",
    TRUE  ~ "Other"
  ),
  smk_bfr = factor(smk_bfr, levels = c("Other", "Current"))
  )
  
transformed_df <- imputed_df %>% 
  mutate(
    # SCORE2-Diabetes transformations
    cage      = (Dmagediag - 60) / 5,            # age
    csbp      = (Sbp - 120) / 20,                # SBP
    ctchol    = (total_chol - 6) / 1,            # total cholesterol (mmol/L)
    chdl      = (Hdlc_mmol - 1.3) / 0.5,         # HDL cholesterol (mmol/L)
    smallbin  = as.integer(smk_bfr == "Current"),# 1 if current smoker
    hxdiabbin = 1,                               # all T2D by design
    cagediab  = (Dmagediag - 50) / 5,            # age at diabetes diagnosis
    chba1c    = (hba1c_mmolmol - 31) / 9.34,     # HbA1c
    lnegfr    = log(egfr_final),                 # ln(eGFR)
    clnegfr   = (lnegfr - 4.5) / 0.15,           # standardized ln(eGFR)
    clnegfr2  = clnegfr^2,                       # squared term
    # interation temrs 
    int_age_smk   = cage * smallbin,
    int_age_sbp   = cage * csbp,
    int_age_diab  = cage * hxdiabbin,
    int_age_tchol = cage * ctchol,
    int_age_hdl   = cage * chdl,
    int_age_hba1c = cage * chba1c,
    int_age_negfr = cage * clnegfr
  ) %>%
  select(
    PatientDurableKey,female,Dmagediag, Bmi, smk_bfr,
    total_chol, Hdlc_mmol, Ldlc_mmol, Tgl_mmol,
    Hba1c, hba1c_mmolmol,
    Sbp, Dbp, egfr_final,
    cage, csbp, ctchol, chdl, smallbin, hxdiabbin,
    cagediab, chba1c, lnegfr, clnegfr, clnegfr2, 
    int_age_smk, int_age_sbp,int_age_diab,
    int_age_tchol,int_age_hdl,int_age_hba1c,
    int_age_negfr
  )
  
## prepare sex specific coefficients 
# Men
coef_male <- c(
  cage=0.5368, smallbin=0.4774, csbp=0.1322, ctchol=0.1102, chdl=-0.1087,
  hxdiabbin=0.6457,
  int_age_smk=-0.0672, int_age_sbp=-0.0268, int_age_diab=-0.0983,
  int_age_tchol=-0.0181, int_age_hdl=0.0095,
  cagediab=-0.0998, chba1c=0.0955, clnegfr=-0.0591, clnegfr2=0.0058,
  int_age_hba1c=-0.0134, int_age_negfr=0.0115
)

# Women
coef_female <- c(
  cage=0.6624, smallbin=0.6139, csbp=0.1421, ctchol=0.1127, chdl=-0.1568,
  hxdiabbin=0.8096,
  int_age_smk=-0.1122, int_age_sbp=-0.0167, int_age_diab=-0.1272,
  int_age_tchol=-0.0200, int_age_hdl=0.0186,
  cagediab=-0.1180, chba1c=0.1173, clnegfr=-0.0640, clnegfr2=0.0062,
  int_age_hba1c=-0.0196, int_age_negfr=0.0169
)


design_cols <- names(coef_male)

X <- as.matrix(transformed_df[, design_cols])
# Two LPs by sex, then combine into one vector
lp_male   <- as.numeric(X %*% coef_male)
lp_female <- as.numeric(X %*% coef_female)

transformed_df$score2_lp <- ifelse(transformed_df$female == 1, 
                                   lp_female, 
                                   lp_male)

## microvascular 
retinopathy_df = read_parquet(paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan004/retinopathy dataset.parquet")) %>% 
  mutate(subphenotype_category = factor(subphenotype_category,
                                        levels=c("MOD","SIDD","MARD","Not_Classified"),
                                        labels=c("MOD","SIDD","MARD","Mixed"))) %>% 
  select(PatientDurableKey,time_to_retinopathy,time_to_censoring,t,event,subphenotype_category)%>% 
  left_join(transformed_df,by= c("PatientDurableKey"))


neuropathy_df = read_parquet(paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan004/neuropathy dataset.parquet")) %>% 
  mutate(subphenotype_category = factor(subphenotype_category,
                                        levels=c("MOD","SIDD","MARD","Not_Classified"),
                                        labels=c("MOD","SIDD","MARD","Mixed"))) %>% 
  select(PatientDurableKey,time_to_neuropathy,time_to_censoring,t,event,subphenotype_category)%>% 
  left_join(transformed_df,by= c("PatientDurableKey"))

nephropathy_df = read_parquet(paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan004/nephropathy dataset.parquet")) %>% 
  mutate(subphenotype_category = factor(subphenotype_category,
                                        levels=c("MOD","SIDD","MARD","Not_Classified"),
                                        labels=c("MOD","SIDD","MARD","Mixed"))) %>% 
  select(PatientDurableKey,time_to_nephropathy,time_to_censoring,t,event,subphenotype_category)%>% 
  left_join(transformed_df,by= c("PatientDurableKey"))



## severe ascvd
nstemi_df <- read_parquet(paste0(path_diabetes_subtypes_macrovascular_folder,"/working/dsman002/nstemi dataset.parquet"))%>% 
  mutate(subphenotype_category = factor(subphenotype_category,
                                        levels=c("MOD","SIDD","MARD","Unclassified"),
                                        labels=c("MOD","SIDD","MARD","Mixed")))
stemi_df <- read_parquet(paste0(path_diabetes_subtypes_macrovascular_folder,"/working/dsman002/stemi dataset.parquet"))%>% 
  mutate(subphenotype_category = factor(subphenotype_category,
                                        levels=c("MOD","SIDD","MARD","Unclassified"),
                                        labels=c("MOD","SIDD","MARD","Mixed")))
otheracs_df <- read_parquet(paste0(path_diabetes_subtypes_macrovascular_folder,"/working/dsman002/otheracs dataset.parquet"))%>% 
  mutate(subphenotype_category = factor(subphenotype_category,
                                        levels=c("MOD","SIDD","MARD","Unclassified"),
                                        labels=c("MOD","SIDD","MARD","Mixed")))
stroke_df <- read_parquet(paste0(path_diabetes_subtypes_macrovascular_folder,"/working/dsman002/stroke dataset.parquet"))%>% 
  mutate(subphenotype_category = factor(subphenotype_category,
                                        levels=c("MOD","SIDD","MARD","Unclassified"),
                                        labels=c("MOD","SIDD","MARD","Mixed")))

## heart failure
hfpef_df <- read_parquet(paste0(path_diabetes_subtypes_macrovascular_folder,"/working/dsman002/hfpef dataset.parquet"))%>% 
  mutate(subphenotype_category = factor(subphenotype_category,
                                        levels=c("MOD","SIDD","MARD","Unclassified"),
                                        labels=c("MOD","SIDD","MARD","Mixed")))
hfref_df <- read_parquet(paste0(path_diabetes_subtypes_macrovascular_folder,"/working/dsman002/hfref dataset.parquet"))%>% 
  mutate(subphenotype_category = factor(subphenotype_category,
                                        levels=c("MOD","SIDD","MARD","Unclassified"),
                                        labels=c("MOD","SIDD","MARD","Mixed")))

## other ascvd
tia_df <- read_parquet(paste0(path_diabetes_subtypes_macrovascular_folder,"/working/dsman002/tia dataset.parquet"))%>% 
  mutate(subphenotype_category = factor(subphenotype_category,
                                        levels=c("MOD","SIDD","MARD","Unclassified"),
                                        labels=c("MOD","SIDD","MARD","Mixed")))
stableangina_df <- read_parquet(paste0(path_diabetes_subtypes_macrovascular_folder,"/working/dsman002/stableangina dataset.parquet"))%>% 
  mutate(subphenotype_category = factor(subphenotype_category,
                                        levels=c("MOD","SIDD","MARD","Unclassified"),
                                        labels=c("MOD","SIDD","MARD","Mixed")))
chd_df <- read_parquet(paste0(path_diabetes_subtypes_macrovascular_folder,"/working/dsman002/chd dataset.parquet"))%>% 
  mutate(subphenotype_category = factor(subphenotype_category,
                                        levels=c("MOD","SIDD","MARD","Unclassified"),
                                        labels=c("MOD","SIDD","MARD","Mixed")))
cardiomyopathy_df <- read_parquet(paste0(path_diabetes_subtypes_macrovascular_folder,"/working/dsman002/cardiomyopathy dataset.parquet"))%>% 
  mutate(subphenotype_category = factor(subphenotype_category,
                                        levels=c("MOD","SIDD","MARD","Unclassified"),
                                        labels=c("MOD","SIDD","MARD","Mixed")))
afib_df <- read_parquet(paste0(path_diabetes_subtypes_macrovascular_folder,"/working/dsman002/afib dataset.parquet"))%>% 
  mutate(subphenotype_category = factor(subphenotype_category,
                                        levels=c("MOD","SIDD","MARD","Unclassified"),
                                        labels=c("MOD","SIDD","MARD","Mixed")))
pad_df <- read_parquet(paste0(path_diabetes_subtypes_macrovascular_folder,"/working/dsman002/pad dataset.parquet"))%>% 
  mutate(subphenotype_category = factor(subphenotype_category,
                                        levels=c("MOD","SIDD","MARD","Unclassified"),
                                        labels=c("MOD","SIDD","MARD","Mixed")))

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
  select(PatientDurableKey,subphenotype_category,t,event)%>% 
  left_join(transformed_df,by= c("PatientDurableKey"))

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
  select(PatientDurableKey,t,event,subphenotype_category)%>% 
  left_join(transformed_df,by= c("PatientDurableKey"))

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
  select(PatientDurableKey,t,event,subphenotype_category)%>% 
  left_join(transformed_df,by= c("PatientDurableKey"))

dups<-hf_df %>%
  count(PatientDurableKey) %>%
  dplyr::filter(n > 1)
# Macrovascular --------------

## Model M1a: age + gender (baseline model)

## Model M1b: age + gender + subtype (our model) 

## Model M2a: age + SCORE2 (replicates published SCORE2)

## Model M2b: age + SCORE2 + subtype (test incremental value of subtype)

cidx <- function(fit) {
  cc <- survival::concordance(fit)
  C  <- as.numeric(cc$concordance)
  SE <- sqrt(as.numeric(cc$var))
  tibble(C_index = C, SE = SE, LCL = C - 1.96*SE, UCL = C + 1.96*SE)
}

fit_four_models_macro <- function(df, dataset_name) {
  df0 <- df %>%
    mutate(
      Sex = factor(female, levels = c(0,1), labels = c("male","female")),
    )
  
  # M1a: Age + Sex
  fit_m1a <- coxph(Surv(t, event) ~ Dmagediag + Sex, data = df0, na.action = na.omit)
  
  # M1b: Age + Sex + subtype
  fit_m1b <- coxph(Surv(t, event) ~ Dmagediag + Sex + subphenotype_category, data = df0, na.action = na.omit)
  
  # M2a: SCORE2 LP + Age + Sex
  fit_m2a <- coxph(Surv(t, event) ~ Dmagediag + strata(Sex) + score2_lp, data = df0, na.action = na.omit)
  
  # M2b: SCORE2 LP + subtype + Age+ Sex
  fit_m2b <- coxph(Surv(t, event) ~ Dmagediag + + strata(Sex) + score2_lp + subphenotype_category, data = df0, na.action = na.omit)
  
  bind_rows(
    cidx(fit_m1a) %>% mutate(Model = "M1a: Age + Sex",        n = fit_m1a$n, events = fit_m1a$nevent),
    cidx(fit_m1b) %>% mutate(Model = "M1b: M1a + Subtype", n = fit_m1b$n, events = fit_m1b$nevent),
    cidx(fit_m2a) %>% mutate(Model = "M2a: Age + SCORE2-LP + strata(Sex)", n = fit_m2a$n, events = fit_m2a$nevent),
    cidx(fit_m2b) %>% mutate(Model = "M2b: M1a+ SCORE2-LP + Subtype + strata(Sex)", n = fit_m2b$n, events = fit_m2b$nevent)
  ) %>%
    mutate(Dataset = dataset_name, .before = 1)
}


results_sascvd <- fit_four_models_macro(sv_ascvd_df, "Severe ASCVD")
results_oascvd <- fit_four_models_macro(other_ascvd_df, "Other ASCVD")
results_hf <- fit_four_models_macro(hf_df, "Heart failure")

# Microvascular --------------

## Model m1: age + gender
## Model m2: age + gender + subtype
## Model m3: age + HbA1c + BMI + SBP (from reviewer) [we will use the imputed HbA1c, BMI, and SBp values if they are missing]
## Model m4: age + HbA1c + BMI + SBP + subtype

fit_four_models_micro <- function(df, dataset_name) {
  df0 <- df %>%
    mutate(
      Sex = factor(female, levels = c(0,1), labels = c("male","female")),
    )
  
  # M1a: Age + Sex
  fit_m1a <- coxph(Surv(t, event) ~ Dmagediag + Sex, data = df0, na.action = na.omit)
  
  # M1b: Age + Sex + subtype
  fit_m1b <- coxph(Surv(t, event) ~ Dmagediag + Sex + subphenotype_category, data = df0, na.action = na.omit)
  
  # M2a: SCORE2 LP + Age + Sex
  fit_m2a <- coxph(Surv(t, event) ~ Dmagediag + Sex + Hba1c  + Bmi + Sbp, data = df0, na.action = na.omit)
  
  # M2b: SCORE2 LP + subtype + Age+ Sex
  fit_m2b <- coxph(Surv(t, event) ~ Dmagediag + + Sex + Hba1c  + Bmi + Sbp + subphenotype_category, data = df0, na.action = na.omit)
  
  bind_rows(
    cidx(fit_m1a) %>% mutate(Model = "M1a: Age + Sex",        n = fit_m1a$n, events = fit_m1a$nevent),
    cidx(fit_m1b) %>% mutate(Model = "M1b: M1a + Subtype", n = fit_m1b$n, events = fit_m1b$nevent),
    cidx(fit_m2a) %>% mutate(Model = "M2a: HbA1c + BMI + SBP", n = fit_m2a$n, events = fit_m2a$nevent),
    cidx(fit_m2b) %>% mutate(Model = "M2b: M2a + subtype", n = fit_m2b$n, events = fit_m2b$nevent)
  ) %>%
    mutate(Dataset = dataset_name, .before = 1)
}

results_reti <- fit_four_models_micro(retinopathy_df, "Retinopathy")
results_neph <- fit_four_models_micro(nephropathy_df, "nephropathy")
results_neuro <- fit_four_models_micro(neuropathy_df, "neuropathy")

## output results 

all_results <- dplyr::bind_rows(results_reti, results_neph, results_neuro,
                                results_sascvd,results_oascvd,results_hf) %>% 
  write_csv(.,"analysis/sensitivity analysis SCORE2 and individual predictors.csv")





