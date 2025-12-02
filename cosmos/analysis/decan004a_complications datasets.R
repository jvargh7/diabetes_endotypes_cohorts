rm(list=ls());gc();source(".Rprofile")

library(survival)
library(ggsurvfit)

last_diagnosis_after <- read_parquet(paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan001/last diagnosis after diagnosis date.parquet"))

source("analysis/decan002c_imputed phenotyping dataset processing.R")

# imputed phenotyping dataset.rds
phenotyping_df_imputed <- phenotyping_df_imputed %>% 
  mutate(sidd_category = case_when(is_sidd >= sidd_cutoff ~ "SIDD",
                                   TRUE ~ "NonSIDD"),
         subphenotype_category = case_when(is_sidd >= sidd_cutoff ~ "SIDD",
                                           is_mard >= mard_cutoff ~ "MARD",
                                           is_mod >= mod_cutoff ~ "MOD",
                                           TRUE ~ "Not_Classified")) %>% 
  mutate(subphenotype_category=factor(subphenotype_category,levels=c("MOD","Not_Classified","MARD","SIDD"))) %>% 
  dplyr::select(PatientDurableKey,is_sidd,subphenotype_category)

phenotyping_df <- read_parquet(paste0(path_diabetes_endotypes_cosmos_folder,"/working/phenotyping dataset.parquet")) %>% 
  left_join(last_diagnosis_after %>% 
              rename(lastdx_YearMonthKey = YearMonthKey),
            by=c("PatientDurableKey")) %>% 
  mutate(time_to_censoring = as.numeric(difftime(ymd(lastdx_YearMonthKey),ymd(diagnosis_datekey+1),units="days"))/30.5) %>% 
  left_join(phenotyping_df_imputed,
            by=c("PatientDurableKey")) %>% 
  mutate(sidd_category = case_when(is_sidd >= sidd_cutoff ~ "SIDD",
                                   TRUE ~ "NonSIDD")) %>% 
  dplyr::filter(!is.na(time_to_censoring))
  

complications_after <- read_parquet(paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan001/complications after diagnosis date.parquet"))
complications_before <- read_parquet(paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan001/complications before diagnosis date.parquet"))


# ANY COMPLICATIONS ---------

any_history = complications_before %>% 
  dplyr::filter(type %in% c("afib","chd","hfpef","hfref",
                            "nephropathy","neuropathy","pad","retinopathy",
                            "stroke")) %>% 
  distinct(PatientDurableKey)

any_df_incl0 = phenotyping_df %>% 
  dplyr::filter(!PatientDurableKey %in% any_history$PatientDurableKey) %>% 
  left_join(complications_after %>% 
              dplyr::filter(type %in% c("afib","chd","hfpef","hfref",
                                        "nephropathy","neuropathy","pad","retinopathy",
                                        "stroke")) %>% 
              dplyr::select(PatientDurableKey,ICD10_Value,YearMonthKey) %>% 
              rename(any_YearMonthKey = YearMonthKey) %>% 
              group_by(PatientDurableKey) %>% 
              dplyr::filter(any_YearMonthKey == min(any_YearMonthKey)) %>% 
              group_by(PatientDurableKey) %>% 
              summarize(any_ICD10_Value = paste0("(",paste0(ICD10_Value,collapse=","),")"),
                        any_YearMonthKey = min(any_YearMonthKey)) %>% 
              ungroup(),
            by="PatientDurableKey") %>% 
  mutate(time_to_any = as.numeric(difftime(ymd(any_YearMonthKey),ymd(diagnosis_datekey+1),units="days"))/30.5) 


any_df_incl0 %>% 
  mutate(early_complications = case_when(time_to_any>=0 & time_to_any<=12 ~ 1,
                                         TRUE ~ 0)) %>% 
  group_by(subphenotype_category) %>% 
  summarize(early = mean(early_complications)) 


any_df <- any_df_incl0 %>% 
  ## Future incidence ---------
dplyr::filter(time_to_any > 0 | is.na(time_to_any)) %>% 
  mutate(t = case_when(is.na(time_to_any) ~ time_to_censoring,
                       TRUE ~ time_to_any),
         event = case_when(!is.na(any_ICD10_Value) ~ 1,
                           TRUE ~ 0))

phenotyping_df %>% 
  dplyr::filter(!PatientDurableKey %in% any_df$PatientDurableKey) %>% 
  mutate(exclusion = case_when(PatientDurableKey %in% any_history$PatientDurableKey ~ "Before",
                               TRUE ~ "Month Of")) %>% 
  # group_by(exclusion) %>% 
  tally()

table(any_df$event)

any_fit = survfit(Surv(t,event) ~ sidd_category,data=any_df)  
ggsurvfit(any_fit)

# RETINOPATHY ---------------

## No History ----------

retinopathy_history = complications_before %>% 
  dplyr::filter(type == "retinopathy") %>% 
  distinct(PatientDurableKey)

retinopathy_df = phenotyping_df %>% 
  dplyr::filter(!PatientDurableKey %in% retinopathy_history$PatientDurableKey) %>% 
  left_join(complications_after %>% 
              dplyr::filter(type == "retinopathy") %>% 
              dplyr::select(PatientDurableKey,ICD10_Value,YearMonthKey) %>% 
              rename(retinopathy_YearMonthKey = YearMonthKey) %>% 
              group_by(PatientDurableKey) %>% 
              dplyr::filter(retinopathy_YearMonthKey == min(retinopathy_YearMonthKey)) %>% 
              group_by(PatientDurableKey) %>% 
              summarize(retinopathy_ICD10_Value = paste0("(",paste0(ICD10_Value,collapse=","),")"),
                        retinopathy_YearMonthKey = min(retinopathy_YearMonthKey)) %>% 
              ungroup(),
            by="PatientDurableKey") %>% 
  mutate(time_to_retinopathy = as.numeric(difftime(ymd(retinopathy_YearMonthKey),ymd(diagnosis_datekey+1),units="days"))/30.5) %>% 
  ## Future incidence ---------
  dplyr::filter(time_to_retinopathy > 0 | is.na(time_to_retinopathy)) %>% 
  mutate(t = case_when(is.na(time_to_retinopathy) ~ time_to_censoring,
                       TRUE ~ time_to_retinopathy),
         event = case_when(!is.na(retinopathy_ICD10_Value) ~ 1,
                           TRUE ~ 0))
  
phenotyping_df %>% 
  dplyr::filter(!PatientDurableKey %in% retinopathy_df$PatientDurableKey) %>% 
  mutate(exclusion = case_when(PatientDurableKey %in% retinopathy_history$PatientDurableKey ~ "Before",
                               TRUE ~ "Month Of")) %>% 
  # group_by(exclusion) %>% 
  tally()

table(retinopathy_df$event)

retinopathy_fit = survfit(Surv(t,event) ~ sidd_category,data=retinopathy_df)  
ggsurvfit(retinopathy_fit)


# NEPHROPATHY ---------------

## No History ----------

nephropathy_history = complications_before %>% 
  dplyr::filter(type == "nephropathy") %>% 
  distinct(PatientDurableKey)

nephropathy_df = phenotyping_df %>% 
  dplyr::filter(!PatientDurableKey %in% nephropathy_history$PatientDurableKey) %>% 
  left_join(complications_after %>% 
              dplyr::filter(type == "nephropathy") %>% 
              dplyr::select(PatientDurableKey,ICD10_Value,YearMonthKey) %>% 
              rename(nephropathy_YearMonthKey = YearMonthKey) %>% 
              group_by(PatientDurableKey) %>% 
              dplyr::filter(nephropathy_YearMonthKey == min(nephropathy_YearMonthKey)) %>% 
              group_by(PatientDurableKey) %>% 
              summarize(nephropathy_ICD10_Value = paste0("(",paste0(ICD10_Value,collapse=","),")"),
                        nephropathy_YearMonthKey = min(nephropathy_YearMonthKey)) %>% 
              ungroup(),
            by="PatientDurableKey") %>% 
  mutate(time_to_nephropathy = as.numeric(difftime(ymd(nephropathy_YearMonthKey),ymd(diagnosis_datekey+1),units="days"))/30.5) %>% 
  ## Future incidence ---------
dplyr::filter(time_to_nephropathy > 0 | is.na(time_to_nephropathy)) %>% 
  mutate(t = case_when(is.na(time_to_nephropathy) ~ time_to_censoring,
                       TRUE ~ time_to_nephropathy),
         event = case_when(!is.na(nephropathy_ICD10_Value) ~ 1,
                           TRUE ~ 0))

phenotyping_df %>% 
  dplyr::filter(!PatientDurableKey %in% nephropathy_df$PatientDurableKey) %>% 
  mutate(exclusion = case_when(PatientDurableKey %in% nephropathy_history$PatientDurableKey ~ "Before",
                               TRUE ~ "Month Of")) %>% 
  # group_by(exclusion) %>% 
  tally()


table(nephropathy_df$event)

nephropathy_fit = survfit(Surv(t,event) ~ sidd_category,data=nephropathy_df)  
ggsurvfit(nephropathy_fit)


# NEUROPATHY ---------------

## No History ----------

neuropathy_history = complications_before %>% 
  dplyr::filter(type == "neuropathy") %>% 
  distinct(PatientDurableKey)

neuropathy_df = phenotyping_df %>% 
  dplyr::filter(!PatientDurableKey %in% neuropathy_history$PatientDurableKey) %>% 
  left_join(complications_after %>% 
              dplyr::filter(type == "neuropathy") %>% 
              dplyr::select(PatientDurableKey,ICD10_Value,YearMonthKey) %>% 
              rename(neuropathy_YearMonthKey = YearMonthKey) %>% 
              group_by(PatientDurableKey) %>% 
              dplyr::filter(neuropathy_YearMonthKey == min(neuropathy_YearMonthKey)) %>% 
              group_by(PatientDurableKey) %>% 
              summarize(neuropathy_ICD10_Value = paste0("(",paste0(ICD10_Value,collapse=","),")"),
                        neuropathy_YearMonthKey = min(neuropathy_YearMonthKey)) %>% 
              ungroup(),
            by="PatientDurableKey") %>% 
  mutate(time_to_neuropathy = as.numeric(difftime(ymd(neuropathy_YearMonthKey),ymd(diagnosis_datekey+1),units="days"))/30.5) %>% 
  ## Future incidence ---------
dplyr::filter(time_to_neuropathy > 0 | is.na(time_to_neuropathy)) %>% 
  mutate(t = case_when(is.na(time_to_neuropathy) ~ time_to_censoring,
                       TRUE ~ time_to_neuropathy),
         event = case_when(!is.na(neuropathy_ICD10_Value) ~ 1,
                           TRUE ~ 0))

phenotyping_df %>% 
  dplyr::filter(!PatientDurableKey %in% neuropathy_df$PatientDurableKey) %>% 
  mutate(exclusion = case_when(PatientDurableKey %in% neuropathy_history$PatientDurableKey ~ "Before",
                               TRUE ~ "Month Of")) %>% 
  # group_by(exclusion) %>% 
  tally()

table(neuropathy_df$event)

neuropathy_fit = survfit(Surv(t,event) ~ sidd_category,data=neuropathy_df)  
ggsurvfit(neuropathy_fit)

# Unique without complications

length(unique(retinopathy_df$PatientDurableKey,nephropathy_df$PatientDurableKey,neuropathy_df$PatientDurableKey))

# # CKD ---------------
# 
# ## No History ----------
# 
# ckd_history = complications_before %>% 
#   dplyr::filter(type == "ckd") %>% 
#   distinct(PatientDurableKey)
# 
# ckd_df = phenotyping_df %>% 
#   dplyr::filter(!PatientDurableKey %in% ckd_history$PatientDurableKey) %>% 
#   left_join(complications_after %>% 
#               dplyr::filter(type == "ckd") %>% 
#               dplyr::select(PatientDurableKey,ICD10_Value,YearMonthKey) %>% 
#               rename(ckd_YearMonthKey = YearMonthKey) %>% 
#               group_by(PatientDurableKey) %>% 
#               dplyr::filter(ckd_YearMonthKey == min(ckd_YearMonthKey)) %>% 
#               group_by(PatientDurableKey) %>% 
#               summarize(ckd_ICD10_Value = paste0("(",paste0(ICD10_Value,collapse=","),")"),
#                         ckd_YearMonthKey = min(ckd_YearMonthKey)) %>% 
#               ungroup(),
#             by="PatientDurableKey") %>% 
#   mutate(time_to_ckd = as.numeric(difftime(ymd(ckd_YearMonthKey),ymd(diagnosis_datekey+1),units="days"))/30.5) %>% 
#   ## Future incidence ---------
# dplyr::filter(time_to_ckd > 0 | is.na(time_to_ckd)) %>% 
#   mutate(t = case_when(is.na(time_to_ckd) ~ time_to_censoring,
#                        TRUE ~ time_to_ckd),
#          event = case_when(!is.na(ckd_ICD10_Value) ~ 1,
#                            TRUE ~ 0))
# 
# phenotyping_df %>% 
#   dplyr::filter(!PatientDurableKey %in% ckd_df$PatientDurableKey) %>% 
#   mutate(exclusion = case_when(PatientDurableKey %in% ckd_history$PatientDurableKey ~ "Before",
#                                TRUE ~ "Month Of")) %>% 
#   # group_by(exclusion) %>% 
#   tally()
# 
# table(ckd_df$event)
# 
# ckd_fit = survfit(Surv(t,event) ~ sidd_category,data=ckd_df)  
# ggsurvfit(ckd_fit)

# CHD ---------------

## No History ----------

chd_history = complications_before %>% 
  dplyr::filter(type == "chd") %>% 
  distinct(PatientDurableKey)

chd_df = phenotyping_df %>% 
  dplyr::filter(!PatientDurableKey %in% chd_history$PatientDurableKey) %>% 
  left_join(complications_after %>% 
              dplyr::filter(type == "chd") %>% 
              dplyr::select(PatientDurableKey,ICD10_Value,YearMonthKey) %>% 
              rename(chd_YearMonthKey = YearMonthKey) %>% 
              group_by(PatientDurableKey) %>% 
              dplyr::filter(chd_YearMonthKey == min(chd_YearMonthKey)) %>% 
              group_by(PatientDurableKey) %>% 
              summarize(chd_ICD10_Value = paste0("(",paste0(ICD10_Value,collapse=","),")"),
                        chd_YearMonthKey = min(chd_YearMonthKey)) %>% 
              ungroup(),
            by="PatientDurableKey") %>% 
  mutate(time_to_chd = as.numeric(difftime(ymd(chd_YearMonthKey),ymd(diagnosis_datekey+1),units="days"))/30.5) %>% 
  ## Future incidence ---------
dplyr::filter(time_to_chd > 0 | is.na(time_to_chd)) %>% 
  mutate(t = case_when(is.na(time_to_chd) ~ time_to_censoring,
                       TRUE ~ time_to_chd),
         event = case_when(!is.na(chd_ICD10_Value) ~ 1,
                           TRUE ~ 0))

phenotyping_df %>% 
  dplyr::filter(!PatientDurableKey %in% chd_df$PatientDurableKey) %>% 
  mutate(exclusion = case_when(PatientDurableKey %in% chd_history$PatientDurableKey ~ "Before",
                               TRUE ~ "Month Of")) %>% 
  # group_by(exclusion) %>% 
  tally()

table(chd_df$event)

chd_fit = survfit(Surv(t,event) ~ sidd_category,data=chd_df)  
ggsurvfit(chd_fit)



# AFIB ---------------

## No History ----------

afib_history = complications_before %>% 
  dplyr::filter(type == "afib") %>% 
  distinct(PatientDurableKey)

afib_df = phenotyping_df %>% 
  dplyr::filter(!PatientDurableKey %in% afib_history$PatientDurableKey) %>% 
  left_join(complications_after %>% 
              dplyr::filter(type == "afib") %>% 
              dplyr::select(PatientDurableKey,ICD10_Value,YearMonthKey) %>% 
              rename(afib_YearMonthKey = YearMonthKey) %>% 
              group_by(PatientDurableKey) %>% 
              dplyr::filter(afib_YearMonthKey == min(afib_YearMonthKey)) %>% 
              group_by(PatientDurableKey) %>% 
              summarize(afib_ICD10_Value = paste0("(",paste0(ICD10_Value,collapse=","),")"),
                        afib_YearMonthKey = min(afib_YearMonthKey)) %>% 
              ungroup(),
            by="PatientDurableKey") %>% 
  mutate(time_to_afib = as.numeric(difftime(ymd(afib_YearMonthKey),ymd(diagnosis_datekey+1),units="days"))/30.5) %>% 
  ## Future incidence ---------
dplyr::filter(time_to_afib > 0 | is.na(time_to_afib)) %>% 
  mutate(t = case_when(is.na(time_to_afib) ~ time_to_censoring,
                       TRUE ~ time_to_afib),
         event = case_when(!is.na(afib_ICD10_Value) ~ 1,
                           TRUE ~ 0))

phenotyping_df %>% 
  dplyr::filter(!PatientDurableKey %in% afib_df$PatientDurableKey) %>% 
  mutate(exclusion = case_when(PatientDurableKey %in% afib_history$PatientDurableKey ~ "Before",
                               TRUE ~ "Month Of")) %>% 
  # group_by(exclusion) %>% 
  tally()

table(afib_df$event)

afib_fit = survfit(Surv(t,event) ~ sidd_category,data=afib_df)  
ggsurvfit(afib_fit)

# STROKE ---------------

## No History ----------

stroke_history = complications_before %>% 
  dplyr::filter(type == "stroke") %>% 
  distinct(PatientDurableKey)

stroke_df = phenotyping_df %>% 
  dplyr::filter(!PatientDurableKey %in% stroke_history$PatientDurableKey) %>% 
  left_join(complications_after %>% 
              dplyr::filter(type == "stroke") %>% 
              dplyr::select(PatientDurableKey,ICD10_Value,YearMonthKey) %>% 
              rename(stroke_YearMonthKey = YearMonthKey) %>% 
              group_by(PatientDurableKey) %>% 
              dplyr::filter(stroke_YearMonthKey == min(stroke_YearMonthKey)) %>% 
              group_by(PatientDurableKey) %>% 
              summarize(stroke_ICD10_Value = paste0("(",paste0(ICD10_Value,collapse=","),")"),
                        stroke_YearMonthKey = min(stroke_YearMonthKey)) %>% 
              ungroup(),
            by="PatientDurableKey") %>% 
  mutate(time_to_stroke = as.numeric(difftime(ymd(stroke_YearMonthKey),ymd(diagnosis_datekey+1),units="days"))/30.5) %>% 
  ## Future incidence ---------
dplyr::filter(time_to_stroke > 0 | is.na(time_to_stroke)) %>% 
  mutate(t = case_when(is.na(time_to_stroke) ~ time_to_censoring,
                       TRUE ~ time_to_stroke),
         event = case_when(!is.na(stroke_ICD10_Value) ~ 1,
                           TRUE ~ 0))

phenotyping_df %>% 
  dplyr::filter(!PatientDurableKey %in% stroke_df$PatientDurableKey) %>% 
  mutate(exclusion = case_when(PatientDurableKey %in% stroke_history$PatientDurableKey ~ "Before",
                               TRUE ~ "Month Of")) %>% 
  # group_by(exclusion) %>% 
  tally()

table(stroke_df$event)

stroke_fit = survfit(Surv(t,event) ~ sidd_category,data=stroke_df)  
ggsurvfit(stroke_fit)

# PAD ---------------

## No History ----------

pad_history = complications_before %>% 
  dplyr::filter(type == "pad") %>% 
  distinct(PatientDurableKey)

pad_df = phenotyping_df %>% 
  dplyr::filter(!PatientDurableKey %in% pad_history$PatientDurableKey) %>% 
  left_join(complications_after %>% 
              dplyr::filter(type == "pad") %>% 
              dplyr::select(PatientDurableKey,ICD10_Value,YearMonthKey) %>% 
              rename(pad_YearMonthKey = YearMonthKey) %>% 
              group_by(PatientDurableKey) %>% 
              dplyr::filter(pad_YearMonthKey == min(pad_YearMonthKey)) %>% 
              group_by(PatientDurableKey) %>% 
              summarize(pad_ICD10_Value = paste0("(",paste0(ICD10_Value,collapse=","),")"),
                        pad_YearMonthKey = min(pad_YearMonthKey)) %>% 
              ungroup(),
            by="PatientDurableKey") %>% 
  mutate(time_to_pad = as.numeric(difftime(ymd(pad_YearMonthKey),ymd(diagnosis_datekey+1),units="days"))/30.5) %>% 
  ## Future incidence ---------
dplyr::filter(time_to_pad > 0 | is.na(time_to_pad)) %>% 
  mutate(t = case_when(is.na(time_to_pad) ~ time_to_censoring,
                       TRUE ~ time_to_pad),
         event = case_when(!is.na(pad_ICD10_Value) ~ 1,
                           TRUE ~ 0))

phenotyping_df %>% 
  dplyr::filter(!PatientDurableKey %in% pad_df$PatientDurableKey) %>% 
  mutate(exclusion = case_when(PatientDurableKey %in% pad_history$PatientDurableKey ~ "Before",
                               TRUE ~ "Month Of")) %>% 
  # group_by(exclusion) %>% 
  tally()

table(pad_df$event)

pad_fit = survfit(Surv(t,event) ~ sidd_category,data=pad_df)  
ggsurvfit(pad_fit)

# HFPEF ---------------

## No History ----------

hfpef_history = complications_before %>% 
  dplyr::filter(type == "hfpef") %>% 
  distinct(PatientDurableKey)

hfpef_df = phenotyping_df %>% 
  dplyr::filter(!PatientDurableKey %in% hfpef_history$PatientDurableKey) %>% 
  left_join(complications_after %>% 
              dplyr::filter(type == "hfpef") %>% 
              dplyr::select(PatientDurableKey,ICD10_Value,YearMonthKey) %>% 
              rename(hfpef_YearMonthKey = YearMonthKey) %>% 
              group_by(PatientDurableKey) %>% 
              dplyr::filter(hfpef_YearMonthKey == min(hfpef_YearMonthKey)) %>% 
              group_by(PatientDurableKey) %>% 
              summarize(hfpef_ICD10_Value = paste0("(",paste0(ICD10_Value,collapse=","),")"),
                        hfpef_YearMonthKey = min(hfpef_YearMonthKey)) %>% 
              ungroup(),
            by="PatientDurableKey") %>% 
  mutate(time_to_hfpef = as.numeric(difftime(ymd(hfpef_YearMonthKey),ymd(diagnosis_datekey+1),units="days"))/30.5) %>% 
  ## Future incidence ---------
dplyr::filter(time_to_hfpef > 0 | is.na(time_to_hfpef)) %>% 
  mutate(t = case_when(is.na(time_to_hfpef) ~ time_to_censoring,
                       TRUE ~ time_to_hfpef),
         event = case_when(!is.na(hfpef_ICD10_Value) ~ 1,
                           TRUE ~ 0))

phenotyping_df %>% 
  dplyr::filter(!PatientDurableKey %in% hfpef_df$PatientDurableKey) %>% 
  mutate(exclusion = case_when(PatientDurableKey %in% hfpef_history$PatientDurableKey ~ "Before",
                               TRUE ~ "Month Of")) %>% 
  # group_by(exclusion) %>% 
  tally()

table(hfpef_df$event)

hfpef_fit = survfit(Surv(t,event) ~ sidd_category,data=hfpef_df)  
ggsurvfit(hfpef_fit)

# HFREF ---------------

## No History ----------

hfref_history = complications_before %>% 
  dplyr::filter(type == "hfref") %>% 
  distinct(PatientDurableKey)

hfref_df = phenotyping_df %>% 
  dplyr::filter(!PatientDurableKey %in% hfref_history$PatientDurableKey) %>% 
  left_join(complications_after %>% 
              dplyr::filter(type == "hfref") %>% 
              dplyr::select(PatientDurableKey,ICD10_Value,YearMonthKey) %>% 
              rename(hfref_YearMonthKey = YearMonthKey) %>% 
              group_by(PatientDurableKey) %>% 
              dplyr::filter(hfref_YearMonthKey == min(hfref_YearMonthKey)) %>% 
              group_by(PatientDurableKey) %>% 
              summarize(hfref_ICD10_Value = paste0("(",paste0(ICD10_Value,collapse=","),")"),
                        hfref_YearMonthKey = min(hfref_YearMonthKey)) %>% 
              ungroup(),
            by="PatientDurableKey") %>% 
  mutate(time_to_hfref = as.numeric(difftime(ymd(hfref_YearMonthKey),ymd(diagnosis_datekey+1),units="days"))/30.5) %>% 
  ## Future incidence ---------
dplyr::filter(time_to_hfref > 0 | is.na(time_to_hfref)) %>% 
  mutate(t = case_when(is.na(time_to_hfref) ~ time_to_censoring,
                       TRUE ~ time_to_hfref),
         event = case_when(!is.na(hfref_ICD10_Value) ~ 1,
                           TRUE ~ 0))

phenotyping_df %>% 
  dplyr::filter(!PatientDurableKey %in% hfref_df$PatientDurableKey) %>% 
  mutate(exclusion = case_when(PatientDurableKey %in% hfref_history$PatientDurableKey ~ "Before",
                               TRUE ~ "Month Of")) %>% 
  # group_by(exclusion) %>% 
  tally()

table(hfref_df$event)

hfref_fit = survfit(Surv(t,event) ~ sidd_category,data=hfref_df)  
ggsurvfit(hfref_fit)



# SAVE ----------
write_parquet(retinopathy_df,paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan004/retinopathy dataset.parquet"))
write_parquet(neuropathy_df,paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan004/neuropathy dataset.parquet"))
write_parquet(nephropathy_df,paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan004/nephropathy dataset.parquet"))
write_parquet(any_df,paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan004/any complication dataset.parquet"))


# write_parquet(ckd_df,paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan004/ckd dataset.parquet"))
write_parquet(chd_df,paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan004/chd dataset.parquet"))
write_parquet(afib_df,paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan004/afib dataset.parquet"))
write_parquet(stroke_df,paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan004/stroke dataset.parquet"))
write_parquet(pad_df,paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan004/pad dataset.parquet"))
write_parquet(hfpef_df,paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan004/hfpef dataset.parquet"))
write_parquet(hfref_df,paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan004/hfref dataset.parquet"))

