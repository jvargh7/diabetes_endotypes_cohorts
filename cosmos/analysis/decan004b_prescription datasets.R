rm(list=ls());gc();source(".Rprofile")

library(survival)
library(ggsurvfit)

last_prescription_after <- read_parquet(paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan001/last prescription after diagnosis date.parquet"))

source("analysis/decan002c_imputed phenotyping dataset processing.R")

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
  left_join(last_prescription_after %>% 
              rename(lastrx_YearMonthKey = YearMonthKey),
            by=c("PatientDurableKey")) %>% 
  mutate(time_to_censoring = as.numeric(difftime(ymd(lastrx_YearMonthKey),ymd(diagnosis_datekey+1),units="days"))/30.5) %>% 
  left_join(phenotyping_df_imputed,
            by=c("PatientDurableKey")) %>% 
  mutate(sidd_category = case_when(is_sidd >= sidd_cutoff ~ "SIDD",
                                   TRUE ~ "NonSIDD")) %>% 
  dplyr::filter(!is.na(time_to_censoring))


prescriptions_after <- read_parquet(paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan001/prescriptions after diagnosis date.parquet"))
prescriptions_before <- read_parquet(paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan001/prescriptions before diagnosis date.parquet"))


# INSULIN ------------

## No History ----------

insulin_history = prescriptions_before %>% 
  dplyr::filter(type == "insulin") %>% 
  distinct(PatientDurableKey)

insulin_df = phenotyping_df %>% 
  dplyr::filter(!PatientDurableKey %in% insulin_history$PatientDurableKey) %>% 
  left_join(prescriptions_after %>% 
              dplyr::filter(type == "insulin") %>% 
              dplyr::select(PatientDurableKey,SimpleGenericName,YearMonthKey) %>% 
              rename(insulin_YearMonthKey = YearMonthKey) %>% 
              group_by(PatientDurableKey) %>% 
              dplyr::filter(insulin_YearMonthKey == min(insulin_YearMonthKey)) %>% 
              group_by(PatientDurableKey) %>% 
              summarize(insulin_SimpleGenericName = paste0("(",paste0(SimpleGenericName,collapse=","),")"),
                        insulin_YearMonthKey = min(insulin_YearMonthKey)) %>% 
              ungroup(),
            by="PatientDurableKey") %>% 
  mutate(time_to_insulin = as.numeric(difftime(ymd(insulin_YearMonthKey),ymd(diagnosis_datekey+1),units="days"))/30.5) %>% 
  ## Future incidence ---------
dplyr::filter(time_to_insulin > 0 | is.na(time_to_insulin)) %>% 
  mutate(t = case_when(is.na(time_to_insulin) ~ time_to_censoring,
                       TRUE ~ time_to_insulin),
         event = case_when(!is.na(insulin_SimpleGenericName) ~ 1,
                           TRUE ~ 0))

phenotyping_df %>% 
  dplyr::filter(!PatientDurableKey %in% insulin_df$PatientDurableKey) %>% 
  mutate(exclusion = case_when(PatientDurableKey %in% insulin_history$PatientDurableKey ~ "Before",
                               TRUE ~ "Month Of")) %>% 
  # group_by(exclusion) %>% 
  tally()

phenotyping_df %>% 
  mutate(history = case_when(PatientDurableKey %in% insulin_df$PatientDurableKey ~ 0,
                               TRUE ~ 1)) %>% 
  group_by(sidd_category) %>% 
  summarize(history_insulin = mean(history))


table(insulin_df$event)

insulin_fit = survfit(Surv(t,event) ~ sidd_category,data=insulin_df)  
ggsurvfit(insulin_fit)


# METFORMIN ------------

## No History ----------

metformin_history = prescriptions_before %>% 
  dplyr::filter(str_detect(type,"biguanide")) %>% 
  distinct(PatientDurableKey)

metformin_df = phenotyping_df %>% 
  dplyr::filter(!PatientDurableKey %in% metformin_history$PatientDurableKey) %>% 
  left_join(prescriptions_after %>% 
              dplyr::filter(str_detect(type,"biguanide")) %>% 
              dplyr::select(PatientDurableKey,SimpleGenericName,YearMonthKey) %>% 
              rename(metformin_YearMonthKey = YearMonthKey) %>% 
              group_by(PatientDurableKey) %>% 
              dplyr::filter(metformin_YearMonthKey == min(metformin_YearMonthKey)) %>% 
              group_by(PatientDurableKey) %>% 
              summarize(metformin_SimpleGenericName = paste0("(",paste0(SimpleGenericName,collapse=","),")"),
                        metformin_YearMonthKey = min(metformin_YearMonthKey)) %>% 
              ungroup(),
            by="PatientDurableKey") %>% 
  mutate(time_to_metformin = as.numeric(difftime(ymd(metformin_YearMonthKey),ymd(diagnosis_datekey+1),units="days"))/30.5) %>% 
  ## Future incidence ---------
dplyr::filter(time_to_metformin > 0 | is.na(time_to_metformin)) %>% 
  mutate(t = case_when(is.na(time_to_metformin) ~ time_to_censoring,
                       TRUE ~ time_to_metformin),
         event = case_when(!is.na(metformin_SimpleGenericName) ~ 1,
                           TRUE ~ 0))

phenotyping_df %>% 
  dplyr::filter(!PatientDurableKey %in% metformin_df$PatientDurableKey) %>% 
  mutate(exclusion = case_when(PatientDurableKey %in% metformin_history$PatientDurableKey ~ "Before",
                               TRUE ~ "Month Of")) %>% 
  # group_by(exclusion) %>% 
  tally()

phenotyping_df %>% 
  mutate(history = case_when(PatientDurableKey %in% metformin_df$PatientDurableKey ~ 0,
                             TRUE ~ 1)) %>% 
  group_by(sidd_category) %>% 
  summarize(history_metformin = mean(history))


table(metformin_df$event)

metformin_fit = survfit(Surv(t,event) ~ sidd_category,data=metformin_df)  
ggsurvfit(metformin_fit)



# GLP1-RA/GIP ---------------

## No History ----------

incretin_history = prescriptions_before %>% 
  dplyr::filter(str_detect(type,"glp")) %>% 
  distinct(PatientDurableKey)

incretin_df = phenotyping_df %>% 
  dplyr::filter(!PatientDurableKey %in% incretin_history$PatientDurableKey) %>% 
  left_join(prescriptions_after %>% 
              dplyr::filter(str_detect(type,"glp")) %>% 
              dplyr::select(PatientDurableKey,SimpleGenericName,YearMonthKey) %>% 
              rename(incretin_YearMonthKey = YearMonthKey) %>% 
              group_by(PatientDurableKey) %>% 
              dplyr::filter(incretin_YearMonthKey == min(incretin_YearMonthKey)) %>% 
              group_by(PatientDurableKey) %>% 
              summarize(incretin_SimpleGenericName = paste0("(",paste0(SimpleGenericName,collapse=","),")"),
                        incretin_YearMonthKey = min(incretin_YearMonthKey)) %>% 
              ungroup(),
            by="PatientDurableKey") %>% 
  mutate(time_to_incretin = as.numeric(difftime(ymd(incretin_YearMonthKey),ymd(diagnosis_datekey+1),units="days"))/30.5) %>% 
  ## Future incidence ---------
dplyr::filter(time_to_incretin > 0 | is.na(time_to_incretin)) %>% 
  mutate(t = case_when(is.na(time_to_incretin) ~ time_to_censoring,
                       TRUE ~ time_to_incretin),
         event = case_when(!is.na(incretin_SimpleGenericName) ~ 1,
                           TRUE ~ 0))

phenotyping_df %>% 
  dplyr::filter(!PatientDurableKey %in% incretin_df$PatientDurableKey) %>% 
  mutate(exclusion = case_when(PatientDurableKey %in% incretin_history$PatientDurableKey ~ "Before",
                               TRUE ~ "Month Of")) %>% 
  # group_by(exclusion) %>% 
  tally()

phenotyping_df %>% 
  mutate(history = case_when(PatientDurableKey %in% incretin_df$PatientDurableKey ~ 0,
                             TRUE ~ 1)) %>% 
  group_by(sidd_category) %>% 
  summarize(history_incretin = mean(history))


table(incretin_df$event)

incretin_fit = survfit(Surv(t,event) ~ sidd_category,data=incretin_df)  
ggsurvfit(incretin_fit)

# SAVE ----------
write_parquet(insulin_df,paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan004/insulin dataset.parquet"))
write_parquet(metformin_df,paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan004/metformin dataset.parquet"))
write_parquet(incretin_df,paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan004/incretin dataset.parquet"))

