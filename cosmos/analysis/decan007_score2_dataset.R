# the purpose of this file is to extract and prepare the new T2D dataset for SCORE2 implementation 
# We will first extract the eGFR values for the 1 year range post T2D DX and use the mean value of the earliest month 
# We will impute serum creatinine based on BMI, HbA1c, Age, gender, SBP/DBP, and urinary creatinine and albumin
# we will then calculate eGFR (existing equation) from the imputed serum creatinine and use those values for those who miss eGFR

rm(list=ls());gc();source(".Rprofile")

library(tidymodels)

decdat03 <- open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/decdat03/eligible patients.parquet")) %>% 
  dplyr::select(PatientDurableKey,diagnosis_datekey,criterion1_date,criterion2_date,diagnosis_date,ValidatedStateOrProvince_X,
                PrimaryRUCA_X,raceeth,female,age) %>% 
  mutate(diagnosis_datekey_plus1y = diagnosis_datekey + 10000,
         diagnosis_datekey_minus1y = diagnosis_datekey - 10000)

phenotyping_df = read_parquet(paste0(path_diabetes_endotypes_cosmos_folder,"/working/phenotyping dataset.parquet"))
view(phenotyping_df)

## deccoh02nb. eGFR  -----------

egfr_after <- open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh02nb"),partitioning = c("Year")) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         YearMonthKey = paste0(YearMonth,"01")) %>% 
  mutate(YearMonthKey = as.numeric(YearMonthKey)) %>% 
  left_join(decdat03 %>% 
              dplyr::select(PatientDurableKey,diagnosis_datekey,diagnosis_datekey_plus1y),
            by = c("PatientDurableKey")) %>%
  mutate(NumericValue = as.numeric(NumericValue))%>% 
  mutate(egfr = case_when(NumericValue >= 5 & NumericValue <= 200 ~ NumericValue,
                         TRUE ~ NA_real_)) %>% 
  to_duckdb()  %>%
  mutate(period = case_when(YearMonthKey >= diagnosis_datekey & YearMonthKey < diagnosis_datekey_plus1y ~ "Y1",
                            TRUE ~ "Y2 or later")) %>% 
  dplyr::filter(period %in% c("Y1"),!is.na(egfr))  %>% 
  group_by(PatientDurableKey) %>% 
  dplyr::filter(YearMonthKey == min(YearMonthKey)) %>% 
  summarize(egfr_after = mean(egfr),
            YearMonthKey_egfr_after = min(YearMonthKey)) %>%   
  ungroup()  %>% 
  collect() 


# construct addtional variables for KNN imputation  -----------

## deccoh02lb. urine_albumin

u_alb_after <- open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh02lb"),partitioning = c("Year")) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         YearMonthKey = paste0(YearMonth,"01")) %>% 
  mutate(YearMonthKey = as.numeric(YearMonthKey)) %>% 
  left_join(decdat03 %>% 
              dplyr::select(PatientDurableKey,diagnosis_datekey,diagnosis_datekey_plus1y),
            by = c("PatientDurableKey")) %>%
  mutate(NumericValue = as.numeric(NumericValue))%>% 
  mutate(u_alb = case_when(NumericValue > 0 & NumericValue <= 1500 ~ NumericValue,
                         TRUE ~ NA_real_)) %>% 
  to_duckdb()  %>%
  mutate(period = case_when(YearMonthKey >= diagnosis_datekey & YearMonthKey < diagnosis_datekey_plus1y ~ "Y1",
                            TRUE ~ "Y2 or later")) %>% 
  dplyr::filter(period %in% c("Y1"),!is.na(u_alb))  %>% 
  group_by(PatientDurableKey) %>% 
  dplyr::filter(YearMonthKey == min(YearMonthKey)) %>% 
  summarize(u_alb_after = mean(u_alb),
            YearMonthKey_u_alb_after = min(YearMonthKey)) %>%   
  ungroup()  %>% 
  collect() 

view(u_alb_after)
summary(u_alb_after$u_alb_after)

## deccoh02kb. urine_creatinine

u_crtn_after <- open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh02kb"),partitioning = c("Year")) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         YearMonthKey = paste0(YearMonth,"01")) %>% 
  mutate(YearMonthKey = as.numeric(YearMonthKey)) %>% 
  left_join(decdat03 %>% 
              dplyr::select(PatientDurableKey,diagnosis_datekey,diagnosis_datekey_plus1y),
            by = c("PatientDurableKey")) %>%
  mutate(NumericValue = as.numeric(NumericValue))%>% 
  mutate(u_crtn = case_when(NumericValue > 0 & NumericValue <= 500 ~ NumericValue,
                         TRUE ~ NA_real_)) %>% 
  to_duckdb()  %>%
  mutate(period = case_when(YearMonthKey >= diagnosis_datekey & YearMonthKey < diagnosis_datekey_plus1y ~ "Y1",
                            TRUE ~ "Y2 or later")) %>% 
  dplyr::filter(period %in% c("Y1"),!is.na(u_crtn))  %>% 
  group_by(PatientDurableKey) %>% 
  dplyr::filter(YearMonthKey == min(YearMonthKey)) %>% 
  summarize(u_crtn_after = mean(u_crtn),
            YearMonthKey_u_crtn_after = min(YearMonthKey)) %>%   
  ungroup()  %>% 
  collect() 

view(u_crtn_after)
summary(u_crtn_after$u_crtn_after)
  
## deccoh02ib. serum_creatinine

s_crtn_after <- open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh02ib"),partitioning = c("Year")) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         YearMonthKey = paste0(YearMonth,"01")) %>% 
  mutate(YearMonthKey = as.numeric(YearMonthKey)) %>% 
  left_join(decdat03 %>% 
              dplyr::select(PatientDurableKey,diagnosis_datekey,diagnosis_datekey_plus1y),
            by = c("PatientDurableKey")) %>%
  mutate(NumericValue = as.numeric(NumericValue))%>% 
  mutate(s_crtn = case_when(NumericValue > 0 & NumericValue <= 20 ~ NumericValue,
                         TRUE ~ NA_real_)) %>% 
  to_duckdb()  %>%
  mutate(period = case_when(YearMonthKey >= diagnosis_datekey & YearMonthKey < diagnosis_datekey_plus1y ~ "Y1",
                            TRUE ~ "Y2 or later")) %>% 
  dplyr::filter(period %in% c("Y1"),!is.na(s_crtn))  %>% 
  group_by(PatientDurableKey) %>% 
  dplyr::filter(YearMonthKey == min(YearMonthKey)) %>% 
  summarize(s_crtn_after = mean(s_crtn),
            YearMonthKey_s_crtn_after = min(YearMonthKey)) %>%   
  ungroup()  %>% 
  collect() 

view(s_crtn_after)


phenotyping_df_extended <- phenotyping_df %>%
  left_join(u_alb_after, by = "PatientDurableKey") %>%
  left_join(u_crtn_after, by = "PatientDurableKey") %>%
  left_join(s_crtn_after, by = "PatientDurableKey")

colSums(is.na(phenotyping_df_extended[, c("s_crtn_after", "u_crtn_after","u_alb_after")]))
## KNN imputation to impute serum creatinine, urinary creatinine, and urinary albumin

selected_vars <- c("s_crtn_after", "u_alb_after", "u_crtn_after",
                   "Dmagediag", "female", "Hba1c", "Sbp", "Dbp", "Bmi")

phenotyping_df_restricted <- phenotyping_df_extended %>%
  select(PatientDurableKey, diagnosis_datekey, all_of(selected_vars))

phenotyping_df_imputed <- recipe(diagnosis_datekey ~ ., data = phenotyping_df_restricted) %>%
  update_role(PatientDurableKey, diagnosis_datekey, new_role = "ID") %>%
  step_impute_knn(any_of(selected_vars), neighbors = 5) %>%
  prep(training = phenotyping_df_restricted) %>%
  bake(new_data = phenotyping_df_restricted)

# Retain only the three imputed lab variables
phenotyping_df_imputed <- phenotyping_df_imputed %>%
  dplyr::select(PatientDurableKey, diagnosis_datekey,
                s_crtn_after, u_crtn_after, u_alb_after)
view(phenotyping_df_imputed)
# note which ones are imputed and which ones are not 
imputation_flags <- phenotyping_df_restricted %>%
  dplyr::select(PatientDurableKey, diagnosis_datekey, 
                s_crtn_after_orig = s_crtn_after,
                u_crtn_after_orig = u_crtn_after,
                u_alb_after_orig = u_alb_after) %>%
  left_join(phenotyping_df_imputed,
            by = c("PatientDurableKey", "diagnosis_datekey")) %>%
  mutate(
    s_crtn_imputed = ifelse(is.na(s_crtn_after_orig) & !is.na(s_crtn_after), 1, 0),
    u_crtn_imputed = ifelse(is.na(u_crtn_after_orig) & !is.na(u_crtn_after), 1, 0),
    u_alb_imputed = ifelse(is.na(u_alb_after_orig) & !is.na(u_alb_after), 1, 0)
  ) %>%
  dplyr::select(PatientDurableKey,
                s_crtn_after, s_crtn_imputed,
                u_crtn_after, u_crtn_imputed,
                u_alb_after, u_alb_imputed)


view(imputation_flags)

write_parquet(imputation_flags,paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan001/imputed_creatinine.parquet"))

#imputation_flags <- read_parquet(paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan001/imputed_creatinine.parquet"))
summary(imputation_flags)

# merge with the imputed phenotype dataset with other imputed variables 

source("analysis/decan002c_imputed phenotyping dataset processing.R")
df_phenotyping <- read_parquet(paste0(path_diabetes_endotypes_cosmos_folder,"/working/phenotyping dataset.parquet"))

colnames(phenotyping_df_imputed)
df = phenotyping_df_imputed %>% 
  left_join(df_phenotyping %>% 
              dplyr::select(PatientDurableKey,diagnosis_datekey,female),
            by=c("PatientDurableKey")) %>% 
  mutate(sidd_category = case_when(is_sidd >= sidd_cutoff ~ "SIDD",
                                   TRUE ~ "NonSIDD"),
         
         mod_category = case_when(is_mod >= mod_cutoff ~ "MOD",
                                  TRUE ~ "NonMOD"),
         
         mard_category = case_when(is_mard >= mard_cutoff ~ "MARD",
                                   TRUE ~ "NonMARD"),
         
         
         subphenotype_category = case_when(is_sidd >= sidd_cutoff ~ "SIDD",
                                           is_mard >= mard_cutoff ~ "MARD",
                                           is_mod >= mod_cutoff ~ "MOD",
                                           TRUE ~ "Not_Classified")) %>%
  mutate(subphenotype_category = factor(subphenotype_category,levels=c("SIDD","MARD","MOD","Not_Classified"),
                                        labels=c("SIDD","MARD","MOD","Unclassified")))

df_extended <- df %>% 
 left_join (imputation_flags,by = "PatientDurableKey")


colSums(is.na(df_extended[, c("s_crtn_after", "Hba1c","Bmi","Ldlc")]))

# use function to caluclate eGFR 

egfr_ckdepi_2021 <- function(scr, female, age) {
  vec1 <- case_when(
    is.na(scr) ~ NA_real_,
    female == 1 ~ scr / 0.7,
    TRUE ~ scr / 0.9
  )
  
  min1 <- pmin(vec1, 1)^(case_when(female == 1 ~ -0.241, TRUE ~ -0.302))
  max2 <- pmax(vec1, 1)^(-1.200)
  age3 <- (0.9938)^age
  coefsex4 <- case_when(female == 1 ~ 1.012, TRUE ~ 1)
  
  egfr_out <- 142 * min1 * max2 * age3 * coefsex4
  return(egfr_out)
}

df_extended <- df_extended %>%
  mutate(
    egfr_ckdepi = egfr_ckdepi_2021(
      scr = s_crtn_after,
      female = female,
      age = Dmagediag
    )
  )

summary(df_extended$s_crtn_after)

# use egfr if availble; if not, use efgr calculated from serum creatinine 
df_score2 <- df_extended %>% 
  left_join(egfr_after,by=c("PatientDurableKey")) %>% 
  mutate (
    egfr_final = case_when (
      !is.na(egfr_after) ~ egfr_after,
      is.na(egfr_after) ~ egfr_ckdepi
    )
  )

write_parquet(df_score2,paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan001/imputed_score2.parquet"))




