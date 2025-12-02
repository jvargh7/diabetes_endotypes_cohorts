rm(list=ls());gc();source(".Rprofile")

# source("analysis/decan_analytic sample for sidd classification.R")
# rm(predictors_after,predictors_before)
source("analysis/decan002c_imputed phenotyping dataset processing.R")



map_df = read_parquet(paste0(path_diabetes_endotypes_cosmos_folder,"/working/phenotyping dataset.parquet")) %>% 
  left_join(phenotyping_df_imputed %>% 
              dplyr::select(PatientDurableKey,is_sidd,is_mod,is_mard),
            by=c("PatientDurableKey")) %>% 
  mutate(subphenotype_category = case_when(is_sidd >= sidd_cutoff ~ "SIDD",
                                           is_mard >= mard_cutoff ~ "MARD",
                                           is_mod >= mod_cutoff ~ "MOD",
                                           TRUE ~ "Not_Classified")
  )


tab_poststratified = map_df %>% 
  mutate(age_category = case_when(Dmagediag >= 65 ~ "65 plus",
                                  Dmagediag >= 45 ~ "45-64",
                                  Dmagediag >= 18 ~ "18-44",
                                  TRUE ~ NA_character_)) %>% 
  group_by(subphenotype_category,ValidatedStateOrProvince_X,region,raceeth,female,age_category) %>% 
  tally() %>% 
  ungroup() %>%
  group_by(ValidatedStateOrProvince_X,region,raceeth,female,age_category) %>% 
  mutate(prop = n/sum(n),
         total = sum(n))


tab_poststratified %>% 
  mutate(total = case_when(total < 11 ~ "<11",
                           TRUE ~ as.character(total)),
         n = case_when(total < 11 ~ "<11",
                           TRUE ~ as.character(n))) %>% 
  write_csv("paper/table_post stratification tables subtypes.csv")
  
