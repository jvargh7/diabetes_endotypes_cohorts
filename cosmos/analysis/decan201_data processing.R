rm(list=ls());gc();source(".Rprofile")


source("analysis/decan002c_imputed phenotyping dataset processing.R")
# rm(predictors_after,predictors_before)

phenotyping_df = read_parquet(paste0(path_diabetes_endotypes_cosmos_folder,"/working/phenotyping dataset.parquet"))
svi_df_imp = readRDS(paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan200/imputed svi dataset.RDS"))


hist(phenotyping_df_imputed$is_sidd)

table(phenotyping_df_imputed$is_sidd>=sidd_cutoff)

decan201_df = phenotyping_df %>% 
  left_join(phenotyping_df_imputed %>% 
              dplyr::select(PatientDurableKey,is_sidd,is_mod,is_mard),
            by=c("PatientDurableKey")) %>% 
  left_join(svi_df_imp %>% 
              dplyr::select(PatientDurableKey,SviOverallPctlRankByZip2020_imp, SviHouseholdCharacteristicsPctlRankByZip2020_imp,SviRacialEthnicMinorityStatusPctlRankByZip2020_imp),
            by = c("PatientDurableKey")) %>% 
  mutate(sidd_category = case_when(is_sidd >= sidd_cutoff ~ 1,
                                   TRUE ~ 0),
         
         mod_category = case_when(is_mod >= mod_cutoff ~ 1,
                                  TRUE ~ 0),
         
         mard_category = case_when(is_mard >= mard_cutoff ~ 1,
                                   TRUE ~ 0),
         
         # SviOverallPctlRankByZip2020_imp = SviOverallPctlRankByZip2020_imp*100,
         # SviHouseholdCharacteristicsPctlRankByZip2020_imp = SviHouseholdCharacteristicsPctlRankByZip2020_imp*100,
         # SviRacialEthnicMinorityStatusPctlRankByZip2020_imp = SviRacialEthnicMinorityStatusPctlRankByZip2020_imp*100,
         
         raceeth = factor(raceeth,levels=1:4,labels=c("NHW","NHB","HIS","NHO")),
         age_category = case_when(Dmagediag >= 65 ~ "65plus",
                                  Dmagediag >= 45 ~ "45-64",
                                  Dmagediag >= 18 ~ "18-44",
                                  TRUE ~ NA_character_)
         )  


saveRDS(decan201_df,paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan201/glmer dataset.RDS"))

