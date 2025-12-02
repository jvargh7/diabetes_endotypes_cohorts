rm(list=ls());gc();source(".Rprofile")

phenotyping_df = read_parquet(paste0(path_diabetes_endotypes_cosmos_folder,"/working/phenotyping dataset.parquet"))

svi_df = open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/decdat02b"),partitioning = "ValidatedStateOrProvince_X") %>% 
  dplyr::filter(PatientDurableKey %in% phenotyping_df$PatientDurableKey) %>% 
  dplyr::select(PatientDurableKey,contains("Svi")) %>% 
  collect() %>% 
  right_join(phenotyping_df %>% 
              dplyr::select(PatientDurableKey,ValidatedStateOrProvince_X,PrimaryRUCA_X,region))

svi_df %>%
  group_by(ValidatedStateOrProvince_X,PrimaryRUCA_X,region) %>% 
  mutate(SviHouseholdCharacteristicsPctlRankByZip2020_imp = case_when(is.na(SviHouseholdCharacteristicsPctlRankByZip2020_X) ~ median(SviHouseholdCharacteristicsPctlRankByZip2020_X,na.rm=TRUE),
                                                                      TRUE  ~ SviHouseholdCharacteristicsPctlRankByZip2020_X),
         SviRacialEthnicMinorityStatusPctlRankByZip2020_imp = case_when(is.na(SviRacialEthnicMinorityStatusPctlRankByZip2020_X) ~ median(SviRacialEthnicMinorityStatusPctlRankByZip2020_X,na.rm=TRUE),
                                                                        TRUE  ~ SviRacialEthnicMinorityStatusPctlRankByZip2020_X),
         
         SviOverallPctlRankByZip2020_imp = case_when(is.na(SviOverallPctlRankByZip2020_X) ~ median(SviOverallPctlRankByZip2020_X,na.rm=TRUE),
                                                                        TRUE  ~ SviOverallPctlRankByZip2020_X),
         
         SviOverallPctlRankByZip2020_imp = median(SviOverallPctlRankByZip2020_X,na.rm=TRUE)
         ) %>% 
  ungroup() %>% 
  group_by(ValidatedStateOrProvince_X,region) %>% 
  mutate(SviHouseholdCharacteristicsPctlRankByZip2020_imp = case_when(is.na(SviHouseholdCharacteristicsPctlRankByZip2020_imp) ~ median(SviHouseholdCharacteristicsPctlRankByZip2020_X,na.rm=TRUE),
                                                                      TRUE  ~ SviHouseholdCharacteristicsPctlRankByZip2020_imp),
         SviRacialEthnicMinorityStatusPctlRankByZip2020_imp = case_when(is.na(SviRacialEthnicMinorityStatusPctlRankByZip2020_imp) ~ median(SviRacialEthnicMinorityStatusPctlRankByZip2020_X,na.rm=TRUE),
                                                                        TRUE  ~ SviRacialEthnicMinorityStatusPctlRankByZip2020_imp),
         
         SviOverallPctlRankByZip2020_imp = case_when(is.na(SviOverallPctlRankByZip2020_imp) ~ median(SviOverallPctlRankByZip2020_X,na.rm=TRUE),
                                                     TRUE  ~ SviOverallPctlRankByZip2020_imp),
         
         SviOverallPctlRankByZip2020_imp = median(SviOverallPctlRankByZip2020_X,na.rm=TRUE)
  ) %>% 
  ungroup() %>% 
  
  
  
  
  saveRDS(.,paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan200/imputed svi dataset.RDS"))


svi_df_imp = readRDS(paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan200/imputed svi dataset.RDS"))

summary(svi_df_imp)

