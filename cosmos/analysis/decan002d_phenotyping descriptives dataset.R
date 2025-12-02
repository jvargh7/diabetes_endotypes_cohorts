source("analysis/decan002c_imputed phenotyping dataset processing.R")
# rm(predictors_after,predictors_before)

phenotyping_df = read_parquet(paste0(path_diabetes_endotypes_cosmos_folder,"/working/phenotyping dataset.parquet"))


hist(phenotyping_df_imputed$is_sidd)

table(phenotyping_df_imputed$is_sidd>=sidd_cutoff)

descriptives_df = phenotyping_df %>% 
  left_join(phenotyping_df_imputed %>% 
              dplyr::select(PatientDurableKey,is_sidd,is_mod,is_mard),
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
                                           TRUE ~ "Not_Classified"),
         
         SviOverallPctlRankByZip2020_X = SviOverallPctlRankByZip2020_X*100) %>% 
  mutate(subphenotype_category = factor(subphenotype_category,levels=c("SIDD","MARD","MOD","Not_Classified"),
                                        labels=c("SIDD","MARD","MOD","Unclassified")))

rm(phenotyping_df,phenotyping_df_imputed)
  
