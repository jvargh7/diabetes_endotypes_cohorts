

source(paste0(path_diabetes_endotypes_cosmos_repo,"/analysis/decan_one vs all classifiers.R"))

# phenotyping_df_imputed <- readRDS(paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan002/imputed phenotyping dataset.rds")) %>% 
phenotyping_df_imputed <- readRDS(paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan002/imputed phenotyping dataset.rds")) %>% 
  mutate(is_sidd = plogis(sidd_coefs["Intercept"] + Bmi*sidd_coefs["Bmi"] + Hba1c*sidd_coefs["Hba1c"] + Dmagediag*sidd_coefs["Dmagediag"]
                        + Tgl*sidd_coefs["Tgl"] + Ldlc*sidd_coefs["Ldlc"] + Ratio_th*sidd_coefs["Ratio_th"] 
                        + Sbp*sidd_coefs["Sbp"] + Dbp*sidd_coefs["Dbp"] + Hdlc*sidd_coefs["Hdlc"])) %>% 
  mutate(is_mod = plogis(mod_coefs["Intercept"] + Bmi*mod_coefs["Bmi"] + Hba1c*mod_coefs["Hba1c"] + Dmagediag*mod_coefs["Dmagediag"]
                         + Tgl*mod_coefs["Tgl"] + Ldlc*mod_coefs["Ldlc"] + Ratio_th*mod_coefs["Ratio_th"] 
                         + Sbp*mod_coefs["Sbp"] + Dbp*mod_coefs["Dbp"] + Hdlc*mod_coefs["Hdlc"])) %>% 
  mutate(is_mard = plogis(mard_coefs["Intercept"] + Bmi*mard_coefs["Bmi"] + Hba1c*mard_coefs["Hba1c"] + Dmagediag*mard_coefs["Dmagediag"]
                          + Tgl*mard_coefs["Tgl"] + Ldlc*mard_coefs["Ldlc"] + Ratio_th*mard_coefs["Ratio_th"] 
                          + Sbp*mard_coefs["Sbp"] + Dbp*mard_coefs["Dbp"] + Hdlc*mard_coefs["Hdlc"])) 

