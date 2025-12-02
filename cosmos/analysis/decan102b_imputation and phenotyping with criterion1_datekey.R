rm(list=ls());gc();source(".Rprofile")

source("analysis/decan_sidd classifier.R")
# source("analysis/decan_analytic sample for sidd classification.R")
# rm(predictors_after,predictors_before)

phenotyping_df = read_parquet(paste0(path_diabetes_endotypes_cosmos_folder,"/working/sidd classification dataset with criterion1_datekey.parquet"))



library(tidymodels)

coefs = coef_classifier$Coefficient
names(coefs) = coef_classifier$Variable


selected_vars = c("Bmi","Hba1c","Dmagediag",
                  "Tgl","Ldlc","Hdlc",
                  "Sbp","Dbp")

phenotyping_df_restricted = phenotyping_df %>% 
  dplyr::select(PatientDurableKey,diagnosis_datekey,
                one_of(selected_vars))

phenotyping_df_imputed = recipe(diagnosis_datekey ~ .,
                                data = phenotyping_df_restricted) %>% 
  
  update_role(PatientDurableKey,diagnosis_datekey,
              new_role="ID") %>% 
  
  # https://stackoverflow.com/questions/67963439/error-warning-there-are-new-levels-in-a-factor-na
  # https://github.com/tidymodels/recipes/issues/756 
  # There might be scenarios where it is more reasonable to impute missing values per group/subsample.
  # step_impute_bag(any_of(c("Hba1c","Tgl","Ldlc","Hdlc",
  #                             "Sbp","Dbp","Bmi"))) %>%
  step_impute_knn(any_of(selected_vars),neighbors=5) %>%
  # step_impute_linear(any_of(c("Bmi")),impute_with = imp_vars(one_of(c("female","Dmagediag","raceeth","ValidatedStateOrProvince_X","region")))) %>%
  # step_impute_linear(any_of(c("Sbp","Dbp")),impute_with = imp_vars(one_of(c("female","Dmagediag","raceeth","ValidatedStateOrProvince_X","region",
  #                                                                           "Bmi")))) %>%
  # step_impute_linear(any_of(c("Hba1c")),impute_with = imp_vars(one_of(c("female","Dmagediag","raceeth","ValidatedStateOrProvince_X","region",
  #                                                                       "Bmi","Sbp","Dbp")))) %>%
  # step_impute_linear(any_of(c("Tgl","Ldlc","Hdlc")),impute_with = imp_vars(one_of(c("female","Dmagediag","raceeth","ValidatedStateOrProvince_X","region",
  #                                                                                   "Hba1c","Sbp","Dbp","Bmi")))) %>%
  prep(.,training = sidd_df_restricted) %>% 
  # bake(): For a recipe with at least one preprocessing operation that has been trained by prep(), apply the computations to new data.
  bake(.,new_data=sidd_df_restricted) 

phenotyping_df_imputed %>%
  as.data.frame() %>% 
  mutate(Ratio_th = Tgl/Hdlc) %>% 
  mutate(is_sidd = plogis(coefs["Intercept"] + Bmi*coefs["Bmi"] + Hba1c*coefs["Hba1c"] + Dmagediag*coefs["Dmagediag"]
                          + Tgl*coefs["Tgl"] + Ldlc*coefs["Ldlc"] + Ratio_th*coefs["Ratio_th"] 
                          + Sbp*coefs["Sbp"] + Dbp*coefs["Dbp"] + Hdlc*coefs["Hdlc"])) %>% 
  saveRDS(.,paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan102/imputed phenotyping classification dataset with criterion1_datekey.RDS"))


