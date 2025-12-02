rm(list=ls());gc();source(".Rprofile")

phenotyping_df = read_parquet(paste0(path_diabetes_endotypes_cosmos_folder,"/working/phenotyping dataset.parquet"))


library(tidymodels)




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
saveRDS(.,paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan002/imputed phenotyping dataset.RDS"))


