

rm(list=ls());gc();source(".Rprofile")

source("H:/code/functions/cosmos/demographics.R")

# decdat02b: New pull on 17 Nov 2025
demographics(con_Cosmos, 
             project_string = "PROJECTS.ProjectD0C076.[ET4003\\shdw_1208_jvargh1].rfd03") %>% 
  write_dataset(.,path=paste0(path_diabetes_endotypes_cosmos_folder,"/working/decdat02"),partitioning = "ValidatedStateOrProvince_X")

open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/decdat02"),format="parquet",partitioning = "ValidatedStateOrProvince_X") %>% 
  dim()

open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/decdat02"),format="parquet",partitioning = "ValidatedStateOrProvince_X") %>% 
  distinct(PatientDurableKey) %>%
  collect() %>% 
  dim()

open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/decdat02"),format="parquet",partitioning = "ValidatedStateOrProvince_X") %>% 
  dplyr::filter(PrimaryRUCA_X != "*Unspecified") %>% 
  head() %>%
  collect()

