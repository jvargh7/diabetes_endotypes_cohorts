rm(list=ls())
gc()
source(".Rprofile")
newly_detected <- read_parquet(paste0(path_diabetes_endotypes_cosmos_folder,"/working/decdat03/eligible patients.parquet"))

