
source("functions/data_extract.R")

library(tidyverse)

# Read about how .Rprofile is useful
if(Sys.info()["user"] == "JVARGH7"){
  path_endotypes_folder <- "C:/Cloud/Emory/li, zhongyu - Diabetes Endotypes Project (JV and ZL)"
  cluster_labels = c("MOD","SIRD","MARD","SIDD")
  cluster_colors = c("MOD"="#F8BDA4","SIRD"="#A1C3AC","SIDD"="#ACD9EA","MARD"="#D0ACC9")
}

if(Sys.info()["user"] == "zhongyuli"){
  path_endotypes_folder <- "/Users/zhongyuli/Library/CloudStorage/OneDrive-EmoryUniversity/Diabetes Endotypes Project (JV and ZL)"
  cluster_labels = c("MOD","SIRD","MARD","SIDD")
  cluster_colors = c("MOD"="#F8BDA4","SIRD"="#A1C3AC","SIDD"="#ACD9EA","MARD"="#D0ACC9")
}

if(Sys.info()["user"] == "JGUO258"){
  path_endotypes_folder <- "C:/Users/JGUO258/OneDrive - Emory/Diabetes Endotypes Project (JV and ZL)"
  cluster_labels = c("MOD","SIRD","MARD","SIDD")
  cluster_colors = c("MOD"="#F8BDA4","SIRD"="#A1C3AC","SIDD"="#ACD9EA","MARD"="#D0ACC9")
  
}

duration_cutoff <- 1
lab_cutoff <- c(0:365)
# Example for Look AHEAD study on how to use relative paths
path_look_ahead_folder <- paste0(path_endotypes_folder,"/working/look ahead")
# Create relative paths for other cohorts 
path_accord_folder <- paste0(path_endotypes_folder,"/working/accord")
path_aric_folder <- paste0(path_endotypes_folder,"/working/aric")
path_bhs_folder <- paste0(path_endotypes_folder,"/working/bhs")
path_cardia_folder <- paste0(path_endotypes_folder,"/working/cardia")
path_dpp_folder <- paste0(path_endotypes_folder,"/working/dpp")
path_dppos_folder <- paste0(path_endotypes_folder,"/working/dppos")
path_jhs_folder <- paste0(path_endotypes_folder,"/working/jhs")
path_mesa_folder <- paste0(path_endotypes_folder,"/working/mesa")
path_cleaned_folder <- paste0(path_endotypes_folder,"/working/cleaned")

