rm(list=ls());gc();source(".Rprofile")

source("H:/code/functions/cosmos/procedures_distinct_Year_index.R")


t = Sys.time()
for(year in c(2010:2024)){
  print(year)
  print("Lookback")
  procedures_distinct_Year_index(con_Cosmos,project_string = "PROJECTS.ProjectD0C076.[ET4003\\shdw_1208_jvargh1].decdat05",IndexDateKey_Var = "criterion1_datekey",
                                LookBackInterval = 2,FollowUpInterval = 0,filter_year = year,
                                by_codes = FALSE,cpt_codes=NULL,
                                detection_string = "procd.CptCode") %>% 
    write_dataset(.,path=paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh17a"),partitioning = c("Year"))
  
  print("Followup")
  procedures_distinct_Year_index(con_Cosmos,project_string = "PROJECTS.ProjectD0C076.[ET4003\\shdw_1208_jvargh1].decdat05",IndexDateKey_Var = "criterion1_datekey",
                                     LookBackInterval = 0,FollowUpInterval = 2,filter_year = year,
                                 by_codes = FALSE,cpt_codes=NULL,) %>% 
    write_dataset(.,path=paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh17b"),partitioning = c("Year"))
}
t - Sys.time()

open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh17b"),partitioning = c("Year")) %>%
  arrange(PatientDurableKey,Year) %>%
  head(n= 1000) %>%
  collect() %>% View()

