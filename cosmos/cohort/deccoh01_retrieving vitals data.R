rm(list=ls());gc();source(".Rprofile")

source("H:/code/functions/cosmos/vitals_index.R")

t = Sys.time()
for(year in c(2010:2023)){
  print(year)
  print("Lookback")
  vitals_YearMonth_index(con_Cosmos,project_string = "PROJECTS.ProjectD0C076.[ET4003\\shdw_1208_jvargh1].decdat03",IndexDateKey_Var = "diagnosis_datekey",
                         LookBackInterval = 2,FollowUpInterval = 0,filter_year = year) %>%
    write_dataset(.,path=paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh01a"),partitioning = c("Year"))
  print("Followup")
  vitals_YearMonth_index(con_Cosmos,project_string = "PROJECTS.ProjectD0C076.[ET4003\\shdw_1208_jvargh1].decdat03",IndexDateKey_Var = "diagnosis_datekey",
                         LookBackInterval = 0,FollowUpInterval = 6,filter_year = year) %>% 
    write_dataset(.,path=paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh01b"),partitioning = c("Year"))
  
}
Sys.time() - t


open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh01a"),partitioning = c("Year")) %>%
  arrange(PatientDurableKey,Year) %>%
  head(n= 1000) %>%
  collect() %>% View()

