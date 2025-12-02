rm(list=ls());gc();source(".Rprofile")

source("H:/code/functions/cosmos/encounter_count_Range_index.R")


t = Sys.time()
for(year in c(2010:2024)){
  print(year)
  print("Lookback 2 year")
  encounter_count_Range_index(connection_Cosmos = con_Cosmos,filter_year = year,IndexDateKey_Var="criterion1_datekey",
                                 project_string = "PROJECTS.ProjectD0C076.[ET4003\\shdw_1208_jvargh1].decdat05",LowerInterval = -2,UpperInterval = -1) %>% 
    write_dataset(.,path=paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh18a"),partitioning = c("Year"))
  
  print("Lookback 1 year")
  
  encounter_count_Range_index(connection_Cosmos = con_Cosmos,filter_year = year,IndexDateKey_Var="criterion1_datekey",
                                 project_string = "PROJECTS.ProjectD0C076.[ET4003\\shdw_1208_jvargh1].decdat05",LowerInterval = -1,UpperInterval = 0) %>% 
    write_dataset(.,path=paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh18b"),partitioning = c("Year"))
  
  print("Followup 1 year")
  
  encounter_count_Range_index(connection_Cosmos = con_Cosmos,filter_year = year,IndexDateKey_Var="criterion1_datekey",
                              project_string = "PROJECTS.ProjectD0C076.[ET4003\\shdw_1208_jvargh1].decdat05",LowerInterval = 0,UpperInterval = 1) %>% 
    write_dataset(.,path=paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh18c"),partitioning = c("Year"))
  
  print("Followup 2 year")
  
  encounter_count_Range_index(connection_Cosmos = con_Cosmos,filter_year = year,IndexDateKey_Var="criterion1_datekey",
                              project_string = "PROJECTS.ProjectD0C076.[ET4003\\shdw_1208_jvargh1].decdat05",LowerInterval = 1,UpperInterval = 2) %>% 
    write_dataset(.,path=paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh18d"),partitioning = c("Year"))
  
  
}

open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh18b"),partitioning = c("Year")) %>% 
  arrange(PatientDurableKey,Year) %>% 
  head(n = 1000) %>% 
  collect() %>% 
  View()

