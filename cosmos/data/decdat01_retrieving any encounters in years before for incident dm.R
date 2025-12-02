rm(list=ls());gc();source(".Rprofile")

source("H:/code/functions/cosmos/encounter_distinct_Range_index.R")


t = Sys.time()
for(year in c(2010:2023)){
  print(year)
  encounter_distinct_Range_index(connection_Cosmos = con_Cosmos,filter_year = year,IndexDateKey_Var="diagnosis_datekey",
                                 project_string = "PROJECTS.ProjectD0C076.[ET4003\\shdw_1208_jvargh1].rfd03",LowerInterval = -2,UpperInterval = -1) %>% 
    write_dataset(.,path=paste0(path_diabetes_endotypes_cosmos_folder,"/working/decdat01a"),partitioning = c("Year"))
  
  encounter_distinct_Range_index(connection_Cosmos = con_Cosmos,filter_year = year,IndexDateKey_Var="diagnosis_datekey",
                                 project_string = "PROJECTS.ProjectD0C076.[ET4003\\shdw_1208_jvargh1].rfd03",LowerInterval = -1,UpperInterval = 0) %>% 
    write_dataset(.,path=paste0(path_diabetes_endotypes_cosmos_folder,"/working/decdat01b"),partitioning = c("Year"))
  
  
}
t - Sys.time()


open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/decdat01a"),format = "parquet",partitioning = "Year") %>% 
  head() %>% 
  collect()

open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/decdat01b"),format = "parquet",partitioning = "Year") %>% 
  tally() %>% 
  collect()

