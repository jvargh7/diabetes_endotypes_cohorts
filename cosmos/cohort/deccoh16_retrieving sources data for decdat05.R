rm(list=ls());gc();source(".Rprofile")

source("H:/code/functions/cosmos/sources_distinct_Year_index.R")


t = Sys.time()
for(year in c(2010:2024)){
  print(year)
  print("Lookback 2 year")
  sources_distinct_YearMonth_index(con_Cosmos,project_string = "PROJECTS.ProjectD0C076.[ET4003\\shdw_1208_jvargh1].decdat05",IndexDateKey_Var = "criterion1_datekey",
                                LookBackInterval = 2,FollowUpInterval = 0,filter_year = year) %>% 
    write_dataset(.,path=paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh16a"),partitioning = c("Year","SourceKey"))
  
  print("Followup 3 year")
  
  sources_distinct_YearMonth_index(con_Cosmos,project_string = "PROJECTS.ProjectD0C076.[ET4003\\shdw_1208_jvargh1].decdat05",IndexDateKey_Var = "criterion1_datekey",
                                   LookBackInterval = 0,FollowUpInterval = 2,filter_year = year) %>% 
    write_dataset(.,path=paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh16b"),partitioning = c("Year","SourceKey"))
  

}

open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh16b"),partitioning = c("Year","SourceKey")) %>% 
  arrange(PatientDurableKey,YearMonth) %>% 
  head(n = 1000) %>% 
  collect() %>% 
  View()

