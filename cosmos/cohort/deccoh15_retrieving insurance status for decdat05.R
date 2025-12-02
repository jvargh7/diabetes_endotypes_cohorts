rm(list=ls());gc();source(".Rprofile")

source("H:/code/functions/cosmos/insurance_distinct_Range_index.R")


t = Sys.time()
for(year in c(2010:2024)){
  print(year)
  print("Lookback 1 year")
  insurance_distinct_Range_index(con_Cosmos,project_string = "PROJECTS.ProjectD0C076.[ET4003\\shdw_1208_jvargh1].decdat05",
                                  filter_year = year,IndexDateKey_Var="criterion1_datekey",
            LowerInterval = -1,UpperInterval = 0) %>% 
    write_dataset(.,path=paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh15"),partitioning = c("Year"))
  
  
}
t - Sys.time()

open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh15"),partitioning = c("Year")) %>%
  arrange(PatientDurableKey,Year) %>%
  head(n= 1000) %>%
  collect() %>% View()

