rm(list=ls());gc();source(".Rprofile")

source("H:/code/functions/cosmos/insurance_distinct_Range_index.R")


t = Sys.time()
for(year in c(2010:2024)){
  print(year)
  print("Lookback 1 year")
  insurance_distinct_Range_index(con_Cosmos,project_string = "PROJECTS.ProjectD0C076.[ET4003\\shdw_1208_jvargh1].decdat03",
                                  filter_year = year,IndexDateKey_Var="diagnosis_datekey",
                                 LowerInterval = -1,UpperInterval = 0,lower_geq = FALSE,upper_leq = TRUE) %>%
    write_dataset(.,path=paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh05"),partitioning = c("Year"))
  
  insurance_distinct_Range_index(con_Cosmos,project_string = "PROJECTS.ProjectD0C076.[ET4003\\shdw_1208_jvargh1].decdat03",
                                 filter_year = year,IndexDateKey_Var="diagnosis_datekey",
                                 LowerInterval = 0,UpperInterval = 6,lower_geq = FALSE,upper_leq = TRUE) %>% 
    write_dataset(.,path=paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh05b"),partitioning = c("Year"))
  
  
}
t - Sys.time()

open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh05b"),partitioning = c("Year")) %>%
  arrange(PatientDurableKey,Year) %>%
  head(n= 1000) %>%
  collect() %>% View()

