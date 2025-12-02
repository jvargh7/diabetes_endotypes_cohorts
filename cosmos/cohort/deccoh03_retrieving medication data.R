rm(list=ls());gc();source(".Rprofile")

source("H:/code/functions/cosmos/medications_distinct_Range_index.R")

# We used medications_distinct_YearMonth_index here because we are interested in time to event 

t = Sys.time()
for(year in c(2010:2024)){
  print(year)
  print("Lookback")
  medications_distinct_Range_index(con_Cosmos,project_string = "PROJECTS.ProjectD0C076.[ET4003\\shdw_1208_jvargh1].decdat03",IndexDateKey_Var = "diagnosis_datekey",
                                  LowerInterval = -1,UpperInterval = 0,filter_year = year) %>% 
    write_dataset(.,path=paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh03a"),partitioning = c("Year"))
  
  print("Follow-up 5 years")
  medications_distinct_Range_index(con_Cosmos,project_string = "PROJECTS.ProjectD0C076.[ET4003\\shdw_1208_jvargh1].decdat03",IndexDateKey_Var = "diagnosis_datekey",
                                  LowerInterval = 0,UpperInterval = 5,filter_year = year) %>% 
    write_dataset(.,path=paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh03b"),partitioning = c("Year"))
  
}
t - Sys.time()

open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh03a"),partitioning = c("Year")) %>%
  arrange(PatientDurableKey,Year) %>%
  head(n= 1000) %>%
  collect() %>% View()

open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh03b"),partitioning = c("Year")) %>%
  to_duckdb() %>% 
  dplyr::filter(str_detect(PharmaceuticalClass,"ANTIHYPERGLY")) %>% 
  distinct(PharmaceuticalClass,SimpleGenericName) %>% 
  collect() %>% 
  write_csv("cohort/deccoh03b_distinct ANTIHYPERGLY PharmaceuticalClass.csv")


open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh03b"),partitioning = c("Year")) %>%

  to_duckdb() %>% 
  dplyr::filter(str_detect(PharmaceuticalClass,"INSULIN")) %>% 
  distinct(PharmaceuticalClass,SimpleGenericName) %>% 
  collect() %>% 
  write_csv("cohort/deccoh03b_distinct INSULIN PharmaceuticalClass.csv")


open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh03b"),partitioning = c("Year")) %>%
  to_duckdb() %>% 
  dplyr::filter(str_detect(PharmaceuticalClass,"ANTIHYPERTEN")) %>% 
  distinct(PharmaceuticalClass,SimpleGenericName) %>% 
  collect() %>% 
  write_csv("cohort/deccoh03b_distinct ANTIHYPERTEN PharmaceuticalClass.csv")

