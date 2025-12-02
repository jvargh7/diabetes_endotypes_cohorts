rm(list=ls());gc();source(".Rprofile")

source("H:/code/functions/cosmos/diagnosis_distinct_Range_index.R")
source("H:/code/functions/cosmos/diagnosis_distinct_Year_index.R")


t = Sys.time()
for(year in c(2010:2024)){
  print(year)
  print("Lookback")
  diagnosis_distinct_Range_index(con_Cosmos,project_string = "PROJECTS.ProjectD0C076.[ET4003\\shdw_1208_jvargh1].decdat05",IndexDateKey_Var = "criterion1_datekey",
                                LowerInterval = -3,UpperInterval = 0,filter_year = year,
                                by_letters = TRUE,alphabets=c("E","F","G","I","J","N"),
                                detection_string = "SUBSTRING(dtd.Value,1,1)") %>% 
    write_dataset(.,path=paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh14a"),partitioning = c("Year","Value_Grouper2"))
  
  print("Followup")
  diagnosis_distinct_Range_index(con_Cosmos,project_string = "PROJECTS.ProjectD0C076.[ET4003\\shdw_1208_jvargh1].decdat05",IndexDateKey_Var = "criterion1_datekey",
                                LowerInterval = 0,UpperInterval = 10,filter_year = year,
                                by_letters = TRUE,alphabets=c("E","G","I","J","N"),
                                detection_string = "SUBSTRING(dtd.Value,1,1)") %>% 
    write_dataset(.,path=paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh14b"),partitioning = c("Year","Value_Grouper2"))
}
t - Sys.time()

open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh14b"),partitioning = c("Year","Value_Grouper2")) %>%
  arrange(PatientDurableKey,Year) %>%
  head(n= 1000) %>%
  collect() %>% View()

