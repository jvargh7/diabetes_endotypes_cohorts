rm(list=ls());gc();source(".Rprofile")

source("H:/code/functions/cosmos/medications_providers_distinct_Range_index.R")


t = Sys.time()
for(year in c(2010:2024)){
  print(year)
  print("Lookback ANTIHYPERGLYCEMICS")
  medications_providers_distinct_Range_index(con_Cosmos,project_string = "PROJECTS.ProjectD0C076.[ET4003\\shdw_1208_jvargh1].decdat03",IndexDateKey_Var = "diagnosis_datekey",
                                             LowerInterval = -2,UpperInterval = 5,filter_year = year,therapeutic_class = "ANTIHYPERGLYCEMICS",end_date = FALSE) %>% 
    write_dataset(.,path=paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh10"),partitioning = c("Year","TherapeuticClass","PharmaceuticalClass"))
  
  print("Lookback ANTI-OBESITY DRUGS")
  
  medications_providers_distinct_Range_index(con_Cosmos,project_string = "PROJECTS.ProjectD0C076.[ET4003\\shdw_1208_jvargh1].decdat03",IndexDateKey_Var = "diagnosis_datekey",
                                             LowerInterval = -2,UpperInterval = 5,filter_year = year,therapeutic_class = "ANTI-OBESITY DRUGS") %>% 
    write_dataset(.,path=paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh10"),partitioning = c("Year","TherapeuticClass","PharmaceuticalClass"))
  
  print("Lookback PSYCHOTHERAPEUTIC DRUGS")
  medications_providers_distinct_Range_index(con_Cosmos,project_string = "PROJECTS.ProjectD0C076.[ET4003\\shdw_1208_jvargh1].decdat03",IndexDateKey_Var = "diagnosis_datekey",
                                             LowerInterval = -2,UpperInterval = 5,filter_year = year,therapeutic_class = "PSYCHOTHERAPEUTIC DRUGS") %>% 
    write_dataset(.,path=paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh10"),partitioning = c("Year","TherapeuticClass","PharmaceuticalClass"))
  
  
  print("Lookback CARDIOVASCULAR")
  medications_providers_distinct_Range_index(con_Cosmos,project_string = "PROJECTS.ProjectD0C076.[ET4003\\shdw_1208_jvargh1].decdat03",IndexDateKey_Var = "diagnosis_datekey",
                                             LowerInterval = -2,UpperInterval = 5,filter_year = year,therapeutic_class = "CARDIOVASCULAR",end_date = FALSE) %>% 
    write_dataset(.,path=paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh10"),partitioning = c("Year","TherapeuticClass","PharmaceuticalClass"))
  
  print("Lookback DIURETICS")
  medications_providers_distinct_Range_index(con_Cosmos,project_string = "PROJECTS.ProjectD0C076.[ET4003\\shdw_1208_jvargh1].decdat03",IndexDateKey_Var = "diagnosis_datekey",
                                             LowerInterval = -2,UpperInterval = 5,filter_year = year,therapeutic_class = "DIURETICS",end_date = FALSE) %>% 
    write_dataset(.,path=paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh10"),partitioning = c("Year","TherapeuticClass","PharmaceuticalClass"))
  
  print("Lookback CARDIAC")
  medications_providers_distinct_Range_index(con_Cosmos,project_string = "PROJECTS.ProjectD0C076.[ET4003\\shdw_1208_jvargh1].decdat03",IndexDateKey_Var = "diagnosis_datekey",
                                             LowerInterval = -2,UpperInterval = 5,filter_year = year,therapeutic_class = "CARDIAC DRUGS",end_date = FALSE) %>% 
    write_dataset(.,path=paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh10"),partitioning = c("Year","TherapeuticClass","PharmaceuticalClass"))
  
  print("Lookback IMMUNOSUPPRESSANTS")
  medications_providers_distinct_Range_index(con_Cosmos,project_string = "PROJECTS.ProjectD0C076.[ET4003\\shdw_1208_jvargh1].decdat03",IndexDateKey_Var = "diagnosis_datekey",
                                             LowerInterval = -2,UpperInterval = 5,filter_year = year,therapeutic_class = "IMMUNOSUPPRESSANTS",end_date = FALSE) %>% 
    write_dataset(.,path=paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh10"),partitioning = c("Year","TherapeuticClass","PharmaceuticalClass"))
  
  print("Lookback ANTIARTHRITICS")
  medications_providers_distinct_Range_index(con_Cosmos,project_string = "PROJECTS.ProjectD0C076.[ET4003\\shdw_1208_jvargh1].decdat03",IndexDateKey_Var = "diagnosis_datekey",
                                             LowerInterval = -2,UpperInterval = 5,filter_year = year,therapeutic_class = "ANTIARTHRITICS",end_date = FALSE) %>% 
    write_dataset(.,path=paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh10"),partitioning = c("Year","TherapeuticClass","PharmaceuticalClass"))
  
}
t - Sys.time()

# open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh10a"),partitioning = c("Year")) %>%
#   dplyr::filter(PatientDurableKey == 185170) %>% 
#   collect() %>% View()
# 
open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh10c"),partitioning = c("Year")) %>% 
  group_by(PrimarySpecialty,SecondSpecialty) %>% 
  tally() %>% 
  collect() %>% 
  write_csv(.,"cohort/deccoh10_distinct primary and second specialty.csv")

