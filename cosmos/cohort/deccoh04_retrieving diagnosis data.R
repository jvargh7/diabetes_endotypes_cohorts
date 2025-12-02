rm(list=ls());gc();source(".Rprofile")

source("H:/code/functions/cosmos/diagnosis_distinct_Range_index.R")
source("H:/code/functions/cosmos/project_string_looper.R")


t = Sys.time()
for(year in c(2010:2024)){
  print(year)
  print("Lookback")
  diagnosis_distinct_Range_index(con_Cosmos,project_string = "PROJECTS.ProjectD0C076.[ET4003\\shdw_1208_jvargh1].decdat03",IndexDateKey_Var = "diagnosis_datekey",
                                 LowerInterval = -3,UpperInterval = 0,filter_year = year,
                                 # "E","F","G","I","J",
                                by_letters = TRUE,alphabets=c("E","F","G","I","J"),
                                detection_string = "SUBSTRING(dtd.Value,1,1)") %>%
    write_dataset(.,path=paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh04a"),partitioning = c("Year","Value_Grouper2"))

  print("Followup")
  diagnosis_distinct_Range_index(con_Cosmos,project_string = "PROJECTS.ProjectD0C076.[ET4003\\shdw_1208_jvargh1].decdat03",IndexDateKey_Var = "diagnosis_datekey",
                                LowerInterval = 0,UpperInterval = 10,filter_year = year,
                                #
                                by_letters = TRUE,alphabets=c("E","F","G","I","J"),
                                detection_string = "SUBSTRING(dtd.Value,1,1)") %>%
    write_dataset(.,path=paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh04b"),partitioning = c("Year","Value_Grouper2"))
  # 
  # 
  }
t - Sys.time()

# open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh04b"),partitioning = c("Year","Value_Grouper2")) %>%
#   arrange(PatientDurableKey,Year) %>%
#   head(n= 1000) %>%
#   collect() %>% View()

# for(year in c(2010:2024)){
#   print(year)
#   print("Lookback")
#   diagnosis_distinct_Range_index(con_Cosmos,project_string = "PROJECTS.ProjectD0C076.[ET4003\\shdw_1208_jvargh1].decdat03",IndexDateKey_Var = "diagnosis_datekey",
#                                  LowerInterval = -3,UpperInterval = 0,filter_year = year,
#                                  by_letters = TRUE,alphabets=c("C15","C23","C64","C65","C73","C90","K35","J30"),
#                                  detection_string = "SUBSTRING(dtd_internal.Value,1,3)") %>% 
#     write_dataset(.,path=paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh04ab"),partitioning = c("Year"))
#   
#   print("Followup")
#   diagnosis_distinct_Range_index(con_Cosmos,project_string = "PROJECTS.ProjectD0C076.[ET4003\\shdw_1208_jvargh1].decdat03",IndexDateKey_Var = "diagnosis_datekey",
#                                  LowerInterval = 0,UpperInterval = 10,filter_year = year,
#                                  by_letters = TRUE,alphabets=c("C15","C23","C64","C65","C73","C90","K35","J30"),
#                                  detection_string = "SUBSTRING(dtd_internal.Value,1,3)") %>% 
#     write_dataset(.,path=paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh04bb"),partitioning = c("Year"))
#   
# }


min = 0
max = read_parquet(paste0(path_diabetes_endotypes_cosmos_folder,"/working/decdat03/eligible patients.parquet")) %>% 
  nrow()
skip = 50000

i = min
iter = 1

################################
iter = 10
i = skip*(iter-1)
#############################

while(i < max){

  cat("Iteration = ",iter,", Row skip = ",i)
  ps = project_string_looper("PROJECTS.ProjectD0C076.[ET4003\\shdw_1208_jvargh1].decdat03",i,skip)
  
  if(!dir.exists(paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh04ab/Iteration=",iter))){
    dir.create(paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh04ab/Iteration=",iter))
  }
  
  if(!dir.exists(paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh04bb/Iteration=",iter))){
    dir.create(paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh04bb/Iteration=",iter))
  }
  
  for(year in c(2010:2024)){
    # alphabets = c("C15","C23","C64","C65","C73","C90","K35","J30")
    # detection_string = "SUBSTRING(dtd_internal.Value,1,3)"
    print(year)
    print("Lookback")
    diagnosis_distinct_Range_index(con_Cosmos,project_string = ps,IndexDateKey_Var = "diagnosis_datekey",
                                   LowerInterval = -3,UpperInterval = 0,filter_year = year,
                                   by_letters = TRUE,alphabets=c("C","D","H","K","N"),
                                   detection_string = "SUBSTRING(dtd_internal.Value,1,1)") %>% 
      write_dataset(.,path=paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh04ab/Iteration=",iter),partitioning = c("Year","Value_Grouper2"))
    
    print("Followup")
    diagnosis_distinct_Range_index(con_Cosmos,project_string = ps,IndexDateKey_Var = "diagnosis_datekey",
                                   LowerInterval = 0,UpperInterval = 10,filter_year = year,
                                   by_letters = TRUE,alphabets=c("C","D","H","K","N"),
                                   detection_string = "SUBSTRING(dtd_internal.Value,1,1)") %>% 
      write_dataset(.,path=paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh04bb/Iteration=",iter),partitioning = c("Year","Value_Grouper2"))
    
  }
  
  i = i + skip
  iter = iter + 1
  
  
  
}





