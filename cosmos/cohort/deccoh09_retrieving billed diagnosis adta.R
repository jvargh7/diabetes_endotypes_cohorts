rm(list=ls());gc();source(".Rprofile")

source("H:/code/functions/cosmos/diagnosis_distinct_Range_index.R")
source("H:/code/functions/cosmos/project_string_looper.R")




min = 0
max = read_parquet(paste0(path_diabetes_endotypes_cosmos_folder,"/working/decdat03/eligible patients.parquet")) %>% 
  nrow()
skip = 50000

i = min
iter = 1

################################
iter = 1
i = skip*(iter-1)
#############################

while(i < max){
  
  cat("Iteration = ",iter,", Row skip = ",i)
  ps = project_string_looper("PROJECTS.ProjectD0C076.[ET4003\\shdw_1208_jvargh1].decdat03",i,skip)
  
  if(!dir.exists(paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh09a/Iteration=",iter))){
    dir.create(paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh09a/Iteration=",iter))
  }
  
  if(!dir.exists(paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh09b/Iteration=",iter))){
    dir.create(paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh09b/Iteration=",iter))
  }
  
  for(year in c(2010:2024)){
    # alphabets = c("C15","C23","C64","C65","C73","C90","K35","J30")
    # detection_string = "SUBSTRING(dtd_internal.Value,1,3)"
    print(year)
    print("Lookback")
    diagnosis_distinct_Range_index(con_Cosmos,project_string = ps,IndexDateKey_Var = "diagnosis_datekey",
                                   LowerInterval = -3,UpperInterval = 0,filter_year = year,
                                   # by_letters = TRUE,alphabets=c("E","F","G","I","J",
                                   # "C","D","H","K","L","N", "B","M","P","Z"),
                                   by_letters = TRUE,alphabets=c("O","Q","R","S","T","U","V","W","X","Y"),
                                   detection_string = "SUBSTRING(dtd_internal.Value,1,1)",
                                   diagnosis_type = c("Billing Final Diagnosis", "Billing Admission Diagnosis", 
                                                      "Billing Cause of Injury", "Billing Procedure Linked Diagnosis")) %>% 
      write_dataset(.,path=paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh09a/Iteration=",iter),partitioning = c("Year","Value_Grouper2"))
    
    print("Followup")
    diagnosis_distinct_Range_index(con_Cosmos,project_string = ps,IndexDateKey_Var = "diagnosis_datekey",
                                   LowerInterval = 0,UpperInterval = 10,filter_year = year,
                                   # by_letters = TRUE,alphabets=c("E","F","G","I","J","C","D","H","K","L","N", "B","M","P","Z"),
                                   by_letters = TRUE,alphabets=c("O","Q","R","S","T","U","V","W","X","Y"),
                                   detection_string = "SUBSTRING(dtd_internal.Value,1,1)",
                                   diagnosis_type = c("Billing Final Diagnosis", "Billing Admission Diagnosis", 
                                                      "Billing Cause of Injury", "Billing Procedure Linked Diagnosis")) %>% 
      write_dataset(.,path=paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh09b/Iteration=",iter),partitioning = c("Year","Value_Grouper2"))
    
  }
  
  i = i + skip
  iter = iter + 1
  
  
  
}





