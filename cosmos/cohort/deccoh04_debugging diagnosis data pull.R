rm(list=ls());gc();source(".Rprofile")

source("H:/code/functions/cosmos/diagnosis_distinct_Range_index_trial.R")


for(year in c(2010:2024)){
  print(year)
  print("Lookback")
  diagnosis_distinct_Range_index_count_trial(con_Cosmos,project_string = "PROJECTS.ProjectD0C076.[ET4003\\shdw_1208_jvargh1].decdat03",IndexDateKey_Var = "diagnosis_datekey",
                                 LowerInterval = -3,UpperInterval = 0,filter_year = year,
                                 by_letters = TRUE,alphabets=c("C15","C23","C64","C65","C73","C90","K35","J30"),
                                 detection_string = "SUBSTRING(dtd_internal.Value,1,3)",count_check = TRUE)
  
  print("Followup")
  diagnosis_distinct_Range_index_count_trial(con_Cosmos,project_string = "PROJECTS.ProjectD0C076.[ET4003\\shdw_1208_jvargh1].decdat03",IndexDateKey_Var = "diagnosis_datekey",
                                 LowerInterval = 0,UpperInterval = 10,filter_year = year,
                                 by_letters = TRUE,alphabets=c("C15","C23","C64","C65","C73","C90","K35","J30"),
                                 detection_string = "SUBSTRING(dtd_internal.Value,1,3)",count_check = TRUE) 
  
}

