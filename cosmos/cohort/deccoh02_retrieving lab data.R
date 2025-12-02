rm(list=ls());gc();source(".Rprofile")

source("H:/code/functions/cosmos/labs_index.R")

t = Sys.time()
for(year in c(2010:2024)){
  print(year)
  print("Lookback hba1c")
  labs_YearMonth_index(con_Cosmos,project_string = "PROJECTS.ProjectD0C076.[ET4003\\shdw_1208_jvargh1].decdat03",type = "hba1c",
                       IndexDateKey_Var = "diagnosis_datekey",
                       LookBackInterval = 1,FollowUpInterval = 0,filter_year = year) %>%
    write_dataset(.,path=paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh02aa"),partitioning = c("Year"))
  print("Followup hba1c")
  labs_YearMonth_index(con_Cosmos,project_string = "PROJECTS.ProjectD0C076.[ET4003\\shdw_1208_jvargh1].decdat03",type = "hba1c",
                       IndexDateKey_Var = "diagnosis_datekey",
                       LookBackInterval = 0,FollowUpInterval = 6,filter_year = year) %>%
    write_dataset(.,path=paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh02ab"),partitioning = c("Year"))

  # print("Lookback fpg")
  # labs_YearMonth_index(con_Cosmos,project_string = "PROJECTS.ProjectD0C076.[ET4003\\shdw_1208_jvargh1].decdat03",type = "fpg",
  #                      IndexDateKey_Var = "diagnosis_datekey",
  #                      LookBackInterval = 1,FollowUpInterval = 0,filter_year = year) %>%
  #   write_dataset(.,path=paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh02ba"),partitioning = c("Year"))
  # print("Followup fpg")
  # labs_YearMonth_index(con_Cosmos,project_string = "PROJECTS.ProjectD0C076.[ET4003\\shdw_1208_jvargh1].decdat03",type = "fpg",
  #                      IndexDateKey_Var = "diagnosis_datekey",
  #                      LookBackInterval = 0,FollowUpInterval = 6,filter_year = year) %>%
  #   write_dataset(.,path=paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh02bb"),partitioning = c("Year"))
  # 
  # 
  # print("Lookback ldl")
  # labs_YearMonth_index(con_Cosmos,project_string = "PROJECTS.ProjectD0C076.[ET4003\\shdw_1208_jvargh1].decdat03",type = "ldl",
  #                      IndexDateKey_Var = "diagnosis_datekey",
  #                      LookBackInterval = 1,FollowUpInterval = 0,filter_year = year) %>%
  #   write_dataset(.,path=paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh02da"),partitioning = c("Year"))
  # print("Followup ldl")
  # labs_YearMonth_index(con_Cosmos,project_string = "PROJECTS.ProjectD0C076.[ET4003\\shdw_1208_jvargh1].decdat03",type = "ldl",
  #                      IndexDateKey_Var = "diagnosis_datekey",
  #                      LookBackInterval = 0,FollowUpInterval = 6,filter_year = year) %>%
  #   write_dataset(.,path=paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh02db"),partitioning = c("Year"))
  # 
  # 
  # print("Lookback hdl")
  # labs_YearMonth_index(con_Cosmos,project_string = "PROJECTS.ProjectD0C076.[ET4003\\shdw_1208_jvargh1].decdat03",type = "hdl",
  #                      IndexDateKey_Var = "diagnosis_datekey",
  #                      LookBackInterval = 1,FollowUpInterval = 0,filter_year = year) %>%
  #   write_dataset(.,path=paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh02ea"),partitioning = c("Year"))
  # print("Followup hdl")
  # labs_YearMonth_index(con_Cosmos,project_string = "PROJECTS.ProjectD0C076.[ET4003\\shdw_1208_jvargh1].decdat03",type = "hdl",
  #                      IndexDateKey_Var = "diagnosis_datekey",
  #                      LookBackInterval = 0,FollowUpInterval = 6,filter_year = year) %>%
  #   write_dataset(.,path=paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh02eb"),partitioning = c("Year"))
  # 
  # 
  # print("Lookback tgl")
  # labs_YearMonth_index(con_Cosmos,project_string = "PROJECTS.ProjectD0C076.[ET4003\\shdw_1208_jvargh1].decdat03",type = "tgl",
  #                      IndexDateKey_Var = "diagnosis_datekey",
  #                      LookBackInterval = 1,FollowUpInterval = 0,filter_year = year) %>%
  #   write_dataset(.,path=paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh02fa"),partitioning = c("Year"))
  # print("Followup tgl")
  # labs_YearMonth_index(con_Cosmos,project_string = "PROJECTS.ProjectD0C076.[ET4003\\shdw_1208_jvargh1].decdat03",type = "tgl",
  #                      IndexDateKey_Var = "diagnosis_datekey",
  #                      LookBackInterval = 0,FollowUpInterval = 6,filter_year = year) %>%
  #   write_dataset(.,path=paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh02fb"),partitioning = c("Year"))
  # 
  # print("Lookback alt")
  # labs_YearMonth_index(con_Cosmos,project_string = "PROJECTS.ProjectD0C076.[ET4003\\shdw_1208_jvargh1].decdat03",type = "alt",
  #                      IndexDateKey_Var = "diagnosis_datekey",
  #                      LookBackInterval = 1,FollowUpInterval = 0,filter_year = year) %>%
  #   write_dataset(.,path=paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh02ga"),partitioning = c("Year"))
  # print("Followup alt")
  # labs_YearMonth_index(con_Cosmos,project_string = "PROJECTS.ProjectD0C076.[ET4003\\shdw_1208_jvargh1].decdat03",type = "alt",
  #                      IndexDateKey_Var = "diagnosis_datekey",
  #                      LookBackInterval = 0,FollowUpInterval = 6,filter_year = year) %>%
  #   write_dataset(.,path=paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh02gb"),partitioning = c("Year"))
  # 
  # print("Lookback ast")
  # labs_YearMonth_index(con_Cosmos,project_string = "PROJECTS.ProjectD0C076.[ET4003\\shdw_1208_jvargh1].decdat03",type = "ast",
  #                      IndexDateKey_Var = "diagnosis_datekey",
  #                      LookBackInterval = 1,FollowUpInterval = 0,filter_year = year) %>%
  #   write_dataset(.,path=paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh02ha"),partitioning = c("Year"))
  # print("Followup ast")
  # labs_YearMonth_index(con_Cosmos,project_string = "PROJECTS.ProjectD0C076.[ET4003\\shdw_1208_jvargh1].decdat03",type = "ast",
  #                      IndexDateKey_Var = "diagnosis_datekey",
  #                      LookBackInterval = 0,FollowUpInterval = 6,filter_year = year) %>%
  #   write_dataset(.,path=paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh02hb"),partitioning = c("Year"))
  # 
  # print("Lookback creatinine")
  # labs_YearMonth_index(con_Cosmos,project_string = "PROJECTS.ProjectD0C076.[ET4003\\shdw_1208_jvargh1].decdat03",type = "creatinine",
  #                      IndexDateKey_Var = "diagnosis_datekey",
  #                      LookBackInterval = 1,FollowUpInterval = 0,filter_year = year) %>%
  #   write_dataset(.,path=paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh02ia"),partitioning = c("Year"))
  # print("Followup creatinine")
  # labs_YearMonth_index(con_Cosmos,project_string = "PROJECTS.ProjectD0C076.[ET4003\\shdw_1208_jvargh1].decdat03",type = "creatinine",
  #                      IndexDateKey_Var = "diagnosis_datekey",
  #                      LookBackInterval = 0,FollowUpInterval = 6,filter_year = year) %>%
  #   write_dataset(.,path=paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh02ib"),partitioning = c("Year"))
  # 
  # print("Lookback fins")
  # labs_YearMonth_index(con_Cosmos,project_string = "PROJECTS.ProjectD0C076.[ET4003\\shdw_1208_jvargh1].decdat03",type = "fins",
  #                      IndexDateKey_Var = "diagnosis_datekey",
  #                      LookBackInterval = 1,FollowUpInterval = 0,filter_year = year) %>%
  #   write_dataset(.,path=paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh02ja"),partitioning = c("Year"))
  # print("Followup fins")
  # labs_YearMonth_index(con_Cosmos,project_string = "PROJECTS.ProjectD0C076.[ET4003\\shdw_1208_jvargh1].decdat03",type = "fins",
  #                      IndexDateKey_Var = "diagnosis_datekey",
  #                      LookBackInterval = 0,FollowUpInterval = 6,filter_year = year) %>%
  #   write_dataset(.,path=paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh02jb"),partitioning = c("Year"))
  # 
  # print("Lookback urinary_creatinine")
  # labs_YearMonth_index(con_Cosmos,project_string = "PROJECTS.ProjectD0C076.[ET4003\\shdw_1208_jvargh1].decdat03",type = "urinary_creatinine",
  #                      IndexDateKey_Var = "diagnosis_datekey",
  #                      LookBackInterval = 1,FollowUpInterval = 0,filter_year = year) %>%
  #   write_dataset(.,path=paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh02ka"),partitioning = c("Year"))
  # print("Followup urinary_creatinine")
  # labs_YearMonth_index(con_Cosmos,project_string = "PROJECTS.ProjectD0C076.[ET4003\\shdw_1208_jvargh1].decdat03",type = "urinary_creatinine",
  #                      IndexDateKey_Var = "diagnosis_datekey",
  #                      LookBackInterval = 0,FollowUpInterval = 6,filter_year = year) %>%
  #   write_dataset(.,path=paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh02kb"),partitioning = c("Year"))
  # 
  # print("Lookback urinary_albumin")
  # labs_YearMonth_index(con_Cosmos,project_string = "PROJECTS.ProjectD0C076.[ET4003\\shdw_1208_jvargh1].decdat03",type = "urinary_albumin",
  #                      IndexDateKey_Var = "diagnosis_datekey",
  #                      LookBackInterval = 1,FollowUpInterval = 0,filter_year = year) %>%
  #   write_dataset(.,path=paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh02la"),partitioning = c("Year"))
  # print("Followup urine_albumin")
  # labs_YearMonth_index(con_Cosmos,project_string = "PROJECTS.ProjectD0C076.[ET4003\\shdw_1208_jvargh1].decdat03",type = "urinary_albumin",
  #                      IndexDateKey_Var = "diagnosis_datekey",
  #                      LookBackInterval = 0,FollowUpInterval = 6,filter_year = year) %>%
  #   write_dataset(.,path=paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh02lb"),partitioning = c("Year"))
  # 
  # print("Lookback uacr")
  # labs_YearMonth_index(con_Cosmos,project_string = "PROJECTS.ProjectD0C076.[ET4003\\shdw_1208_jvargh1].decdat03",type = "uacr",
  #                      IndexDateKey_Var = "diagnosis_datekey",
  #                      LookBackInterval = 1,FollowUpInterval = 0,filter_year = year) %>%
  #   write_dataset(.,path=paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh02ma"),partitioning = c("Year"))
  # print("Followup uacr")
  # labs_YearMonth_index(con_Cosmos,project_string = "PROJECTS.ProjectD0C076.[ET4003\\shdw_1208_jvargh1].decdat03",type = "uacr",
  #                      IndexDateKey_Var = "diagnosis_datekey",
  #                      LookBackInterval = 0,FollowUpInterval = 6,filter_year = year) %>%
  #   write_dataset(.,path=paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh02mb"),partitioning = c("Year"))
  # 
  # print("Lookback egfr_ckdepi")
  # labs_YearMonth_index(con_Cosmos,project_string = "PROJECTS.ProjectD0C076.[ET4003\\shdw_1208_jvargh1].decdat03",type = "egfr_ckdepi",
  #                      IndexDateKey_Var = "diagnosis_datekey",
  #                      LookBackInterval = 1,FollowUpInterval = 0,filter_year = year) %>%
  #   write_dataset(.,path=paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh02na"),partitioning = c("Year"))
  # print("Followup egfr_ckdepi")
  # labs_YearMonth_index(con_Cosmos,project_string = "PROJECTS.ProjectD0C076.[ET4003\\shdw_1208_jvargh1].decdat03",type = "egfr_ckdepi",
  #                      IndexDateKey_Var = "diagnosis_datekey",
  #                      LookBackInterval = 0,FollowUpInterval = 6,filter_year = year) %>%
  #   write_dataset(.,path=paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh02nb"),partitioning = c("Year"))
  # 
  # print("Lookback egfr_ckdepi2")
  # labs_YearMonth_index(con_Cosmos,project_string = "PROJECTS.ProjectD0C076.[ET4003\\shdw_1208_jvargh1].decdat03",type = "egfr_ckdepi2",
  #                      IndexDateKey_Var = "diagnosis_datekey",
  #                      LookBackInterval = 1,FollowUpInterval = 0,filter_year = year) %>%
  #   write_dataset(.,path=paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh02oa"),partitioning = c("Year"))
  # print("Followup egfr_ckdepi2")
  # labs_YearMonth_index(con_Cosmos,project_string = "PROJECTS.ProjectD0C076.[ET4003\\shdw_1208_jvargh1].decdat03",type = "egfr_ckdepi2",
  #                      IndexDateKey_Var = "diagnosis_datekey",
  #                      LookBackInterval = 0,FollowUpInterval = 6,filter_year = year) %>%
  #   write_dataset(.,path=paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh02ob"),partitioning = c("Year"))
  # 
  

  
}
Sys.time() - t


# open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh02aa"),partitioning = c("Year")) %>%
#   arrange(PatientDurableKey,YearMonth) %>%
#   head(n= 1000) %>%
#   collect() %>% View()

