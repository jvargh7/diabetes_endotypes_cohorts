rm(list=ls());gc();source(".Rprofile")

source("H:/code/functions/cosmos/labs_index.R")

t = Sys.time()
for(year in c(2010:2024)){
  print(year)
  print("Lookback hba1c")
  labs_YearMonth_index(con_Cosmos,project_string = "PROJECTS.ProjectD0C076.[ET4003\\shdw_1208_jvargh1].decdat05",type = "hba1c",
                       IndexDateKey_Var = "criterion1_datekey",
                       LookBackInterval = 1,FollowUpInterval = 0,filter_year = year) %>% 
    write_dataset(.,path=paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh12aa"),partitioning = c("Year"))
  print("Followup hba1c")
  labs_YearMonth_index(con_Cosmos,project_string = "PROJECTS.ProjectD0C076.[ET4003\\shdw_1208_jvargh1].decdat05",type = "hba1c",
                       IndexDateKey_Var = "criterion1_datekey",
                       LookBackInterval = 0,FollowUpInterval = 2,filter_year = year) %>% 
    write_dataset(.,path=paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh12ab"),partitioning = c("Year"))
  
  print("Lookback fpg")
  labs_YearMonth_index(con_Cosmos,project_string = "PROJECTS.ProjectD0C076.[ET4003\\shdw_1208_jvargh1].decdat05",type = "fpg",
                       IndexDateKey_Var = "criterion1_datekey",
                       LookBackInterval = 1,FollowUpInterval = 0,filter_year = year) %>% 
    write_dataset(.,path=paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh12ba"),partitioning = c("Year"))
  print("Followup fpg")
  labs_YearMonth_index(con_Cosmos,project_string = "PROJECTS.ProjectD0C076.[ET4003\\shdw_1208_jvargh1].decdat05",type = "fpg",
                       IndexDateKey_Var = "criterion1_datekey",
                       LookBackInterval = 0,FollowUpInterval = 2,filter_year = year) %>% 
    write_dataset(.,path=paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh12bb"),partitioning = c("Year"))
  
  
  print("Lookback ldl")
  labs_YearMonth_index(con_Cosmos,project_string = "PROJECTS.ProjectD0C076.[ET4003\\shdw_1208_jvargh1].decdat05",type = "ldl",
                       IndexDateKey_Var = "criterion1_datekey",
                       LookBackInterval = 1,FollowUpInterval = 0,filter_year = year) %>% 
    write_dataset(.,path=paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh12da"),partitioning = c("Year"))
  print("Followup ldl")
  labs_YearMonth_index(con_Cosmos,project_string = "PROJECTS.ProjectD0C076.[ET4003\\shdw_1208_jvargh1].decdat05",type = "ldl",
                       IndexDateKey_Var = "criterion1_datekey",
                       LookBackInterval = 0,FollowUpInterval = 2,filter_year = year) %>% 
    write_dataset(.,path=paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh12db"),partitioning = c("Year"))
  
  
  print("Lookback hdl")
  labs_YearMonth_index(con_Cosmos,project_string = "PROJECTS.ProjectD0C076.[ET4003\\shdw_1208_jvargh1].decdat05",type = "hdl",
                       IndexDateKey_Var = "criterion1_datekey",
                       LookBackInterval = 1,FollowUpInterval = 0,filter_year = year) %>% 
    write_dataset(.,path=paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh12ea"),partitioning = c("Year"))
  print("Followup hdl")
  labs_YearMonth_index(con_Cosmos,project_string = "PROJECTS.ProjectD0C076.[ET4003\\shdw_1208_jvargh1].decdat05",type = "hdl",
                       IndexDateKey_Var = "criterion1_datekey",
                       LookBackInterval = 0,FollowUpInterval = 2,filter_year = year) %>% 
    write_dataset(.,path=paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh12eb"),partitioning = c("Year"))
  
  
  print("Lookback tgl")
  labs_YearMonth_index(con_Cosmos,project_string = "PROJECTS.ProjectD0C076.[ET4003\\shdw_1208_jvargh1].decdat05",type = "tgl",
                       IndexDateKey_Var = "criterion1_datekey",
                       LookBackInterval = 1,FollowUpInterval = 0,filter_year = year) %>% 
    write_dataset(.,path=paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh12fa"),partitioning = c("Year"))
  print("Followup tgl")
  labs_YearMonth_index(con_Cosmos,project_string = "PROJECTS.ProjectD0C076.[ET4003\\shdw_1208_jvargh1].decdat05",type = "tgl",
                       IndexDateKey_Var = "criterion1_datekey",
                       LookBackInterval = 0,FollowUpInterval = 2,filter_year = year) %>% 
    write_dataset(.,path=paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh12fb"),partitioning = c("Year"))
  
  print("Lookback alt")
  labs_YearMonth_index(con_Cosmos,project_string = "PROJECTS.ProjectD0C076.[ET4003\\shdw_1208_jvargh1].decdat05",type = "alt",
                       IndexDateKey_Var = "criterion1_datekey",
                       LookBackInterval = 1,FollowUpInterval = 0,filter_year = year) %>% 
    write_dataset(.,path=paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh12ga"),partitioning = c("Year"))
  print("Followup alt")
  labs_YearMonth_index(con_Cosmos,project_string = "PROJECTS.ProjectD0C076.[ET4003\\shdw_1208_jvargh1].decdat05",type = "alt",
                       IndexDateKey_Var = "criterion1_datekey",
                       LookBackInterval = 0,FollowUpInterval = 2,filter_year = year) %>% 
    write_dataset(.,path=paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh12gb"),partitioning = c("Year"))
  
  print("Lookback ast")
  labs_YearMonth_index(con_Cosmos,project_string = "PROJECTS.ProjectD0C076.[ET4003\\shdw_1208_jvargh1].decdat05",type = "ast",
                       IndexDateKey_Var = "criterion1_datekey",
                       LookBackInterval = 1,FollowUpInterval = 0,filter_year = year) %>% 
    write_dataset(.,path=paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh12ha"),partitioning = c("Year"))
  print("Followup ast")
  labs_YearMonth_index(con_Cosmos,project_string = "PROJECTS.ProjectD0C076.[ET4003\\shdw_1208_jvargh1].decdat05",type = "ast",
                       IndexDateKey_Var = "criterion1_datekey",
                       LookBackInterval = 0,FollowUpInterval = 2,filter_year = year) %>% 
    write_dataset(.,path=paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh12hb"),partitioning = c("Year"))
  
  print("Lookback creatinine")
  labs_YearMonth_index(con_Cosmos,project_string = "PROJECTS.ProjectD0C076.[ET4003\\shdw_1208_jvargh1].decdat05",type = "creatinine",
                       IndexDateKey_Var = "criterion1_datekey",
                       LookBackInterval = 1,FollowUpInterval = 0,filter_year = year) %>% 
    write_dataset(.,path=paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh12ia"),partitioning = c("Year"))
  print("Followup creatinine")
  labs_YearMonth_index(con_Cosmos,project_string = "PROJECTS.ProjectD0C076.[ET4003\\shdw_1208_jvargh1].decdat05",type = "creatinine",
                       IndexDateKey_Var = "criterion1_datekey",
                       LookBackInterval = 0,FollowUpInterval = 2,filter_year = year) %>% 
    write_dataset(.,path=paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh12ib"),partitioning = c("Year"))
  
}
Sys.time() - t


# open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh12aa"),partitioning = c("Year")) %>%
#   arrange(PatientDurableKey,YearMonth) %>%
#   head(n= 1000) %>%
#   collect() %>% View()

