rm(list=ls());gc();source(".Rprofile")

source("H:/code/functions/cosmos/procedures_distinct_Year_index.R")


nephropathy_codes = c(80047, 80048, 80050, 80053, 80069, 82565,82043,82570)
retinopathy_codes = c(67028, 67030, 67031, 67036, 67039:67043, 67101, 67105, 67107, 
                      67108, 67110, 67113, 67121, 67141, 67145, 67208, 67210, 67218, 67220, 67221, 
                      67227, 67228, 92002, 92004, 92012, 92014, 92018, 92019, 92134, 
                      92225:92228, 92230, 92235, 92240, 92250, 92260, 99203:99215, 99242:99245)
neuropathy_codes = c(11721,11055, 11056,11057,11719,11720,11721) # removing nail material from six or more nails

t = Sys.time()
for(year in c(2010:2024)){
  
  # print(year)
  # print("Lookback")
  # procedures_distinct_YearMonth_index(con_Cosmos,project_string = "PROJECTS.ProjectD0C076.[ET4003\\shdw_1208_jvargh1].decdat03",IndexDateKey_Var = "diagnosis_datekey",
  #                                     LookBackInterval = 2,FollowUpInterval = 0,filter_year = year,
  #                                     by_codes = FALSE,cpt_codes=NULL) %>% 
  #   write_dataset(.,path=paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh07a"),partitioning = c("Year"))
  # 
  # print("Followup")
  # procedures_distinct_YearMonth_index(con_Cosmos,project_string = "PROJECTS.ProjectD0C076.[ET4003\\shdw_1208_jvargh1].decdat03",IndexDateKey_Var = "diagnosis_datekey",
  #                                     LookBackInterval = 0,FollowUpInterval = 6,filter_year = year,
  #                                     by_codes = FALSE,cpt_codes=NULL) %>% 
  #   write_dataset(.,path=paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh07b"),partitioning = c("Year"))
  
  
  print(year)
  print("Lookback")
  procedures_distinct_YearMonth_index(con_Cosmos,project_string = "PROJECTS.ProjectD0C076.[ET4003\\shdw_1208_jvargh1].decdat03",IndexDateKey_Var = "diagnosis_datekey",
                                LookBackInterval = 2,FollowUpInterval = 0,filter_year = year,
                                by_codes = TRUE,cpt_codes=c(nephropathy_codes,retinopathy_codes,neuropathy_codes),
                                detection_string = "procd.CptCode") %>% 
    write_dataset(.,path=paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh07aa"),partitioning = c("Year"))
  
  print("Followup")
  procedures_distinct_YearMonth_index(con_Cosmos,project_string = "PROJECTS.ProjectD0C076.[ET4003\\shdw_1208_jvargh1].decdat03",IndexDateKey_Var = "diagnosis_datekey",
                                     LookBackInterval = 0,FollowUpInterval = 6,filter_year = year,
                                 by_codes = TRUE,cpt_codes=c(nephropathy_codes,retinopathy_codes,neuropathy_codes),
                                 detection_string = "procd.CptCode") %>% 
    write_dataset(.,path=paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh07ba"),partitioning = c("Year"))
}
t - Sys.time()

open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh07b"),partitioning = c("Year")) %>%
  arrange(PatientDurableKey,Year) %>%
  head(n= 1000) %>%
  collect() %>% View()

