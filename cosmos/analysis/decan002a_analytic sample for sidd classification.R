
rm(list=ls());gc();source(".Rprofile")

predictors_before <- read_parquet(paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan001/predictors 1y before diagnosis date.parquet"))
predictors_after <- read_parquet(paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan001/predictors 1y after diagnosis date.parquet"))
insurance_before <- read_parquet(paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan001/insurance before diagnosis date.parquet"))



phenotyping_df <- predictors_after %>% 
  mutate(ref_period = 1) %>% 
  rename_all(~str_replace(.,"_after","")) %>% 
  left_join(predictors_before %>% 
              dplyr::select(PatientDurableKey,contains("before")),
            by= "PatientDurableKey") %>% 
  mutate(Ratio_th = case_when(!is.na(Hdlc) ~ Tgl/Hdlc,
                              TRUE ~ NA_real_),
         Ratio_th_before = case_when(!is.na(Hdlc_before) ~ Tgl_before/Hdlc_before,
                                     TRUE ~ NA_real_)) %>% 
  rename(Dmagediag = age) %>% 
  dplyr::filter(!ValidatedStateOrProvince_X %in% c("*Masked","*Unspecified","Northern Mariana Islands",
                                                   "Virgin Islands","American Samoa, South Pacific",
                                                   "Guam","Puerto Rico")) %>% 
  
  mutate(region = case_when(PrimaryRUCA_X %in% as.character(c(1:6)) ~ "Urban",
                            PrimaryRUCA_X %in% as.character(c(7:10)) ~ "Rural",
                            TRUE ~ "Urban")) %>% 
  left_join(insurance_before %>% 
              dplyr::select(-YearMonth),
            by = c("PatientDurableKey")) %>% 
  mutate(across(starts_with("insurance_"),.fns=function(x) case_when(is.na(x) ~ 0,TRUE ~ x)))
  


svi <- open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/decdat02"),format="parquet",partitioning = "ValidatedStateOrProvince_X") %>% 
  # dplyr::filter(PatientDurableKey %in% sidd_df$PatientDurableKey) %>% 
  dplyr::select(PatientDurableKey,SviOverallPctlRankByZip2020_X) %>% 
  collect()

phenotyping_df %>% 
  left_join(svi,
            by="PatientDurableKey") %>% 
  write_parquet(.,paste0(path_diabetes_endotypes_cosmos_folder,"/working/phenotyping dataset.parquet"))

