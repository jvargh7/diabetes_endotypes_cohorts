

predictors_before <- read_parquet(paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan101/predictors 1y before criterion1 date.parquet"))
predictors_after <- read_parquet(paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan101/predictors 1y after criterion1 date.parquet"))
insurance_before <- read_parquet(paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan101/insurance before criterion1 date.parquet"))



sidd_df <- bind_rows(
  predictors_before %>% 
    mutate(ref_period = -1) %>% 
    rename_all(~str_replace(.,"_before","")),
  predictors_after %>% 
    mutate(ref_period = 1) %>% 
    rename_all(~str_replace(.,"_after",""))
) %>% 
  arrange(PatientDurableKey,ref_period) %>% 
  group_by(PatientDurableKey) %>% 
  fill(.,all_of(c("Hba1c","Tgl","Ldlc","Hdlc",
                  "Sbp","Dbp","Bmi")),contains("YearMonthKey"),.direction="downup") %>% 
  ungroup() %>% 
  dplyr::filter(ref_period == 1) %>% 
  mutate(Ratio_th = case_when(!is.na(Hdlc) ~ Tgl/Hdlc,
                              TRUE ~ NA_real_)) %>% 
  rename(Dmagediag = age) %>% 
  dplyr::filter(!ValidatedStateOrProvince_X %in% c("*Masked","*Unspecified","Northern Mariana Islands",
                                                   "Virgin Islands","American Samoa, South Pacific",
                                                   "Guam")) %>% 
  
  mutate(region = case_when(PrimaryRUCA_X %in% as.character(c(1:6)) ~ "Urban",
                            PrimaryRUCA_X %in% as.character(c(7:10)) ~ "Rural",
                            TRUE ~ "Urban")) %>% 
  left_join(insurance_before %>% 
              dplyr::select(-YearMonth),
            by = c("PatientDurableKey")) %>% 
  mutate(across(starts_with("insurance_"),.fns=function(x) case_when(is.na(x) ~ 0,TRUE ~ x)))



svi <- open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/decdat02"),format="parquet",partitioning = "ValidatedStateOrProvince_X") %>% 
  dplyr::filter(PatientDurableKey %in% sidd_df$PatientDurableKey) %>% 
  dplyr::select(PatientDurableKey,SviOverallPctlRankByZip2020_X) %>% 
  collect()

sidd_df %>% 
  left_join(svi,
            by="PatientDurableKey") %>% 
  write_parquet(.,paste0(path_diabetes_endotypes_cosmos_folder,"/working/sidd classification dataset with criterion1_datekey.parquet"))

