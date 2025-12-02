rm(list=ls());gc();source(".Rprofile")


decdat05 <- open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/decdat05/eligible patients with criterion1_datekey.parquet")) %>% 
  dplyr::select(PatientDurableKey,criterion1_datekey,diagnosis_datekey,criterion1_date,criterion2_date,diagnosis_date,ValidatedStateOrProvince_X,
                PrimaryRUCA_X,raceeth,female,age) %>% 
  mutate(criterion1_datekey = as.numeric(criterion1_datekey)) %>% 
  mutate(criterion1_datekey_plus1y = criterion1_datekey + 10000,
         criterion1_datekey_minus1y = criterion1_datekey - 10000)

decdat05 %>% dim()


# BEFORE EXPOSURE -------------

## deccoh11a. BMI -----------
bmi_before = open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh11a"),partitioning = c("Year")) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         YearMonthKey = paste0(YearMonth,"01")) %>% 
  mutate(YearMonthKey = as.numeric(YearMonthKey)) %>% 
  left_join(decdat05 %>% 
              dplyr::select(PatientDurableKey,criterion1_datekey,criterion1_datekey_minus1y),
            by = c("PatientDurableKey")) %>% 
  mutate(
    BodyMassIndex = case_when(BodyMassIndex >50 | BodyMassIndex <12 ~ NA_real_,
                              TRUE ~ BodyMassIndex)) %>% 
  to_duckdb()  %>%
  mutate(period = case_when(YearMonthKey >= criterion1_datekey_minus1y & YearMonthKey < criterion1_datekey ~ "Ym1",
                            TRUE ~ "Ym2 or earlier")) %>% 
  dplyr::filter(period %in% c("Ym1"),!is.na(BodyMassIndex))  %>% 
  group_by(PatientDurableKey) %>% 
  dplyr::filter(YearMonthKey == max(YearMonthKey)) %>% 
  summarize(Bmi_before = mean(BodyMassIndex),
            YearMonthKey_Bmi_before = min(YearMonthKey)) %>%   
  ungroup()  %>% 
  collect() 

## deccoh12aa. HbA1c -----------

hba1c_before <- open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh12aa"),partitioning = c("Year")) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         YearMonthKey = paste0(YearMonth,"01")) %>% 
  mutate(YearMonthKey = as.numeric(YearMonthKey)) %>% 
  left_join(decdat05 %>% 
              dplyr::select(PatientDurableKey,criterion1_datekey,criterion1_datekey_minus1y),
            by = c("PatientDurableKey")) %>%
  mutate(NumericValue = as.numeric(NumericValue)) %>% 
  mutate(hba1c = case_when(NumericValue >= 3 & NumericValue <= 20 ~ NumericValue,
                           TRUE ~ NA_real_)) %>% 
  to_duckdb()  %>%
  mutate(period = case_when(YearMonthKey >= criterion1_datekey_minus1y & YearMonthKey < criterion1_datekey ~ "Ym1",
                            TRUE ~ "Ym2 or earlier")) %>% 
  dplyr::filter(period %in% c("Ym1"),!is.na(hba1c))  %>% 
  group_by(PatientDurableKey) %>% 
  dplyr::filter(YearMonthKey == max(YearMonthKey)) %>% 
  summarize(Hba1c_before = mean(hba1c),
            YearMonthKey_Hba1c_before = min(YearMonthKey)) %>%   
  ungroup()  %>% 
  collect() 

## deccoh12fa. Tgl -----------

tgl_before <- open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh12fa"),partitioning = c("Year")) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         YearMonthKey = paste0(YearMonth,"01")) %>% 
  mutate(YearMonthKey = as.numeric(YearMonthKey)) %>% 
  left_join(decdat05 %>% 
              dplyr::select(PatientDurableKey,criterion1_datekey,criterion1_datekey_minus1y),
            by = c("PatientDurableKey")) %>%
  mutate(NumericValue = as.numeric(NumericValue)) %>% 
  mutate(tgl = case_when(NumericValue >= 10 & NumericValue <= 600 ~ NumericValue,
                         TRUE ~ NA_real_)) %>% 
  to_duckdb()  %>%
  mutate(period = case_when(YearMonthKey >= criterion1_datekey_minus1y & YearMonthKey < criterion1_datekey ~ "Ym1",
                            TRUE ~ "Ym2 or earlier")) %>% 
  dplyr::filter(period %in% c("Ym1"),!is.na(tgl))  %>% 
  group_by(PatientDurableKey) %>% 
  dplyr::filter(YearMonthKey == max(YearMonthKey)) %>% 
  summarize(Tgl_before = mean(tgl),
            YearMonthKey_Tgl_before = min(YearMonthKey)) %>%   
  ungroup()  %>% 
  collect() 

## deccoh12fa. Ldlc -----------

ldl_before <- open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh12da"),partitioning = c("Year")) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         YearMonthKey = paste0(YearMonth,"01")) %>% 
  mutate(YearMonthKey = as.numeric(YearMonthKey)) %>% 
  left_join(decdat05 %>% 
              dplyr::select(PatientDurableKey,criterion1_datekey,criterion1_datekey_minus1y),
            by = c("PatientDurableKey")) %>%
  mutate(NumericValue = as.numeric(NumericValue)) %>% 
  mutate(ldl = case_when(NumericValue >= 10 & NumericValue <= 600 ~ NumericValue,
                         TRUE ~ NA_real_)) %>% 
  to_duckdb()  %>%
  mutate(period = case_when(YearMonthKey >= criterion1_datekey_minus1y & YearMonthKey < criterion1_datekey ~ "Ym1",
                            TRUE ~ "Ym2 or earlier")) %>% 
  dplyr::filter(period %in% c("Ym1"),!is.na(ldl))  %>% 
  group_by(PatientDurableKey) %>% 
  dplyr::filter(YearMonthKey == max(YearMonthKey)) %>% 
  summarize(Ldlc_before = mean(ldl),
            YearMonthKey_Ldlc_before = min(YearMonthKey)) %>%   
  ungroup()  %>% 
  collect() 

## deccoh12fa. Hdlc -----------

hdl_before <- open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh12ea"),partitioning = c("Year")) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         YearMonthKey = paste0(YearMonth,"01")) %>% 
  mutate(YearMonthKey = as.numeric(YearMonthKey)) %>% 
  left_join(decdat05 %>% 
              dplyr::select(PatientDurableKey,criterion1_datekey,criterion1_datekey_minus1y),
            by = c("PatientDurableKey")) %>%
  mutate(NumericValue = as.numeric(NumericValue)) %>% 
  mutate(hdl = case_when(NumericValue >= 5 & NumericValue <= 300 ~ NumericValue,
                         TRUE ~ NA_real_)) %>% 
  to_duckdb()  %>%
  mutate(period = case_when(YearMonthKey >= criterion1_datekey_minus1y & YearMonthKey < criterion1_datekey ~ "Ym1",
                            TRUE ~ "Ym2 or earlier")) %>% 
  dplyr::filter(period %in% c("Ym1"),!is.na(hdl))  %>% 
  group_by(PatientDurableKey) %>% 
  dplyr::filter(YearMonthKey == max(YearMonthKey)) %>% 
  summarize(Hdlc_before = mean(hdl),
            YearMonthKey_Hdlc_before = min(YearMonthKey)) %>%   
  ungroup()  %>% 
  collect() 

## deccoh11a. Blood Pressure -----------
blood_pressure_before = open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh11a"),partitioning = c("Year")) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         YearMonthKey = paste0(YearMonth,"01")) %>% 
  mutate(YearMonthKey = as.numeric(YearMonthKey)) %>% 
  left_join(decdat05 %>% 
              dplyr::select(PatientDurableKey,criterion1_datekey,criterion1_datekey_minus1y),
            by = c("PatientDurableKey")) %>% 
  mutate(
    SystolicBloodPressure = case_when(SystolicBloodPressure > 300 | SystolicBloodPressure < 50 ~ NA_real_,
                                      TRUE ~ SystolicBloodPressure),
    DiastolicBloodPressure = case_when(DiastolicBloodPressure > 300 | DiastolicBloodPressure < 30 ~ NA_real_,
                                       TRUE ~ DiastolicBloodPressure)) %>% 
  to_duckdb()  %>%
  mutate(period = case_when(YearMonthKey >= criterion1_datekey_minus1y & YearMonthKey < criterion1_datekey ~ "Ym1",
                            TRUE ~ "Ym2 or earlier")) %>% 
  dplyr::filter(period %in% c("Ym1"),!is.na(SystolicBloodPressure),!is.na(DiastolicBloodPressure))  %>% 
  group_by(PatientDurableKey) %>% 
  dplyr::filter(YearMonthKey == max(YearMonthKey)) %>% 
  summarize(Sbp_before = mean(SystolicBloodPressure),
            Dbp_before = mean(DiastolicBloodPressure),
            YearMonthKey_Sbp_before = min(YearMonthKey)) %>%   
  ungroup()  %>% 
  collect() 


before <- decdat05 %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey)) %>% 
  left_join(hba1c_before,
            by="PatientDurableKey") %>% 
  left_join(tgl_before,
            by="PatientDurableKey") %>% 
  left_join(ldl_before,
            by="PatientDurableKey") %>% 
  left_join(hdl_before,
            by="PatientDurableKey") %>% 
  left_join(blood_pressure_before,
            by="PatientDurableKey") %>% 
  left_join(bmi_before,
            by="PatientDurableKey") %>% 
  collect()

write_parquet(before,paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan101/predictors 1y before criterion1 date.parquet"))
rm(hba1c_before,tgl_before,ldl_before,hdl_before,blood_pressure_before,bmi_before);gc();



# deccoh15. INSURANCE BEFORE EXPOSURE ---------
insurance_before <- open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh15"),partitioning = c("Year")) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey)) %>% 
  mutate(insurance_medicare = case_when(CoverageFinancialClass == "Medicare" ~ 1,
                                        TRUE ~ 0),
         insurance_other = case_when(CoverageFinancialClass == "Miscellaneous/Other" ~ 1,
                                     TRUE ~ 0),
         insurance_medicaid = case_when(CoverageFinancialClass == "Medicaid" ~ 1,
                                        TRUE ~ 0),
         insurance_selfpay = case_when(CoverageFinancialClass == "Self-Pay" ~ 1,
                                       TRUE ~ 0)) %>% 
  group_by(PatientDurableKey,YearMonth) %>% 
  summarize(insurance_medicare = sum(insurance_medicare),
            insurance_other = sum(insurance_other),
            insurance_medicaid = sum(insurance_medicaid),
            insurance_selfpay = sum(insurance_selfpay)) %>% 
  collect() %>% 
  group_by(PatientDurableKey) %>% 
  dplyr::filter(YearMonth == max(YearMonth)) %>% 
  ungroup()
write_parquet(insurance_before,paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan101/insurance before criterion1 date.parquet"))
rm(insurance_before)
# AFTER EXPOSURE --------------


## deccoh11b. BMI -----------
bmi_after = open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh11b"),partitioning = c("Year")) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         YearMonthKey = paste0(YearMonth,"01")) %>% 
  mutate(YearMonthKey = as.numeric(YearMonthKey)) %>% 
  left_join(decdat05 %>% 
              dplyr::select(PatientDurableKey,criterion1_datekey,criterion1_datekey_plus1y),
            by = c("PatientDurableKey")) %>% 
  mutate(
    BodyMassIndex = case_when(BodyMassIndex >50 | BodyMassIndex <12 ~ NA_real_,
                              TRUE ~ BodyMassIndex)) %>% 
  to_duckdb()  %>%
  mutate(period = case_when(YearMonthKey >= criterion1_datekey & YearMonthKey < criterion1_datekey_plus1y ~ "Y1",
                            TRUE ~ "Y2 or later")) %>% 
  dplyr::filter(period %in% c("Y1"),!is.na(BodyMassIndex))  %>% 
  group_by(PatientDurableKey) %>% 
  dplyr::filter(YearMonthKey == min(YearMonthKey)) %>% 
  summarize(Bmi_after = mean(BodyMassIndex),
            YearMonthKey_Bmi_after = min(YearMonthKey)) %>%   
  ungroup()  %>% 
  collect() 

## deccoh12ab. HbA1c -----------

hba1c_after <- open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh12ab"),partitioning = c("Year")) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         YearMonthKey = paste0(YearMonth,"01")) %>% 
  mutate(YearMonthKey = as.numeric(YearMonthKey)) %>% 
  left_join(decdat05 %>% 
              dplyr::select(PatientDurableKey,criterion1_datekey,criterion1_datekey_plus1y),
            by = c("PatientDurableKey")) %>%
  mutate(NumericValue = as.numeric(NumericValue)) %>% 
  mutate(hba1c = case_when(NumericValue >= 3 & NumericValue <= 20 ~ NumericValue,
                           TRUE ~ NA_real_)) %>% 
  to_duckdb()  %>%
  mutate(period = case_when(YearMonthKey >= criterion1_datekey & YearMonthKey < criterion1_datekey_plus1y ~ "Y1",
                            TRUE ~ "Y2 or later")) %>% 
  dplyr::filter(period %in% c("Y1"),!is.na(hba1c))  %>% 
  group_by(PatientDurableKey) %>% 
  dplyr::filter(YearMonthKey == min(YearMonthKey)) %>% 
  summarize(Hba1c_after = mean(hba1c),
            YearMonthKey_Hba1c_after = min(YearMonthKey)) %>%   
  ungroup()  %>% 
  collect() 

## deccoh12fb. Tgl -----------

tgl_after <- open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh12fb"),partitioning = c("Year")) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         YearMonthKey = paste0(YearMonth,"01")) %>% 
  mutate(YearMonthKey = as.numeric(YearMonthKey)) %>% 
  left_join(decdat05 %>% 
              dplyr::select(PatientDurableKey,criterion1_datekey,criterion1_datekey_plus1y),
            by = c("PatientDurableKey")) %>%
  mutate(NumericValue = as.numeric(NumericValue)) %>% 
  mutate(tgl = case_when(NumericValue >= 10 & NumericValue <= 600 ~ NumericValue,
                         TRUE ~ NA_real_)) %>% 
  to_duckdb()  %>%
  mutate(period = case_when(YearMonthKey >= criterion1_datekey & YearMonthKey < criterion1_datekey_plus1y ~ "Y1",
                            TRUE ~ "Y2 or later")) %>% 
  dplyr::filter(period %in% c("Y1"),!is.na(tgl))  %>% 
  group_by(PatientDurableKey) %>% 
  dplyr::filter(YearMonthKey == min(YearMonthKey)) %>% 
  summarize(Tgl_after = mean(tgl),
            YearMonthKey_Tgl_after = min(YearMonthKey)) %>%   
  ungroup()  %>% 
  collect() 

## deccoh12fb. Ldlc -----------

ldl_after <- open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh12db"),partitioning = c("Year")) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         YearMonthKey = paste0(YearMonth,"01")) %>% 
  mutate(YearMonthKey = as.numeric(YearMonthKey)) %>% 
  left_join(decdat05 %>% 
              dplyr::select(PatientDurableKey,criterion1_datekey,criterion1_datekey_plus1y),
            by = c("PatientDurableKey")) %>%
  mutate(NumericValue = as.numeric(NumericValue)) %>% 
  mutate(ldl = case_when(NumericValue >= 10 & NumericValue <= 600 ~ NumericValue,
                         TRUE ~ NA_real_)) %>% 
  to_duckdb()  %>%
  mutate(period = case_when(YearMonthKey >= criterion1_datekey & YearMonthKey < criterion1_datekey_plus1y ~ "Y1",
                            TRUE ~ "Y2 or later")) %>% 
  dplyr::filter(period %in% c("Y1"),!is.na(ldl))  %>% 
  group_by(PatientDurableKey) %>% 
  dplyr::filter(YearMonthKey == min(YearMonthKey)) %>% 
  summarize(Ldlc_after = mean(ldl),
            YearMonthKey_Ldlc_after = min(YearMonthKey)) %>%   
  ungroup()  %>% 
  collect() 

## deccoh12fb. Hdlc -----------

hdl_after <- open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh12eb"),partitioning = c("Year")) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         YearMonthKey = paste0(YearMonth,"01")) %>% 
  mutate(YearMonthKey = as.numeric(YearMonthKey)) %>% 
  left_join(decdat05 %>% 
              dplyr::select(PatientDurableKey,criterion1_datekey,criterion1_datekey_plus1y),
            by = c("PatientDurableKey")) %>%
  mutate(NumericValue = as.numeric(NumericValue)) %>% 
  mutate(hdl = case_when(NumericValue >= 5 & NumericValue <= 300 ~ NumericValue,
                         TRUE ~ NA_real_)) %>% 
  to_duckdb()  %>%
  mutate(period = case_when(YearMonthKey >= criterion1_datekey & YearMonthKey < criterion1_datekey_plus1y ~ "Y1",
                            TRUE ~ "Y2 or later")) %>% 
  dplyr::filter(period %in% c("Y1"),!is.na(hdl))  %>% 
  group_by(PatientDurableKey) %>% 
  dplyr::filter(YearMonthKey == min(YearMonthKey)) %>% 
  summarize(Hdlc_after = mean(hdl),
            YearMonthKey_Hdlc_after = min(YearMonthKey)) %>%   
  ungroup()  %>% 
  collect() 


## deccoh11b. Blood Pressure ------------
blood_pressure_after = open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh11b"),partitioning = c("Year")) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         YearMonthKey = paste0(YearMonth,"01")) %>% 
  mutate(YearMonthKey = as.numeric(YearMonthKey)) %>% 
  left_join(decdat05 %>% 
              dplyr::select(PatientDurableKey,criterion1_datekey,criterion1_datekey_plus1y),
            by = c("PatientDurableKey")) %>% 
  mutate(
    SystolicBloodPressure = case_when(SystolicBloodPressure > 300 | SystolicBloodPressure < 50 ~ NA_real_,
                                      TRUE ~ SystolicBloodPressure),
    DiastolicBloodPressure = case_when(DiastolicBloodPressure > 300 | DiastolicBloodPressure < 30 ~ NA_real_,
                                       TRUE ~ DiastolicBloodPressure)) %>% 
  to_duckdb()  %>%
  mutate(period = case_when(YearMonthKey >= criterion1_datekey & YearMonthKey < criterion1_datekey_plus1y ~ "Y1",
                            TRUE ~ "Y2 or later")) %>% 
  dplyr::filter(period %in% c("Y1"),!is.na(SystolicBloodPressure),!is.na(DiastolicBloodPressure))  %>% 
  group_by(PatientDurableKey) %>% 
  dplyr::filter(YearMonthKey == min(YearMonthKey)) %>% 
  summarize(Sbp_after = mean(SystolicBloodPressure),
            Dbp_after = mean(DiastolicBloodPressure),
            YearMonthKey_Sbp_after = min(YearMonthKey)) %>%   
  ungroup()  %>% 
  collect() 


after <- decdat05 %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey)) %>% 
  left_join(hba1c_after,
            by="PatientDurableKey") %>% 
  left_join(tgl_after,
            by="PatientDurableKey") %>% 
  left_join(ldl_after,
            by="PatientDurableKey") %>% 
  left_join(hdl_after,
            by="PatientDurableKey") %>% 
  left_join(blood_pressure_after,
            by="PatientDurableKey") %>% 
  left_join(bmi_after,
            by="PatientDurableKey") %>% 
  collect()

write_parquet(after,paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan101/predictors 1y after criterion1 date.parquet"))
rm(hba1c_after,tgl_after,ldl_after,hdl_after,blood_pressure_after,bmi_after);gc();

# COMPLICATIONS BEFORE EXPOSURE ------------


complications_before <- open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh14a"),partitioning = c("Year","Value_Grouper2")) %>% 
  mutate(type = case_when(str_detect(ICD10_Value,"(E10|E11)\\.(21|22|29)") ~ "nephropathy",
                          str_detect(ICD10_Value,"(E10|E11)\\.(31|32|33|34|35|37)") ~ "retinopathy",
                          str_detect(ICD10_Value,"(E10|E11)\\.(41|42|43|44|49)") ~ "neuropathy",
                          TRUE ~ NA_character_)) %>% 
  dplyr::filter(!is.na(type)) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey)) %>% 
  left_join(decdat05 %>% 
              dplyr::select(PatientDurableKey,criterion1_datekey),
            by = c("PatientDurableKey")) %>% 
  to_duckdb()  %>%
  group_by(PatientDurableKey) %>% 
  dplyr::filter(Year == min(Year)) %>% 
  ungroup() %>% 
  collect()

write_parquet(complications_before,paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan101/complications before criterion1 date.parquet"))


# COMPLICATIONS AFTER EXPOSURE ------------


complications_after <- open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh14b"),partitioning = c("Year","Value_Grouper2")) %>% 
  mutate(type = case_when(str_detect(ICD10_Value,"(E10|E11)\\.(21|22|29)") ~ "nephropathy",
                          str_detect(ICD10_Value,"(E10|E11)\\.(31|32|33|34|35|37)") ~ "retinopathy",
                          str_detect(ICD10_Value,"(E10|E11)\\.(41|42|43|44|49)") ~ "neuropathy",
                          TRUE ~ NA_character_)) %>% 
  dplyr::filter(!is.na(type)) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         YearMonthKey = paste0(YearMonth,"01")) %>% 
  left_join(decdat05 %>% 
              dplyr::select(PatientDurableKey,criterion1_datekey),
            by = c("PatientDurableKey")) %>% 
  to_duckdb()  %>%
  group_by(PatientDurableKey,type) %>% 
  dplyr::filter(YearMonthKey == min(YearMonthKey)) %>% 
  ungroup() %>% 
  collect()
write_parquet(complications_after,paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan101/complications after criterion1 date.parquet"))


# PRESCRIPTIONS BEFORE EXPOSURE ------------

prescriptions_before <- open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh13a"),partitioning = c("Year")) %>% 
  mutate(type = case_when(PharmaceuticalClass %in% c("INSULINS") ~ "insulin",
                          PharmaceuticalClass %in% c("ANTIHYPERGLYCEMIC, BIGUANIDE TYPE") ~ "biguanide",
                          PharmaceuticalClass %in% c('ANTIHYPERGLYCEMIC,INSULIN-RELEASE STIM.-BIGUANIDE') ~ "sulfonylurea;biguanide",
                          PharmaceuticalClass %in% c('ANTIHYPERGLYCEMIC, DPP-4 INHIBITORS') ~ "dpp4i",
                          PharmaceuticalClass %in% c('ANTIHYPERGLYCEMIC, INSULIN-RELEASE STIMULANT TYPE') ~ "sulfonylurea",
                          PharmaceuticalClass %in% c('ANTIHYPERGLYCEMIC,THIAZOLIDINEDIONE(PPARG AGONIST)') ~ "tzd",
                          PharmaceuticalClass %in% c('ANTIHYPERGLYCEMIC,DPP-4 INHIBITOR-BIGUANIDE COMBS.') ~ "dpp4i;biguanide",
                          PharmaceuticalClass %in% c('ANTIHYPERGLY,INCRETIN MIMETIC(GLP-1 RECEP.AGONIST)') ~ "glp1ra",
                          PharmaceuticalClass %in% c('ANTIHYPERGLYCEMIC, AMYLIN ANALOG-TYPE') ~ "amylin",
                          PharmaceuticalClass %in% c('ANTIHYPERGLYCEMIC, THIAZOLIDINEDIONE AND BIGUANIDE') ~ "tzd;biguanide",
                          PharmaceuticalClass %in% c('ANTIHYPERGLYCEMIC, ALPHA-GLUCOSIDASE INHIBITORS') ~ "agi",
                          PharmaceuticalClass %in% c('ANTIHYPERGLYCEMIC - DOPAMINE RECEPTOR AGONISTS') ~ "dopamine",
                          PharmaceuticalClass %in% c('ANTIHYPERGLYCEMIC, THIAZOLIDINEDIONE-SULFONYLUREA') ~ "tzd;sulfonylurea",
                          PharmaceuticalClass %in% c('ANTIHYPERGLYCEMIC-SOD/GLUC COTRANSPORT2(SGLT2) INH') ~ "sglt2i",
                          PharmaceuticalClass %in% c('ANTIHYPERGLY. DPP-4 INHIBITORS-HMG COA RI(STATINS)') ~ "dpp4i;statin",
                          PharmaceuticalClass %in% c('ANTIHYPERGLY,DPP-4 ENZYME INHIB.-THIAZOLIDINEDIONE') ~ "dpp4i;tzd",
                          PharmaceuticalClass %in% c('ANTIHYPERGLYCEMIC-SGLT2 INHIBITOR-BIGUANIDE COMBS.') ~ "sglt2i;biguanide",
                          PharmaceuticalClass %in% c('ANTIHYPERGLYCEMIC, SGLT-2 AND DPP-4 INHIBITOR COMB') ~ "sglt2i;dpp4i",
                          PharmaceuticalClass %in% c('ANTIHYPERGLYCEMIC-GLUCOCORTICOID RECEPTOR BLOCKER') ~ "glucocorticoid",
                          PharmaceuticalClass %in% c('ANTIHYPERGLY,INSULIN,LONG ACT-GLP-1 RECEPT.AGONIST') ~ "glp1ra",
                          PharmaceuticalClass %in% c('ANTIHYPERGLY-SGLT-2 INHIB,DPP-4 INHIB,BIGUANIDE CB') ~ "sglt2i",
                          PharmaceuticalClass %in% c('ANTIHYPERGLYCEMIC - INCRETIN MIMETICS COMBINATION') ~ "glpgip",
                          TRUE ~ NA_character_),
  ) %>% 
  dplyr::filter(!is.na(type)) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         YearMonthKey = paste0(YearMonth,"01")) %>% 
  left_join(decdat05 %>% 
              dplyr::select(PatientDurableKey,criterion1_datekey),
            by = c("PatientDurableKey")) %>% 
  to_duckdb()  %>%
  group_by(PatientDurableKey,type) %>% 
  dplyr::filter(YearMonthKey == min(YearMonthKey)) %>% 
  ungroup() %>% 
  collect()
write_parquet(prescriptions_before,paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan101/prescriptions before criterion1 date.parquet"))
rm(prescriptions_before)


# PRESCRIPTIONS AFTER EXPOSURE ------------

prescriptions_after <- open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh13b"),partitioning = c("Year")) %>% 
  mutate(type = case_when(PharmaceuticalClass %in% c("INSULINS") ~ "insulin",
                          PharmaceuticalClass %in% c("ANTIHYPERGLYCEMIC, BIGUANIDE TYPE") ~ "biguanide",
                          PharmaceuticalClass %in% c('ANTIHYPERGLYCEMIC,INSULIN-RELEASE STIM.-BIGUANIDE') ~ "sulfonylurea;biguanide",
                          PharmaceuticalClass %in% c('ANTIHYPERGLYCEMIC, DPP-4 INHIBITORS') ~ "dpp4i",
                          PharmaceuticalClass %in% c('ANTIHYPERGLYCEMIC, INSULIN-RELEASE STIMULANT TYPE') ~ "sulfonylurea",
                          PharmaceuticalClass %in% c('ANTIHYPERGLYCEMIC,THIAZOLIDINEDIONE(PPARG AGONIST)') ~ "tzd",
                          PharmaceuticalClass %in% c('ANTIHYPERGLYCEMIC,DPP-4 INHIBITOR-BIGUANIDE COMBS.') ~ "dpp4i;biguanide",
                          PharmaceuticalClass %in% c('ANTIHYPERGLY,INCRETIN MIMETIC(GLP-1 RECEP.AGONIST)') ~ "glp1ra",
                          PharmaceuticalClass %in% c('ANTIHYPERGLYCEMIC, AMYLIN ANALOG-TYPE') ~ "amylin",
                          PharmaceuticalClass %in% c('ANTIHYPERGLYCEMIC, THIAZOLIDINEDIONE AND BIGUANIDE') ~ "tzd;biguanide",
                          PharmaceuticalClass %in% c('ANTIHYPERGLYCEMIC, ALPHA-GLUCOSIDASE INHIBITORS') ~ "agi",
                          PharmaceuticalClass %in% c('ANTIHYPERGLYCEMIC - DOPAMINE RECEPTOR AGONISTS') ~ "dopamine",
                          PharmaceuticalClass %in% c('ANTIHYPERGLYCEMIC, THIAZOLIDINEDIONE-SULFONYLUREA') ~ "tzd;sulfonylurea",
                          PharmaceuticalClass %in% c('ANTIHYPERGLYCEMIC-SOD/GLUC COTRANSPORT2(SGLT2) INH') ~ "sglt2i",
                          PharmaceuticalClass %in% c('ANTIHYPERGLY. DPP-4 INHIBITORS-HMG COA RI(STATINS)') ~ "dpp4i;statin",
                          PharmaceuticalClass %in% c('ANTIHYPERGLY,DPP-4 ENZYME INHIB.-THIAZOLIDINEDIONE') ~ "dpp4i;tzd",
                          PharmaceuticalClass %in% c('ANTIHYPERGLYCEMIC-SGLT2 INHIBITOR-BIGUANIDE COMBS.') ~ "sglt2i;biguanide",
                          PharmaceuticalClass %in% c('ANTIHYPERGLYCEMIC, SGLT-2 AND DPP-4 INHIBITOR COMB') ~ "sglt2i;dpp4i",
                          PharmaceuticalClass %in% c('ANTIHYPERGLYCEMIC-GLUCOCORTICOID RECEPTOR BLOCKER') ~ "glucocorticoid",
                          PharmaceuticalClass %in% c('ANTIHYPERGLY,INSULIN,LONG ACT-GLP-1 RECEPT.AGONIST') ~ "glp1ra",
                          PharmaceuticalClass %in% c('ANTIHYPERGLY-SGLT-2 INHIB,DPP-4 INHIB,BIGUANIDE CB') ~ "sglt2i",
                          PharmaceuticalClass %in% c('ANTIHYPERGLYCEMIC - INCRETIN MIMETICS COMBINATION') ~ "glpgip",
                          TRUE ~ NA_character_),
  ) %>% 
  dplyr::filter(!is.na(type)) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         YearMonthKey = paste0(YearMonth,"01")) %>% 
  left_join(decdat05 %>% 
              dplyr::select(PatientDurableKey,criterion1_datekey),
            by = c("PatientDurableKey")) %>% 
  to_duckdb()  %>%
  group_by(PatientDurableKey,type) %>% 
  dplyr::filter(YearMonthKey == min(YearMonthKey)) %>% 
  ungroup() %>% 
  collect()
write_parquet(prescriptions_after,paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan101/prescriptions after criterion1 date.parquet"))
rm(prescriptions_after)



# saveRDS(before,paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan101_before criterion1 date.RDS"))
# saveRDS(after,paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan101_after criterion1 date.RDS"))

