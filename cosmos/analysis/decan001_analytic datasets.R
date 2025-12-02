rm(list=ls());gc();source(".Rprofile")


decdat03 <- open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/decdat03/eligible patients.parquet")) %>% 
  dplyr::select(PatientDurableKey,diagnosis_datekey,criterion1_date,criterion2_date,diagnosis_date,ValidatedStateOrProvince_X,
                PrimaryRUCA_X,raceeth,female,age) %>% 
  mutate(diagnosis_datekey_plus1y = diagnosis_datekey + 10000,
         diagnosis_datekey_minus1y = diagnosis_datekey - 10000)

decdat03 %>% dim()


# BEFORE EXPOSURE -------------

## deccoh01a. BMI -----------
bmi_before = open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh01a"),partitioning = c("Year")) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         YearMonthKey = paste0(YearMonth,"01")) %>% 
  mutate(YearMonthKey = as.numeric(YearMonthKey)) %>% 
  left_join(decdat03 %>% 
              dplyr::select(PatientDurableKey,diagnosis_datekey,diagnosis_datekey_minus1y),
            by = c("PatientDurableKey")) %>% 
  mutate(
    BodyMassIndex = case_when(BodyMassIndex >50 | BodyMassIndex <12 ~ NA_real_,
                              TRUE ~ BodyMassIndex)) %>% 
  to_duckdb()  %>%
  mutate(period = case_when(YearMonthKey >= diagnosis_datekey_minus1y & YearMonthKey < diagnosis_datekey ~ "Ym1",
                            TRUE ~ "Ym2 or earlier")) %>% 
  dplyr::filter(period %in% c("Ym1"),!is.na(BodyMassIndex))  %>% 
  group_by(PatientDurableKey) %>% 
  dplyr::filter(YearMonthKey == max(YearMonthKey)) %>% 
  summarize(Bmi_before = mean(BodyMassIndex),
            YearMonthKey_Bmi_before = min(YearMonthKey)) %>%   
  ungroup()  %>% 
  collect() 

## deccoh02aa. HbA1c -----------

hba1c_before <- open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh02aa"),partitioning = c("Year")) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         YearMonthKey = paste0(YearMonth,"01")) %>% 
  mutate(YearMonthKey = as.numeric(YearMonthKey)) %>% 
  left_join(decdat03 %>% 
              dplyr::select(PatientDurableKey,diagnosis_datekey,diagnosis_datekey_minus1y),
            by = c("PatientDurableKey")) %>%
  mutate(NumericValue = as.numeric(NumericValue)) %>% 
  mutate(hba1c = case_when(NumericValue >= 3 & NumericValue <= 20 ~ NumericValue,
                                 TRUE ~ NA_real_)) %>% 
  to_duckdb()  %>%
  mutate(period = case_when(YearMonthKey >= diagnosis_datekey_minus1y & YearMonthKey < diagnosis_datekey ~ "Ym1",
                            TRUE ~ "Ym2 or earlier")) %>% 
  dplyr::filter(period %in% c("Ym1"),!is.na(hba1c))  %>% 
  group_by(PatientDurableKey) %>% 
  dplyr::filter(YearMonthKey == max(YearMonthKey)) %>% 
  summarize(Hba1c_before = mean(hba1c),
            YearMonthKey_Hba1c_before = min(YearMonthKey)) %>%   
  ungroup()  %>% 
  collect() 

## deccoh02ba. Fasting Glucose -----------

fpg_before <- open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh02ba"),partitioning = c("Year")) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         YearMonthKey = paste0(YearMonth,"01")) %>% 
  mutate(YearMonthKey = as.numeric(YearMonthKey)) %>% 
  left_join(decdat03 %>% 
              dplyr::select(PatientDurableKey,diagnosis_datekey,diagnosis_datekey_minus1y),
            by = c("PatientDurableKey")) %>%
  mutate(NumericValue = as.numeric(NumericValue)) %>% 
  mutate(fpg = case_when(NumericValue >= 10 & NumericValue <= 400 ~ NumericValue,
                           TRUE ~ NA_real_)) %>% 
  to_duckdb()  %>%
  mutate(period = case_when(YearMonthKey >= diagnosis_datekey_minus1y & YearMonthKey < diagnosis_datekey ~ "Ym1",
                            TRUE ~ "Ym2 or earlier")) %>% 
  dplyr::filter(period %in% c("Ym1"),!is.na(fpg))  %>% 
  group_by(PatientDurableKey) %>% 
  dplyr::filter(YearMonthKey == max(YearMonthKey)) %>% 
  summarize(fpg_before = mean(fpg),
            YearMonthKey_fpg_before = min(YearMonthKey)) %>%   
  ungroup()  %>% 
  collect() 

## deccoh02fa. Tgl -----------

tgl_before <- open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh02fa"),partitioning = c("Year")) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         YearMonthKey = paste0(YearMonth,"01")) %>% 
  mutate(YearMonthKey = as.numeric(YearMonthKey)) %>% 
  left_join(decdat03 %>% 
              dplyr::select(PatientDurableKey,diagnosis_datekey,diagnosis_datekey_minus1y),
            by = c("PatientDurableKey")) %>%
  mutate(NumericValue = as.numeric(NumericValue)) %>% 
  mutate(tgl = case_when(NumericValue >= 10 & NumericValue <= 600 ~ NumericValue,
                           TRUE ~ NA_real_)) %>% 
  to_duckdb()  %>%
  mutate(period = case_when(YearMonthKey >= diagnosis_datekey_minus1y & YearMonthKey < diagnosis_datekey ~ "Ym1",
                            TRUE ~ "Ym2 or earlier")) %>% 
  dplyr::filter(period %in% c("Ym1"),!is.na(tgl))  %>% 
  group_by(PatientDurableKey) %>% 
  dplyr::filter(YearMonthKey == max(YearMonthKey)) %>% 
  summarize(Tgl_before = mean(tgl),
            YearMonthKey_Tgl_before = min(YearMonthKey)) %>%   
  ungroup()  %>% 
  collect() 

## deccoh02fa. Ldlc -----------

ldl_before <- open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh02da"),partitioning = c("Year")) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         YearMonthKey = paste0(YearMonth,"01")) %>% 
  mutate(YearMonthKey = as.numeric(YearMonthKey)) %>% 
  left_join(decdat03 %>% 
              dplyr::select(PatientDurableKey,diagnosis_datekey,diagnosis_datekey_minus1y),
            by = c("PatientDurableKey")) %>%
  mutate(NumericValue = as.numeric(NumericValue)) %>% 
  mutate(ldl = case_when(NumericValue >= 10 & NumericValue <= 600 ~ NumericValue,
                         TRUE ~ NA_real_)) %>% 
  to_duckdb()  %>%
  mutate(period = case_when(YearMonthKey >= diagnosis_datekey_minus1y & YearMonthKey < diagnosis_datekey ~ "Ym1",
                            TRUE ~ "Ym2 or earlier")) %>% 
  dplyr::filter(period %in% c("Ym1"),!is.na(ldl))  %>% 
  group_by(PatientDurableKey) %>% 
  dplyr::filter(YearMonthKey == max(YearMonthKey)) %>% 
  summarize(Ldlc_before = mean(ldl),
            YearMonthKey_Ldlc_before = min(YearMonthKey)) %>%   
  ungroup()  %>% 
  collect() 

## deccoh02fa. Hdlc -----------

hdl_before <- open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh02ea"),partitioning = c("Year")) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         YearMonthKey = paste0(YearMonth,"01")) %>% 
  mutate(YearMonthKey = as.numeric(YearMonthKey)) %>% 
  left_join(decdat03 %>% 
              dplyr::select(PatientDurableKey,diagnosis_datekey,diagnosis_datekey_minus1y),
            by = c("PatientDurableKey")) %>%
  mutate(NumericValue = as.numeric(NumericValue)) %>% 
  mutate(hdl = case_when(NumericValue >= 5 & NumericValue <= 300 ~ NumericValue,
                         TRUE ~ NA_real_)) %>% 
  to_duckdb()  %>%
  mutate(period = case_when(YearMonthKey >= diagnosis_datekey_minus1y & YearMonthKey < diagnosis_datekey ~ "Ym1",
                            TRUE ~ "Ym2 or earlier")) %>% 
  dplyr::filter(period %in% c("Ym1"),!is.na(hdl))  %>% 
  group_by(PatientDurableKey) %>% 
  dplyr::filter(YearMonthKey == max(YearMonthKey)) %>% 
  summarize(Hdlc_before = mean(hdl),
            YearMonthKey_Hdlc_before = min(YearMonthKey)) %>%   
  ungroup()  %>% 
  collect() 

## deccoh02ja. Fasting Insulin -----------

fins_before <- open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh02ja"),partitioning = c("Year")) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         YearMonthKey = paste0(YearMonth,"01")) %>% 
  mutate(YearMonthKey = as.numeric(YearMonthKey)) %>% 
  left_join(decdat03 %>% 
              dplyr::select(PatientDurableKey,diagnosis_datekey,diagnosis_datekey_minus1y),
            by = c("PatientDurableKey")) %>%
  mutate(NumericValue = as.numeric(NumericValue)) %>% 
  mutate(fins = case_when(NumericValue >= 0 & NumericValue <= 200 ~ NumericValue,
                         TRUE ~ NA_real_)) %>% 
  to_duckdb()  %>%
  mutate(period = case_when(YearMonthKey >= diagnosis_datekey_minus1y & YearMonthKey < diagnosis_datekey ~ "Ym1",
                            TRUE ~ "Ym2 or earlier")) %>% 
  dplyr::filter(period %in% c("Ym1"),!is.na(fins))  %>% 
  group_by(PatientDurableKey) %>% 
  dplyr::filter(YearMonthKey == max(YearMonthKey)) %>% 
  summarize(fins_before = mean(fins),
            YearMonthKey_fins_before = min(YearMonthKey)) %>%   
  ungroup()  %>% 
  collect() 

## deccoh01a. Blood Pressure -----------
blood_pressure_before = open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh01a"),partitioning = c("Year")) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         YearMonthKey = paste0(YearMonth,"01")) %>% 
  mutate(YearMonthKey = as.numeric(YearMonthKey)) %>% 
  left_join(decdat03 %>% 
              dplyr::select(PatientDurableKey,diagnosis_datekey,diagnosis_datekey_minus1y),
            by = c("PatientDurableKey")) %>% 
  mutate(
    SystolicBloodPressure = case_when(SystolicBloodPressure > 300 | SystolicBloodPressure < 50 ~ NA_real_,
                                      TRUE ~ SystolicBloodPressure),
    DiastolicBloodPressure = case_when(DiastolicBloodPressure > 300 | DiastolicBloodPressure < 30 ~ NA_real_,
                                       TRUE ~ DiastolicBloodPressure)) %>% 
  to_duckdb()  %>%
  mutate(period = case_when(YearMonthKey >= diagnosis_datekey_minus1y & YearMonthKey < diagnosis_datekey ~ "Ym1",
                            TRUE ~ "Ym2 or earlier")) %>% 
  dplyr::filter(period %in% c("Ym1"),!is.na(SystolicBloodPressure),!is.na(DiastolicBloodPressure))  %>% 
  group_by(PatientDurableKey) %>% 
  dplyr::filter(YearMonthKey == max(YearMonthKey)) %>% 
  summarize(Sbp_before = mean(SystolicBloodPressure),
            Dbp_before = mean(DiastolicBloodPressure),
            YearMonthKey_Sbp_before = min(YearMonthKey)) %>%   
  ungroup()  %>% 
  collect() 


before <- decdat03 %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey)) %>% 
  left_join(hba1c_before,
            by="PatientDurableKey") %>% 
  left_join(fpg_before,
            by="PatientDurableKey") %>% 
  left_join(tgl_before,
            by="PatientDurableKey") %>% 
  left_join(ldl_before,
            by="PatientDurableKey") %>% 
  left_join(hdl_before,
            by="PatientDurableKey") %>% 
  left_join(fins_before,
            by="PatientDurableKey") %>% 
  left_join(blood_pressure_before,
            by="PatientDurableKey") %>% 
  left_join(bmi_before,
            by="PatientDurableKey") %>% 
  collect()

write_parquet(before,paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan001/predictors 1y before diagnosis date.parquet"))
rm(hba1c_before,tgl_before,ldl_before,hdl_before,blood_pressure_before,bmi_before);gc();



# deccoh05. INSURANCE BEFORE EXPOSURE ---------
insurance_before <- open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh05"),partitioning = c("Year")) %>% 
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
write_parquet(insurance_before,paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan001/insurance before diagnosis date.parquet"))
rm(insurance_before)
# AFTER EXPOSURE --------------



## deccoh01b. BMI -----------
bmi_after = open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh01b"),partitioning = c("Year")) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         YearMonthKey = paste0(YearMonth,"01")) %>% 
  mutate(YearMonthKey = as.numeric(YearMonthKey)) %>% 
  left_join(decdat03 %>% 
              dplyr::select(PatientDurableKey,diagnosis_datekey,diagnosis_datekey_plus1y),
            by = c("PatientDurableKey")) %>% 
  mutate(
    BodyMassIndex = case_when(BodyMassIndex >50 | BodyMassIndex <12 ~ NA_real_,
                              TRUE ~ BodyMassIndex)) %>% 
  to_duckdb()  %>%
  mutate(period = case_when(YearMonthKey >= diagnosis_datekey & YearMonthKey < diagnosis_datekey_plus1y ~ "Y1",
                            TRUE ~ "Y2 or later")) %>% 
  dplyr::filter(period %in% c("Y1"),!is.na(BodyMassIndex))  %>% 
  group_by(PatientDurableKey) %>% 
  dplyr::filter(YearMonthKey == min(YearMonthKey)) %>% 
  summarize(Bmi_after = mean(BodyMassIndex),
            YearMonthKey_Bmi_after = min(YearMonthKey)) %>%   
  ungroup()  %>% 
  collect() 

## deccoh02ab. HbA1c -----------

hba1c_after <- open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh02ab"),partitioning = c("Year")) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         YearMonthKey = paste0(YearMonth,"01")) %>% 
  mutate(YearMonthKey = as.numeric(YearMonthKey)) %>% 
  left_join(decdat03 %>% 
              dplyr::select(PatientDurableKey,diagnosis_datekey,diagnosis_datekey_plus1y),
            by = c("PatientDurableKey")) %>%
  mutate(NumericValue = as.numeric(NumericValue)) %>% 
  mutate(hba1c = case_when(NumericValue >= 3 & NumericValue <= 20 ~ NumericValue,
                           TRUE ~ NA_real_)) %>% 
  to_duckdb()  %>%
  mutate(period = case_when(YearMonthKey >= diagnosis_datekey & YearMonthKey < diagnosis_datekey_plus1y ~ "Y1",
                            TRUE ~ "Y2 or later")) %>% 
  dplyr::filter(period %in% c("Y1"),!is.na(hba1c))  %>% 
  group_by(PatientDurableKey) %>% 
  dplyr::filter(YearMonthKey == min(YearMonthKey)) %>% 
  summarize(Hba1c_after = mean(hba1c),
            YearMonthKey_Hba1c_after = min(YearMonthKey)) %>%   
  ungroup()  %>% 
  collect() 

## deccoh02bb. Fasting Glucose -----------

fpg_after <- open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh02bb"),partitioning = c("Year")) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         YearMonthKey = paste0(YearMonth,"01")) %>% 
  mutate(YearMonthKey = as.numeric(YearMonthKey)) %>% 
  left_join(decdat03 %>% 
              dplyr::select(PatientDurableKey,diagnosis_datekey,diagnosis_datekey_plus1y),
            by = c("PatientDurableKey")) %>%
  mutate(NumericValue = as.numeric(NumericValue)) %>% 
  mutate(fpg = case_when(NumericValue >= 10 & NumericValue <= 400 ~ NumericValue,
                           TRUE ~ NA_real_)) %>% 
  to_duckdb()  %>%
  mutate(period = case_when(YearMonthKey >= diagnosis_datekey & YearMonthKey < diagnosis_datekey_plus1y ~ "Y1",
                            TRUE ~ "Y2 or later")) %>% 
  dplyr::filter(period %in% c("Y1"),!is.na(fpg))  %>% 
  group_by(PatientDurableKey) %>% 
  dplyr::filter(YearMonthKey == min(YearMonthKey)) %>% 
  summarize(fpg_after = mean(fpg),
            YearMonthKey_fpg_after = min(YearMonthKey)) %>%   
  ungroup()  %>% 
  collect() 



  
## deccoh02fb. Tgl -----------

tgl_after <- open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh02fb"),partitioning = c("Year")) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         YearMonthKey = paste0(YearMonth,"01")) %>% 
  mutate(YearMonthKey = as.numeric(YearMonthKey)) %>% 
  left_join(decdat03 %>% 
              dplyr::select(PatientDurableKey,diagnosis_datekey,diagnosis_datekey_plus1y),
            by = c("PatientDurableKey")) %>%
  mutate(NumericValue = as.numeric(NumericValue)) %>% 
  mutate(tgl = case_when(NumericValue >= 10 & NumericValue <= 600 ~ NumericValue,
                           TRUE ~ NA_real_)) %>% 
  to_duckdb()  %>%
  mutate(period = case_when(YearMonthKey >= diagnosis_datekey & YearMonthKey < diagnosis_datekey_plus1y ~ "Y1",
                            TRUE ~ "Y2 or later")) %>% 
  dplyr::filter(period %in% c("Y1"),!is.na(tgl))  %>% 
  group_by(PatientDurableKey) %>% 
  dplyr::filter(YearMonthKey == min(YearMonthKey)) %>% 
  summarize(Tgl_after = mean(tgl),
            YearMonthKey_Tgl_after = min(YearMonthKey)) %>%   
  ungroup()  %>% 
  collect() 

## deccoh02fb. Ldlc -----------

ldl_after <- open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh02db"),partitioning = c("Year")) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         YearMonthKey = paste0(YearMonth,"01")) %>% 
  mutate(YearMonthKey = as.numeric(YearMonthKey)) %>% 
  left_join(decdat03 %>% 
              dplyr::select(PatientDurableKey,diagnosis_datekey,diagnosis_datekey_plus1y),
            by = c("PatientDurableKey")) %>%
  mutate(NumericValue = as.numeric(NumericValue)) %>% 
  mutate(ldl = case_when(NumericValue >= 10 & NumericValue <= 600 ~ NumericValue,
                         TRUE ~ NA_real_)) %>% 
  to_duckdb()  %>%
  mutate(period = case_when(YearMonthKey >= diagnosis_datekey & YearMonthKey < diagnosis_datekey_plus1y ~ "Y1",
                            TRUE ~ "Y2 or later")) %>% 
  dplyr::filter(period %in% c("Y1"),!is.na(ldl))  %>% 
  group_by(PatientDurableKey) %>% 
  dplyr::filter(YearMonthKey == min(YearMonthKey)) %>% 
  summarize(Ldlc_after = mean(ldl),
            YearMonthKey_Ldlc_after = min(YearMonthKey)) %>%   
  ungroup()  %>% 
  collect() 

## deccoh02fb. Hdlc -----------

hdl_after <- open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh02eb"),partitioning = c("Year")) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         YearMonthKey = paste0(YearMonth,"01")) %>% 
  mutate(YearMonthKey = as.numeric(YearMonthKey)) %>% 
  left_join(decdat03 %>% 
              dplyr::select(PatientDurableKey,diagnosis_datekey,diagnosis_datekey_plus1y),
            by = c("PatientDurableKey")) %>%
  mutate(NumericValue = as.numeric(NumericValue)) %>% 
  mutate(hdl = case_when(NumericValue >= 5 & NumericValue <= 300 ~ NumericValue,
                         TRUE ~ NA_real_)) %>% 
  to_duckdb()  %>%
  mutate(period = case_when(YearMonthKey >= diagnosis_datekey & YearMonthKey < diagnosis_datekey_plus1y ~ "Y1",
                            TRUE ~ "Y2 or later")) %>% 
  dplyr::filter(period %in% c("Y1"),!is.na(hdl))  %>% 
  group_by(PatientDurableKey) %>% 
  dplyr::filter(YearMonthKey == min(YearMonthKey)) %>% 
  summarize(Hdlc_after = mean(hdl),
            YearMonthKey_Hdlc_after = min(YearMonthKey)) %>%   
  ungroup()  %>% 
  collect() 

## deccoh02jb. Fasting Insulin -----------

fins_after <- open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh02jb"),partitioning = c("Year")) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         YearMonthKey = paste0(YearMonth,"01")) %>% 
  mutate(YearMonthKey = as.numeric(YearMonthKey)) %>% 
  left_join(decdat03 %>% 
              dplyr::select(PatientDurableKey,diagnosis_datekey,diagnosis_datekey_plus1y),
            by = c("PatientDurableKey")) %>%
  mutate(NumericValue = as.numeric(NumericValue)) %>% 
  mutate(fins = case_when(NumericValue >= 0 & NumericValue <= 200 ~ NumericValue,
                         TRUE ~ NA_real_)) %>% 
  to_duckdb()  %>%
  mutate(period = case_when(YearMonthKey >= diagnosis_datekey & YearMonthKey < diagnosis_datekey_plus1y ~ "Y1",
                            TRUE ~ "Y2 or later")) %>% 
  dplyr::filter(period %in% c("Y1"),!is.na(fins))  %>% 
  group_by(PatientDurableKey) %>% 
  dplyr::filter(YearMonthKey == min(YearMonthKey)) %>% 
  summarize(fins_after = mean(fins),
            YearMonthKey_fins_after = min(YearMonthKey)) %>%   
  ungroup()  %>% 
  collect() 


## deccoh01b. Blood Pressure ------------
blood_pressure_after = open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh01b"),partitioning = c("Year")) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         YearMonthKey = paste0(YearMonth,"01")) %>% 
  mutate(YearMonthKey = as.numeric(YearMonthKey)) %>% 
  left_join(decdat03 %>% 
              dplyr::select(PatientDurableKey,diagnosis_datekey,diagnosis_datekey_plus1y),
            by = c("PatientDurableKey")) %>% 
  mutate(
    SystolicBloodPressure = case_when(SystolicBloodPressure > 300 | SystolicBloodPressure < 50 ~ NA_real_,
                                      TRUE ~ SystolicBloodPressure),
    DiastolicBloodPressure = case_when(DiastolicBloodPressure > 300 | DiastolicBloodPressure < 30 ~ NA_real_,
                                       TRUE ~ DiastolicBloodPressure)) %>% 
  to_duckdb()  %>%
  mutate(period = case_when(YearMonthKey >= diagnosis_datekey & YearMonthKey < diagnosis_datekey_plus1y ~ "Y1",
                            TRUE ~ "Y2 or later")) %>% 
  dplyr::filter(period %in% c("Y1"),!is.na(SystolicBloodPressure),!is.na(DiastolicBloodPressure))  %>% 
  group_by(PatientDurableKey) %>% 
  dplyr::filter(YearMonthKey == min(YearMonthKey)) %>% 
  summarize(Sbp_after = mean(SystolicBloodPressure),
            Dbp_after = mean(DiastolicBloodPressure),
            YearMonthKey_Sbp_after = min(YearMonthKey)) %>%   
  ungroup()  %>% 
  collect() 


after <- decdat03 %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey)) %>% 
  left_join(hba1c_after,
            by="PatientDurableKey") %>% 
  left_join(fpg_after,
            by="PatientDurableKey") %>% 
  left_join(tgl_after,
            by="PatientDurableKey") %>% 
  left_join(ldl_after,
            by="PatientDurableKey") %>% 
  left_join(hdl_after,
            by="PatientDurableKey") %>% 
  left_join(fins_after,
            by="PatientDurableKey") %>% 
  left_join(blood_pressure_after,
            by="PatientDurableKey") %>% 
  left_join(bmi_after,
            by="PatientDurableKey") %>% 
  collect()

write_parquet(after,paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan001/predictors 1y after diagnosis date.parquet"))
rm(hba1c_after,tgl_after,ldl_after,hdl_after,blood_pressure_after,bmi_after);gc();

# COMPLICATIONS BEFORE EXPOSURE ------------


complications_before <- open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh04a"),partitioning = c("Year","Value_Grouper2")) %>% 
  mutate(type = case_when(str_detect(ICD10_Value,"(E10|E11)\\.(21|22|29)") ~ "nephropathy",
                          str_detect(ICD10_Value,"(E10|E11)\\.(31|32|33|34|35|37)") ~ "retinopathy",
                          str_detect(ICD10_Value,"(E10|E11)\\.(41|42|43|44|49)") ~ "neuropathy",
                          # str_detect(ICD10_Value,"(N18)\\.(4|5|6)") ~ "ckd",
                          str_detect(ICD10_Value,"(I25)\\.(10)") ~ "chd",
                          str_detect(ICD10_Value,"(I48)\\.(0|1|2|3|4|9)") ~ "afib",
                          str_detect(ICD10_Value,"(G45)\\.(0|1|8|9)") ~ "stroke",
                          str_detect(ICD10_Value,"(I60)\\.(9)") ~ "stroke",
                          str_detect(ICD10_Value,"(I61)\\.(9)") ~ "stroke",
                          str_detect(ICD10_Value,"(I63)\\.(30|40)") ~ "stroke",
                          str_detect(ICD10_Value,"(I67)\\.(89)") ~ "stroke",
                          str_detect(ICD10_Value,"(I73)\\.(0|1|8|9)") ~ "pad",
                          str_detect(ICD10_Value,"(I50)\\.(30|31)") ~ "hfpef",
                          str_detect(ICD10_Value,"(I50)\\.(21|22)") ~ "hfref",
                          Value_Grouper %in% htn_dx_codes ~ "htn",
                          Value_Grouper %in% hld_dx_codes ~ "hld",
                          ICD10_Value %in% cerebro_dx_codes ~ "other_cerebro",
                          Value_Grouper %in% cardiovascular_dx_codes ~ "other_cardiovascular",
                          Value_Grouper %in% pulmonary_dx_codes ~ "pulmonary",
                          ICD10_Value %in% pulmonary_dx_codes ~ "pulmonary",
                          ICD10_Value %in% obesity_dx_codes ~ "obesity",
                          ICD10_Value %in% mafld_dx_codes ~ "mafld",
                          ICD10_Value %in% pcos_dx_codes ~ "pcos",
                          
                          
                          TRUE ~ NA_character_)) %>% 
  dplyr::filter(!is.na(type)) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey)) %>% 
  left_join(decdat03 %>% 
              dplyr::select(PatientDurableKey,diagnosis_datekey),
            by = c("PatientDurableKey")) %>% 
  to_duckdb()  %>%
  group_by(PatientDurableKey, type) %>% 
  dplyr::filter(Year == min(Year)) %>% 
  ungroup() %>% 
  collect()

write_parquet(complications_before,paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan001/complications before diagnosis date.parquet"))
rm(complications_before);gc()

# COMPLICATIONS AFTER EXPOSURE ------------


complications_after <- open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh04b"),partitioning = c("Year","Value_Grouper2")) %>% 
  mutate(type = case_when(str_detect(ICD10_Value,"(E10|E11)\\.(21|22|29)") ~ "nephropathy",
                          str_detect(ICD10_Value,"(E10|E11)\\.(31|32|33|34|35|37)") ~ "retinopathy",
                          str_detect(ICD10_Value,"(E10|E11)\\.(41|42|43|44|49)") ~ "neuropathy",
                          # str_detect(ICD10_Value,"(N18)\\.(4|5|6)") ~ "ckd",
                          str_detect(ICD10_Value,"(I25)\\.(10)") ~ "chd",
                          str_detect(ICD10_Value,"(I48)\\.(0|1|2|3|4|9)") ~ "afib",
                          str_detect(ICD10_Value,"(G45)\\.(0|1|8|9)") ~ "stroke",
                          str_detect(ICD10_Value,"(I60)\\.(9)") ~ "stroke",
                          str_detect(ICD10_Value,"(I61)\\.(9)") ~ "stroke",
                          str_detect(ICD10_Value,"(I63)\\.(30|40)") ~ "stroke",
                          str_detect(ICD10_Value,"(I67)\\.(89)") ~ "stroke",
                          str_detect(ICD10_Value,"(I73)\\.(0|1|8|9)") ~ "pad",
                          str_detect(ICD10_Value,"(I50)\\.(30|31)") ~ "hfpef",
                          str_detect(ICD10_Value,"(I50)\\.(21|22)") ~ "hfref",
                          Value_Grouper %in% htn_dx_codes ~ "htn",
                          Value_Grouper %in% hld_dx_codes ~ "hld",
                          ICD10_Value %in% cerebro_dx_codes ~ "other_cerebro",
                          Value_Grouper %in% cardiovascular_dx_codes ~ "other_cardiovascular",
                          Value_Grouper %in% pulmonary_dx_codes ~ "pulmonary",
                          ICD10_Value %in% pulmonary_dx_codes ~ "pulmonary",
                          ICD10_Value %in% obesity_dx_codes ~ "obesity",
                          ICD10_Value %in% mafld_dx_codes ~ "mafld",
                          ICD10_Value %in% pcos_dx_codes ~ "pcos",
                          
                          
                          TRUE ~ NA_character_)) %>% 
  dplyr::filter(!is.na(type)) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         YearMonthKey = paste0(YearMonth,"01")) %>% 
  left_join(decdat03 %>% 
              dplyr::select(PatientDurableKey,diagnosis_datekey),
            by = c("PatientDurableKey")) %>% 
  to_duckdb()  %>%
  group_by(PatientDurableKey,type) %>% 
  dplyr::filter(YearMonthKey == min(YearMonthKey)) %>% 
  ungroup() %>% 
  collect()
write_parquet(complications_after,paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan001/complications after diagnosis date.parquet"))

rm(complications_after);gc()

# LAST DIAGNOSIS AFTER EXPOSURE ------------


last_diagnosis_after <- open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh04b"),partitioning = c("Year","Value_Grouper2")) %>% 
  dplyr::filter(!is.na(ICD10_Value)) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         YearMonthKey = paste0(YearMonth,"01")) %>% 
  to_duckdb()  %>%
  group_by(PatientDurableKey) %>% 
  dplyr::filter(YearMonthKey == max(YearMonthKey)) %>% 
  ungroup() %>% 
  distinct(PatientDurableKey,YearMonthKey) %>% 
  collect()
write_parquet(last_diagnosis_after,paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan001/last diagnosis after diagnosis date.parquet"))




# PRESCRIPTIONS BEFORE EXPOSURE ------------

prescriptions_before <- open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh03a"),partitioning = c("Year")) %>% 
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
  left_join(decdat03 %>% 
              dplyr::select(PatientDurableKey,diagnosis_datekey),
            by = c("PatientDurableKey")) %>% 
  to_duckdb()  %>%
  group_by(PatientDurableKey,type) %>% 
  dplyr::filter(YearMonthKey == min(YearMonthKey)) %>% 
  ungroup() %>% 
  collect()
write_parquet(prescriptions_before,paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan001/prescriptions before diagnosis date.parquet"))
rm(prescriptions_before)


# PRESCRIPTIONS AFTER EXPOSURE ------------

prescriptions_after <- open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh03b"),partitioning = c("Year")) %>% 
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
  left_join(decdat03 %>% 
              dplyr::select(PatientDurableKey,diagnosis_datekey),
            by = c("PatientDurableKey")) %>% 
  to_duckdb()  %>%
  group_by(PatientDurableKey,type) %>% 
  dplyr::filter(YearMonthKey == min(YearMonthKey)) %>% 
  ungroup() %>% 
  collect()
write_parquet(prescriptions_after,paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan001/prescriptions after diagnosis date.parquet"))
rm(prescriptions_after)

# LAST PRESCRIPTION AFTER EXPOSURE ---------

last_prescription_after <- open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh03b"),partitioning = c("Year")) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         YearMonthKey = paste0(YearMonth,"01")) %>% 
  to_duckdb()  %>%
  group_by(PatientDurableKey) %>% 
  dplyr::filter(YearMonthKey == max(YearMonthKey)) %>% 
  ungroup() %>% 
  distinct(PatientDurableKey,YearMonthKey) %>% 
  collect()

write_parquet(last_prescription_after,paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan001/last prescription after diagnosis date.parquet"))
rm(last_prescription_after)


# SOURCES ON MONTH OF EXPOSURE ---------------

sources_exposure <- open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/deccoh06b"),partitioning = c("Year","SourceKey"))  %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         YearMonthKey = paste0(YearMonth,"00")) %>% 
  left_join(decdat03 %>% 
              dplyr::select(PatientDurableKey,diagnosis_datekey),
            by = c("PatientDurableKey")) %>% 
  to_duckdb()  %>%
  dplyr::filter(YearMonthKey == diagnosis_datekey) %>% 
  distinct(PatientDurableKey,YearMonthKey,SourceKey) %>% 
  ungroup() %>% 
  collect()

write_parquet(sources_exposure,paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan001/sources on month of diagnosis date.parquet"))



# saveRDS(before,paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan001_before diagnosis date.RDS"))
# saveRDS(after,paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan001_after diagnosis date.RDS"))

