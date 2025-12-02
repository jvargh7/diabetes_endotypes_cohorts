rm(list=ls());gc();source(".Rprofile")

rfd03 = open_dataset(paste0(path_retinopathy_fragmentation_folder,"/working/rfd03/combined cp before encounter check.parquet"))


decdat01a = open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/decdat01a"),partitioning = c("Year")) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey)) %>% 
  group_by(PatientDurableKey,diagnosis_datekey) %>% 
  tally() %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         diagnosis_datekey = as.numeric(diagnosis_datekey)
         ) %>% 
  ungroup()

decdat01a %>% 
  collect() %>% 
  nrow()

decdat01a %>% 
  distinct(PatientDurableKey) %>% 
  tally() %>% 
  collect()

decdat01b = open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/decdat01b"),partitioning = c("Year")) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey)) %>% 
  group_by(PatientDurableKey,diagnosis_datekey) %>% 
  tally() %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         diagnosis_datekey = as.numeric(diagnosis_datekey)
  ) %>% 
  ungroup()

open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/decdat01b"),partitioning = c("Year")) %>% 
  dim()


decdat01b %>% 
  collect() %>% 
  nrow()

decdat01b %>% 
  group_by(n) %>% 
  tally() %>% 
  collect()

decdat02 = open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/decdat02"),format="parquet",partitioning = "ValidatedStateOrProvince_X") %>% 
  mutate(BirthDate = ymd(BirthDate),
         PatientDurableKey = as.numeric(PatientDurableKey)) %>% 
  mutate(AdultDate = BirthDate + dyears(18)) %>% 
  mutate(raceeth = case_when(Ethnicity == "Hispanic or Latino" ~ 3,
                             FirstRace == "Black or African American" | 
                               SecondRace == "Black or African American" | 
                               ThirdRace == "Black or African American" ~ 2,
                             FirstRace == "White" & SecondRace == "" ~ 1,
                             TRUE ~ 4),
         race_nih = case_when(
                              FirstRace == "American Indian or Alaska Native" &
                                SecondRace %in% c("","*Masked","American Indian or Alaska Native") & ThirdRace %in% c("","*Masked","American Indian or Alaska Native") ~ "American Indian or Alaska Native",
                              
                              FirstRace == "Asian" &
                                SecondRace %in% c("","*Masked","Asian") & ThirdRace %in% c("","*Masked","Asian") ~ "Asian",
                              
                              FirstRace == "Black or African American" &
                                SecondRace  %in% c("","*Masked","Black or African American") & ThirdRace %in% c("","*Masked","Black or African American") ~ "Black or African American",
                              
                              FirstRace == "Native Hawaiian or Other Pacific Islander" &
                                SecondRace %in% c("","*Masked","Native Hawaiian or Other Pacific Islander") & ThirdRace %in% c("","*Masked","Native Hawaiian or Other Pacific Islander") ~ "Native Hawaiian or Other Pacific Islander",
                              
                              FirstRace == "Other Race" &
                                SecondRace %in% c("","*Masked","Other Race") & ThirdRace %in% c("","*Masked","Other Race") ~ "Other Race",
                              
                              FirstRace == "White" &
                                SecondRace %in% c("","*Masked","White") & ThirdRace %in% c("","*Masked","White") ~ "White",
                              
                              FirstRace %in% c("American Indian or Alaska Native",
                                                 "Asian",
                                                 "Black or African American",
                                                "Native Hawaiian or Other Pacific Islander",
                                                "Other Race",
                                                "White")  &
                                (FirstRace != ThirdRace | FirstRace != SecondRace | SecondRace != ThirdRace) & 
                                SecondRace %in% c("American Indian or Alaska Native",
                                                  "Asian",
                                                  "Black or African American",
                                                  "Native Hawaiian or Other Pacific Islander",
                                                  "Other Race",
                                                  "White") & 
                                
                                ThirdRace %in% c("American Indian or Alaska Native",
                                                                              "Asian",
                                                                              "Black or African American",
                                                                              "Native Hawaiian or Other Pacific Islander",
                                                                              "Other Race",
                                                                              "White") ~ "More than One Race",
                              
                              
                              TRUE ~ "Unknown"
                              
                              ),
         
         female = case_when(Sex == "Female" ~ 1,
                            TRUE ~ 0))


newly_detected = rfd03 %>% 
  left_join(decdat02 %>% 
              dplyr::select(PatientDurableKey,AdultDate,BirthDate,ValidatedStateOrProvince_X,PrimaryRUCA_X,
                            SviOverallPctlRankByZip2020_X,
                            raceeth,female,
                            race_nih,Sex,Ethnicity),
            by="PatientDurableKey")  %>% 
  left_join(decdat01a %>% 
              dplyr::select(-diagnosis_datekey) %>% 
              rename(count_minus2y = n),
            by=c("PatientDurableKey")) %>%
  left_join(decdat01b %>% 
              dplyr::select(-diagnosis_datekey) %>% 
              rename(count_minus1y = n),
            by=c("PatientDurableKey")) %>% 
  to_duckdb()  %>% 
  mutate(count_minus2y = case_when(is.na(count_minus2y) ~ 0,
                                   TRUE ~ count_minus2y),
         count_minus1y = case_when(is.na(count_minus1y) ~ 0,
                                   TRUE ~ count_minus1y)) %>% 
  dplyr::filter(diagnosis_date >= AdultDate)  %>% 
  dplyr::filter(count_minus2y >= 1 & count_minus1y >= 1) %>% 
  collect()  %>% 
  mutate(age = as.numeric(difftime(diagnosis_date,BirthDate,units="days")/365.25)) %>%
  dplyr::filter(age >= 18, age <100) %>%
  distinct(PatientDurableKey,diagnosis_datekey,.keep_all = TRUE) %>% 
  dplyr::select(PatientDurableKey,diagnosis_datekey,everything())

View(newly_detected %>% group_by(ValidatedStateOrProvince_X) %>% tally())
View(newly_detected %>% group_by(female,raceeth) %>% tally())

newly_detected %>% 
  write_parquet(.,paste0(path_diabetes_endotypes_cosmos_folder,"/working/decdat03/eligible patients.parquet"))
newly_detected %>% 
  dplyr::select(PatientDurableKey,diagnosis_datekey) %>% 
  write_csv(.,paste0(path_diabetes_endotypes_cosmos_folder,"/working/decdat03/eligible patients.csv"))

