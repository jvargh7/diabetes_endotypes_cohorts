rm(list=ls());gc();source(".Rprofile")

rfd03 = open_dataset(paste0(path_retinopathy_fragmentation_folder,"/working/rfd03/combined cp before encounter check.parquet"))


decdat04a = open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/decdat11a"),partitioning = c("Year")) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey)) %>% 
  group_by(PatientDurableKey,criterion1_datekey) %>% 
  tally() %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         criterion1_datekey = as.numeric(criterion1_datekey)
  ) %>% 
  ungroup()

decdat04a %>% 
  collect() %>% 
  nrow()

decdat04a %>% 
  distinct(PatientDurableKey) %>% 
  tally() %>% 
  collect()

decdat04b = open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/decdat11b"),partitioning = c("Year")) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey)) %>% 
  group_by(PatientDurableKey,criterion1_datekey) %>% 
  tally() %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         criterion1_datekey = as.numeric(criterion1_datekey)
  ) %>% 
  ungroup()

open_dataset(paste0(path_diabetes_endotypes_cosmos_folder,"/working/decdat11b"),partitioning = c("Year")) %>% 
  dim()


decdat04b %>% 
  collect() %>% 
  nrow()

decdat04b %>% 
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
         female = case_when(Sex == "Female" ~ 1,
                            TRUE ~ 0))


newly_detected = rfd03 %>% 
  left_join(decdat02 %>% 
              dplyr::select(PatientDurableKey,AdultDate,BirthDate,ValidatedStateOrProvince_X,PrimaryRUCA_X,
                            raceeth,female),
            by="PatientDurableKey")  %>% 
  left_join(decdat04a %>% 
              dplyr::select(-criterion1_datekey) %>% 
              rename(count_minus2y = n),
            by=c("PatientDurableKey")) %>%
  left_join(decdat04b %>% 
              dplyr::select(-criterion1_datekey) %>% 
              rename(count_minus1y = n),
            by=c("PatientDurableKey")) %>% 
  to_duckdb()  %>% 
  mutate(count_minus2y = case_when(is.na(count_minus2y) ~ 0,
                                   TRUE ~ count_minus2y),
         count_minus1y = case_when(is.na(count_minus1y) ~ 0,
                                   TRUE ~ count_minus1y)) %>% 
  dplyr::filter(criterion1_date >= AdultDate)  %>% 
  dplyr::filter(count_minus2y >= 1 & count_minus1y >= 1) %>% 
  collect()  %>% 
  mutate(age = as.numeric(difftime(criterion1_date,BirthDate,units="days")/365.25)) %>%
  dplyr::filter(age >= 18, age <100) %>%
  distinct(PatientDurableKey,criterion1_datekey,.keep_all = TRUE) %>% 
  dplyr::select(PatientDurableKey,criterion1_datekey,everything())

View(newly_detected %>% group_by(ValidatedStateOrProvince_X) %>% tally())
View(newly_detected %>% group_by(female,raceeth) %>% tally())

newly_detected %>% 
  write_parquet(.,paste0(path_diabetes_endotypes_cosmos_folder,"/working/decdat12/eligible patients with criterion1_datekey.parquet"))

newly_detected %>% 
  dplyr::select(PatientDurableKey,criterion1_datekey,diagnosis_datekey) %>% 
  write_csv(.,paste0(path_diabetes_endotypes_cosmos_folder,"/working/decdat12/eligible patients with criterion1_datekey.csv"))


decdat03 <- read_parquet(paste0(path_diabetes_endotypes_cosmos_folder,"/working/decdat03/eligible patients.parquet"))

table(decdat03$PatientDurableKey %in% newly_detected$PatientDurableKey)
table(newly_detected$PatientDurableKey %in% decdat03$PatientDurableKey)

