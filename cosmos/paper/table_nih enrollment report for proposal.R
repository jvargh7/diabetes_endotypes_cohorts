rm(list=ls());gc();source(".Rprofile")

phenotyping_df = read_parquet(paste0(path_diabetes_endotypes_cosmos_folder,"/working/phenotyping dataset.parquet"))
df = read_parquet(paste0(path_diabetes_endotypes_cosmos_folder,"/working/decdat03/eligible patients.parquet")) %>% 
  dplyr::filter(PatientDurableKey %in% phenotyping_df$PatientDurableKey) %>% 
  mutate(ethnicity_nih = case_when(Ethnicity == "Hispanic or Latino" ~ "hispanic",
                                   Ethnicity == "Not Hispanic or Latino" ~ "not_hispanic",
                                   Ethnicity == "*Unspecified"~ "unknown",
                                   TRUE ~ "unknown"),
         sex_nih = case_when(Sex %in% c("Female","Male") ~ Sex,
                             Sex %in% c("*Unspecified","Unknown") ~ "unknown",
                             TRUE ~ "unknown")) %>% 
  mutate(race_nih = factor(race_nih,levels=c("American Indian or Alaska Native",
                                             "Asian",
                                             "Native Hawaiian or Other Pacific Islander",
                                             "Black or African American",
                                             "White",
                                             
                                             "More than One Race",
                                             "Other Race","Unknown"
  ),
  labels=c("American Indian or Alaska Native",
           "Asian",
           "Native Hawaiian or Other Pacific Islander",
           "Black or African American",
           "White",
           "More than One Race",
           "Unknown","Unknown")))

