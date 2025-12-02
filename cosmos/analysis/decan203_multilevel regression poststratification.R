rm(list=ls());gc();source(".Rprofile")



decan201_df = readRDS(paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan201/glmer dataset.RDS"))

glmer_sidd = readRDS(paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan202/glmer_sidd.RDS"))
glmer_mod = readRDS(paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan202/glmer_mod.RDS"))
glmer_mard = readRDS(paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan202/glmer_mard.RDS"))

ps_table = readxl::read_excel("data/ktcos01_cosmos poststratification table for state-ruca.xlsx") %>% 
  mutate(female = case_when(gender == "female" ~ 1,
                            TRUE ~ 0))

# library(merTools)
source("merTools/direct_poststratification_merTools.R")




sidd_estimates = direct_poststratification_merTools(reference_data = ps_table,model=glmer_sidd,group.vars = c("ValidatedStateOrProvince_X","PrimaryRUCA_X"),n.sims=200)
write_csv(sidd_estimates,"analysis/decan203_sidd poststratified estimates.csv")

mod_estimates = direct_poststratification_merTools(reference_data = ps_table,model=glmer_mod,group.vars = c("ValidatedStateOrProvince_X","PrimaryRUCA_X"),n.sims=200)
write_csv(mod_estimates,"analysis/decan203_mod poststratified estimates.csv")

mard_estimates = direct_poststratification_merTools(reference_data = ps_table,model=glmer_mard,group.vars = c("ValidatedStateOrProvince_X","PrimaryRUCA_X"),n.sims=200)

write_csv(mard_estimates,"analysis/decan203_mard poststratified estimates.csv")

