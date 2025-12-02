rm(list=ls());gc();source(".Rprofile")

decan201_df = readRDS(paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan201/glmer dataset.RDS"))

decan202_vcov = function(mod){
  
  vcov_mat = vcov(mod,full=TRUE)@factors$correlation 
  
  matrix(vcov_mat,nrow=vcov_mat@Dim[1],ncol=vcov_mat@Dim[2],dimnames = vcov_mat@Dimnames) %>% 
    data.frame() %>% 
    return()
  
}

library(lme4)

glmer_sidd = glmer(sidd_category ~ (1|ValidatedStateOrProvince_X/PrimaryRUCA_X) + female*raceeth + # age_category + 
                   SviOverallPctlRankByZip2020_imp + SviHouseholdCharacteristicsPctlRankByZip2020_imp + SviRacialEthnicMinorityStatusPctlRankByZip2020_imp,
                 
                 family = binomial(link = "logit"),
                 data = decan201_df,
                 # nAGQ = 1L,
                 # verbose = 1,
                 control = glmerControl(optimizer = "nloptwrap"))

saveRDS(glmer_sidd,paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan202/glmer_sidd.RDS"))

broom.mixed::tidy(glmer_sidd) %>% 
  write_csv("analysis/decan202_glmer sidd coefficients.csv")

decan202_vcov(glmer_sidd) %>% 
  write_csv("analysis/decan202_glmer sidd vcov.csv")


glmer_mod = glmer(mod_category ~ (1|ValidatedStateOrProvince_X/PrimaryRUCA_X) + female*raceeth + # age_category + 
                   SviOverallPctlRankByZip2020_imp + SviHouseholdCharacteristicsPctlRankByZip2020_imp + SviRacialEthnicMinorityStatusPctlRankByZip2020_imp,
                 
                 family = binomial(link = "logit"),
                 data = decan201_df,
                 # nAGQ = 1L,
                 # verbose = 1,
                 control = glmerControl(optimizer = "nloptwrap"))
saveRDS(glmer_mod,paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan202/glmer_mod.RDS"))

broom.mixed::tidy(glmer_mod) %>% 
  write_csv("analysis/decan202_glmer mod coefficients.csv")
decan202_vcov(glmer_mod) %>% 
  write_csv("analysis/decan202_glmer mod vcov.csv")



glmer_mard = glmer(mard_category ~ (1|ValidatedStateOrProvince_X/PrimaryRUCA_X) + female*raceeth + # age_category + 
                   SviOverallPctlRankByZip2020_imp + SviHouseholdCharacteristicsPctlRankByZip2020_imp + SviRacialEthnicMinorityStatusPctlRankByZip2020_imp,
                 
                 family = binomial(link = "logit"),
                 data = decan201_df,
                 # nAGQ = 1L,
                 # verbose = 1,
                 control = glmerControl(optimizer = "nloptwrap"))

saveRDS(glmer_mard,paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan202/glmer_mard.RDS"))

broom.mixed::tidy(glmer_mard) %>% 
  write_csv("analysis/decan202_glmer mard coefficients.csv")

decan202_vcov(glmer_mard) %>% 
  write_csv("analysis/decan202_glmer mard vcov.csv")



getME(glmer_sidd, "b")[,] %>% 
  data.frame() %>% 
  mutate(Zt = getME(glmer_sidd, "Zt")@Dimnames[[1]]) %>% 
  write_csv("analysis/decan202_glmer sidd b.csv")

# getME(glmer_sidd, "Zt") %>% 
#   write_csv("analysis/decan202_glmer sidd Zt.csv")

getME(glmer_mod, "b")[,] %>% 
  data.frame() %>% 
  mutate(Zt = getME(glmer_mod, "Zt")@Dimnames[[1]]) %>% 
  write_csv("analysis/decan202_glmer mod b.csv")

# getME(glmer_mod, "Zt") %>% 
#   write_csv("analysis/decan202_glmer mod Zt.csv")
# 

getME(glmer_mard, "b")[,] %>% 
  data.frame() %>% 
  mutate(Zt = getME(glmer_mard, "Zt")@Dimnames[[1]]) %>% 
  write_csv("analysis/decan202_glmer mard b.csv")

# getME(glmer_mard, "Zt") %>% 
#   write_csv("analysis/decan202_glmer mard Zt.csv")


ranef(glmer_sidd) %>%
  data.frame() %>% 
  write_csv("analysis/decan202_glmer sidd ranef.csv")
ranef(glmer_mod) %>%
  data.frame() %>% 
  write_csv("analysis/decan202_glmer mod ranef.csv")
ranef(glmer_mard) %>%
  data.frame() %>% 
  write_csv("analysis/decan202_glmer mard ranef.csv")

