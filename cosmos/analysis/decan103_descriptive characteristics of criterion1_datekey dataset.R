rm(list=ls());gc();source(".Rprofile")

# source("analysis/decan_analytic sample for sidd classification with criterion1_datekey.R")
# rm(predictors_after,predictors_before)

sidd_df = read_parquet(paste0(path_diabetes_endotypes_cosmos_folder,"/working/sidd classification dataset with criterion1_datekey.parquet"))

sidd_df_imputed <- readRDS(paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan102/imputed sidd classification dataset.rds")) %>% 
  dplyr::select(PatientDurableKey,is_sidd)

hist(sidd_df_imputed$is_sidd)

table(sidd_df_imputed$is_sidd>=0.5)

library(gtsummary)

(descriptives <- sidd_df %>% 
    left_join(sidd_df_imputed,
              by=c("PatientDurableKey")) %>% 
    mutate(sidd_category = case_when(is_sidd >= 0.5 ~ "SIDD",
                                     TRUE ~ "NonSIDD"),
           SviOverallPctlRankByZip2020_X = SviOverallPctlRankByZip2020_X*100) %>% 
    tbl_summary(by=sidd_category,
                include = c(female,raceeth,
                            SviOverallPctlRankByZip2020_X,ValidatedStateOrProvince_X,
                            insurance_medicare, insurance_medicaid, insurance_other, insurance_selfpay,
                            
                            Dmagediag, Hba1c, Bmi, Sbp, Dbp, Ldlc, Tgl, Hdlc, Ratio_th
                ),
                missing = "ifany",
                missing_text = "Missing",
                type = list(Dmagediag ~ "continuous",
                            female ~ "dichotomous",
                            raceeth ~ "categorical",
                            SviOverallPctlRankByZip2020_X ~ "continuous2",
                            ValidatedStateOrProvince_X ~ "categorical",
                            
                            insurance_medicare ~ "dichotomous",
                            insurance_other ~ "dichotomous",
                            insurance_medicaid ~ "dichotomous",
                            insurance_selfpay ~ "dichotomous",
                            
                            Bmi ~ "continuous", Sbp ~ "continuous", Dbp ~ "continuous",
                            Hba1c ~ "continuous2", Ldlc ~ "continuous", Hdlc ~ "continuous", Tgl ~ "continuous", Ratio_th ~ "continuous2"),
                digits = list(Dmagediag ~ c(1,1),
                              SviOverallPctlRankByZip2020_X ~ c(1,1,1,1,1),
                              Bmi ~ c(1,1), Sbp ~ c(1,1), Dbp ~ c(1,1),
                              Hba1c ~ c(1,1,1,1,1), Ldlc ~ c(1,1), Hdlc ~ c(1,1), Tgl ~ c(1,1), Ratio_th ~ c(1,1,1,1,1)
                ),
                statistic = list(all_continuous() ~ "{mean} ({sd})",
                                 all_continuous2() ~ c("{median} ({p25}, {p75})", "{min}, {max}"))
    ) %>% 
    add_n() %>% 
    add_overall()) %>%
  as_gt() %>%
  gt::gtsave(filename = "analysis/decan103_descriptive characteristics of criterion1_datekey dataset.html")

