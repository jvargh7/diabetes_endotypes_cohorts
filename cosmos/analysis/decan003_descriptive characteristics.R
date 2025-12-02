rm(list=ls());gc();source(".Rprofile")

library(gtsummary)


source("analysis/decan002d_phenotyping descriptives dataset.R")

descriptives_df %>% 
  dplyr::select(one_of(c("Dmagediag","Bmi","Hba1c","fpg","fins",
                         "Ldlc","Hdlc","Tgl",
                         "Sbp","Dbp"))) %>% 
  summarize_all(~sum(is.na(.))) %>% 
  write_csv(.,"analysis/decan003_missing counts.csv")


(descriptives <- descriptives_df %>% 
    tbl_summary(by=sidd_category,
                include = c(female,raceeth,
                            SviOverallPctlRankByZip2020_X,ValidatedStateOrProvince_X,
                            insurance_medicare, insurance_medicaid, insurance_other, insurance_selfpay,
                            
                            Dmagediag, Hba1c, Bmi, Sbp, Dbp, Ldlc, Tgl, Hdlc, Ratio_th,
                            fins,fpg,
                            
                            mod_category, mard_category, subphenotype_category
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
                            Hba1c ~ "continuous2", Ldlc ~ "continuous", Hdlc ~ "continuous", Tgl ~ "continuous", Ratio_th ~ "continuous2",
                            fins ~ "continuous2",fpg ~ "continuous",
                            
                            mod_category ~ "categorical", mard_category ~ "categorical", subphenotype_category ~ "categorical"
                            ),
                digits = list(Dmagediag ~ c(1,1),
                              SviOverallPctlRankByZip2020_X ~ c(1,1,1,1,1),
                              Bmi ~ c(1,1), Sbp ~ c(1,1), Dbp ~ c(1,1),
                              Hba1c ~ c(1,1,1,1,1), Ldlc ~ c(1,1), Hdlc ~ c(1,1), Tgl ~ c(1,1), Ratio_th ~ c(1,1,1,1,1),
                              fins ~ c(1,1,1,1,1), fpg ~ c(1,1)
                ),
                statistic = list(all_continuous() ~ "{mean} ({sd})",
                                 all_continuous2() ~ c("{median} ({p25}, {p75})", "{min}, {max}"))
    ) %>% 
    add_n() %>% 
    add_overall()) %>%
  as_gt() %>%
  gt::gtsave(filename = "analysis/decan003_descriptive characteristics.html")

(descriptives_all <- descriptives_df %>% 
    tbl_summary(by=subphenotype_category,
                include = c(female,raceeth,
                            SviOverallPctlRankByZip2020_X,ValidatedStateOrProvince_X,
                            insurance_medicare, insurance_medicaid, insurance_other, insurance_selfpay,
                            
                            Dmagediag, Hba1c, Bmi, Sbp, Dbp, Ldlc, Tgl, Hdlc, Ratio_th,
                            fins,fpg,
                            
                            mod_category, mard_category, sidd_category
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
                            Hba1c ~ "continuous2", Ldlc ~ "continuous", Hdlc ~ "continuous", Tgl ~ "continuous", Ratio_th ~ "continuous2",
                            fins ~ "continuous2",fpg ~ "continuous",
                            
                            mod_category ~ "categorical", mard_category ~ "categorical", sidd_category ~ "categorical"
                ),
                digits = list(Dmagediag ~ c(1,1),
                              SviOverallPctlRankByZip2020_X ~ c(1,1,1,1,1),
                              Bmi ~ c(1,1), Sbp ~ c(1,1), Dbp ~ c(1,1),
                              Hba1c ~ c(1,1,1,1,1), Ldlc ~ c(1,1), Hdlc ~ c(1,1), Tgl ~ c(1,1), Ratio_th ~ c(1,1,1,1,1),
                              fins ~ c(1,1,1,1,1), fpg ~ c(1,1)
                ),
                statistic = list(all_continuous() ~ "{mean} ({sd})",
                                 all_continuous2() ~ c("{median} ({p25}, {p75})", "{min}, {max}"))
    ) %>% 
    add_n() %>% 
    add_overall()) %>%
  as_gt() %>%
  gt::gtsave(filename = "analysis/decan003_descriptive characteristics all subphenotypes.html")

