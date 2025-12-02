rm(list=ls());gc();source(".Rprofile")

library(gtsummary)


source("analysis/decan002d_phenotyping descriptives dataset.R")

# descriptives_df %>% 
#   dplyr::select(one_of(c("Dmagediag","Bmi","Hba1c","fpg","fins",
#                          "Ldlc","Hdlc","Tgl",
#                          "Sbp","Dbp"))) %>% 
#   summarize_all(~sum(is.na(.))) %>% 
#   write_csv(.,"analysis/decan003_missing counts.csv")


(descriptives <- descriptives_df %>% 
    dplyr::filter(Dmagediag >=40, Dmagediag<=80) %>% 
    tbl_summary(by=sidd_category,
                include = c(female,raceeth,Dmagediag,
                            subphenotype_category
                ),
                missing = "ifany",
                missing_text = "Missing",
                type = list(Dmagediag ~ "continuous",
                            female ~ "dichotomous",
                            raceeth ~ "categorical",
                            subphenotype_category ~ "categorical"
                ),
                digits = list(Dmagediag ~ c(1,1)
                ),
                statistic = list(all_continuous() ~ "{mean} ({sd})",
                                 all_continuous2() ~ c("{median} ({p25}, {p75})", "{min}, {max}"))
    ) %>% 
    add_n() %>% 
    add_overall()) %>%
  as_gt() %>%
  gt::gtsave(filename = "t2d-spear/dects01_analytic sample characteristics.html")


