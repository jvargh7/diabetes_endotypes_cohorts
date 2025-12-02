
exposure_main = "sidd_category + female"
exposure_main_a = "strata(sidd_category) + female"

exposure_all = "subphenotype_category + female"

exposure_all_a = "strata(subphenotype_category) + female"

covariates = "+ Dmagediag"
covariates_rx = "+ statin + ace + arb + betablocker"
covariates_cluster = "+ ValidatedStateOrProvince_X"

outcome_main = "Surv(t,event) ~ "


f0 = paste0(outcome_main,exposure_main)
f1 = paste0(outcome_main,exposure_main,covariates)
f2 = paste0(outcome_main,exposure_main_a,covariates)


s0 = paste0(outcome_main,exposure_all)
s1 = paste0(outcome_main,exposure_all,covariates)
s2 = paste0(outcome_main,exposure_all,covariates,covariates_rx)
s3 = paste0(outcome_main,exposure_all,covariates,covariates_rx,covariates_cluster)


a0 = paste0(outcome_main,exposure_all_a)
a1 = paste0(outcome_main,exposure_all_a,covariates)
a2 = paste0(outcome_main,exposure_all_a,covariates,covariates_rx)
a3 = paste0(outcome_main,exposure_all_a,covariates,covariates_rx,covariates_cluster)


