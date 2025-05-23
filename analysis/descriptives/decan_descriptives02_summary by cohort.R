rm(list=ls());gc();source(".Rprofile")

source("analysis/decan_analytic sample.R")

source("C:/code/external/functions/nhst/table1_summary.R")

# Zhongyu's Macbook
source(url("https://raw.githubusercontent.com/jvargh7/functions/main/nhst/table1_summary.R"))

names(analytic_dataset_cluster)

c_vars = c("dmagediag","bmi","hba1c","glucosef2","insulinf2","homa2b","homa2ir","ldlc","hdlc","tgl","sbp","dbp","ratio_th",
           "ast","alt","urinealbumin","urinecreatinine","uacr","egfr")

p_vars = c("female")
g_vars = c("race_rev","cluster")

table_df = analytic_dataset_cluster %>% 
  # bind_rows(.,
  #           {.} %>% 
  #             mutate(cluster="Total")) %>% 
  table1_summary(.,c_vars = c_vars,p_vars = p_vars,g_vars = g_vars,id_vars = "study")

write_csv(table_df,"analysis/descriptives/decan_descriptives02_summary by cohort.csv")

