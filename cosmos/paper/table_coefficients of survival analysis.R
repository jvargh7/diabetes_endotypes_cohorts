rm(list=ls());gc();source(".Rprofile")

phenotyping_df = read_parquet(paste0(path_diabetes_endotypes_cosmos_folder,"/working/phenotyping dataset.parquet"))


insulin_df = read_parquet(paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan004/insulin dataset.parquet")) %>% 
  mutate(sidd_category = factor(sidd_category,levels=c("NonSIDD","SIDD")))
metformin_df = read_parquet(paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan004/metformin dataset.parquet")) %>% 
  mutate(sidd_category = factor(sidd_category,levels=c("NonSIDD","SIDD")))
incretin_df = read_parquet(paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan004/incretin dataset.parquet")) %>% 
  mutate(sidd_category = factor(sidd_category,levels=c("NonSIDD","SIDD")))

retinopathy_df = read_parquet(paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan004/retinopathy dataset.parquet")) %>% 
  mutate(sidd_category = factor(sidd_category,levels=c("NonSIDD","SIDD")))
neuropathy_df = read_parquet(paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan004/neuropathy dataset.parquet")) %>% 
  mutate(sidd_category = factor(sidd_category,levels=c("NonSIDD","SIDD")))
nephropathy_df = read_parquet(paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan004/nephropathy dataset.parquet")) %>% 
  mutate(sidd_category = factor(sidd_category,levels=c("NonSIDD","SIDD")))


descriptives = bind_rows(insulin_df %>% mutate(outcome = "Insulin"),
          metformin_df %>% mutate(outcome = "Metformin"),
          incretin_df %>% mutate(outcome = "Incretin"),
          retinopathy_df %>% mutate(outcome = "Retinopathy"),
          neuropathy_df %>% mutate(outcome = "Neuropathy"),
          nephropathy_df %>% mutate(outcome = "Nephropathy")
          ) %>% 
  bind_rows(.,
            {.} %>% 
              mutate(sidd_category = "Total")) %>% 
  group_by(outcome,sidd_category) %>% 
  summarize(events = sum(event),
            censored = n() - sum(event),
            followup = median(t)) %>% 
  ungroup()


coefs_df = read_csv("analysis/decan005_coefficients of risk of prescriptions and complications.csv") %>% 
  dplyr::filter(str_detect(term,"sidd_category")) %>% 
  mutate(coef = exp(estimate),
         lci = exp(estimate - 1.96*std.error),
         uci = exp(estimate + 1.96*std.error)) %>% 
  mutate(coef_ci = paste0(round(coef,2)," (",
                          round(lci,2),", ",
                          round(uci,2),")")) %>% 
  dplyr::select(outcome,model,coef_ci) %>% 
  pivot_wider(names_from = model,values_from=coef_ci)  %>% 
  left_join(descriptives %>% 
              dplyr::filter(sidd_category == "Total"),
            by = c("outcome")) %>% 
  dplyr::select(outcome,sidd_category,events,censored,followup,F0,F1) %>% 
  mutate(excluded = nrow(phenotyping_df) - events-censored)


write_csv(coefs_df,"paper/table_coefficients of survival analysis.csv")

