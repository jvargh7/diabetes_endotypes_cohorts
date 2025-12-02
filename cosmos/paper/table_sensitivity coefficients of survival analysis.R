rm(list=ls());gc();source(".Rprofile")

phenotyping_df = read_parquet(paste0(path_diabetes_endotypes_cosmos_folder,"/working/phenotyping dataset.parquet"))


insulin_df = read_parquet(paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan004/insulin dataset.parquet"))
metformin_df = read_parquet(paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan004/metformin dataset.parquet")) 
incretin_df = read_parquet(paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan004/incretin dataset.parquet")) 

retinopathy_df = read_parquet(paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan004/retinopathy dataset.parquet")) 
neuropathy_df = read_parquet(paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan004/neuropathy dataset.parquet")) 
nephropathy_df = read_parquet(paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan004/nephropathy dataset.parquet")) 

chd_df <- read_parquet(paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan004/chd dataset.parquet"))
afib_df <- read_parquet(paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan004/afib dataset.parquet"))
stroke_df <- read_parquet(paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan004/stroke dataset.parquet"))
pad_df <- read_parquet(paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan004/pad dataset.parquet"))
hfpef_df <- read_parquet(paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan004/hfpef dataset.parquet"))
hfref_df <- read_parquet(paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan004/hfref dataset.parquet"))



descriptives = bind_rows(insulin_df %>% mutate(outcome = "Insulin"),
                         metformin_df %>% mutate(outcome = "Metformin"),
                         incretin_df %>% mutate(outcome = "Incretin"),
                         retinopathy_df %>% mutate(outcome = "Retinopathy"),
                         neuropathy_df %>% mutate(outcome = "Neuropathy"),
                         nephropathy_df %>% mutate(outcome = "Nephropathy"),
                         chd_df %>% mutate(outcome = "CHD"),
                         afib_df %>% mutate(outcome = "AFib"),
                         stroke_df %>% mutate(outcome = "Stroke"),
                         pad_df %>% mutate(outcome = "PAD"),
                         hfpef_df %>% mutate(outcome = "HFpEF"),
                         hfref_df %>% mutate(outcome = "HFrEF")
) %>% 
  bind_rows(.,
            {.} %>% 
              mutate(subphenotype_category = "Total")) %>% 
  group_by(outcome,subphenotype_category) %>% 
  summarize(events = sum(event),
            censored = n() - sum(event),
            followup = median(t)) %>% 
  ungroup()


coefs_df = read_csv("analysis/decan006_coefficients of all subphenotypes risk of prescriptions and complications.csv") %>% 
  dplyr::filter(str_detect(term,"subphenotype_category")) %>% 
  mutate(coef = exp(estimate),
         lci = exp(estimate - 1.96*std.error),
         uci = exp(estimate + 1.96*std.error)) %>% 
  mutate(coef_ci = paste0(round(coef,2)," (",
                          round(lci,2),", ",
                          round(uci,2),")")) %>% 
  dplyr::select(term,outcome,model,coef_ci) %>% 
  pivot_wider(names_from = model,values_from=coef_ci)  %>% 
  mutate(subphenotype_category = str_replace(term,"subphenotype_category","")) %>% 
  right_join(descriptives ,
            by = c("outcome","subphenotype_category")) %>% 
  dplyr::select(outcome,subphenotype_category,events,censored,followup,s0,s1) %>% 
  mutate(excluded = nrow(phenotyping_df) - events-censored) %>% 
  mutate(across(one_of(c("s0","s1")), function(x) case_when(is.na(x) ~ "1.00",
                                                         TRUE ~ x))) %>% 
  mutate(subphenotype_category = factor(subphenotype_category,levels=c("Total","MOD","Not_Classified","MARD","SIDD"))) %>% 
  arrange(outcome,subphenotype_category)


write_csv(coefs_df,"paper/table_sensitivity all subphenotypes coefficients of survival analysis.csv")


(coefs_df %>% 
  dplyr::select(subphenotype_category,s1,outcome) %>% 
  pivot_wider(names_from=subphenotype_category,values_from=s1) %>% 
  dplyr::select(outcome,MOD, SIDD, MARD, Not_Classified) %>% 
  write_csv(.,"paper/table_adjusted hazard ratios all subphenotypes coefficients of survival analysis.csv")) %>% 
  View()


