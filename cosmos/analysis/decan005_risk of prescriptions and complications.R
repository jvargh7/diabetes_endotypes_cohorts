rm(list=ls());gc();source(".Rprofile")

library(survival)
library(ggsurvfit)

source("analysis/decan_survival analysis equations.R")

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


# ckd_df <- read_parquet(,paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan004/ckd dataset.parquet")) %>% 
# mutate(sidd_category = factor(sidd_category,levels=c("NonSIDD","SIDD")))
chd_df <- read_parquet(paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan004/chd dataset.parquet")) %>% 
  mutate(sidd_category = factor(sidd_category,levels=c("NonSIDD","SIDD")))
afib_df <- read_parquet(paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan004/afib dataset.parquet")) %>% 
  mutate(sidd_category = factor(sidd_category,levels=c("NonSIDD","SIDD")))
stroke_df <- read_parquet(paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan004/stroke dataset.parquet")) %>% 
  mutate(sidd_category = factor(sidd_category,levels=c("NonSIDD","SIDD")))
pad_df <- read_parquet(paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan004/pad dataset.parquet")) %>% 
  mutate(sidd_category = factor(sidd_category,levels=c("NonSIDD","SIDD")))
hfpef_df <- read_parquet(paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan004/hfpef dataset.parquet")) %>% 
  mutate(sidd_category = factor(sidd_category,levels=c("NonSIDD","SIDD")))
hfref_df <- read_parquet(paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan004/hfref dataset.parquet")) %>% 
  mutate(sidd_category = factor(sidd_category,levels=c("NonSIDD","SIDD")))


any_df = read_parquet(paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan004/any complication dataset.parquet")) %>% 
  mutate(sidd_category = factor(sidd_category,levels=c("NonSIDD","SIDD")))

# MODELS ----------

insulin0 = coxph(as.formula(f0),data=insulin_df)
insulin1 = coxph(as.formula(f1),data=insulin_df)


metformin0 = coxph(as.formula(f0),data=metformin_df)
metformin1 = coxph(as.formula(f1),data=metformin_df)


incretin0 = coxph(as.formula(f0),data=incretin_df)
incretin1 = coxph(as.formula(f1),data=incretin_df)


retinopathy0 = coxph(as.formula(f0),data=retinopathy_df)
retinopathy1 = coxph(as.formula(f1),data=retinopathy_df)


neuropathy0 = coxph(as.formula(f0),data=neuropathy_df)
neuropathy1 = coxph(as.formula(f1),data=neuropathy_df)


nephropathy0 = coxph(as.formula(f0),data=nephropathy_df)
nephropathy1 = coxph(as.formula(f1),data=nephropathy_df)


# ckd0 = coxph(as.formula(f0),data=ckd_df)
# ckd1 = coxph(as.formula(f1),data=ckd_df)

# MACROVASCULAR -------
chd0 = coxph(as.formula(f0),data=chd_df)
chd1 = coxph(as.formula(f1),data=chd_df)

afib0 = coxph(as.formula(f0),data=afib_df)
afib1 = coxph(as.formula(f1),data=afib_df)

stroke0 = coxph(as.formula(f0),data=stroke_df)
stroke1 = coxph(as.formula(f1),data=stroke_df)

pad0 = coxph(as.formula(f0),data=pad_df)
pad1 = coxph(as.formula(f1),data=pad_df)

hfpef0 = coxph(as.formula(f0),data=hfpef_df)
hfpef1 = coxph(as.formula(f1),data=hfpef_df)

hfref0 = coxph(as.formula(f0),data=hfref_df)
hfref1 = coxph(as.formula(f1),data=hfref_df)



bind_rows(
  broom::tidy(insulin0) %>% mutate(model = "F0",outcome = "Insulin"),
  broom::tidy(insulin1) %>% mutate(model = "F1",outcome = "Insulin"),
  
  broom::tidy(metformin0) %>% mutate(model = "F0",outcome = "Metformin"),
  broom::tidy(metformin1) %>% mutate(model = "F1",outcome = "Metformin"),
  
  broom::tidy(incretin0) %>% mutate(model = "F0",outcome = "Incretin"),
  broom::tidy(incretin1) %>% mutate(model = "F1",outcome = "Incretin"),
  
  broom::tidy(retinopathy0) %>% mutate(model = "F0",outcome = "Retinopathy"),
  broom::tidy(retinopathy1) %>% mutate(model = "F1",outcome = "Retinopathy"),
  
  broom::tidy(neuropathy0) %>% mutate(model = "F0",outcome = "Neuropathy"),
  broom::tidy(neuropathy1) %>% mutate(model = "F1",outcome = "Neuropathy"),
  
  broom::tidy(nephropathy0) %>% mutate(model = "F0",outcome = "Nephropathy"),
  broom::tidy(nephropathy1) %>% mutate(model = "F1",outcome = "Nephropathy"),
  
  # broom::tidy(ckd0) %>% mutate(model = "F0",outcome = "CKD"),
  # broom::tidy(ckd1) %>% mutate(model = "F1",outcome = "CKD"),
  
  broom::tidy(chd0) %>% mutate(model = "F0",outcome = "CHD"),
  broom::tidy(chd1) %>% mutate(model = "F1",outcome = "CHD"),
  
  broom::tidy(afib0) %>% mutate(model = "F0",outcome = "AFib"),
  broom::tidy(afib1) %>% mutate(model = "F1",outcome = "AFib"),
  
  broom::tidy(stroke0) %>% mutate(model = "F0",outcome = "Stroke"),
  broom::tidy(stroke1) %>% mutate(model = "F1",outcome = "Stroke"),
  
  broom::tidy(pad0) %>% mutate(model = "F0",outcome = "PAD"),
  broom::tidy(pad1) %>% mutate(model = "F1",outcome = "PAD"),
  
  broom::tidy(hfpef0) %>% mutate(model = "F0",outcome = "HFpEF"),
  broom::tidy(hfpef1) %>% mutate(model = "F1",outcome = "HFpEF"),
  
  broom::tidy(hfref0) %>% mutate(model = "F0",outcome = "HFrEF"),
  broom::tidy(hfref1) %>% mutate(model = "F1",outcome = "HFrEF")
  
  
) %>% 
  write_csv(.,"analysis/decan005_coefficients of risk of prescriptions and complications.csv")


