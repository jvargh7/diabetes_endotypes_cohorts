# the purpose of this file is to create adjusted survival curves 
rm(list=ls());gc();source(".Rprofile");source("analysis/decan_survival analysis equations.R")

library(survival)
library(ggsurvfit)


insulin_df = read_parquet(paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan004/insulin dataset.parquet"))
metformin_df = read_parquet(paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan004/metformin dataset.parquet"))
incretin_df = read_parquet(paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan004/incretin dataset.parquet"))

retinopathy_df = read_parquet(paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan004/retinopathy dataset.parquet"))
neuropathy_df = read_parquet(paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan004/neuropathy dataset.parquet"))
nephropathy_df = read_parquet(paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan004/nephropathy dataset.parquet"))

# Added macro vascular outcomes in Nov 2024 (adjusted survival curves are saved in the macrovascular project R folder)
## severe ascvd
nstemi_df <- read_parquet(paste0(path_diabetes_subtypes_macrovascular_folder,"/working/dsman002/nstemi dataset.parquet"))
stemi_df <- read_parquet(paste0(path_diabetes_subtypes_macrovascular_folder,"/working/dsman002/stemi dataset.parquet"))
otheracs_df <- read_parquet(paste0(path_diabetes_subtypes_macrovascular_folder,"/working/dsman002/otheracs dataset.parquet"))
stroke_df <- read_parquet(paste0(path_diabetes_subtypes_macrovascular_folder,"/working/dsman002/stroke dataset.parquet"))

## heart failure
hfpef_df <- read_parquet(paste0(path_diabetes_subtypes_macrovascular_folder,"/working/dsman002/hfpef dataset.parquet"))
hfref_df <- read_parquet(paste0(path_diabetes_subtypes_macrovascular_folder,"/working/dsman002/hfref dataset.parquet"))

## other ascvd
tia_df <- read_parquet(paste0(path_diabetes_subtypes_macrovascular_folder,"/working/dsman002/tia dataset.parquet"))
stableangina_df <- read_parquet(paste0(path_diabetes_subtypes_macrovascular_folder,"/working/dsman002/stableangina dataset.parquet"))
chd_df <- read_parquet(paste0(path_diabetes_subtypes_macrovascular_folder,"/working/dsman002/chd dataset.parquet"))
cardiomyopathy_df <- read_parquet(paste0(path_diabetes_subtypes_macrovascular_folder,"/working/dsman002/cardiomyopathy dataset.parquet"))
afib_df <- read_parquet(paste0(path_diabetes_subtypes_macrovascular_folder,"/working/dsman002/afib dataset.parquet"))
pad_df <- read_parquet(paste0(path_diabetes_subtypes_macrovascular_folder,"/working/dsman002/pad dataset.parquet"))

# create composite variables in the ascvd outcome

## severe ASCVD outcomes 

nstemi_df2 <- nstemi_df %>%
  dplyr::select(PatientDurableKey, t, event, female, Dmagediag,subphenotype_category,sidd_category) %>%
  dplyr::rename(t_nstemi = t, event_nstemi = event)

stemi_df2 <- stemi_df %>%
  dplyr::select(PatientDurableKey, t, event, female, Dmagediag,subphenotype_category,sidd_category) %>%
  dplyr::rename(t_stemi = t, event_stemi = event)

otheracs_df2 <- otheracs_df %>%
  dplyr::select(PatientDurableKey, t, event, female, Dmagediag,subphenotype_category,sidd_category) %>%
  dplyr::rename(t_otheracs = t, event_otheracs = event)

stroke_df2 <- stroke_df %>%
  dplyr::select(PatientDurableKey, t, event, female, Dmagediag,subphenotype_category,sidd_category) %>%
  dplyr::rename(t_stroke = t, event_stroke = event)


sv_ascvd_df <- reduce(list(nstemi_df2, stemi_df2, otheracs_df2, stroke_df2), 
                      function(x, y) merge(x, y, by = c("PatientDurableKey", "female", "Dmagediag","subphenotype_category","sidd_category"), all = TRUE))%>% 
  mutate(
    sv_ascvd = if_else(rowSums(cbind(event_nstemi, event_stemi, event_otheracs, event_stroke), na.rm = TRUE) > 0, 1, 0)
  ) %>% 
  mutate(
    time_to_event_composite = if_else(
      sv_ascvd == 1,  # Check if any severe ASCVD event occurred
      pmin(t_nstemi, t_stemi, t_otheracs, t_stroke, na.rm = TRUE),  # Earliest event time
      pmax(t_nstemi, t_stemi, t_otheracs, t_stroke, na.rm = TRUE)   # Latest censoring time
    )
  )%>% 
  dplyr::rename(t= time_to_event_composite, event =sv_ascvd)

## other ASCVD outcomes 

tia_df2 <- tia_df %>%
  dplyr::select(PatientDurableKey, t, event, female, Dmagediag,subphenotype_category,sidd_category) %>%
  dplyr::rename(t_tia = t, event_tia = event)

chd_df2 <- chd_df %>%
  dplyr::select(PatientDurableKey, t, event, female, Dmagediag,subphenotype_category,sidd_category) %>%
  dplyr::rename(t_chd = t, event_chd = event)

cardiomyopathy_df2 <- cardiomyopathy_df %>%
  dplyr::select(PatientDurableKey, t, event, female, Dmagediag,subphenotype_category,sidd_category) %>%
  dplyr::rename(t_cardiomyopathy = t, event_cardiomyopathy = event)

afib_df2 <- afib_df %>%
  dplyr::select(PatientDurableKey, t, event, female, Dmagediag,subphenotype_category,sidd_category) %>%
  dplyr::rename(t_afib = t, event_afib = event)


stableangina_df2 <- stableangina_df %>%
  dplyr::select(PatientDurableKey, t, event, female, Dmagediag,subphenotype_category,sidd_category) %>%
  dplyr::rename(t_stableangina = t, event_stableangina = event)

pad_df2 <- pad_df %>%
  dplyr::select(PatientDurableKey, t, event, female, Dmagediag,subphenotype_category,sidd_category) %>%
  dplyr::rename(t_pad = t, event_pad = event)



other_ascvd_df <- reduce(list(tia_df2, chd_df2, cardiomyopathy_df2, afib_df2,stableangina_df2,pad_df2), 
                         function(x, y) merge(x, y, by = c("PatientDurableKey", "female", "Dmagediag","subphenotype_category","sidd_category"), all = TRUE))%>% 
  mutate(
    other_ascvd = if_else(rowSums(cbind(event_tia, event_chd, event_cardiomyopathy, event_afib,event_stableangina,event_pad), na.rm = TRUE) > 0, 1, 0)
  ) %>% 
  mutate(
    time_to_event_composite = if_else(
      other_ascvd == 1,  # Check if any severe ASCVD event occurred
      pmin(t_tia, t_chd, t_cardiomyopathy, t_afib,t_stableangina,t_pad, na.rm = TRUE),  # Earliest event time
      pmax(t_tia, t_chd, t_cardiomyopathy, t_afib,t_stableangina,t_pad, na.rm = TRUE)   # Latest censoring time
    )
  )%>% 
  dplyr::rename(t= time_to_event_composite, event =other_ascvd)



# create composite variables in the heart failure outcome 

hfpef_df2 <- hfpef_df %>%
  dplyr::select(PatientDurableKey, t, event, female, Dmagediag,subphenotype_category,sidd_category) %>%
  dplyr::rename(t_hfpef = t, event_hfpef = event)

hfref_df2 <- hfref_df %>%
  dplyr::select(PatientDurableKey, t, event, female, Dmagediag,subphenotype_category,sidd_category) %>%
  dplyr::rename(t_hfref = t, event_hfref = event)

hf_df <- reduce(list(hfref_df2,hfpef_df2), 
                function(x, y) merge(x, y, by = c("PatientDurableKey", "female", "Dmagediag","subphenotype_category","sidd_category"), all = TRUE)) %>% 
  mutate(
    any_hf = if_else(rowSums(cbind(event_hfpef, event_hfref), na.rm = TRUE) > 0, 1, 0)
  ) %>% 
  mutate(
    time_to_event_composite = if_else(
      any_hf == 1,
      pmin(t_hfref,t_hfpef, na.rm = TRUE),  # Earliest event time
      pmax(t_hfref,t_hfpef, na.rm = TRUE)   # Latest censoring time
    )
  ) %>% 
  dplyr::rename(t= time_to_event_composite, event =any_hf)

# PRESCRIPTION ------------


insulin1 = coxph(as.formula(f2),data=insulin_df)

insulin_cur <- survfit2(insulin1)

figA1 = ggsurvfit(insulin_cur,type = "risk") +
  xlab("Time to insulin (months)") +
  ylab("Event (proportion)") +
  add_confidence_interval() +
  theme_bw() +
  
  scale_color_manual(name="",values=cluster_colors_cosmos) +
  scale_fill_manual(name="",values=cluster_colors_cosmos) +
  theme(axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.position = c(0.2,0.8), 
        axis.title = element_text(size = 12)) + 
  xlim(0,60)

figA1 <- figA1 +
  scale_x_continuous(breaks = seq(0, 50, by = 10), limits = c(0, 60))+
  add_risktable()

print(figA1)

metformin1 = coxph(as.formula(f2),data=metformin_df) %>% survfit2()
figB1 = ggsurvfit(metformin1,type = "risk") +
  xlab("Time to metformin (months)") +
  ylab("Event (proportion)") +
  add_confidence_interval() +
  theme_bw() +
  
  scale_color_manual(name="",values=cluster_colors_cosmos) +
  scale_fill_manual(name="",values=cluster_colors_cosmos) +
  theme(axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.position = "none", 
        axis.title = element_text(size = 12))

figB1 <- figB1 +
  scale_x_continuous(breaks = seq(0, 50, by = 10), limits = c(0, 60))+
  add_risktable()

print(figB1)


incretin1 = coxph(as.formula(f2),data=incretin_df) %>% survfit2()
figC1 = ggsurvfit(incretin1,type = "risk") +
  xlab("Time to GLP1-RA or \nGLP1-RA/GIP (months)") +
  ylab("Event (proportion)") +
  add_confidence_interval() +
  theme_bw() +
  scale_color_manual(name="",values=cluster_colors_cosmos) +
  scale_fill_manual(name="",values=cluster_colors_cosmos) +
  theme(axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.position = "none", 
        axis.title = element_text(size = 12))


figC1 <- figC1 +
  scale_x_continuous(breaks = seq(0, 50, by = 10), limits = c(0, 60))+
  add_risktable()


# COMPLICATIONS - microvascular ---------

retinopathy1 = coxph(as.formula(f2),data=retinopathy_df) %>% survfit2()

figA2 =ggsurvfit(retinopathy1,type = "risk") +
  xlab("Time to retinopathy (months)") +
  ylab("Event (proportion)") +
  add_confidence_interval() +
  theme_bw() +
  
  scale_color_manual(name="",values=cluster_colors_cosmos) +
  scale_fill_manual(name="",values=cluster_colors_cosmos) +
  theme(axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.position = "none", 
        axis.title = element_text(size = 12))

figA2 <- figA2 +
  scale_x_continuous(breaks = seq(0, 100, by = 20), limits = c(0, 120))+
  add_risktable()

neuropathy1 = coxph(as.formula(f2),data=neuropathy_df) %>% survfit2()
figB2 = ggsurvfit(neuropathy1,type = "risk") +
  xlab("Time to neuropathy (months)") +
  ylab("Event (proportion)") +
  add_confidence_interval() +
  theme_bw() +
  
  scale_color_manual(name="",values=cluster_colors_cosmos) +
  scale_fill_manual(name="",values=cluster_colors_cosmos) +
  theme(axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.position = "none", 
        axis.title = element_text(size = 12))


figB2 <- figB2 +
  scale_x_continuous(breaks = seq(0, 100, by = 20), limits = c(0, 120))+
  add_risktable()

nephropathy1 = coxph(as.formula(f2),data=nephropathy_df) %>% survfit2()
figC2 = ggsurvfit(nephropathy1,type = "risk") +
  xlab("Time to nephropathy (months)") +
  ylab("Event (proportion)") +
  add_confidence_interval() +
  theme_bw() +
  scale_color_manual(name="",values=cluster_colors_cosmos) +
  scale_fill_manual(name="",values=cluster_colors_cosmos) +
  theme(axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.position = "none", 
        axis.title = element_text(size = 12))

figC2 <- figC2 +
  scale_x_continuous(breaks = seq(0, 100, by = 20), limits = c(0, 120))+
  add_risktable()


# COMPLICATIONS - macrovascular

ascvd1 = coxph(as.formula(f2),data=sv_ascvd_df) %>% survfit2()
figA3 = ggsurvfit(ascvd1,type = "risk") +
  xlab("Time to Severe ASCVD (months)") +
  ylab("Event (proportion)") +
  add_confidence_interval() +
  theme_bw() +
  scale_color_manual(name="",values=cluster_colors_cosmos) +
  scale_fill_manual(name="",values=cluster_colors_cosmos) +
  theme(axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.position = "none", 
        axis.title = element_text(size = 12))

figA3 <- figA3 +
  scale_x_continuous(breaks = seq(0, 100, by = 20), limits = c(0, 120))+
  add_risktable()


other_ascvd1 = coxph(as.formula(f2),data=other_ascvd_df) %>% survfit2()
figB3 = ggsurvfit(other_ascvd1,type = "risk") +
  xlab("Time to Other ASCVD (months)") +
  ylab("Event (proportion)") +
  add_confidence_interval() +
  theme_bw() +
  scale_color_manual(name="",values=cluster_colors_cosmos) +
  scale_fill_manual(name="",values=cluster_colors_cosmos) +
  theme(axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.position = "none", 
        axis.title = element_text(size = 12))

figB3 <- figB3 +
  scale_x_continuous(breaks = seq(0, 100, by = 20), limits = c(0, 120))+
  add_risktable()



hf1 = coxph(as.formula(f2),data=hf_df) %>% survfit2()
figC3 = ggsurvfit(hf1,type = "risk") +
  xlab("Time to Heart Failure (months)") +
  ylab("Event (proportion)") +
  add_confidence_interval() +
  theme_bw() +
  scale_color_manual(name="",values=cluster_colors_cosmos) +
  scale_fill_manual(name="",values=cluster_colors_cosmos) +
  theme(axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.position = "none", 
        axis.title = element_text(size = 12))

figC3 <- figC3 +
  scale_x_continuous(breaks = seq(0, 100, by = 20), limits = c(0, 120))+
  add_risktable()


library(ggpubr)
ggarrange(figA1,
          figB1,
          figC1,
          figA2,
          figB2,
          figC2,
          figA3,
          figB3,
          figC3,
          nrow = 3,
          common.legend = TRUE,
          legend = "bottom",
          ncol = 3) %>% print()


library(ggpubr)
ggarrange(figA1,
          figB1,
          figC1,
          figA2,
          figB2,
          figC2,
          nrow = 2,
          common.legend = TRUE,
          legend = "bottom",
          ncol = 3) %>% 
  ggsave(.,filename=paste0(path_diabetes_endotypes_cosmos_folder,"/figures/adjusted survival curves of prescriptions and complications_SIDD_non_SIDD.jpg"),width=12,height = 8)

