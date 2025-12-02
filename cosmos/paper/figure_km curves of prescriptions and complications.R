rm(list=ls());gc();source(".Rprofile")

library(survival)
library(ggsurvfit)


insulin_df = read_parquet(paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan004/insulin dataset.parquet"))
metformin_df = read_parquet(paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan004/metformin dataset.parquet"))
incretin_df = read_parquet(paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan004/incretin dataset.parquet"))

retinopathy_df = read_parquet(paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan004/retinopathy dataset.parquet"))
neuropathy_df = read_parquet(paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan004/neuropathy dataset.parquet"))
nephropathy_df = read_parquet(paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan004/nephropathy dataset.parquet"))


# Added macro vascular outcomes in Nov 2024 (severe ASCVD, Heart failure, and other ASCVD)
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

# create composite variables in the ASCVD outcomes 

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
  dplyr::rename(t= time_to_event_composite, event =sv_ascvd)%>% 
  mutate(subphenotype_category = factor(subphenotype_category,
                                        levels=c("MOD","SIDD","MARD","Unclassified"),
                                        labels=c("MOD","SIDD","MARD","Mixed")))

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
  dplyr::rename(t= time_to_event_composite, event =other_ascvd)%>% 
  mutate(subphenotype_category = factor(subphenotype_category,
                                        levels=c("MOD","SIDD","MARD","Unclassified"),
                                        labels=c("MOD","SIDD","MARD","Mixed")))


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
  dplyr::rename(t= time_to_event_composite, event =any_hf)%>% 
  mutate(subphenotype_category = factor(subphenotype_category,
                                        levels=c("MOD","SIDD","MARD","Unclassified"),
                                        labels=c("MOD","SIDD","MARD","Mixed")))



# PRESCRIPTION ------------

figA1 = survfit2(Surv(t,event) ~ sidd_category,
                 data=insulin_df )  %>% 
  ggsurvfit(.,type="risk") +
  xlab("Time to insulin (months)") +
  ylab("Event (proportion)") +
  add_confidence_interval() +
  theme_bw() +
  
  scale_color_manual(name="",values=cluster_colors_cosmos) +
  scale_fill_manual(name="",values=cluster_colors_cosmos) +
  theme(axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        axis.title = element_text(size = 12))

figB1 = survfit2(Surv(t,event) ~ sidd_category,
                 data=metformin_df )  %>% 
  ggsurvfit(.,type="risk") +
  xlab("Time to metformin (months)") +
  ylab("Event (proportion)") +
  add_confidence_interval() +
  theme_bw() +
  
  scale_color_manual(name="",values=cluster_colors_cosmos) +
  scale_fill_manual(name="",values=cluster_colors_cosmos) +
  theme(axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        axis.title = element_text(size = 12))

figC1 = survfit2(Surv(t,event) ~ sidd_category,
                 data=incretin_df )  %>% 
  ggsurvfit(.,type="risk") +
  xlab("Time to GLP1-RA or \nGLP1-RA/GIP (months)") +
  ylab("Event (proportion)") +
  add_confidence_interval() +
  theme_bw() +
  scale_color_manual(name="",values=cluster_colors_cosmos) +
  scale_fill_manual(name="",values=cluster_colors_cosmos) +
  theme(axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        axis.title = element_text(size = 12))



# COMPLICATIONS - microvascular ---------
figA2 = survfit2(Surv(t,event) ~ sidd_category,
               data=retinopathy_df )  %>% 
  ggsurvfit(.,type="risk") +
  xlab("Time to retinopathy (months)") +
  ylab("Event (proportion)") +
  add_confidence_interval() +
  theme_bw() +
  
  scale_color_manual(name="",values=cluster_colors_cosmos) +
  scale_fill_manual(name="",values=cluster_colors_cosmos) +
  theme(axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        axis.title = element_text(size = 12))

figB2 = survfit2(Surv(t,event) ~ sidd_category,
                data=neuropathy_df )  %>% 
  ggsurvfit(.,type="risk") +
  xlab("Time to neuropathy (months)") +
  ylab("Event (proportion)") +
  add_confidence_interval() +
  theme_bw() +
  
  scale_color_manual(name="",values=cluster_colors_cosmos) +
  scale_fill_manual(name="",values=cluster_colors_cosmos) +
  theme(axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        axis.title = element_text(size = 12))

figC2 = survfit2(Surv(t,event) ~ sidd_category,
                data=nephropathy_df )  %>% 
  ggsurvfit(.,type="risk") +
  xlab("Time to nephropathy (months)") +
  ylab("Event (proportion)") +
  add_confidence_interval() +
  theme_bw() +
  scale_color_manual(name="",values=cluster_colors_cosmos) +
  scale_fill_manual(name="",values=cluster_colors_cosmos) +
  theme(axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        axis.title = element_text(size = 12))

# COMPLICATIONS - marco vascular ---------

figA3 = survfit2(Surv(t,event) ~ sidd_category,
                 data=sv_ascvd_df )  %>% 
  ggsurvfit(.,type="risk") +
  xlab("Time to Severe ASCVD (months)") +
  ylab("Event (proportion)") +
  add_confidence_interval() +
  theme_bw() +
  scale_color_manual(name="",values=cluster_colors_cosmos) +
  scale_fill_manual(name="",values=cluster_colors_cosmos) +
  theme(axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        axis.title = element_text(size = 12))
  

figB3 = survfit2(Surv(t,event) ~ sidd_category,
                 data= other_ascvd_df )  %>% 
  ggsurvfit(.,type="risk") +
  xlab("Time to Other ASCVD (months)") +
  ylab("Event (proportion)") +
  add_confidence_interval() +
  theme_bw() +
  scale_color_manual(name="",values=cluster_colors_cosmos) +
  scale_fill_manual(name="",values=cluster_colors_cosmos) +
  theme(axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        axis.title = element_text(size = 12))
      
               
        
figC3 = survfit2(Surv(t,event) ~ sidd_category,
                 data=hf_df )  %>% 
  ggsurvfit(.,type="risk") +
  xlab("Time to Heart Failure (months)") +
  ylab("Event (proportion)") +
  add_confidence_interval() +
  theme_bw() +
  scale_color_manual(name="",values=cluster_colors_cosmos) +
  scale_fill_manual(name="",values=cluster_colors_cosmos) +
  theme(axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        axis.title = element_text(size = 12))


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
  ggsave(.,filename=paste0(path_diabetes_endotypes_cosmos_folder,"/figures/km curves of prescriptions and complications.jpg"),width=12,height = 8)

ggsave(
  print(ggarrange(figA1, figB1, figC1, figA2, figB2, figC2,
                  nrow = 2, common.legend = TRUE, legend = "bottom", ncol = 3)),
  filename = paste0(path_diabetes_endotypes_cosmos_folder, "/figures/km_curves_of_prescriptions_and_complications.jpg"),
  width = 12,
  height = 8
)

