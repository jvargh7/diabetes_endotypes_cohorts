# the purpose of this file is to create adjusted survival curves 
rm(list=ls());gc();source(".Rprofile");source("analysis/decan_survival analysis equations.R")

library(survival)
library(ggsurvfit)



insulin_df = read_parquet(paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan004/insulin dataset.parquet")) %>% 
  mutate(subphenotype_category = factor(subphenotype_category,
                                        levels=c("MOD","SIDD","MARD","Not_Classified"),
                                        labels=c("MOD","SIDD","MARD","Mixed")))
metformin_df = read_parquet(paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan004/metformin dataset.parquet")) %>% 
  mutate(subphenotype_category = factor(subphenotype_category,
                                        levels=c("MOD","SIDD","MARD","Not_Classified"),
                                        labels=c("MOD","SIDD","MARD","Mixed")))
incretin_df = read_parquet(paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan004/incretin dataset.parquet")) %>% 
  mutate(subphenotype_category = factor(subphenotype_category,
                                        levels=c("MOD","SIDD","MARD","Not_Classified"),
                                        labels=c("MOD","SIDD","MARD","Mixed")))

retinopathy_df = read_parquet(paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan004/retinopathy dataset.parquet")) %>% 
  mutate(subphenotype_category = factor(subphenotype_category,
                                        levels=c("MOD","SIDD","MARD","Not_Classified"),
                                        labels=c("MOD","SIDD","MARD","Mixed")))
neuropathy_df = read_parquet(paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan004/neuropathy dataset.parquet")) %>% 
  mutate(subphenotype_category = factor(subphenotype_category,
                                        levels=c("MOD","SIDD","MARD","Not_Classified"),
                                        labels=c("MOD","SIDD","MARD","Mixed")))
nephropathy_df = read_parquet(paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan004/nephropathy dataset.parquet")) %>% 
  mutate(subphenotype_category = factor(subphenotype_category,
                                        levels=c("MOD","SIDD","MARD","Not_Classified"),
                                        labels=c("MOD","SIDD","MARD","Mixed")))



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

# check duplciations
dups<-sv_ascvd_df %>%
  count(PatientDurableKey) %>%
  dplyr::filter(n > 1)


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

dups<-other_ascvd_df %>%
  count(PatientDurableKey) %>%
  dplyr::filter(n > 1)


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
  dplyr::rename(t= time_to_event_composite, event =any_hf) %>% 
  mutate(subphenotype_category = factor(subphenotype_category,
                                        levels=c("MOD","SIDD","MARD","Unclassified"),
                                        labels=c("MOD","SIDD","MARD","Mixed")))

dups<-hf_df %>%
  count(PatientDurableKey) %>%
  dplyr::filter(n > 1)

# PRESCRIPTION ------------


insulin1 = coxph(as.formula(a1),data=insulin_df)

insulin_cur <- survfit2(insulin1)

figA1 = ggsurvfit(insulin_cur,type = "risk") +
  xlab("Time to insulin (months)") +
  ylab("Event (proportion)") +
  add_confidence_interval() +
  theme_bw() +
  
  scale_color_manual(name="",values=cluster_colors_cosmos_all2) +
  scale_fill_manual(name="",values=cluster_colors_cosmos_all2) +
  theme(axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.position = "none", 
        axis.title = element_text(size = 12),
        panel.grid = element_blank(),
        panel.border = element_blank(),  # Remove all border lines
        axis.line.x = element_line(color = "black"),  # Keep bottom x-axis line
        axis.line.y = element_line(color = "black"))

figA1 <- figA1 +
  scale_x_continuous(breaks = seq(0, 50, by = 10), limits = c(0, 60))+
  add_risktable(risktable_stats = c("n.risk"),size =3,risktable_height = 0.3)

print(figA1)

metformin1 = coxph(as.formula(a1),data=metformin_df) %>% survfit2()
figB1 = ggsurvfit(metformin1,type = "risk") +
  xlab("Time to metformin (months)") +
  ylab("Event (proportion)") +
  add_confidence_interval() +
  theme_bw() +
  
  scale_color_manual(name="",values=cluster_colors_cosmos_all2) +
  scale_fill_manual(name="",values=cluster_colors_cosmos_all2) +
  theme(axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.position = "none", 
        axis.title = element_text(size = 12),
        panel.grid = element_blank(),
        panel.border = element_blank(),  # Remove all border lines
        axis.line.x = element_line(color = "black"),  # Keep bottom x-axis line
        axis.line.y = element_line(color = "black") )

figB1 <- figB1 +
  scale_x_continuous(breaks = seq(0, 50, by = 10), limits = c(0, 60))+
  add_risktable(risktable_stats = c("n.risk"),size =3,risktable_height = 0.3)

print(figB1)


incretin1 = coxph(as.formula(a1),data=incretin_df) %>% survfit2()
figC1 = ggsurvfit(incretin1,type = "risk") +
  xlab("Time to GLP1-RA or \nGLP1-RA/GIP (months)") +
  ylab("Event (proportion)") +
  add_confidence_interval() +
  theme_bw() +
  scale_color_manual(name="",values=cluster_colors_cosmos_all2) +
  scale_fill_manual(name="",values=cluster_colors_cosmos_all2) +
  theme(axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.position = "none", 
        axis.title = element_text(size = 12),
        panel.grid = element_blank(),
        panel.border = element_blank(),  # Remove all border lines
        axis.line.x = element_line(color = "black"),  # Keep bottom x-axis line
        axis.line.y = element_line(color = "black") )


figC1 <- figC1 +
  scale_x_continuous(breaks = seq(0, 50, by = 10), limits = c(0, 60))+
  add_risktable(risktable_stats = c("n.risk"),size =3,risktable_height = 0.3)

print(figC1)

library(ggpubr)
#ggarrange(figA1,
#         figB1,
#         figC1,
#         common.legend = FALSE,
#         labels = c("A", "B", "C"), 
#         ncol = 3) %>% 
  #ggsave(.,filename=paste0(path_diabetes_endotypes_cosmos_folder,"/figures/adjusted survival curves of all subphenotypes prescriptions only.jpg"),width=25,height = 8)


# COMPLICATIONS - Microvascular---------

retinopathy1 = coxph(as.formula(a1),data=retinopathy_df) %>% survfit2()

figA2 =ggsurvfit(retinopathy1,type = "risk") +
  xlab("Time to retinopathy (months)") +
  ylab("Event (proportion)") +
  add_confidence_interval() +
  theme_bw() +
  
  scale_color_manual(name="",values=cluster_colors_cosmos_all2) +
  scale_fill_manual(name="",values=cluster_colors_cosmos_all2) +
  theme(axis.text = element_text(size =12),
        legend.text = element_text(size = 12),
        legend.position = "none", 
        axis.title = element_text(size = 12),
        panel.grid = element_blank(),
        panel.border = element_blank(),  # Remove all border lines
        axis.line.x = element_line(color = "black"),  # Keep bottom x-axis line
        axis.line.y = element_line(color = "black") )

figA2 <- figA2 +
  scale_x_continuous(breaks = seq(0, 100, by = 20), limits = c(0, 120))+
  add_risktable(risktable_stats = c("n.risk"),size =3,risktable_height = 0.3)

print(figA2)

neuropathy1 = coxph(as.formula(a1),data=neuropathy_df) %>% survfit2()
figB2 = ggsurvfit(neuropathy1,type = "risk") +
  xlab("Time to neuropathy (months)") +
  ylab("Event (proportion)") +
  add_confidence_interval() +
  theme_bw() +
  
  scale_color_manual(name="",values=cluster_colors_cosmos_all2) +
  scale_fill_manual(name="",values=cluster_colors_cosmos_all2) +
  theme(axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.position = "none", 
        axis.title = element_text(size = 12),
        panel.grid = element_blank(),
        panel.border = element_blank(),  # Remove all border lines
        axis.line.x = element_line(color = "black"),  # Keep bottom x-axis line
        axis.line.y = element_line(color = "black") )


figB2 <- figB2 +
  scale_x_continuous(breaks = seq(0, 100, by = 20), limits = c(0, 120))+
  add_risktable(risktable_stats = c("n.risk"),size =3,risktable_height = 0.3)

print(figB2)

nephropathy1 = coxph(as.formula(a1),data=nephropathy_df) %>% survfit2()
figC2 = ggsurvfit(nephropathy1,type = "risk") +
  xlab("Time to nephropathy (months)") +
  ylab("Event (proportion)") +
  add_confidence_interval() +
  theme_bw() +
  scale_color_manual(name="",values=cluster_colors_cosmos_all2) +
  scale_fill_manual(name="",values=cluster_colors_cosmos_all2) +
  theme(axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.position = "none", 
        axis.title = element_text(size = 12),
        panel.grid = element_blank(),
        panel.border = element_blank(),  # Remove all border lines
        axis.line.x = element_line(color = "black"),  # Keep bottom x-axis line
        axis.line.y = element_line(color = "black") )

figC2 <- figC2 +
  scale_x_continuous(breaks = seq(0, 100, by = 20), limits = c(0, 120))+
  add_risktable(risktable_stats = c("n.risk"),size =3,risktable_height = 0.3)

print(figC2)


### Macrovascular 


ascvd1 = coxph(as.formula(a1),data=sv_ascvd_df) %>% survfit2()
figA3 = ggsurvfit(ascvd1,type = "risk") +
  xlab("Time to Severe ASCVD (months)") +
  ylab("Event (proportion)") +
  add_confidence_interval() +
  theme_bw() +
  scale_color_manual(name="",values=cluster_colors_cosmos_all2) +
  scale_fill_manual(name="",values=cluster_colors_cosmos_all2) +
  theme(axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.position = "none", 
        axis.title = element_text(size = 12),
        panel.grid = element_blank(),
        panel.border = element_blank(),  # Remove all border lines
        axis.line.x = element_line(color = "black"),  # Keep bottom x-axis line
        axis.line.y = element_line(color = "black") )

figA3<- figA3 +
  scale_x_continuous(breaks = seq(0, 100, by = 20), limits = c(0, 120))+
  add_risktable(risktable_stats = c("n.risk"),size =3,risktable_height = 0.3)

other_ascvd1 = coxph(as.formula(a1),data=other_ascvd_df) %>% survfit2()
figB3 = ggsurvfit(other_ascvd1,type = "risk") +
  xlab("Time to Other ASCVD (months)") +
  ylab("Event (proportion)") +
  add_confidence_interval() +
  theme_bw() +
  scale_color_manual(name="",values=cluster_colors_cosmos_all) +
  scale_fill_manual(name="",values=cluster_colors_cosmos_all) +
  theme(axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.position = "none", 
        axis.title = element_text(size = 12),
        panel.grid = element_blank(),
        panel.border = element_blank(),  # Remove all border lines
        axis.line.x = element_line(color = "black"),  # Keep bottom x-axis line
        axis.line.y = element_line(color = "black") )

figB3<- figB3 +
  scale_x_continuous(breaks = seq(0, 100, by = 20), limits = c(0, 120))+
  add_risktable(risktable_stats = c("n.risk"),size =3,risktable_height = 0.3)

hf1 = coxph(as.formula(a1),data=hf_df) %>% survfit2()
figC3 = ggsurvfit(hf1,type = "risk") +
  xlab("Time to Heart Failure (months)") +
  ylab("Event (proportion)") +
  add_confidence_interval() +
  theme_bw() +
  scale_color_manual(name="",values=cluster_colors_cosmos_all2) +
  scale_fill_manual(name="",values=cluster_colors_cosmos_all2) +
  theme(axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.position = "none", 
        axis.title = element_text(size = 12),
        panel.grid = element_blank(),
          panel.border = element_blank(),  # Remove all border lines
          axis.line.x = element_line(color = "black"),  # Keep bottom x-axis line
          axis.line.y = element_line(color = "black"))

figC3<- figC3 +
  scale_x_continuous(breaks = seq(0, 100, by = 20), limits = c(0, 120))+
  add_risktable(risktable_stats = c("n.risk"),size =3,risktable_height = 0.3)




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

# plot with at risk numbers 
library(ggpubr)
ggarrange(
  ggsurvfit_build(figA1), ggsurvfit_build(figB1), ggsurvfit_build(figC1),
  ggsurvfit_build(figA2), ggsurvfit_build(figB2), ggsurvfit_build(figC2),
  ggsurvfit_build(figA3), ggsurvfit_build(figB3), ggsurvfit_build(figC3),
  ncol = 3, common.legend = TRUE, legend = "bottom",nrow = 3,labels = LETTERS[1:9]
) %>% print() %>% 
  ggsave(.,filename=paste0(path_diabetes_endotypes_cosmos_folder,"/figures/adjusted survival curves.jpg"),width=13,height = 11)






################################# OHTER CODES ######### 
library(ggpubr)
ggarrange(figA2,
          figB2,
          figC2,
          common.legend = FALSE,
          legend = "bottom",
          labels = c("D", "E", "F"), 
          ncol = 3) %>% 
  ggsave(.,filename=paste0(path_diabetes_endotypes_cosmos_folder,"/figures/adjusted survival curves of all subphenotypes microvascular complications only.jpg"),width=25,height = 8)


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
  ggsave(.,filename=paste0(path_diabetes_endotypes_cosmos_folder,"/figures/adjusted survival curves of all subphenotypes prescriptions and complications.jpg"),width=25,height = 8)

library(patchwork)

ggarrange(figA2,
          figB2,
          figC2,
          figA3,
          figB3,
          figC3,
          nrow = 2,
          common.legend = TRUE,
          legend = "bottom",
          labels = LETTERS[1:6], 
          ncol = 3) %>% 
  ggsave(.,filename=paste0(path_diabetes_endotypes_cosmos_folder,"/figures/adjusted survival curves of all subphenotypes micro and macro complications.jpg"),width=12,height = 8)


# MACROVASCULAR -----------------------
chd1 = coxph(as.formula(a1),data=chd_df %>% 
               mutate(subphenotype_category = factor(subphenotype_category,
                                                     levels=c("MOD","SIDD","MARD","Unclassified"),
                                                     labels=c("MOD","SIDD","MARD","Mixed")))) %>% survfit2()
afib1 = coxph(as.formula(a1),data=afib_df %>% 
                mutate(subphenotype_category = factor(subphenotype_category,
                                                      levels=c("MOD","SIDD","MARD","Unclassified"),
                                                      labels=c("MOD","SIDD","MARD","Mixed")))) %>% survfit2()
stroke1 = coxph(as.formula(a1),data=stroke_df %>% 
                  mutate(subphenotype_category = factor(subphenotype_category,
                                                        levels=c("MOD","SIDD","MARD","Unclassified"),
                                                        labels=c("MOD","SIDD","MARD","Mixed")))) %>% survfit2()
pad1 = coxph(as.formula(a1),data=pad_df %>% 
               mutate(subphenotype_category = factor(subphenotype_category,
                                                     levels=c("MOD","SIDD","MARD","Unclassified"),
                                                     labels=c("MOD","SIDD","MARD","Mixed")))) %>% survfit2()
hfpef1 = coxph(as.formula(a1),data=hfpef_df %>% 
                 mutate(subphenotype_category = factor(subphenotype_category,
                                                       levels=c("MOD","SIDD","MARD","Unclassified"),
                                                       labels=c("MOD","SIDD","MARD","Mixed")))) %>% survfit2()
hfref1 = coxph(as.formula(a1),data=hfref_df %>% 
                 mutate(subphenotype_category = factor(subphenotype_category,
                                                       levels=c("MOD","SIDD","MARD","Unclassified"),
                                                       labels=c("MOD","SIDD","MARD","Mixed")))) %>% survfit2()

figM1 =ggsurvfit(chd1,type = "risk") +
  xlab("Time to CHD (months)") +
  ylab("Event (proportion)") +
  add_confidence_interval() +
  theme_bw() +
  
  scale_color_manual(name="",values=cluster_colors_cosmos_all2) +
  scale_fill_manual(name="",values=cluster_colors_cosmos_all2) +
  theme(axis.text = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.position = "none", 
        axis.title = element_text(size = 16))


figM1 <- figM1 +
  scale_x_continuous(breaks = seq(0, 50, by = 10), limits = c(0, 60))+
  add_risktable()

figM2 =ggsurvfit(afib1,type = "risk") +
  xlab("Time to Atrial Fibrillation (months)") +
  ylab("Event (proportion)") +
  add_confidence_interval() +
  theme_bw() +
  
  scale_color_manual(name="",values=cluster_colors_cosmos_all2) +
  scale_fill_manual(name="",values=cluster_colors_cosmos_all2) +
  theme(axis.text = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.position = "none", 
        axis.title = element_text(size = 16))


figM2 <- figM2 +
  scale_x_continuous(breaks = seq(0, 50, by = 10), limits = c(0, 60))+
  add_risktable()


figM3 =ggsurvfit(stroke1,type = "risk") +
  xlab("Time to Stroke (months)") +
  ylab("Event (proportion)") +
  add_confidence_interval() +
  theme_bw() +
  
  scale_color_manual(name="",values=cluster_colors_cosmos_all2) +
  scale_fill_manual(name="",values=cluster_colors_cosmos_all2) +
  theme(axis.text = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.position = "none", 
        axis.title = element_text(size = 16))


figM3 <- figM3 +
  scale_x_continuous(breaks = seq(0, 50, by = 10), limits = c(0, 60))+
  add_risktable()


figM4 =ggsurvfit(pad1,type = "risk") +
  xlab("Time to Peripheral \nArtery Disease (months)") +
  ylab("Event (proportion)") +
  add_confidence_interval() +
  theme_bw() +
  
  scale_color_manual(name="",values=cluster_colors_cosmos_all2) +
  scale_fill_manual(name="",values=cluster_colors_cosmos_all2) +
  theme(axis.text = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.position = "none", 
        axis.title = element_text(size = 16))


figM4 <- figM4 +
  scale_x_continuous(breaks = seq(0, 50, by = 10), limits = c(0, 60))+
  add_risktable()

figM5 =ggsurvfit(hfpef1,type = "risk") +
  xlab("Time to HFpEF (months)") +
  ylab("Event (proportion)") +
  add_confidence_interval() +
  theme_bw() +
  
  scale_color_manual(name="",values=cluster_colors_cosmos_all2) +
  scale_fill_manual(name="",values=cluster_colors_cosmos_all2) +
  theme(axis.text = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.position = "none", 
        axis.title = element_text(size = 16))


figM5 <- figM5 +
  scale_x_continuous(breaks = seq(0, 50, by = 10), limits = c(0, 60))+
  add_risktable()

figM6 =ggsurvfit(hfref1,type = "risk") +
  xlab("Time to HFrEF (months)") +
  ylab("Event (proportion)") +
  add_confidence_interval() +
  theme_bw() +
  
  scale_color_manual(name="",values=cluster_colors_cosmos_all2) +
  scale_fill_manual(name="",values=cluster_colors_cosmos_all2) +
  theme(axis.text = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.position = "none", 
        axis.title = element_text(size = 16))


figM6 <- figM6 +
  scale_x_continuous(breaks = seq(0, 50, by = 10), limits = c(0, 60))+
  add_risktable()

library(ggpubr)
ggarrange(figM1,
          figM2,
          figM3,
          figM4,
          figM5,
          figM6,
          nrow = 2,
          common.legend = TRUE,
          legend = "bottom",
          ncol = 3) %>% 
  ggsave(.,filename=paste0(path_diabetes_endotypes_cosmos_folder,"/figures/adjusted survival curves of all subphenotypes macrovascular complications.jpg"),width=12,height = 8)

