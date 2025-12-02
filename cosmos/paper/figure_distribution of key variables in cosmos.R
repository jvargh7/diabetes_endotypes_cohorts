rm(list=ls());gc();source(".Rprofile")

# source("analysis/decan_analytic sample for sidd classification.R")
# rm(predictors_after,predictors_before)
# phenotyping_df_imputed <- readRDS(paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan002/imputed phenotyping dataset.rds")) %>% 
source("analysis/decan002c_imputed phenotyping dataset processing.R")

source("analysis/decan002d_phenotyping descriptives dataset.R")


phenotyping_df = descriptives_df %>% 
  mutate(subphenotype_category = factor(subphenotype_category,
                                        levels=c("MOD","SIDD","MARD","Unclassified"),
                                        labels=c("MOD","SIDD","MARD","Mixed")))

library(ggplot2)

fig_A = phenotyping_df %>% 
  ggplot(data=.,aes(x=subphenotype_category,y=Hba1c,fill=subphenotype_category)) +
  geom_boxplot(position = position_dodge(width=0.9)) +
  xlab("") +
  ylab("HbA1c (%)") +
  scale_y_continuous(limits=c(0,20),breaks=seq(0,20,by=5)) +
  theme_bw() +
  scale_fill_manual(name="",values=cluster_colors_cosmos_all2) +
  theme(axis.text = element_text(size = 14),
        legend.text = element_text(size = 14),
        axis.title = element_text(size = 16))

fig_B = phenotyping_df %>% 
  ggplot(data=.,aes(x=subphenotype_category,y=Bmi,fill=subphenotype_category)) +
  geom_boxplot(position = position_dodge(width=0.9)) +
  xlab("") +
  ylab(bquote('BMI ( kg' /m^2~')')) +
  scale_y_continuous(limits=c(0,60),breaks=seq(0,60,by=10)) +
  theme_bw() +
  scale_fill_manual(name="",values=cluster_colors_cosmos_all2) +
  theme(axis.text = element_text(size = 14),
        legend.text = element_text(size = 14),
        axis.title = element_text(size = 16))

fig_C = phenotyping_df %>% 
  ggplot(data=.,aes(x=subphenotype_category,y=Dmagediag,fill=subphenotype_category)) +
  geom_boxplot(position = position_dodge(width=0.9)) +
  xlab("") +
  ylab("Age (years)") +
  scale_y_continuous(limits=c(0,100),breaks=seq(0,100,by=25)) +
  theme_bw() +
  scale_fill_manual(name="",values=cluster_colors_cosmos_all2) +
  theme(axis.text = element_text(size = 14),
        legend.text = element_text(size = 14),
        axis.title = element_text(size = 16)) 

fig_D = phenotyping_df %>% 
  ggplot(data=.,aes(x=subphenotype_category,y=Sbp,fill=subphenotype_category)) +
  geom_boxplot(position = position_dodge(width=0.9)) +
  xlab("") +
  ylab("Systolic BP (mmHg)") +
  scale_y_continuous(limits=c(0,300),breaks=seq(0,300,by=50)) +
  theme_bw() +
  scale_fill_manual(name="",values=cluster_colors_cosmos_all2) +
  theme(axis.text = element_text(size = 14),
        legend.text = element_text(size = 14),
        axis.title = element_text(size = 16))


fig_E = phenotyping_df %>% 
  ggplot(data=.,aes(x=subphenotype_category,y=Dbp,fill=subphenotype_category)) +
  geom_boxplot(position = position_dodge(width=0.9)) +
  xlab("") +
  ylab("Diastolic BP (mmHg)") +
  scale_y_continuous(limits=c(0,200),breaks=seq(0,200,by=50)) +
  theme_bw() +
  scale_fill_manual(name="",values=cluster_colors_cosmos_all2) +
  theme(axis.text = element_text(size = 14),
        legend.text = element_text(size = 14),
        axis.title = element_text(size = 16))


fig_F = phenotyping_df %>% 
  ggplot(data=.,aes(x=subphenotype_category,y=Ldlc,fill=subphenotype_category)) +
  geom_boxplot(position = position_dodge(width=0.9)) +
  xlab("") +
  ylab("LDL cholesterol (mg/dL)") +
  scale_y_continuous(limits=c(0,600),breaks=seq(0,600,by=100)) +
  theme_bw() +
  scale_fill_manual(name="",values=cluster_colors_cosmos_all2) +
  theme(axis.text = element_text(size = 14),
        legend.text = element_text(size = 14),
        axis.title = element_text(size = 16))



fig_G = phenotyping_df %>% 
  ggplot(data=.,aes(x=subphenotype_category,y=Hdlc,fill=subphenotype_category)) +
  geom_boxplot(position = position_dodge(width=0.9)) +
  xlab("") +
  ylab("HDL cholesterol (mg/dL)") +
  scale_y_continuous(limits=c(0,300),breaks=seq(0,300,by=50)) +
  theme_bw() +
  scale_fill_manual(name="",values=cluster_colors_cosmos_all2) +
  theme(axis.text = element_text(size = 14),
        legend.text = element_text(size = 14),
        axis.title = element_text(size = 16))

fig_H = phenotyping_df %>% 
  ggplot(data=.,aes(x=subphenotype_category,y=Tgl,fill=subphenotype_category)) +
  geom_boxplot(position = position_dodge(width=0.9)) +
  xlab("") +
  ylab("Triglycerides (mg/dL)") +
  scale_y_continuous(limits=c(0,600),breaks=seq(0,600,by=100)) +
  theme_bw() +
  scale_fill_manual(name="",values=cluster_colors_cosmos_all2) +
  theme(axis.text = element_text(size = 14),
        legend.text = element_text(size = 14),
        axis.title = element_text(size = 16))

fig_I = phenotyping_df %>% 
  ggplot(data=.,aes(x=subphenotype_category,y=Ratio_th,fill=subphenotype_category)) +
  geom_boxplot(position = position_dodge(width=0.9)) +
  xlab("") +
  ylab("Triglycerides-to-HDL") +
  scale_y_continuous(limits=c(0,10),breaks=seq(0,10,by=2)) +
  theme_bw() +
  scale_fill_manual(name="",values=cluster_colors_cosmos_all2) +
  theme(axis.text = element_text(size = 14),
        legend.text = element_text(size = 14),
        axis.title = element_text(size = 16))


library(ggpubr)

ggarrange(fig_A,
          fig_B,
          fig_C,
          fig_D,
          fig_E,
          fig_F,
          fig_G,
          fig_H,
          fig_I,
          nrow=3,
          ncol=3,
          common.legend = TRUE,legend = "none") %>% 
  ggsave(.,filename=paste0(path_diabetes_endotypes_cosmos_folder,"/figures/distribution of key variables in cosmos.jpg"),width=14,height = 8)


