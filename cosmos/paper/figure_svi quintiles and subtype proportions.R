rm(list=ls());gc();source(".Rprofile")

# source("analysis/decan_analytic sample for sidd classification.R")
# rm(predictors_after,predictors_before)
# phenotyping_df_imputed <- readRDS(paste0(path_diabetes_endotypes_cosmos_folder,"/working/decan002/imputed phenotyping dataset.rds")) %>% 
source("analysis/decan002c_imputed phenotyping dataset processing.R")

source("analysis/decan002d_phenotyping descriptives dataset.R")


phenotyping_df = descriptives_df %>% 
  mutate(subphenotype_category = factor(subphenotype_category,
                                        levels=c("MOD","SIDD","MARD","Unclassified"),
                                        labels=c("MOD","SIDD","MARD","Mixed")),
        svi_quintiles = cut(SviOverallPctlRankByZip2020_X,breaks = c(0,20,40,60,80,100),include.lowest = TRUE,right = TRUE,labels = c("Lowest","Low","Middle","High","Highest")))


figure_df = phenotyping_df %>% 
  group_by(svi_quintiles,subphenotype_category) %>% 
  tally() %>% 
  mutate(p = n*100/sum(n)) %>% 
  ungroup() %>% 
  dplyr::filter(!is.na(svi_quintiles))


fig_out = figure_df %>% 
  dplyr::filter(subphenotype_category != "MARD") %>% 
  ggplot(data=.,aes(x=svi_quintiles,fill=subphenotype_category,y=p)) +
  geom_col(position = position_dodge(width = 0.9)) +
  theme_bw() +
  scale_fill_manual(name="",values=cluster_colors_cosmos_all2) +
  xlab("") +
  ylab("Percentage of Newly Diagnosed T2D (%)") +
  theme(legend.position = "bottom",
        axis.text = element_text(size = 14))

fig_out  %>% 
  ggsave(.,filename=paste0(path_diabetes_endotypes_cosmos_folder,"/figures/svi quintiles and subtype proportions.jpg"),width=6,height = 4)

