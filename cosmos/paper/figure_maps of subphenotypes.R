rm(list=ls());gc();source(".Rprofile")

# source("analysis/decan_analytic sample for sidd classification.R")
# rm(predictors_after,predictors_before)
source("analysis/decan002c_imputed phenotyping dataset processing.R")



map_df = read_parquet(paste0(path_diabetes_endotypes_cosmos_folder,"/working/phenotyping dataset.parquet")) %>% 
  left_join(phenotyping_df_imputed %>% 
              dplyr::select(PatientDurableKey,is_sidd,is_mod,is_mard),
            by=c("PatientDurableKey")) %>% 
  mutate(sidd_category = case_when(is_sidd >= sidd_cutoff ~ 1,
                                   TRUE ~ 0),
         mod_category = case_when(is_sidd >= sidd_cutoff ~ 0,
                                           is_mard >= mard_cutoff ~ 0,
                                           is_mod >= mod_cutoff ~ 1,
                                           TRUE ~ 0),
         mard_category = case_when(is_sidd >= sidd_cutoff ~ 0,
                                   is_mard >= mard_cutoff ~ 1,
                                   is_mod >= mod_cutoff ~ 0,
                                   TRUE ~ 0)
         )

table(map_df$mod_category,map_df$mard_category)

mean(map_df$sidd_category)
mean(map_df$mod_category)
mean(map_df$mard_category)

library(tigris)

sidd_pct_categories <- c("<15%","15-19.9%","20-24.9%",">=25%")
mod_pct_categories <- c("<20%","20-24.9%","25-29.9%",">=30%")
mard_pct_categories <- c("<35%","35-39.9%","40-44.9%",">=45%")

colors_sidd_pct_categories <- c("<15%" = "#C1DDF3","15-19.9%" = "#AAD8EB","20-24.9%" = "#A3BDED",">=25%" = "#8792AE")
colors_mod_pct_categories <- c("<20%" = "#FDE3CF","20-24.9%" = "#FBD8C1","25-29.9%" = "#E6A890",">=30%" = "#D0937C")
colors_mard_pct_categories <- c("<35%" = "#F2D0E9","35-39.9%" = "#E1BED9","40-44.9%" = "#BF92B3",">=45%" = "#AE7E9D")

fips = fips_codes %>% 
  as.data.frame() %>% 
  distinct(state,state_code,state_name)

state_proportion <- map_df %>%
  bind_rows(.,
            {.} %>% 
              mutate(raceeth = 0)) %>% 
  group_by(ValidatedStateOrProvince_X,raceeth) %>% 
  summarize(sidd_count = sum(sidd_category),
            mod_count = sum(mod_category),
            mard_count = sum(mard_category),
            sidd_prop = mean(sidd_category),
            mod_prop = mean(mod_category),
            mard_prop = mean(mard_category),
            nobs = n()) %>% 
  left_join(fips,
            by=c("ValidatedStateOrProvince_X" = "state_name")) %>% 
  mutate(sidd_prop_category = case_when(sidd_prop < 0.15 ~ 1,
                                       sidd_prop < 0.20 ~ 2,
                                       sidd_prop < 0.25 ~ 3,
                                       TRUE ~ 4),
         
         mod_prop_category = case_when(mod_prop < 0.20 ~ 1,
                                       mod_prop < 0.25 ~ 2,
                                       mod_prop < 0.30 ~ 3,
                                   TRUE ~ 4 ),
         mard_prop_category = case_when(mard_prop < 0.35 ~ 1,
                                        mard_prop < 0.40 ~ 2,
                                        mard_prop < 0.45 ~ 3,
                                        TRUE ~ 4)
         ) %>% 
  mutate(sidd_prop_category = factor(sidd_prop_category,levels=c(1:4),labels=sidd_pct_categories),
         mod_prop_category = factor(mod_prop_category,levels=c(1:4),labels=mod_pct_categories),
         mard_prop_category = factor(mard_prop_category,levels=c(1:4),labels=mard_pct_categories)) %>% 
  mutate(raceeth = factor(raceeth,levels=c(0:4),labels=c("Total","NHWhite","NHBlack","Hispanic","NHOther")))

write_csv(state_proportion,"paper/table_state subphenotype distribution.csv")

state_counts <- map_df %>% 
  distinct(PatientDurableKey,ValidatedStateOrProvince_X) %>% 
  group_by(ValidatedStateOrProvince_X) %>% 
  tally() %>% 
  left_join(fips,
            by=c("ValidatedStateOrProvince_X" = "state_name")) %>% 
  mutate(n_category = case_when(n < 100 ~ 1,
                                n < 1000 ~ 2,
                                n < 5000 ~ 3,
                                TRUE ~ 4 )) %>% 
  mutate(n_category = factor(n_category,levels=c(1:4),labels=c("<1000","1000-4999","5000-9999",">=10000")))
write_csv(state_counts,"paper/table_state counts of newly diagnosed T2DM.csv")



# state_boundaries <- st_read(dsn = paste0(path_cms_mdpp_folder,"/working/tl_2022_us_state")) 
state_boundaries <- tigris::states(class = "sf", cb = TRUE) %>% 
  tigris::shift_geometry() %>% 
  dplyr::filter(GEOID < 60) %>% 
  left_join(state_proportion %>% 
              mutate(sidd_prop_category = case_when(nobs < 50 ~ NA_character_,
                                                   TRUE ~ sidd_prop_category),
                     mod_prop_category = case_when(nobs < 50 ~ NA_character_,
                                               TRUE ~ mod_prop_category),
                     mard_prop_category = case_when(nobs < 50 ~ NA_character_,
                                                   TRUE ~ mard_prop_category)
                     ) %>% 
              dplyr::select(state_code,raceeth,sidd_prop_category,mod_prop_category,mard_prop_category) %>% 
              pivot_wider(names_from=raceeth,values_from=c(sidd_prop_category,mod_prop_category,mard_prop_category)) %>% 
              mutate(across(starts_with("sidd"),.fns = function(x) factor(x,levels=sidd_pct_categories))) %>% 
              mutate(across(starts_with("mod"),.fns = function(x) factor(x,levels=mod_pct_categories))) %>% 
              mutate(across(starts_with("mard"),.fns = function(x) factor(x,levels=mard_pct_categories))) %>% 
              left_join(state_counts %>% 
                          dplyr::select(state_code,n_category),
                        by=c("state_code")),
            by=c("GEOID"="state_code"))  

# MAPS ------------

## COUNTS ------------


fig_counts <- ggplot() +
  geom_sf(data=state_boundaries,aes(fill = n_category),col="black")  +
  # https://stackoverflow.com/questions/66031935/ggplot2-and-sf-geom-sf-text-within-limits-set-by-coord-sf
  # coord_sf(xlim = c(-140,-60), ylim = c(22,50)) +
  coord_sf(crs = 5070, datum = NA) +
  theme_bw() +
  xlab("") +
  ylab("") +
  scale_fill_manual(name="",values=c("<1000" = "grey90","1000-4999" = "grey70","5000-9999" = "grey50",">=10000" = "grey30"),na.value="white") +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 14))



## SIDD -----------

figA <- ggplot() +
  geom_sf(data=state_boundaries,aes(fill = sidd_prop_category_Total),col="black")  +
  # https://stackoverflow.com/questions/66031935/ggplot2-and-sf-geom-sf-text-within-limits-set-by-coord-sf
  # coord_sf(xlim = c(-140,-60), ylim = c(22,50)) +
  coord_sf(crs = 5070, datum = NA) +
  theme_bw() +
  xlab("") +
  ylab("") +
  # https://stackoverflow.com/questions/33765710/force-ggplot-legend-to-show-all-categories-when-no-values-are-present
  scale_fill_manual(name="",values=colors_sidd_pct_categories,na.value="grey90",drop=FALSE) +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 14))

figA

figA %>% 
  ggsave(.,filename=paste0(path_diabetes_endotypes_cosmos_folder,"/figures/state sidd distribution.jpg"),width=5*1.685,height=5)


## MARD -------------

figB <- ggplot() +
  geom_sf(data=state_boundaries,aes(fill = mard_prop_category_Total),col="black")  +
  # https://stackoverflow.com/questions/66031935/ggplot2-and-sf-geom-sf-text-within-limits-set-by-coord-sf
  # coord_sf(xlim = c(-140,-60), ylim = c(22,50)) +
  coord_sf(crs = 5070, datum = NA) +
  theme_bw() +
  xlab("") +
  ylab("") +
  scale_fill_manual(name="",values=colors_mard_pct_categories,na.value="grey90",drop=FALSE) +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 14))

figB

figB %>% 
  ggsave(.,filename=paste0(path_diabetes_endotypes_cosmos_folder,"/figures/state mard distribution.jpg"),width=5*1.685,height=5)

## MOD -------------

figC <- ggplot() +
  geom_sf(data=state_boundaries,aes(fill = mod_prop_category_Total),col="black")  +
  # https://stackoverflow.com/questions/66031935/ggplot2-and-sf-geom-sf-text-within-limits-set-by-coord-sf
  # coord_sf(xlim = c(-140,-60), ylim = c(22,50)) +
  coord_sf(crs = 5070, datum = NA) +
  theme_bw() +
  xlab("") +
  ylab("") +
  scale_fill_manual(name="",values=colors_mod_pct_categories,na.value="grey90",drop=FALSE) +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 14))

figC

figC %>% 
  ggsave(.,filename=paste0(path_diabetes_endotypes_cosmos_folder,"/figures/state mod distribution.jpg"),width=5*1.685,height=5)


library(ggpubr)

ggarrange(figA,
          figB,
          figC,

          labels=c("A:SIDD","B:MARD","C:MOD"),
          label.x =0.01,
          label.y=0.95,
          legend="bottom",
          nrow=1,ncol=3,common.legend = FALSE) %>% 
  ggsave(.,filename=paste0(path_diabetes_endotypes_cosmos_folder,"/figures/state distribution of subphenotypes total.jpg"),width=12*1.685,height=5)


ggarrange(fig_counts,
          figA,
          figB,
          figC,
          labels=c("A","B","C","D"),
          legend="bottom",
          nrow=2,ncol=2,common.legend = FALSE) %>% 
  ggsave(.,filename=paste0(path_diabetes_endotypes_cosmos_folder,"/figures/state distribution of subphenotypes total with counts.jpg"),width=12,height=9)


# MAPS: RACE & ETHNICITY -----
## SIDD by Race & Ethnicity -------------

figA1 <- ggplot() +
  geom_sf(data=state_boundaries,aes(fill = sidd_prop_category_NHWhite),col="black")  +
  # https://stackoverflow.com/questions/66031935/ggplot2-and-sf-geom-sf-text-within-limits-set-by-coord-sf
  # coord_sf(xlim = c(-140,-60), ylim = c(22,50)) +
  coord_sf(crs = 5070, datum = NA) +
  theme_bw() +
  xlab("Non-Hispanic White") +
  ylab("") +
  scale_fill_manual(name="",values=colors_sidd_pct_categories,na.value="grey90",drop=FALSE) +
  theme(legend.position = "bottom")

figA2 <- ggplot() +
  geom_sf(data=state_boundaries,aes(fill = sidd_prop_category_NHBlack),col="black")  +
  # https://stackoverflow.com/questions/66031935/ggplot2-and-sf-geom-sf-text-within-limits-set-by-coord-sf
  # coord_sf(xlim = c(-140,-60), ylim = c(22,50)) +
  coord_sf(crs = 5070, datum = NA) +
  theme_bw() +
  xlab("Non-Hispanic Black") +
  ylab("") +
  scale_fill_manual(name="",values=colors_sidd_pct_categories,na.value="grey90",drop=FALSE) +
  theme(legend.position = "bottom")

figA3 <- ggplot() +
  geom_sf(data=state_boundaries,aes(fill = sidd_prop_category_Hispanic),col="black")  +
  # https://stackoverflow.com/questions/66031935/ggplot2-and-sf-geom-sf-text-within-limits-set-by-coord-sf
  # coord_sf(xlim = c(-140,-60), ylim = c(22,50)) +
  coord_sf(crs = 5070, datum = NA) +
  theme_bw() +
  xlab("Hispanic") +
  ylab("") +
  scale_fill_manual(name="",values=colors_sidd_pct_categories,na.value="grey90",drop=FALSE) +
  theme(legend.position = "bottom")

figA4 <- ggplot() +
  geom_sf(data=state_boundaries,aes(fill = sidd_prop_category_NHOther),col="black")  +
  # https://stackoverflow.com/questions/66031935/ggplot2-and-sf-geom-sf-text-within-limits-set-by-coord-sf
  # coord_sf(xlim = c(-140,-60), ylim = c(22,50)) +
  coord_sf(crs = 5070, datum = NA) +
  theme_bw() +
  xlab("Non-Hispanic Other") +
  ylab("") +
  scale_fill_manual(name="",values=colors_sidd_pct_categories,na.value="grey90",drop=FALSE) +
  theme(legend.position = "bottom")


## MARD by Race & Ethnicity -------------

figB1 <- ggplot() +
  geom_sf(data=state_boundaries,aes(fill = mard_prop_category_NHWhite),col="black")  +
  # https://stackoverflow.com/questions/66031935/ggplot2-and-sf-geom-sf-text-within-limits-set-by-coord-sf
  # coord_sf(xlim = c(-140,-60), ylim = c(22,50)) +
  coord_sf(crs = 5070, datum = NA) +
  theme_bw() +
  xlab("Non-Hispanic White") +
  ylab("") +
  scale_fill_manual(name="",values=colors_mard_pct_categories,na.value="grey90",drop=FALSE) +
  theme(legend.position = "bottom")

figB2 <- ggplot() +
  geom_sf(data=state_boundaries,aes(fill = mard_prop_category_NHBlack),col="black")  +
  # https://stackoverflow.com/questions/66031935/ggplot2-and-sf-geom-sf-text-within-limits-set-by-coord-sf
  # coord_sf(xlim = c(-140,-60), ylim = c(22,50)) +
  coord_sf(crs = 5070, datum = NA) +
  theme_bw() +
  xlab("Non-Hispanic Black") +
  ylab("") +
  scale_fill_manual(name="",values=colors_mard_pct_categories,na.value="grey90",drop=FALSE) +
  theme(legend.position = "bottom")

figB3 <- ggplot() +
  geom_sf(data=state_boundaries,aes(fill = mard_prop_category_Hispanic),col="black")  +
  # https://stackoverflow.com/questions/66031935/ggplot2-and-sf-geom-sf-text-within-limits-set-by-coord-sf
  # coord_sf(xlim = c(-140,-60), ylim = c(22,50)) +
  coord_sf(crs = 5070, datum = NA) +
  theme_bw() +
  xlab("Hispanic") +
  ylab("") +
  scale_fill_manual(name="",values=colors_mard_pct_categories,na.value="grey90",drop=FALSE) +
  theme(legend.position = "bottom")

figB4 <- ggplot() +
  geom_sf(data=state_boundaries,aes(fill = mard_prop_category_NHOther),col="black")  +
  # https://stackoverflow.com/questions/66031935/ggplot2-and-sf-geom-sf-text-within-limits-set-by-coord-sf
  # coord_sf(xlim = c(-140,-60), ylim = c(22,50)) +
  coord_sf(crs = 5070, datum = NA) +
  theme_bw() +
  xlab("Non-Hispanic Other") +
  ylab("") +
  scale_fill_manual(name="",values=colors_mard_pct_categories,na.value="grey90",drop=FALSE) +
  theme(legend.position = "bottom")

## MOD by Race & Ethnicity -------------

figC1 <- ggplot() +
  geom_sf(data=state_boundaries,aes(fill = mod_prop_category_NHWhite),col="black")  +
  # https://stackoverflow.com/questions/66031935/ggplot2-and-sf-geom-sf-text-within-limits-set-by-coord-sf
  # coord_sf(xlim = c(-140,-60), ylim = c(22,50)) +
  coord_sf(crs = 5070, datum = NA) +
  theme_bw() +
  xlab("Non-Hispanic White") +
  ylab("") +
  scale_fill_manual(name="",values=colors_mod_pct_categories,na.value="grey90",drop=FALSE) +
  theme(legend.position = "bottom")

figC2 <- ggplot() +
  geom_sf(data=state_boundaries,aes(fill = mod_prop_category_NHBlack),col="black")  +
  # https://stackoverflow.com/questions/66031935/ggplot2-and-sf-geom-sf-text-within-limits-set-by-coord-sf
  # coord_sf(xlim = c(-140,-60), ylim = c(22,50)) +
  coord_sf(crs = 5070, datum = NA) +
  theme_bw() +
  xlab("Non-Hispanic Black") +
  ylab("") +
  scale_fill_manual(name="",values=colors_mod_pct_categories,na.value="grey90",drop=FALSE) +
  theme(legend.position = "bottom")

figC3 <- ggplot() +
  geom_sf(data=state_boundaries,aes(fill = mod_prop_category_Hispanic),col="black")  +
  # https://stackoverflow.com/questions/66031935/ggplot2-and-sf-geom-sf-text-within-limits-set-by-coord-sf
  # coord_sf(xlim = c(-140,-60), ylim = c(22,50)) +
  coord_sf(crs = 5070, datum = NA) +
  theme_bw() +
  xlab("Hispanic") +
  ylab("") +
  scale_fill_manual(name="",values=colors_mod_pct_categories,na.value="grey90",drop=FALSE) +
  theme(legend.position = "bottom")

figC4 <- ggplot() +
  geom_sf(data=state_boundaries,aes(fill = mod_prop_category_NHOther),col="black")  +
  # https://stackoverflow.com/questions/66031935/ggplot2-and-sf-geom-sf-text-within-limits-set-by-coord-sf
  # coord_sf(xlim = c(-140,-60), ylim = c(22,50)) +
  coord_sf(crs = 5070, datum = NA) +
  theme_bw() +
  xlab("Non-Hispanic Other") +
  ylab("") +
  scale_fill_manual(name="",values=colors_mod_pct_categories,na.value="grey90",drop=FALSE) +
  theme(legend.position = "bottom")


ggarrange(ggarrange(figA1,
                    figA2,
                    figA3,
                    figA4,legend = "bottom",common.legend = TRUE,
                    nrow = 4,ncol=1),
          ggarrange(figB1,
                    figB2,
                    figB3,
                    figB4,legend = "bottom",common.legend = TRUE,
                    nrow = 4,ncol=1),
          ggarrange(figC1,
                    figC2,
                    figC3,
                    figC4,legend = "bottom",common.legend = TRUE,
                    nrow = 4,ncol=1),
          labels=c("A:SIDD","B:MARD","C:MOD"),
          legend="none",
          nrow=1,ncol=3) %>% 
  ggsave(.,filename=paste0(path_diabetes_endotypes_cosmos_folder,"/figures/state distribution raceeth.jpg"),width=12,height=6)







