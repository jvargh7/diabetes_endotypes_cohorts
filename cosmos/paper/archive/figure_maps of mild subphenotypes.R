rm(list=ls());gc();source(".Rprofile")

# source("analysis/decan_analytic sample for sidd classification.R")
# rm(predictors_after,predictors_before)
source("analysis/decan002c_imputed phenotyping dataset processing.R")

mild_df = read_parquet(paste0(path_diabetes_endotypes_cosmos_folder,"/working/phenotyping dataset.parquet")) %>% 
  left_join(phenotyping_df_imputed %>% 
              dplyr::select(PatientDurableKey,is_sidd,is_mod,is_mard),
            by=c("PatientDurableKey")) %>% 
  mutate(mod_category = case_when(is_sidd >= sidd_cutoff ~ 0,
                                  is_mod >= mod_cutoff ~ 1,
                                  is_mard >= mard_cutoff ~ 0,
                                  TRUE ~ 0),
         mard_category = case_when(is_sidd >= sidd_cutoff ~ 0,
                                  is_mod >= mod_cutoff ~ 0,
                                  is_mard >= mard_cutoff ~ 1,
                                  TRUE ~ 0)
         )
table(mild_df$mod_category,mild_df$mard_category)

mean(mild_df$mod_category)
mean(mild_df$mard_category)

library(tigris)

mod_pct_categories <- c("<20%","20-24.9%","25-29.9%",">=30%")
mard_pct_categories <- c("<35%","35-39.9%","40-44.9%",">=45%")

colors_mod_pct_categories <- c("<20%" = "#FDE3CF","20-24.9%" = "#FBD8C1","25-29.9%" = "#E6A890",">=30%" = "#D0937C")
colors_mard_pct_categories <- c("<35%" = "#F2D0E9","35-39.9%" = "#E1BED9","40-44.9%" = "#BF92B3",">=45%" = "#AE7E9D")

fips = fips_codes %>% 
  as.data.frame() %>% 
  distinct(state,state_code,state_name)

state_proportion <- mild_df %>%
  bind_rows(.,
            {.} %>% 
              mutate(raceeth = 0)) %>% 
  group_by(ValidatedStateOrProvince_X,raceeth) %>% 
  summarize(mod_count = sum(mod_category),
            mard_count = sum(mard_category),
            mod_prop = mean(mod_category),
            mard_prop = mean(mard_category),
            nobs = n()) %>% 
  left_join(fips,
            by=c("ValidatedStateOrProvince_X" = "state_name")) %>% 
  mutate(mod_prop_category = case_when(mod_prop < 0.20 ~ 1,
                                       mod_prop < 0.25 ~ 2,
                                       mod_prop < 0.30 ~ 3,
                                   TRUE ~ 4 ),
         mard_prop_category = case_when(mard_prop < 0.35 ~ 1,
                                        mard_prop < 0.40 ~ 2,
                                        mard_prop < 0.45 ~ 3,
                                        TRUE ~ 4)
         ) %>% 
  mutate(mod_prop_category = factor(mod_prop_category,levels=c(1:4),labels=mod_pct_categories),
         mard_prop_category = factor(mard_prop_category,levels=c(1:4),labels=mard_pct_categories)) %>% 
  mutate(raceeth = factor(raceeth,levels=c(0:4),labels=c("Total","NHWhite","NHBlack","Hispanic","NHOther")))

write_csv(state_proportion,"paper/table_state mild subphenotype distribution.csv")

# state_boundaries <- st_read(dsn = paste0(path_cms_mdpp_folder,"/working/tl_2022_us_state")) 
state_boundaries <- tigris::states(class = "sf", cb = TRUE) %>% 
  tigris::shift_geometry() %>% 
  dplyr::filter(GEOID < 60) %>% 
  left_join(state_proportion %>% 
              mutate(mod_prop_category = case_when(nobs < 50 ~ NA_character_,
                                               TRUE ~ mod_prop_category),
                     mard_prop_category = case_when(nobs < 50 ~ NA_character_,
                                                   TRUE ~ mard_prop_category)
                     ) %>% 
              dplyr::select(state_code,raceeth,mod_prop_category,mard_prop_category) %>% 
              pivot_wider(names_from=raceeth,values_from=c(mod_prop_category,mard_prop_category)) %>% 
              mutate(across(starts_with("mod"),.fns = function(x) factor(x,levels=mod_pct_categories))) %>% 
              mutate(across(starts_with("mard"),.fns = function(x) factor(x,levels=mard_pct_categories))),
            by=c("GEOID"="state_code"))

figA <- ggplot() +
  geom_sf(data=state_boundaries,aes(fill = mod_prop_category_Total),col="black")  +
  # https://stackoverflow.com/questions/66031935/ggplot2-and-sf-geom-sf-text-within-limits-set-by-coord-sf
  # coord_sf(xlim = c(-140,-60), ylim = c(22,50)) +
  coord_sf(crs = 5070, datum = NA) +
  theme_bw() +
  xlab("") +
  ylab("") +
  scale_fill_manual(name="",values=colors_mod_pct_categories,na.value="grey90") +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 14))

figA

figA %>% 
  ggsave(.,filename=paste0(path_diabetes_endotypes_cosmos_folder,"/figures/state mod distribution.jpg"),width=5*1.685,height=5)

figB <- ggplot() +
  geom_sf(data=state_boundaries,aes(fill = mard_prop_category_Total),col="black")  +
  # https://stackoverflow.com/questions/66031935/ggplot2-and-sf-geom-sf-text-within-limits-set-by-coord-sf
  # coord_sf(xlim = c(-140,-60), ylim = c(22,50)) +
  coord_sf(crs = 5070, datum = NA) +
  theme_bw() +
  xlab("") +
  ylab("") +
  scale_fill_manual(name="",values=colors_mard_pct_categories,na.value="grey90") +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 14))

figB

figB %>% 
  ggsave(.,filename=paste0(path_diabetes_endotypes_cosmos_folder,"/figures/state mard distribution.jpg"),width=5*1.685,height=5)


library(ggpubr)

ggarrange(figA,
          figB,
          labels=c("A","B"),legend="bottom",
          nrow=1,ncol=2,common.legend = FALSE) %>% 
  ggsave(.,filename=paste0(path_diabetes_endotypes_cosmos_folder,"/figures/state mild distribution total.jpg"),width=10*1.685,height=6)


