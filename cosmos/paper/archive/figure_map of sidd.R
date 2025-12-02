rm(list=ls());gc();source(".Rprofile")

# source("analysis/decan_analytic sample for sidd classification.R")
# rm(predictors_after,predictors_before)
source("analysis/decan002c_imputed phenotyping dataset processing.R")

sidd_df = read_parquet(paste0(path_diabetes_endotypes_cosmos_folder,"/working/phenotyping dataset.parquet")) %>% 
  left_join(phenotyping_df_imputed %>% 
              dplyr::select(PatientDurableKey,is_sidd),
            by=c("PatientDurableKey")) %>% 
  mutate(sidd_category = case_when(is_sidd >= sidd_cutoff ~ 1,
                                   TRUE ~ 0))
mean(sidd_df$sidd_category)

library(tigris)

pct_categories <- c("<15%","15-17.4%","17.5-19.9%",">=20%")

colors_pct_categories <- c("<15%" = "#C1DDF3","15-17.4%" = "#AAD8EB","17.5-19.9%" = "#A3BDED",">=20%" = "#8792AE")

fips = fips_codes %>% 
  as.data.frame() %>% 
  distinct(state,state_code,state_name)

state_proportion <- sidd_df %>%
  bind_rows(.,
            {.} %>% 
              mutate(raceeth = 0)) %>% 
  group_by(ValidatedStateOrProvince_X,raceeth) %>% 
  summarize(count = sum(sidd_category),
            prop = mean(sidd_category),
            nobs = n()) %>% 
  left_join(fips,
            by=c("ValidatedStateOrProvince_X" = "state_name")) %>% 
  mutate(prop_category = case_when(prop < 0.15 ~ 1,
                                   prop < 0.175 ~ 2,
                                   prop < 0.20 ~ 3,
                                TRUE ~ 4 )) %>% 
  mutate(prop_category = factor(prop_category,levels=c(1:4),labels=pct_categories)) %>% 
  mutate(raceeth = factor(raceeth,levels=c(0:4),labels=c("Total","NHWhite","NHBlack","Hispanic","NHOther")))

write_csv(state_proportion,"paper/table_state sidd distribution.csv")

# state_boundaries <- st_read(dsn = paste0(path_cms_mdpp_folder,"/working/tl_2022_us_state")) 
state_boundaries <- tigris::states(class = "sf", cb = TRUE) %>% 
  tigris::shift_geometry() %>% 
  dplyr::filter(GEOID < 60) %>% 
  left_join(state_proportion %>% 
              mutate(prop_category = case_when(nobs < 50 ~ NA_character_,
                                               TRUE ~ prop_category)) %>% 
              dplyr::select(state_code,raceeth,prop_category) %>% 
              pivot_wider(names_from=raceeth,values_from=prop_category) %>% 
              mutate(across(c("Total","NHWhite","NHBlack","Hispanic","NHOther"),.fns = function(x) factor(x,levels=pct_categories))),
            by=c("GEOID"="state_code"))

figA <- ggplot() +
  geom_sf(data=state_boundaries,aes(fill = Total),col="black")  +
  # https://stackoverflow.com/questions/66031935/ggplot2-and-sf-geom-sf-text-within-limits-set-by-coord-sf
  # coord_sf(xlim = c(-140,-60), ylim = c(22,50)) +
  coord_sf(crs = 5070, datum = NA) +
  theme_bw() +
  xlab("") +
  ylab("") +
  scale_fill_manual(name="",values=colors_pct_categories,na.value="grey90") +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 14))

figA

figA %>% 
  ggsave(.,filename=paste0(path_diabetes_endotypes_cosmos_folder,"/figures/state sidd distribution.jpg"),width=5*1.685,height=5)


figB1 <- ggplot() +
  geom_sf(data=state_boundaries,aes(fill = NHWhite),col="black")  +
  # https://stackoverflow.com/questions/66031935/ggplot2-and-sf-geom-sf-text-within-limits-set-by-coord-sf
  # coord_sf(xlim = c(-140,-60), ylim = c(22,50)) +
  coord_sf(crs = 5070, datum = NA) +
  theme_bw() +
  xlab("") +
  ylab("") +
  scale_fill_manual(name="",values=colors_pct_categories,na.value="grey90") +
  theme(legend.position = "bottom")

figB2 <- ggplot() +
  geom_sf(data=state_boundaries,aes(fill = NHBlack),col="black")  +
  # https://stackoverflow.com/questions/66031935/ggplot2-and-sf-geom-sf-text-within-limits-set-by-coord-sf
  # coord_sf(xlim = c(-140,-60), ylim = c(22,50)) +
  coord_sf(crs = 5070, datum = NA) +
  theme_bw() +
  xlab("") +
  ylab("") +
  scale_fill_manual(name="",values=colors_pct_categories,na.value="grey90") +
  theme(legend.position = "bottom")

figB3 <- ggplot() +
  geom_sf(data=state_boundaries,aes(fill = Hispanic),col="black")  +
  # https://stackoverflow.com/questions/66031935/ggplot2-and-sf-geom-sf-text-within-limits-set-by-coord-sf
  # coord_sf(xlim = c(-140,-60), ylim = c(22,50)) +
  coord_sf(crs = 5070, datum = NA) +
  theme_bw() +
  xlab("") +
  ylab("") +
  scale_fill_manual(name="",values=colors_pct_categories,na.value="grey90") +
  theme(legend.position = "bottom")

figB4 <- ggplot() +
  geom_sf(data=state_boundaries,aes(fill = NHOther),col="black")  +
  # https://stackoverflow.com/questions/66031935/ggplot2-and-sf-geom-sf-text-within-limits-set-by-coord-sf
  # coord_sf(xlim = c(-140,-60), ylim = c(22,50)) +
  coord_sf(crs = 5070, datum = NA) +
  theme_bw() +
  xlab("") +
  ylab("") +
  scale_fill_manual(name="",values=colors_pct_categories,na.value="grey90") +
  theme(legend.position = "bottom")

library(ggpubr)

ggarrange(figA,
          ggarrange(figB1,
                    figB2,
                    figB3,
                    figB4,legend = "none",
                    nrow = 2,ncol=2,labels=c("B","C","D","E")),
          labels=c("A",""),legend="bottom",
          nrow=1,ncol=2,common.legend = TRUE) %>% 
  ggsave(.,filename=paste0(path_diabetes_endotypes_cosmos_folder,"/figures/state sidd distribution total raceeth.jpg"),width=12*1.685,height=6)


