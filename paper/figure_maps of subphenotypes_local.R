rm(list=ls());gc();source(".Rprofile")


state_proportion = read_csv(paste0(path_endotypes_folder,"/working/cosmos/paper/table_state subphenotype distribution_TRANSFER.csv")) %>% 
  mutate(state_code = sprintf("%02d",state_code))%>%
  mutate(
    sidd_prop_category = gsub(">=", "≥", sidd_prop_category),
    mod_prop_category  = gsub(">=", "≥", mod_prop_category),
    mard_prop_category = gsub(">=", "≥", mard_prop_category)
  )
state_counts = read_csv(paste0(path_endotypes_folder,"/working/cosmos/paper/table_state counts of newly diagnosed T2DM.csv"))


sidd_pct_categories <- c("<15%","15-19.9%","20-24.9%","≥25%")
mod_pct_categories <- c("<20%","20-24.9%","25-29.9%","≥30%")
mard_pct_categories <- c("<35%","35-39.9%","40-44.9%","≥45%")

colors_sidd_pct_categories <- c("<15%" = "#C1DDF3","15-19.9%" = "#AAD8EB","20-24.9%" = "#A3BDED","≥25%" = "#8792AE")
colors_mod_pct_categories <- c("<20%" = "#FDE3CF","20-24.9%" = "#FBD8C1","25-29.9%" = "#E6A890","≥30%" = "#D0937C")
colors_mard_pct_categories <- c("<35%" = "#F2D0E9","35-39.9%" = "#E1BED9","40-44.9%" = "#BF92B3","≥45%" = "#AE7E9D")

state_boundaries <- tigris::states(class = "sf", cb = TRUE) %>% 
  tigris::shift_geometry() %>% 
  dplyr::filter(GEOID < 60) %>% 
  left_join(state_proportion %>% 
              # mutate(sidd_prop_category = case_when(nobs < 50 ~ NA_character_,
              #                                       TRUE ~ sidd_prop_category),
              #        mod_prop_category = case_when(nobs < 50 ~ NA_character_,
              #                                      TRUE ~ mod_prop_category),
              #        mard_prop_category = case_when(nobs < 50 ~ NA_character_,
              #                                       TRUE ~ mard_prop_category)
              # ) %>% 
              dplyr::select(state_code,raceeth,sidd_prop_category,mod_prop_category,mard_prop_category) %>% 
              pivot_wider(names_from=raceeth,values_from=c(sidd_prop_category,mod_prop_category,mard_prop_category)) %>% 
              mutate(across(starts_with("sidd"),.fns = function(x) factor(x,levels=sidd_pct_categories))) %>% 
              mutate(across(starts_with("mod"),.fns = function(x) factor(x,levels=mod_pct_categories))) %>% 
              mutate(across(starts_with("mard"),.fns = function(x) factor(x,levels=mard_pct_categories))) %>% 
              left_join(state_counts %>% 
                          dplyr::select(state_code,n_category),
                        by=c("state_code")),
            by=c("GEOID"="state_code"))  


## SIDD -----------

figA <- ggplot() +
  geom_sf(data=state_boundaries,aes(fill = sidd_prop_category_Total),col="black",show.legend=TRUE)  +
  # https://stackoverflow.com/questions/66031935/ggplot2-and-sf-geom-sf-text-within-limits-set-by-coord-sf
  # coord_sf(xlim = c(-140,-60), ylim = c(22,50)) +
  coord_sf(crs = 5070, datum = NA) +
  theme_bw() +
  xlab("") +
  ylab("") +
  # https://stackoverflow.com/questions/33765710/force-ggplot-legend-to-show-all-categories-when-no-values-are-present
  scale_fill_manual(name="",values=colors_sidd_pct_categories,na.value="grey90",drop=FALSE) +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 14),
        # plot.background = element_blank(),
        panel.border = element_blank())



## MARD -------------

figB <- ggplot() +
  geom_sf(data=state_boundaries,aes(fill = mard_prop_category_Total),col="black",show.legend=TRUE)  +
  # https://stackoverflow.com/questions/66031935/ggplot2-and-sf-geom-sf-text-within-limits-set-by-coord-sf
  # coord_sf(xlim = c(-140,-60), ylim = c(22,50)) +
  coord_sf(crs = 5070, datum = NA) +
  theme_bw() +
  xlab("") +
  ylab("") +
  scale_fill_manual(name="",values=colors_mard_pct_categories,na.value="grey90",drop=F) +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 14),
        # plot.background = element_blank(),
        panel.border = element_blank())


## MOD -------------

figC <- ggplot() +
  geom_sf(data=state_boundaries,aes(fill = mod_prop_category_Total),col="black",show.legend=TRUE)  +
  # https://stackoverflow.com/questions/66031935/ggplot2-and-sf-geom-sf-text-within-limits-set-by-coord-sf
  # coord_sf(xlim = c(-140,-60), ylim = c(22,50)) +
  coord_sf(crs = 5070, datum = NA) +
  theme_bw() +
  xlab("") +
  ylab("") +
  scale_fill_manual(name="",values=colors_mod_pct_categories,na.value="grey90",drop=F) +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 14),
        # plot.background = element_blank(),
        panel.border = element_blank())



library(ggpubr)

ggarrange(figA,
          figB,
          figC,
          labels=c("a","b","c"),
          label.x =0.01,
          label.y=0.95,
          legend="bottom",
          nrow=1,ncol=3,common.legend = FALSE) %>% 
  ggsave(.,filename=paste0(path_endotypes_folder,"/figures/state distribution of subphenotypes total_local.pdf"),width=12*1.685,height=5)
