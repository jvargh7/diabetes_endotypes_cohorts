rm(list=ls());gc();source(".Rprofile")

source("analysis/decan002c_imputed phenotyping dataset processing.R")

library(tidyverse)
library(tigris)
library(sf)

##### MAP 1 - COSMOS Pariticipants ######## 
map_df = read_parquet(paste0(path_diabetes_endotypes_cosmos_folder,"/working/phenotyping dataset.parquet")) 

# Count Participants Per State and Create Categories
state_counts <- map_df %>%
  group_by(ValidatedStateOrProvince_X) %>%  # Group by state names
  tally(name = "n") %>%                     # Count participants in each state
  rename(state = ValidatedStateOrProvince_X) %>%
  mutate(n_category = case_when(
    n < 100 ~ 1,
    n < 500 ~ 2,
    n < 1000 ~ 3,
    n < 5000 ~ 4,
    n < 10000 ~ 5,
    n < 50000 ~ 6,
    TRUE ~ 7
  )) %>%
  mutate(n_category = factor(
    n_category,
    levels = c(1:7),
    labels = c("<100", "100-499", "500-999", "1000-4999", "5000-9999", "10000-49999", ">=50000")
  ))

# Load State Boundaries
state_boundaries <- tigris::states(class = "sf", cb = TRUE) %>%
  tigris::shift_geometry() %>%  # Shift geometries for better visualization
  dplyr::filter(GEOID < 60) %>%  # Filter for continental states
  left_join(state_counts, by = c("NAME" = "state"))  # Match boundaries with data

# Create the Map
figA <- ggplot() +
  geom_sf(data = state_boundaries, aes(fill = n_category), col = "black") +
  coord_sf(crs = 5070, datum = NA) +
  theme_bw() +
  xlab("") +
  ylab("") +
  scale_fill_manual(
    name = "cases",  
    values = c(
      "<100" = "#f7dfb9",   # Light beige
      "100-499" = "#f1c08e", # Medium beige
      "500-999" = "#eaa969", # Warm beige
      "1000-4999" = "#e3934c", # Deep beige
      "5000-9999" = "#d97a30", # Dark orange
      "10000-49999" = "#c2631f", # Deep orange-brown
      ">=50000" = "#a6501a"   # Dark brown
    ),
    na.value = "grey90"  # Color for missing values
  ) +
  theme(legend.position = "bottom")+
plot_annotation(
  title = "Epic Cosmos",  # Title for the figure
  theme = theme(
    plot.title = element_text(hjust = 0, size = 12, face = "bold")  # Align top left
  )
)

# Save the Map
print(figA)
ggsave(figA,filename=paste0(path_diabetes_endotypes_cosmos_folder,"/figures/state distribution_all_t2dm.jpg"),width=12,height=6)



##### MAP2 - COHORT data map - re-created here for consistent looks #### 


highlighted_states <- c(
  "Maryland", "North Carolina", "Mississippi", "Minnesota", "Alabama",
  "Arizona", "California", "Colorado", "Florida", "Georgia", "Hawaii",
  "Illinois", "Indiana", "Kentucky", "Louisiana", "Massachusetts",
  "Missouri", "New Mexico", "New York", "Ohio", "Oregon", "Pennsylvania",
  "South Carolina", "Tennessee", "Texas", "Washington", "West Virginia",
  "Wisconsin"
)

state_boundaries <- tigris::states(class = "sf", cb = TRUE) %>%
  tigris::shift_geometry() %>%
  dplyr::filter(GEOID < 60) %>%  # Filter for continental states
  mutate(highlight = if_else(NAME %in% highlighted_states, "Participants recruited", "Not recruited"))


figB <- ggplot() +
  geom_sf(data = state_boundaries, aes(fill = highlight), col = "black") +
  coord_sf(crs = 5070, datum = NA) +
  theme_bw() +
  xlab("") +
  ylab("") +
  scale_fill_manual(
    name = "",
    values = c("Participants recruited" = "#FFA500","Not recruited" = "grey90"),  # Highlighted states are beige
    labels = c("Participants recruited" = "Participants recruited"),  # Only show label for "Highlighted"
    breaks = c("Participants recruited")    # Include only "Participants recruited" in the legend
  ) +
  theme(legend.position = "bottom")+
  plot_annotation(
    title = "Cohort Studies",  # Title for the figure
    theme = theme(
      plot.title = element_text(hjust = 0, size = 12, face = "bold")  # Align top left
    )
  )

print(figB)
ggsave(figB,filename=paste0(path_diabetes_endotypes_cosmos_folder,"/figures/cohort study maps.jpg"),width=12,height=6)



