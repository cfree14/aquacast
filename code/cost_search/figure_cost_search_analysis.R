
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
datadir <- "/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/blue-paper-2/data/output/cost_search_processed"
plotdir <- "figures"
tabledir <- "tables"


# Build data
################################################################################

# Cost scalars
cost_scalars <- c(1.3, 1.5, 1.7, 2, 2.1)

# Loop through cost scalars and build data
x <- 1.3
data_orig <- purrr::map_df(cost_scalars, function(x){
  
  # Read data
  infile <- paste0("global_capture_mariculture_output_merged_cost_search_", x, ".Rds")
  fdata_orig <- readRDS(file.path(datadir, infile))
  
  # Summarize data
  fdata <- fdata_orig %>% 
    group_by(rcp, scenario, period) %>% 
    summarize(meat_kg_person=sum(meat_kg_person)) %>% 
    ungroup() %>% 
    mutate(cost_scalar=x) %>% 
    select(cost_scalar, everything())
  
})

# Data
data <- data_orig %>% 
  filter(scenario=="Progressive reforms" & period=="2091-2100")


# Plot data
################################################################################

g <- ggplot(data, aes(x=cost_scalar, y=meat_kg_person, color=rcp)) +
  geom_line() +
  geom_point() +
  # Reference line
  geom_hline(yintercept=8.918661) +
  # Labels
  labs(x="Cost scalar", y="Seafood production per capita") +
  # Axis
  scale_y_continuous(lim=c(0,NA)) +
  # Theme
  theme_bw()
g












