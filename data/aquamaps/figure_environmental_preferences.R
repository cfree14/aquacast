
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(ggplot2)
library(tidyverse)

# Directories
datadir <- "data/aquamaps/data"
plotdir <- "data/aquamaps/figures"

# Read data
data_orig <- readRDS(file.path(datadir, "aquamaps_environmental_preferences.Rds"))

# Species key <- 
spp_key <- read.csv("data/species/aquaculture_species_key.csv", as.is=T)


# Plot data
################################################################################

# Variables
var_use <- c("Salinity (psu)", "Temperature (C)", "Primary production (mgC/m2/day)")

# Identify plotting rank
spp_order_key <- data_orig %>% 
  left_join(select(spp_key, species, class), by=c("sci_name"="species")) %>% 
  filter(variable=="Temperature (C)") %>% 
  arrange(class, min, max) %>% 
  group_by(class) %>% 
  mutate(rank=1:n()) %>% 
  select(class, sci_name:used, rank, everything()) %>% 
  ungroup()

# Format for plotting
data <- data_orig %>% 
  # Add class
  left_join(select(spp_key, species, class), by=c("sci_name"="species")) %>% 
  # Reshape and format columns
  gather(key="type", value="value", 5:8) %>% 
  mutate(range=ifelse(grepl("q", type), "tolerated", "preferred"),
         perc=as.numeric(recode(type, "min"=0, "q10"=10, "q90"=90, "max"=100)),
         type=ifelse(type %in% c("min", "q10"), "low", "high"),
         used=ifelse(used==0, "No", "Yes")) %>% 
  select(sci_name:used, range, type, perc, everything()) %>% 
  # Reduce to variables of interest
  filter(variable %in% var_use) %>% 
  arrange(variable) %>% 
  # Format variables
  mutate(variable=recode(variable,
                         "Temperature (C)"="SST (°C)",
                         "Primary production (mgC/m2/day)"="Primary production\n(mgC/m2/day)"),
         variable=factor(variable, levels=c("SST (°C)", "Salinity (psu)", "Primary production\n(mgC/m2/day)"))) %>% 
  # Order by SST
  mutate(sci_name=factor(sci_name, levels=spp_order_key$sci_name))

# Plot environmental tolerances
g <- ggplot(data, aes(x=value, y=sci_name, color=class, alpha=range)) +
  facet_grid(class ~ variable, scales="free", space="free_y") +
  geom_line() +
  # Axis labels and limits
  labs(x="", y="") +
  expand_limits(x=0) +
  # Theme
  theme_bw() + 
  theme(axis.text.y=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        legend.position = "none")
g

# Export figure
ggsave(g, filename=file.path(plotdir, "figure_envi_tolerances.pdf"), 
       width=8.5, height=11, units="in", dpi=600)

