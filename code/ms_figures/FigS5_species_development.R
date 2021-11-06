
# Clear workspace
rm(list = ls())

# Turn off scientific notation
options(scipen=999)

# Setup
################################################################################

# Packages
library(tidyverse)
library(grid)
library(gridExtra)

# Directories
datadir <- "data/species/data"
plotdir <- "figures"

# Read data
load(file.path(datadir, "aquaculture_species_key.Rdata"))
data_orig <- data
data_full_orig <- data_full


# Format data
################################################################################

# Brackish ISSCAAPs
brackish_isscaaps <- c("Freshwater molluscs", "Miscellaneous diadromous fishes", 
                       "Miscellaneous freshwater fishes", "River eels", "Shads", 
                       "Sturgeons, paddlefishes", "Tilapias and other cichlids")

# Not lonline bivalves
bad_bivalve_isscaaps <- c("Clams, cockles, arkshells", "Pearls, mother-of-pearl, shells", "Scallops, pectens")
  
# Format data
data <- data_orig %>% 
  # Recode types
  mutate(class=recode(class, 
                      "Bivalvia"="Bivales",
                      "Actinopterygii"="Finfish")) %>% 
  # Remove brackish
  filter(!isscaap %in% c(brackish_isscaaps, bad_bivalve_isscaaps)) %>% 
  # Fill FAO gaps
  mutate(fao_mt_yr=ifelse(is.na(fao_mt_yr), 0, fao_mt_yr)) %>% 
  # Add x begging
  mutate(x1=0.001)


# Plot data
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.text.y=element_text(size=5),
                   axis.title=element_text(size=8),
                   strip.text=element_text(size=8),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"))

# Plot data
g <- ggplot(data, mapping=aes(x=fao_mt_yr+0.001, y=reorder(comm_name, fao_mt_yr))) +
  facet_grid(class~., scales="free_y", space="free_y") +
  # Bar plot
  geom_segment(data=data, mapping=aes(x=x1, xend=fao_mt_yr+0.001, 
                                      y=reorder(comm_name, fao_mt_yr), 
                                      yend=reorder(comm_name, fao_mt_yr)), inherit.aes = F) +
  geom_point(size=1) +
  # Labels
  labs(x="Annual production (mt)", y="") +
  # Scales
  scale_x_continuous(trans="log10",
                     breaks=10^seq(-3, 6, 1),
                     labels=c("0", "0.01", "0.1", "1", "10", "100", "1,000", "10,000", "100,000", "1,000,000")) +
  # Theme
  theme_bw() + my_theme
g

# Export plot
ggsave(g, filename=file.path(plotdir, "FigS5_species_development.png"), 
       width=6.5, height=8.5, units="in", dpi=600)








