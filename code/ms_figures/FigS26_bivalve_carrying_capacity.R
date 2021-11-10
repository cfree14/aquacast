

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(sf)
library(raster)
library(tidyverse)
library(rnaturalearth)

# Directories
plotdir <- "figures"
datadir <- "data/bivalve_stocking_densities/data"

# Read data
data <- read.csv(file=file.path(datadir, "Smaal_etal_2019_bivalve_carrying_capacity_data.csv"), as.is=T)


# Plot data
################################################################################

# Mean low impact density
data %>% 
  filter(impacts=="Low") %>% 
  pull(cultured_mt_sqkm) %>% 
  mean(na.rm=T)

# Setup theme
my_theme <- theme(axis.text=element_text(size=8),
                  axis.title=element_text(size=10),
                  legend.text=element_text(size=8),
                  legend.title=element_text(size=10),
                  plot.title=element_blank(),
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"))


# Plot data
# CT/RT = clearance ratio; CT/PT = grazing ratio. 
g <- ggplot(data, aes(x=log_ctrt, y=log_ctpt, size=cultured_mt_sqkm, color=impacts)) +
  geom_point() + 
  # Reference lines
  geom_hline(yintercept=0) +
  geom_vline(xintercept=0) +
  # Labels
  labs(x="log(Clearance ratio)", y="log(Grazing ratio)") +
  # Legends
  scale_color_discrete(name="Ecological impacts") +
  scale_size_continuous("Ecosystem-wide\ncultured bivalve\ndensity (mt/sqkm)") +
  # Theme
  theme_bw() + my_theme
g

# Export plot
ggsave(g, filename=file.path(plotdir, "FigS26_bivavle_carrying_capacity.png"), 
       width=6.5, height=4.5, units="in", dpi=600)

