
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
tabledir <- "tables"

# Read data
data <- read.csv(file.path(tabledir, "Free_etal_mariculture_species_database.csv"), as.is=T)


# Build data
################################################################################

# Reshape for plotting
hdata <- data %>% 
  select(class, species, linf_cm, harvest_cm, harvest_g, harvest_yr) %>% 
  gather(key="parameter", value="value", 3:ncol(.)) %>% 
  mutate(parameter=recode(parameter, 
                          "harvest_cm"="Harvest size", 
                          "linf_cm"="Linf"))

# Plot
g <- ggplot(hdata, aes(x=value, fill=class)) +
  geom_density(aes(y=..scaled..)) +
  facet_grid(class ~ parameter, scales="free") +
  theme_bw() + my_theme
g


# Plot data
################################################################################

# Setup theme
my_theme <- theme(axis.text=element_text(size=7),
                  axis.title=element_text(size=9),
                  plot.title=element_text(size=11),
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"))
# Finfish
#############################

# Finfish lengths
g1 <- ggplot(filter(hdata, class=="Finfish" & parameter %in% c("Harvest size", "Linf")), aes(x=value, fill=parameter)) +
  geom_density(aes(y=..scaled..), alpha=0.8) +
  labs(x="Length (cm)", y="", title="Finfish") +
  theme_bw() + my_theme +
  scale_fill_discrete(name="") +
  theme(legend.position=c(0.7,0.8),
        legend.text=element_text(size=6),
        legend.background = element_rect(fill=alpha('blue', 0)))
#g1

# Finfish weights
g2 <- ggplot(filter(hdata, class=="Finfish" & parameter %in% c("harvest_g")), aes(x=value/1000)) +
  geom_density(aes(y=..scaled..), alpha=0.8) +
  labs(x="Harvest weight (kg)", y="") +
  xlim(0,25) +
  theme_bw() + my_theme
#g2

# Finfish ages
g3 <- ggplot(filter(hdata, class=="Finfish" & parameter %in% c("harvest_yr")), aes(x=value)) +
  geom_density(aes(y=..scaled..), alpha=0.8) +
  labs(x="Harvest age (yr)", y="") +
  theme_bw() + my_theme
#g3

# Bivalves
#############################

# Bivalve linf and harvest size
g4 <- ggplot(filter(hdata, class=="Bivalves" & parameter %in% c("Harvest size", "Linf")), aes(x=value, fill=parameter)) +
  geom_density(aes(y=..scaled..), alpha=0.8) +
  labs(x="Length (cm)", y="", title="Bivalves") +
  theme_bw() + my_theme +
  scale_fill_discrete(name="") +
  theme(legend.position=c(0.7,0.8),
        legend.text=element_text(size=6),
        legend.background = element_rect(fill=alpha('blue', 0)))
#g4

# Bivalve weights
g5 <- ggplot(filter(hdata, class=="Bivalves" & parameter %in% c("harvest_g")), aes(x=value)) +
  geom_density(aes(y=..scaled..), alpha=0.8) +
  labs(x="Harvest weight (g)", y="") +
  xlim(0,500) +
  theme_bw() + my_theme
#g5

# Bivalve ages
g6 <- ggplot(filter(hdata, class=="Bivalves" & parameter %in% c("harvest_yr")), aes(x=value)) +
  geom_density(aes(y=..scaled..), alpha=0.8) +
  labs(x="Harvest age (yr)", y="") +
  xlim(0,NA) +
  theme_bw() + my_theme
#g6

# Bivalves
#############################

# Merge
g <- grid.arrange(g1, g2, g3, g4, g5, g6, ncol=3)
g

# Export plot
ggsave(g, filename=file.path(plotdir, "FigS12_species_harvest_sizes_ages.png"), 
       width=6.5, height=4.5, units="in", dpi=600)

