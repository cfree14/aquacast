
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

# Identify plotting rank
spp_order_key <- data %>% 
  arrange(class, sst_c_min, sst_c_max) %>% 
  group_by(class) %>% 
  mutate(rank=1:n()) %>% 
  select(class, species, rank, sst_c_min, sst_c_max) %>% 
  ungroup()

# Reshape data for plotting
edata <- data %>% 
  # Reduce
  select(class, species, sst_c_min:sal_psu_max) %>% 
  # Wide-to-long
  gather(key="param", value="value", 3:ncol(.)) %>% 
  # Add meta-data
  mutate(range=ifelse(grepl("q", param), "Preferred", "Tolerated"),
         range=factor(range, levels=c("Tolerated", "Preferred")),
         variable=ifelse(grepl("sst", param), "Temperature (°C)", "Salinity (psu)"),
         variable=factor(variable, levels=c("Temperature (°C)", "Salinity (psu)"))) %>% 
  # Arrange
  select(class, species, param, variable, range, value) %>% 
  # Order by SST
  mutate(species=factor(species, levels=spp_order_key$species))


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

# Plot environmental tolerances
g <- ggplot(edata, aes(x=value, y=species, color=class, alpha=range)) +
  facet_grid(class ~ variable, scales="free", space="free_y") +
  geom_line() +
  # Axis labels and limits
  labs(x="Environmental tolerance", y="Mariculture species") +
  scale_alpha_manual(values = c(0.3, 1)) + # 1=solid, 0=transparent
  expand_limits(x=0) +
  # Theme
  theme_bw() + 
  theme(axis.text.y=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        legend.position = "none")
g

# Export figure
ggsave(g, filename=file.path(plotdir, "FigS9_species_envi_tolerances.png"), 
       width=6.5, height=6, units="in", dpi=600)


