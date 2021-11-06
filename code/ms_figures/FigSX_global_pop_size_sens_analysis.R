
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(raster)
library(ggplot2)
library(tidyverse)
library(countrycode)
library(rnaturalearth)

# Directories
datadir <- "/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/blue-paper-2/data/output/processed"
plotdir <- "figures"
tabledir <- "tables"

# Read data
hist <- readRDS(file.path(datadir, "FAO_1950_2018_wc_aq_seafood_per_capita.Rds"))
data_orig <- readRDS(file=file.path(datadir, "global_capture_mariculture_output_merged_new_costs1.Rds"))

# Plot data
################################################################################

# Extract current supply
curr_kg_person <- sum(hist$meat_kg_person[hist$year==max(hist$year)])

# Format data
data <- data_orig %>% 
  # Simplify
  select(rcp:sector, meat_kg_person:meat_kg_person95) %>% 
  # Gather
  gather(key="pop_size_perc", value="meat_kg_person", 5:ncol(.)) %>% 
  # Format sector
  mutate(sector=factor(sector, levels=c("Capture fisheries", "Finfish mariculture", "Bivalve mariculture"))) %>% 
  # Format period
  mutate(period=gsub("-", "-\n", period)) %>% 
  # Format scenario
  mutate(pop_size_perc=recode_factor(pop_size_perc, 
                                     "meat_kg_person"="Base results\n50th percentile\npopulation size",
                                     "meat_kg_person05"="5th percentile\npopulation size",
                                     "meat_kg_person20"="20th percentile\npopulation size",
                                     "meat_kg_person80"="80th percentile\npopulation size",
                                     "meat_kg_person95"="95th percentile\npopulation size"))

# Subset
data1 <- data %>% 
  filter(scenario=="Progressive reforms")
data2 <- data %>% 
  filter(scenario=="Business-as-usual")


# Setup theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.title=element_text(size=8),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=8),
                   plot.title=element_text(size=8),
                   strip.text=element_text(size=7),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.position="bottom")


# Plot three sectors
g1 <- ggplot(data1, aes(x=period, y=meat_kg_person, fill=sector)) +
  facet_grid(rcp~pop_size_perc) +
  geom_bar(stat="identity", alpha=0.5) +
  # Add reference line
  geom_hline(yintercept=curr_kg_person, linetype="dashed", color="black", lwd=0.3) +
  # Labels
  labs(x="Period", y="Seafood production per capita\n(kg of meat per person per year)", title="Progressive reforms") +
  # Legend
  scale_fill_manual(name="", values=c("darkgreen", "salmon", "navy")) +
  # Theme
  theme_bw() + my_theme
g1

# Export
ggsave(g1, filename=file.path(plotdir, "FigSX_global_pop_size_sens_analysis_reforms.png"), 
       width=6.5, height=4.5, units="in", dpi=600)

# Plot three sectors
g2 <- ggplot(data2, aes(x=period, y=meat_kg_person, fill=sector)) +
  facet_grid(rcp~pop_size_perc) +
  geom_bar(stat="identity", alpha=0.5) +
  # Add reference line
  geom_hline(yintercept=curr_kg_person, linetype="dashed", color="black", lwd=0.3) +
  # Labels
  labs(x="Period", y="Seafood production per capita\n(kg of meat per person per year)", title="Business-as-usual") +
  # Legend
  scale_fill_manual(name="", values=c("darkgreen", "salmon", "navy")) +
  # Theme
  theme_bw() + my_theme
g2
  
# Export
ggsave(g2, filename=file.path(plotdir, "FigSX_global_pop_size_sens_analysis_bau.png"), 
       width=6.5, height=4.5, units="in", dpi=600)
  
