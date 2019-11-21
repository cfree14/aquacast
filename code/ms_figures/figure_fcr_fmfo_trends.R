
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
feeddir <- "data/feed_params/processed"
plotdir <- "figures"

# Read data
load(file.path(datadir, "aquaculture_species_key.Rdata"))
load(file.path(feeddir, "Tacon_Metian_2008_and_2015_fcr_fmfo_data.Rdata"))

# Plot data
################################################################################

# Feed groups in analysis
feed_groups <- sort(unique(data$feed_group))

# FCR trend from Tacon & Metian 2015 (Table 1)
# FM/FO trend from Tacon & Metian 2008 (Table 4)

# Format FCR time series
fcr_ts <- tm15_t1 %>% 
  rename(feed_group=group, value=fcr) %>% 
  mutate(parameter="FCR",
         reference="Tacon & Metian 2015") %>% 
  select(reference, feed_group, year, parameter, value)

# Format FM/FO time series
fmfo_ts <- tm08_t4 %>% 
  rename(reference=source, parameter=ingredient, feed_group=group) %>% 
  select(reference, feed_group, year, parameter, value)

# Merge time series
param_ts <- rbind(fcr_ts, fmfo_ts) %>% 
  mutate(feed_group=recode(feed_group, 
                           "Catfishes"="Catfish",
                           "Chinese carp species"="Chinese fed carps",
                           "Freshwater fish"="Misc freshwater fish",
                           "Marine fish"="Misc marine fish",
                           "Milkfish (Chanos chanos)"="Milkfish",
                           "Other freshwater & diadromous fishes"="Misc freshwater fish")) %>% 
  filter(feed_group %in% feed_groups)

# Setup theme
my_theme <- theme(axis.text=element_text(size=7),
                  axis.title=element_text(size=9),
                  plot.title=element_text(size=11),
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"),
                  axis.text.x = element_text(angle = 90, hjust = 0.5) )

# Plot
g <- ggplot(param_ts, aes(x=year, y=value)) +
  geom_line() +
  facet_grid(parameter ~ feed_group, scales="free") +
  labs(x="", y="Value") +
  theme_bw() + my_theme
g

# Export figure
ggsave(g, filename=file.path(plotdir, "figure_fcr_fmfo_trends.png"), 
       width=6.5, height=4.5, units="in", dpi=600)
