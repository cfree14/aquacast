
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "/Users/cfree/Dropbox/Chris/UCSB/projects/edf_climate/cc_trade/data/gaines/raw"

# Read data
data_orig <- readRDS(file.path(indir, "global_cc_manuscript_results_1nation_20171005.rds"))


# Format data
################################################################################

# Inspects
head(data)
table(data_orig$RCP)
table(data_orig$cc_presence)
table(data_orig$discount_rate)
table(data_orig$scenario)

# Format data
data <- data_orig %>% 
  # Filter data
  filter(cc_presence=="climate_change" & discount_rate==0)
  


# Build total catch over time key
gtot <- data %>% 
  group_by(RCP, cc_presence, discount_rate, scenario, year) %>% 
  summarize(catch_mt=sum(harvest, na.rm=T)) %>% 
  filter(cc_presence=="climate_change" & discount_rate==0 & scenario %in% c("No Adaptation", "Full Adaptation"))

# Plot catch over time
g <- ggplot(gtot, aes(x=year, y=catch_mt/1e6, color=RCP)) +
  geom_line() +
  facet_wrap(~scenario) +
  labs(x="", y="Catch (millions mt)") +
  theme_bw()
g


