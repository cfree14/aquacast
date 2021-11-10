
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
load(file.path(datadir, "aquaculture_species_key_20cages.Rdata"))
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
  # Remove species
  filter(!isscaap %in% c(brackish_isscaaps, bad_bivalve_isscaaps)) %>% 
  # Recode type
  mutate(class=recode(class, "Actinopterygii"="Finfish", "Bivalvia"="Bivalves")) %>% 
  # Remove a few columns
  select(-c(sst_c_min_froehlich, sst_c_max_froehlich))

# Inspect
colnames(data)

# Format data
################################################################################

# Export
write.csv(data, file.path(tabledir, "Free_etal_mariculture_species_database.csv"), row.names = F)





