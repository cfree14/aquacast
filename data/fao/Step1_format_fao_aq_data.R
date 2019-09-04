
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(ggplot2)
library(tidyverse)

# Directories
datadir <- "data/fao"

# Read data
data <- readRDS("/Users/cfree/Dropbox/Chris/UCSB/projects/protein_curve/ocean-protein/aquaculture/data/processed/1950_2018_FAO_aq_production.Rds")


# Identify mariculture species
################################################################################

# Mean production of marine AQ species from 2010-2016
maq_species <- data %>% 
  # Reduce to marine species and 2010-2016
  filter(type2=="marine and coastal" & year>=2010) %>% 
  # Calculate sum each year
  group_by(group, isscaap, order, family, comm_name, species, year) %>% 
  summarize(prod_mt=sum(quantity_mt),
            value_t_usd=sum(value_t_usd)) %>% 
  ungroup() %>% 
  # Calculate average from 2010-2016
  group_by(group, isscaap, order, family, comm_name, species) %>% 
  summarize(prod_mt=mean(prod_mt, na.rm=T),
            value_t_usd=mean(value_t_usd, na.rm=T)) %>% 
  ungroup() %>% 
  # Remove seedweeds
  filter(group %in% c("Finfish", "Molluscs")) %>% 
  filter(!grepl("nei", comm_name)) %>% 
  filter(!grepl("spp", species)) %>% 
  # Fix a few scientific names
  rename(species_orig=species) %>% 
  mutate(species=recode(species_orig, "Acanthopagrus schlegeli"="Acanthopagrus schlegelii",
                        "Diplodus sargus"="Diplodus sargus sargus",
                        "Larimichthys croceus"="Larimichthys crocea",
                        "Lutjanus russelli"="Lutjanus russellii", 
                        "Oncorhynchus masou"="Oncorhynchus masou masou",
                        "Salvelinus alpinus"="Salvelinus alpinus alpinus",
                        "Sebastes schlegeli"="Sebastes schlegelii"))

# Check species names
freeR::check_names(maq_species$species)
freeR::suggest_names(maq_species$species)

# Sample size
table(maq_species$group)









