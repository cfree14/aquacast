
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(raster)
library(ggplot2)
library(tidyverse)

# Directories
codedir <- "code"
sppdir <- "data/species"
outputdir <- "output/production_rasters"

# Read aquacast function
source(file.path(codedir, "aquacast.R"))

# Read species data
spp <- read.csv(file.path(sppdir, "aquaculture_species_key.csv")) %>% 
  # TEMPORARY: Remove species without data requirement
  filter(!is.na(linf_cm) & !is.na(k) & !is.na(a) & !is.na(b) & !is.na(temp_c_min) & !is.na(temp_c_max)) %>% 
  # TEMPORARY: Add type and harvest cm
  mutate(type=recode(class, "Bivalvia"="bivalve", "Actinopterygii"="finfish"),
         harvest_cm=pmin(35, linf_cm*0.7),
         fcr=1.3,
         price_usd_mt=7000)


# Setup for testing
################################################################################

# Put this in parallel

# Loop through species and run model
i <- 2
for(i in 1:nrow(spp)){
  
  # Parameters
  species <- spp[i,]
  years <- 2020
  # years <- c(2020, 2050, 2100)
  
  # Forecast aquaculture potential
  output <- aquacast(species=species, years=years, outdir=outputdir, plot=T)
  
  
}



