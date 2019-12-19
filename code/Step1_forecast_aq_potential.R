
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
sppdir <- "data/species/data"
outputdir <- "output/raw"

# Read aquacast function
source(file.path(codedir, "aquacast.R"))
source(file.path(codedir, "calc_costs.R"))

# Read species data
load(file.path(sppdir, "aquaculture_species_key.Rdata"))


# Setup for testing
################################################################################

# Format data
data <- data %>%
  mutate(type=recode(class, 
                     "Bivalvia"="Bivalve",
                     "Actinopterygii"="Finfish"))

# Subset finfish/bivalves
bivalves <- filter(data, class=="Bivalvia")
finfish <- filter(data, class=="Actinopterygii" & comm_name=="Atlantic salmon")

# Loop through species and run model
i <- 1
for(i in 1:10){
# for(i in 1:nrow(bivalves)){
  
  # Parameters
  species <- finfish[i,]
  # years <- 2021
  years <- c(2021, 2051, 2100)
  
  # Forecast aquaculture potential
  output <- aquacast(species=species, years=years, rcp="rcp45", outdir=outputdir, plot=T)
  
}



