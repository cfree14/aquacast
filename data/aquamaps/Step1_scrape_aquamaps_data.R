
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(ggplot2)
library(tidyverse)

# Directories
datadir <- "data/species/data"
outputdir <- "data/aquamaps/data/raw"
codedir <- "data/aquamaps"

# Read data
spp_key <- read.csv(file.path(datadir, "aquaculture_species_key.csv"), as.is=T) %>% 
  arrange(species)

# Source code to Download AquaMaps data
source(file.path(codedir, "download_aquamaps.R"))


# Download AquaMaps data
################################################################################

# Species to do
species_done <- list.files(outputdir) %>% gsub(".csv", "", .) %>% gsub("_", " ", .)
species_todo <- spp_key$species[!spp_key$species %in% species_done]
species_todo <- c( "Polititapes aureus", "Prototapes gallus")

# Download data
# species <- c("Gadus morhua", "Pomacanthus paru"); savedir <- outputdir
download_aquamaps(species=species_todo, savedir=outputdir, wait_factor=3)



