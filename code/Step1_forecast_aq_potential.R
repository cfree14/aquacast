
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
outputdir <- "/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/blue-paper-2/data/output/raw"
plotdir <- "/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/blue-paper-2/data/output/raw_plots"

# Read aquacast function
source(file.path(codedir, "aquacast_v4.R"))
source(file.path(codedir, "calc_costs.R"))

# Read species data
load(file.path(sppdir, "aquaculture_species_key.Rdata"))


# Setup data
################################################################################

# Format data
data <- data %>%
  mutate(type=recode(class, 
                     "Bivalvia"="Bivalve",
                     "Actinopterygii"="Finfish"))

# Subset finfish/bivalves
data_do <- data
# data_do <- filter(data, class=="Bivalvia")
# data_do <- filter(data, class=="Actinopterygii")


# Check to see which didn't finish
rcp2check <- "RCP26"
files_should <- paste0(rcp2check, "_", gsub(" ", "_", data_do$species), ".Rds")
files_all <- list.files(outputdir) 
files_done <- files_all[grepl(rcp2check, files_all)]
files_missing <- files_should[!files_should%in%files_done]
length(files_missing)
# data_do <- data_do %>% 
#   mutate(file=paste0(rcp2check, "_", gsub(" ", "_", species), ".Rds")) %>% 
#   filter(file %in% files_missing)


# Run forecast (in parallel)
################################################################################

# Setup parallel
# library(doParallel)
# ncores <- detectCores()
# registerDoParallel(cores=ncores)

# Loop through species and forecast
i <- 1
for(i in 1:nrow(data_do)){
# foreach(i=1:nrow(data_do)) %dopar% {
  
  # Parameters
  species <- data_do[i,]
  # periods <- c("2021-2022", "2051-2052", "2091-2092")
  periods <- c("2021-2030", "2051-2060", "2091-2100")
  
  
  # Forecast aquaculture potential
  # For testing: rcp="rcp26"; outdir=outputdir
  output <- aquacast(species=species, periods=periods, rcp="rcp26", outdir=outputdir, plot=T)

}

# Loop through species and forecast
i <- 1
for(i in 1:nrow(data_do)){
  # foreach(i=1:nrow(data_do)) %dopar% {
  
  # Parameters
  species <- data_do[i,]
  # periods <- c("2021-2022", "2051-2052", "2091-2092")
  periods <- c("2021-2030", "2051-2060", "2091-2100")
  
  # Forecast aquaculture potential
  # For testing: rcp="rcp26"; outdir=outputdir
  output <- aquacast(species=species, periods=periods, rcp="rcp45", outdir=outputdir, plot=F)
  
}

# Loop through species and forecast
i <- 1
for(i in 1:nrow(data_do)){
  # foreach(i=1:nrow(data_do)) %dopar% {
  
  # Parameters
  species <- data_do[i,]
  # periods <- c("2021-2022", "2051-2052", "2091-2092")
  periods <- c("2021-2030", "2051-2060", "2091-2100")
  
  # Forecast aquaculture potential
  # For testing: rcp="rcp26"; outdir=outputdir
  output <- aquacast(species=species, periods=periods, rcp="rcp60", outdir=outputdir, plot=F)
  
}

# Loop through species and forecast
for(i in 1:nrow(data_do)){
# foreach(i=1:nrow(data_do)) %dopar% {
  
  # Parameters
  species <- data_do[i,]
  # periods <- c("2021-2022", "2051-2052", "2091-2092")
  periods <- c("2021-2030", "2051-2060", "2091-2100")
  
  # Forecast aquaculture potential
  # For testing: rcp="rcp26"; outdir=outputdir
  output <- aquacast(species=species, periods=periods, rcp="rcp85", outdir=outputdir, plot=F)
  
}




