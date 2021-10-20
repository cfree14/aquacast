

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(ncdf4)
library(raster)
library(tidyverse)
library(lubridate)

# Directories
outdir <- "/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/blue-paper-2/data/climate/Song_etal_2020/4rasters_scaled"
tempdir <- "data/template"


# Scale to match template
################################################################################

# Build mask
# ssp="SSP126"; stat="max"
build_mask <- function(ssp, wave_thresh_m){
  
  # Read data
  infile <- paste0("Song_etal_2020_", ssp, "_sig_wave_height_annual_max_scaled.grd")
  data <- brick(file.path(outdir, infile))

  # Reproject to match template
  data_mask <- data <= wave_thresh_m
  
  # Plot check
  # plot(data[[1]])
  # plot(data_mask[[1]])
  
  # Export rasters if match successful
  outfile <- paste0("Song_etal_2020_", ssp, "_sig_wave_height_mask.grd")
  writeRaster(data_mask, file=file.path(outdir, outfile), overwrite=T)
  
}

# Loop through scenarios
build_mask(ssp="SSP126", wave_thresh_m=5)
build_mask(ssp="SSP245", wave_thresh_m=5)
build_mask(ssp="SSP585", wave_thresh_m=5)




