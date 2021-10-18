

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
indir <- "/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/blue-paper-2/data/climate/Song_etal_2020/3rasters_annualized"
outdir <- "/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/blue-paper-2/data/climate/Song_etal_2020/4rasters_scaled"
tempdir <- "data/template"

# Read raster template
ras_temp <- raster(file.path(tempdir, "world_raster_template_10km.tif"))


# Scale to match template
################################################################################

# Scale annualized rasters to match template
# ssp="SSP126"; stat="max"
scale_raster <- function(ssp, stat){
  
  # Read data
  infile <- paste0("Song_etal_2020_", ssp, "_sig_wave_height_annual_", stat, ".grd")
  data <- brick(file.path(indir, infile))

  # Reproject to match template
  data_proj <- projectRaster(from=data, to=ras_temp)
  
  # Check that reprojected data matches template
  proj_check <- compareRaster(data_proj, ras_temp)
  
  # Export rasters if match successful
  if(proj_check){
    print("Annual raster matches template raster. File exported.")
    outfile <- paste0("Song_etal_2020_", ssp, "_sig_wave_height_annual_", stat, "_scaled.grd")
    writeRaster(data_proj, file=file.path(outdir, outfile), overwrite=T)
  }else{
    print("Annual raster DOES NOT MATCH template raster.")
  }
  
}

# Loop through scenarios
scale_raster(ssp="SSP126", stat="max")
scale_raster(ssp="SSP245", stat="max")
scale_raster(ssp="SSP585", stat="max")

