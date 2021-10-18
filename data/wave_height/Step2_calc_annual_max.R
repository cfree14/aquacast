

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
indir <- "/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/blue-paper-2/data/climate/Song_etal_2020/2rasters/"
outdir <- "/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/blue-paper-2/data/climate/Song_etal_2020/3rasters_annualized"


# Function
################################################################################

# Function to calculate annual stat and scale data to match template
# ssp <- "SSP126";  stat <- "max"
calc_annual_stats <- function(ssp, stat){
  
  # Read data
  filename <- paste0("Song_etal_2020_", ssp, "_sig_wave_height.grd")
  data <- brick(file.path(indir, filename))
  
  # Build a key labeling date years and decades
  dates <- names(data) %>% gsub("X", "", .) %>% ymd()
  date_key <- tibble(date=dates, year=year(dates), index=1:length(dates), decade=floor(year/10)*10)
  decades <- sort(unique(date_key$decade))
  
  # Calculate annual stats for years in a decade
  brick_list <- sapply(decades, function(x){
    layers_do <- date_key$index[date_key$decade==x]
    yrs_do <- date_key$year[date_key$decade==x]
    # If fun=sd b/c sd arbitrarily doesn't work in quotes
    if(stat=="sd"){
      brick_do <- stackApply(data[[layers_do]], indices=yrs_do, fun=sd, na.rm=T)
    }else{
      brick_do <- stackApply(data[[layers_do]], indices=yrs_do, fun=stat, na.rm=T)
    }
    return(brick_do)
  })
  
  # Merge annual stats from each decade file
  data_brick <- brick(brick_list)
  names(data_brick) <- layer_names_annual <- gsub("index_", "", names(data_brick))
  
  # Export
  outfile <- paste0("Song_etal_2020_", ssp, "_sig_wave_height_annual_", stat, ".grd")
  writeRaster(data_brick, file=file.path(outdir, outfile), overwrite=T)

  # Return brick
  return(data_brick)
  
}


# Format data
################################################################################

# Calculate annual stats
calc_annual_stats(ssp="SSP126", stat="max")
calc_annual_stats(ssp="SSP245", stat="max")
calc_annual_stats(ssp="SSP585", stat="max")




