
# Clear workspace
rm(list = ls())

# NOAA's Climate Change Web Portal
# https://www.esrl.noaa.gov/psd/ipcc/ocn/ccwp.html

# Setup
################################################################################

# Packages
library(tidyverse)
library(ncdf4)
library(raster)
library(lubridate)
library(weathermetrics) # kelvin.to.celsius()
library(cmocean)

# Directories
cmipdir <- "/Users/cfree/Dropbox/Chris/UCSB/projects/edf_climate/cc_trade/data/cmip5/raw/"
outputdir <- "data/climate/data/knmi"

# Projections
wgs84 <- CRS("+init=epsg:4326")


# Read data and basic formatting
################################################################################

# Read data as NetCDF (to get dates)
data_ncdf <- nc_open(file.path(cmipdir,  "tos_Omon_modmean_rcp85_ave.nc"))

# Get and format dates
ncatt_get(data_ncdf, varid="time", attname="units") 
start_date <- as.Date("1861-01-01") # ALWAYS CHECK THIS
dates_orig <- ncvar_get(data_ncdf, varid="time")
dates <- start_date %m+% months(dates_orig) 

# Read data as raster brick
res_km <- 10
data <- brick(file.path(cmipdir, "tos_Omon_modmean_rcp85_ave.nc")) %>% 
  # Rename layers as dates
  setNames(dates) %>%
  # Convert from 0-360 to -180-180
  rotate() %>% 
  # Convert Kelvin to Celcius
  kelvin.to.celsius()

# Plot one month's data
sdata <- raster::as.data.frame(data[[1]], xy=T) %>%
  setNames(c("long_dd", "lat_dd", "sst_k"))
g <- ggplot(sdata, aes(x=long_dd, y=lat_dd, fill=sst_k)) +
  geom_raster() +
  labs(x="", y="") +
  scale_fill_gradientn(name="SST (°C)", colors=cmocean("thermal")(100)) +
  theme_bw()
g


# Build annual data
################################################################################

# This code should work but it doesn't, potentially b/c of memory issues
# yrs <- year(dates)
# sst_c_avg <- stackApply(data, indices=yrs, fun=mean, na.rm=T)
# sst_c_min <- stackApply(data, indices=yrs, fun=min, na.rm=T)
# sst_c_max <- stackApply(data, indices=yrs, fun=max, na.rm=T)

# Function to calculate annual stats
# For testing: x <- 1960; stat <- "mean"
calc_annual_stats <- function(stat){
  
  # Build a key labelling date years and decades
  date_key <- tibble(date=dates, year=year(dates), index=1:length(dates), decade=floor(year/10)*10)
  decades <- sort(unique(date_key$decade))
  
  # Calculate annual stats for years in a decades
  brick_list <- sapply(decades, function(x){
    layers_do <- date_key$index[date_key$decade==x]
    yrs_do <- date_key$year[date_key$decade==x]
    brick_do <- stackApply(data[[layers_do]], indices=yrs_do, fun=stat, na.rm=T)
    return(brick_do)
  })
  
  # Merge stats
  sst_brick <- brick(brick_list)
  names(sst_brick) <- gsub("index_", "", names(sst_brick))
  return(sst_brick)
  
}

# Calculate annual stats
sst_c_avg <- calc_annual_stats("mean")
sst_c_min <- calc_annual_stats("min")
sst_c_max <- calc_annual_stats("max")

# Reproject to Mollweide
res_km <- 10
sst_c_min_moll <- sst_c_min %>% 
  projectRaster(res=res_km*1000, crs=CRS("+proj=moll"))
sst_c_max_moll <- sst_c_max %>% 
  projectRaster(res=res_km*1000, crs=CRS("+proj=moll"))


# Export data
################################################################################

# Export data
writeRaster(sst_c_min_moll, file=file.path(outputdir, "RCP85_annual_sst_min.tif"), overwrite=T)
writeRaster(sst_c_max_moll, file=file.path(outputdir, "RCP85_annual_sst_max.tif"), overwrite=T)
# save(sst_c_avg, sst_c_min, sst_c_max, file=file.path(outputdir, "RCP85_annual_sst_stats.Rdata"))












