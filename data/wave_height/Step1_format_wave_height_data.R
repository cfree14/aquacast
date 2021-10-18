
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(ncdf4)
library(raster)
library(lubridate)
library(tidyverse)

# Directories
indir <- "/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/blue-paper-2/data/climate/Song_etal_2020/1netcdfs/"
outdir <- "/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/blue-paper-2/data/climate/Song_etal_2020/2rasters/"

# Projections
wgs84 <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

# World
world <- rnaturalearth::ne_countries(returnclass = "sf", scale="small") %>% 
  sf::st_transform(wgs84)

# Paper: https://www.nature.com/articles/s41597-020-0566-8#Sec5


# Read data
################################################################################

# File to format dta
# filename <- "Hs_glob_FIO_FIO-ESM-2-0_ssp126_r1i1p1f1_mon_201501-210012_1x1d.nc"; scenario <- "SSP126"
format_wave_data <- function(filename, scenario){
  
  # Read NetCDF file
  data_nc <- ncdf4::nc_open(file.path(indir, filename))
  data_orig <- raster::brick(file.path(indir, filename))
  hs_vals <- ncdf4::ncvar_get(data_nc, varid="Hs")
  print(max(hs_vals, na.rm=T))
  
  # Plot check
  # plot(data_orig[[1]])
  
  # 1. Rotate raster
  extent2 <- extent1 <- extent(data_orig)
  extent1@xmax <- 180
  extent2@xmin <- 180
  data_e <- crop(data_orig, extent1)
  data_w <- shift(crop(data_orig, extent2), -360)
  data_merged <- merge(data_w, data_e)
  
  # Plot check
  # plot(data_merged[[1]])
  
  # Reproject raster
  data <- raster::projectRaster(data_merged, crs=wgs84)
  
  # Add layer names
  months <- seq(ymd("2015-01-01"), ymd("2100-12-01"), by="1 month")
  length(months)
  names(data) <- months
  
  # Plot check
  # plot(data_orig[[1]])
  
  # Plot check
  data_ex <- data[[1]] %>% 
    as.data.frame(xy=T) %>% 
    setNames(c("long_dd", "lat_dd", "val")) %>% 
    filter(!is.na(val))
  g <- ggplot() +
    geom_tile(data=data_ex, mapping=aes(x=long_dd, y=lat_dd, fill=val)) +
    geom_sf(data=world, fill="grey", color="white", lwd=0.3) +
    theme_bw()
  print(g)
  
  # Export data
  outfile <- paste0("Song_etal_2020_", scenario, "_sig_wave_height.grd")
  writeRaster(data, file=file.path(outdir, outfile), overwrite=T)
  
}

# Apply function
format_wave_data(filename="Hs_glob_FIO_FIO-ESM-2-0_ssp126_r1i1p1f1_mon_201501-210012_1x1d.nc", scenario <- "SSP126")
format_wave_data(filename="Hs_glob_FIO_FIO-ESM-2-0_ssp245_r1i1p1f1_mon_201501-210012_1x1d.nc", scenario <- "SSP245")
format_wave_data(filename="Hs_glob_FIO_FIO-ESM-2-0_ssp585_r1i1p1f1_mon_201501-210012_1x1d.nc", scenario <- "SSP585")



