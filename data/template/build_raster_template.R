
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)
library(ncdf4)
library(raster)


# Directories
datadir <- "data/template"

# Projections
wgs84 <- CRS("+init=epsg:4326")
moll <- CRS("+proj=moll")

# Create WGS84 template
res_km <- 10
ras_temp_wgs <- raster::raster(crs=wgs84, xmn=-180, xmx=180, ymn=-90, ymx=90)
ras_temp_moll <- projectRaster(ras_temp_wgs, crs=moll, res=res_km*1000)

# Export template
filename <- paste0("world_raster_template_", res_km, "km.tif")
writeRaster(ras_temp_moll, file=file.path(datadir, filename))
