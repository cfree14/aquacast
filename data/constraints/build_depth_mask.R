

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(sf)
library(raster)
library(tidyverse)
library(fasterize)
library(countrycode)

# Directories
datadir <- "data/constraints/masks"
tempdir <- "data/template"
depthdir <- "/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/blue-paper-2/data/bathymetry/"

# Projections
moll <- CRS("+proj=moll")
wgs84 <- CRS("+init=epsg:4326")

# Read bathymetry data
# depths <- raster(file.path(depthdir, "gebco_2020_netcdf/GEBCO_2020.nc"))
depths <- raster(file.path(depthdir, "World_e-Atlas-UCSD_SRTM30-plus_v8.tif"))

# Read raster template
ras_temp <- raster(file.path(tempdir, "world_raster_template_10km.tif"))


# Build data
################################################################################

# Project raster
depths_proj <- projectRaster(from=depths, to=ras_temp)
plot(depths_proj)

# Identify areas 0-200 m deep
depth_mask <- depths_proj < 0 & depths_proj >= -225
plot(depth_mask)

# Check against template
compareRaster(depth_mask, ras_temp)

# Export data
writeRaster(depth_mask, file=file.path(datadir, "depth_10km.grd"), overwrite=T)

