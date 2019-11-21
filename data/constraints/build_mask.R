

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
eezdir <- "data/eezs"
datadir <- "data/constraints/masks"
tempdir <- "data/template"

# Projections
moll <- CRS("+proj=moll")
wgs84 <- CRS("+init=epsg:4326")

# Read raster template
ras_temp <- raster(file.path(tempdir, "world_raster_template_10km.tif"))

# Read EEZs
eezs <- raster(file.path(eezdir, "eezs_v10_raster_10km.tif")) 

# Read masks
mpa_mask <- raster(file.path(datadir, "mpa_mask_10km.tif")) 
shipping_mask <- raster(file.path(datadir, "shipping_mask_10km.tif")) 
oil_mask <- raster(file.path(datadir, "oil_mask_10km.tif")) 


# Build data
################################################################################

# Areas in use
used <- mpa_mask==1 | shipping_mask==1 | oil_mask==1
plot(used, main="Used area")

# Clip EEZs
mask <- eezs 
mask[!is.na(mask)] <- 1
mask[used==1 | is.na(mask)] <- 0
plot(mask)

# Compare mask to raster template
raster::compareRaster(mask, ras_temp)

# Export mask
writeRaster(mask, file=file.path(datadir, "mask_10km.tif"), overwrite=T)

