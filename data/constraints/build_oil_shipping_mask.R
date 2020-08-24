

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

# Projections
moll <- CRS("+proj=moll")
wgs84 <- CRS("+init=epsg:4326")

# Read impact layers
oil <- raster("data/constraints/cum_impacts/raw_2013_oil_rigs_lzw_mol_20150714093841/oil_rigs.tif")
shipping <- raster("data/constraints/cum_impacts/raw_2013_shipping_mol_20150714094045/shipping.tif")

# Read raster template
ras_temp <- raster(file.path(tempdir, "world_raster_template_10km.tif"))


# Build shipping data
################################################################################

# Plot shipping
plot(shipping)

# Shipping values
values_all <- getValues(shipping)
values_nonzero <- values_all[values_all>0 & !is.na(values_all)]

# Top-5th percentile of shipping
# top5 <- quantile(values_nonzero, probs=0.95)

# Create to-5th percentile shipping raster
shipping_top5 <- shipping >= 5000

# Plot shipping mask
plot(shipping_top5)

# Project to match template
shipping_mask <- projectRaster(from=shipping_top5, to=ras_temp)

# Export data
writeRaster(shipping_mask, file=file.path(datadir, "shipping_mask_10km.grd"), overwrite=T)


# Build oil data
################################################################################

# Project to match template
oil_mask <- projectRaster(from=oil, to=ras_temp)
writeRaster(oil_mask, file=file.path(datadir, "oil_mask_10km.grd"))




