

# Clear
rm(list = ls())

# Setup
#####################################################################################

# Packages
library(rgeos)
library(raster)
library(tidyverse)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(fasterize)

# Directories
tempdir <- "data/template"
costdir <-  "data/costs/data"
eezdir <- "data/eezs"

# Read raster template
ras_temp <- raster(file.path(tempdir, "world_raster_template_10km.tif"))
values(ras_temp) <- 1:ncell(ras_temp)

# Read EEZ raster and key
eezs <- raster(file.path(eezdir, "eezs_v10_raster_10km.tif"))
eezs_sf <- readRDS(file.path(eezdir, "eezs_v10_polygons.Rds"))
eez_key <- read.csv(file.path(eezdir, "eezs_v10_key.csv"), as.is=T)
compareRaster(eezs, ras_temp)

# Get coastline
coast1 <- rnaturalearth::ne_coastline(scale="small", returnclass="sf") %>% sf::st_transform(crs(ras_temp)) %>% sf::as_Spatial() # most coarse
coast2 <- rnaturalearth::ne_coastline(scale="medium", returnclass="sf") %>% sf::st_transform(crs(ras_temp))
coast3 <- rnaturalearth::ne_coastline(scale="large", returnclass="sf") %>% sf::st_transform(crs(ras_temp)) # most detailed

# Get land
land <- rnaturalearth::ne_countries(scale="large", returnclass="sf") %>% sf::st_transform(crs(ras_temp))


# Calculate distance from land
#####################################################################################

# Rasterize land
land_ras <- fasterize(sf=land, raster=ras_temp, background=NA)

# Distance from land
cdist_m <- raster::distance(land_ras)
cdist_km <- cdist_m / 1000

# Plot distance from land
plot(cdist_km)

# Check against template raster
compareRaster(cdist_km, ras_temp)

# Export
writeRaster(cdist_km, filename=file.path(costdir, "dist_to_coast_km_10km_inhouse.grd"), format="raster")






