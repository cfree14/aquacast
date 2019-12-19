

# Clear
rm(list = ls())

# Setup
#####################################################################################

# Packages
library(sf)
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
# coast1 <- rnaturalearth::ne_coastline(scale="small", returnclass="sf") %>% sf::st_transform(crs(ras_temp)) %>% sf::as_Spatial() # most coarse
# coast2 <- rnaturalearth::ne_coastline(scale="medium", returnclass="sf") %>% sf::st_transform(crs(ras_temp))
# coast3 <- rnaturalearth::ne_coastline(scale="large", returnclass="sf") %>% sf::st_transform(crs(ras_temp)) # most detailed

# Get most countries
most <- rnaturalearth::ne_countries(scale="large", returnclass="sf") %>% sf::st_transform(crs(ras_temp))
most <- rnaturalearth::ne_countries(scale="large", returnclass="sf") %>% sf::st_transform(crs(ras_temp))

# Get and buffer tiny countries
tiny <- rnaturalearth::ne_countries(type="tiny_countries", returnclass="sf") %>% sf::st_transform(crs(ras_temp))
tiny_2km <- sf::st_buffer(tiny, dist=2000) %>% 
  sf::st_cast("MULTIPOLYGON")

# Create missing islands
missing_2km <- tibble(name_long="Howland and Baker islands",
                  x=-176.616389,
                  y=0.806667) %>% 
  sf::st_as_sf(coords=c("x", "y"), crs=CRS("+init=epsg:4326")) %>% 
  sf::st_transform(crs(ras_temp)) %>% 
  sf::st_buffer(dist=2000) %>% 
  sf::st_cast("MULTIPOLYGON")

# Merge
land <- rbind(most %>% select(name_long, geometry), 
              tiny_2km %>% select(name_long, geometry),
              missing_2km)
land_df <- sf::st_drop_geometry(land)

# Plot countries
# g <- ggplot() +
#   geom_sf(most, mapping=aes(fill="grey60")) +
#   geom_sf(tiny, mapping=aes(col="black"))
# g

# Howland and Baker Island
# Bouvet Island Exclusive Economic Zone
# Amsterdam Island & St. Paul Island Exclusive Economic Zone
# British Indian Ocean Territory Exclusive Economic Zone
# Bermudian Exclusive Economic Zone
# Clipperton Island Exclusive Economic Zone


# Calculate distance from land
#####################################################################################

# Rasterize land (using fasterize package which misses tiny geometries)
# land_ras <- fasterize(sf=land, raster=ras_temp, background=NA)

# Rasterize land (using rasterize)
land_ras <- rasterize(x=land, y=ras_temp, background=NA, getCover=T)
land_ras_rec <- reclassify(land_ras, rcl=matrix(data=c(0,1,1), ncol=3))
land_ras_rec[land_ras_rec==0] <- NA

# Distance from land
cdist_m <- raster::distance(land_ras_rec)
cdist_km <- cdist_m / 1000

# Plot distance from land
plot(cdist_km)

# Check against template raster
compareRaster(cdist_km, ras_temp)

# Export
writeRaster(cdist_km, filename=file.path(costdir, "dist_to_coast_km_10km_inhouse.grd"), format="raster", overwrite=T)






