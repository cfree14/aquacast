

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
wdpadir <- "/Users/cfree/Dropbox/Chris/UCSB/data/wdpa/WDPA_Sep2019_Public/WDPA_Sep2019_Public.gdb/WDPA_Sep2019_Public.gdb/WDPA_Sep2019_Public.gdb"

# Projections
moll <- CRS("+proj=moll")
wgs84 <- CRS("+init=epsg:4326")

# Read WDPA data
sf::st_layers(dsn=wdpadir)
data_orig <- sf::st_read(dsn=wdpadir, layer="WDPA_poly_Sep2019") # %>% st_transform("+proj=moll")
data_orig_df <- data_orig %>% st_drop_geometry()

# Read raster template
ras_temp <- raster(file.path(tempdir, "world_raster_template_10km.tif"))


# Build data
################################################################################

# Inspect
table(data_orig_df$MARINE)
table(data_orig_df$STATUS)
table(data_orig_df$IUCN_CAT)

# Build data
data <- data_orig %>% 
  filter(MARINE %in% c(1,2)) %>% 
  filter(STATUS %in% c("Designated", "Established", "Inscribed")) %>% 
  filter(IUCN_CAT %in% c("Ia", "Ib", "II", "III")) %>%
  st_transform("+proj=moll")

# Rasterize data
data_ras <- fasterize(sf=data, raster=ras_temp, background=NA)

# Plot data
plot(data_ras)


# Export data
################################################################################

# Export data
writeRaster(data_ras, file=file.path(datadir, "mpa_mask_10km.grd"), overwrite=T)




