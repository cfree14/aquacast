

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(sf)
library(raster)
library(tidyverse)
library(rnaturalearth)

# Directories
datadir <- "data/constraints/masks"
plotdir <- "data/constraints/figures"

# Projections
moll <- CRS("+proj=moll")
wgs84 <- CRS("+init=epsg:4326")

# Read masks
mpa_mask <- raster(file.path(datadir, "mpa_mask_10km.grd")) 
shipping_mask <- raster(file.path(datadir, "shipping_mask_10km.grd")) 
oil_mask <- raster(file.path(datadir, "oil_mask_10km.grd")) 
depth_mask <- raster(file.path(datadir, "depth_10km.grd"))
mask <- raster(file.path(datadir, "depth_10km.grd"))


# Plot data
################################################################################

# World layer
world <- rnaturalearth::ne_countries(scale = "large", type = "countries", returnclass = "sf") %>% sf::st_transform(wgs84)



plot(mpa_mask)
plot(oil_mask)
plot(depth_mask)
plot(shipping_mask)



# # Plot MPA mask
# mpa_mask_df <- mpa_mask %>% 
#   projectRaster(crs=wgs84) %>% 
#   as.data.frame(xy=T) %>% 
#   setNames(c("x", "y", "value")) %>% 
#   filter(!is.na(value))
# 
# g <- ggplot() +
#   geom_tile(data=mpa_mask_df, mapping=aes(x=x, y=y, fill=value)) +
#   geom_sf(data=world, fill="grey80", color="white", lwd=0.05)
# g



