

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(cmocean)
library(ncdf4)
library(raster)
library(tidyverse)
library(lubridate)
library(cowplot)
library(grid)
library(gridExtra)
library(rnaturalearth)

# Directories
plotdir <- "figures"
datadir <- "data/constraints/masks"

# Read data
mpas <- raster(file.path(datadir, "mpa_mask_10km.tif"))
shipping <- raster(file.path(datadir, "shipping_mask_10km.tif"))

# World layer
world <- rnaturalearth::ne_countries(scale = "large", type = "countries", returnclass = "sf")
world_proj <- sf::st_transform(world, crs=crs(shipping))

# Format masks
mpas_df <- mpas %>% 
  as.data.frame(xy=T) %>% 
  setNames(c("x", "y", "value")) %>% 
  mutate(constraint="MPA",
         value=ifelse(value==0, NA, 1)) 
shipping_df <- shipping %>% 
  as.data.frame(xy=T) %>% 
  setNames(c("x", "y", "value")) %>% 
  mutate(constraint="Shipping",
         value=ifelse(value==0, NA, 1))
constraint_df <- rbind(mpas_df, shipping_df) %>% 
  filter(!is.na(value))

# Plot data
################################################################################

# Plot
g <- ggplot(world_proj) +
  geom_sf(fill="grey80", lwd=0.05, color="white") + 
  geom_raster(constraint_df, mapping=aes(x=x, y=y, fill=constraint)) +
  labs(x="", y="") +
  scale_fill_discrete(name="Human use constraint") +
  theme_bw()
g
