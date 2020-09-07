

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
plotdir <- "figures"
datadir <- "data/constraints/masks"

# Projections
moll <- CRS("+proj=moll")
wgs84 <- CRS("+init=epsg:4326")

# Read masks
mpa_mask <- raster(file.path(datadir, "mpa_mask_10km.grd")) 
shipping_mask <- raster(file.path(datadir, "shipping_mask_10km.grd")) 
oil_mask <- raster(file.path(datadir, "oil_mask_10km.grd")) 
depth_mask <- raster(file.path(datadir, "depth_10km.grd"))
mask <- raster(file.path(datadir, "depth_10km.grd"))


# Format data
################################################################################

# Quick plots
# plot(mpa_mask)
# plot(oil_mask)
# plot(depth_mask)
# plot(shipping_mask)

# Convert to dataframe
mpa_mask_df <- mpa_mask %>%
  projectRaster(crs=wgs84) %>%
  as.data.frame(xy=T) %>%
  setNames(c("x", "y", "value")) %>%
  filter(!is.na(value) & value!=0)

shipping_mask_df <- shipping_mask %>%
  projectRaster(crs=wgs84) %>%
  as.data.frame(xy=T) %>%
  setNames(c("x", "y", "value")) %>%
  filter(!is.na(value) & value!=0)

depth_mask_df <- depth_mask %>%
  projectRaster(crs=wgs84) %>%
  as.data.frame(xy=T) %>%
  setNames(c("x", "y", "value")) %>%
  filter(!is.na(value) & value!=0)

mask_df <- mask %>%
  projectRaster(crs=wgs84) %>%
  as.data.frame(xy=T) %>%
  setNames(c("x", "y", "value")) %>%
  filter(!is.na(value) & value!=0)

# Sample for testing
mpa_mask_df1 <- sample_frac(mpa_mask_df, size=0.1)
shipping_mask_df1 <- sample_frac(shipping_mask_df, size=0.1)
depth_mask_df1 <- sample_frac(depth_mask_df, size=0.1)
mask_df1 <- sample_frac(mask_df, size=0.1)


# Plot data
################################################################################

# Theme
base_theme <- theme(axis.text=element_blank(), 
                    axis.title=element_blank(),
                    plot.title=element_text(size=10),
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(), 
                    axis.line = element_line(colour = "black"),
                    legend.background = element_rect(fill=alpha('blue', 0)))

# Get world
world <- rnaturalearth::ne_countries(scale="small", type="countries", returnclas="sf")

# Plot MPA mask
g1 <- ggplot() +
  geom_sf(data=world, fill="grey80", color="white", lwd=0.1) +
  geom_tile(data=mpa_mask_df, mapping=aes(x=x, y=y), fill="darkred") +
  labs(title="A. Marine protected areas") +
  theme_bw() + base_theme
g1

# Plot shipping mask
g2 <- ggplot() +
  geom_sf(data=world, fill="grey80", color="white", lwd=0.1) +
  geom_tile(data=shipping_mask_df, mapping=aes(x=x, y=y), fill="darkred") +
  labs(title="B. High density shipping lanes") +
  theme_bw() + base_theme
g2

# Plot depth mask
g3 <- ggplot() +
  geom_sf(data=world, fill="grey80", color="white", lwd=0.1) +
  geom_tile(data=depth_mask_df, mapping=aes(x=x, y=y), fill="darkred") +
  labs(title="C. Waters less than 200 m deep") +
  theme_bw() + base_theme
g3

# Plot overall mask
g4 <- ggplot() +
  geom_sf(data=world, fill="grey80", color="white", lwd=0.1) +
  geom_tile(data=mask_df, mapping=aes(x=x, y=y), fill="darkred") +
  labs(title="D. Areas considered for mariculture development") +
  theme_bw() + base_theme
g4

# Merge plots
g <- gridExtra::grid.arrange(g1,g2,g3,g4, ncol=2)

# Export
ggsave(g, filename=file.path(plotdir, "FigS17_use constraints.png"), 
       width=6.5, height=4, units="in", dpi=600)



