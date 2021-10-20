

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

# Directories
plotdir <- "figures"
datadir <- "/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/blue-paper-2/data/climate/GFDL-ESM2G/4rasters_scaled"
wavedir <- "/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/blue-paper-2/data/climate/Song_etal_2020/4rasters_scaled"


# Current speed mask
################################################################################

# Read data
########################

# Read data
rcp85 <- brick(file.path(datadir, "GFDL_ESM2G_rcp85_current_speed_mask.grd"))
rcp60 <- brick(file.path(datadir, "GFDL_ESM2G_rcp60_current_speed_mask.grd"))
rcp45 <- brick(file.path(datadir, "GFDL_ESM2G_rcp45_current_speed_mask.grd"))
rcp26 <- brick(file.path(datadir, "GFDL_ESM2G_rcp26_current_speed_mask.grd"))

# Build data
########################

# Subset data
yrs <- c(2021,2051,2100)
rcp85 <- rcp85[[paste0("X", yrs)]]
rcp60 <- rcp60[[paste0("X", yrs)]]
rcp45 <- rcp45[[paste0("X", yrs)]]
rcp26 <- rcp26[[paste0("X", yrs)]]

# Create rasters
rcp85_df <- as.data.frame(rcp85, xy=T) %>% mutate(rcp="RCP 8.5")
rcp60_df <- as.data.frame(rcp60, xy=T) %>% mutate(rcp="RCP 6.0")
rcp45_df <- as.data.frame(rcp45, xy=T) %>% mutate(rcp="RCP 4.5")
rcp26_df <- as.data.frame(rcp26, xy=T) %>% mutate(rcp="RCP 2.6")
  
# Merge rasters
data <- rbind(rcp26_df, rcp45_df, rcp60_df, rcp85_df) %>% 
  select(rcp, everything()) %>% 
  gather(key="year", value="value", 4:ncol(.)) %>% 
  rename(lat_dd=y, long_dd=x) %>% 
  mutate(year=gsub("X", "", year)) %>% 
  select(rcp, year, everything()) %>% 
  filter(!is.na(value) & value==1)

data_test <- data %>% 
  sample_frac(size=0.05)
  
# Plot data
########################

# Setup theme
my_theme <- theme(legend.margin=margin(0,0,0,0),
                  legend.box.margin=margin(rep(0,4)),
                  axis.text=element_blank(),
                  axis.ticks=element_blank(),
                  axis.title=element_text(size=10),
                  plot.title=element_text(size=12),
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), legend.position="bottom")
  
# World
world <- rnaturalearth::ne_countries(scale="small", returnclass = "sf") %>% 
  sf::st_transform(crs( rcp26 ))
  
# Plot data
g <- ggplot(data, aes(x=long_dd, y=lat_dd, fill=value)) +
  facet_grid(rcp ~ year) +
  # World
  geom_sf(data=world, fill="grey90", color="white", lwd=0.1, inherit.aes = F) +
  # Variable
  geom_tile() +
  # Labels
  labs(x="", y="") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position="none")
  
# Export figure
outfile <- "figure_current_speed_mask.png"
ggsave(g, filename=file.path(plotdir, outfile), 
       width=6.5, height=6.5, units="in", dpi=600)


# Wave height mask
################################################################################

# Read data
########################

# Read data
rcp85 <- brick(file.path(wavedir, "Song_etal_2020_SSP585_sig_wave_height_annual_max_scaled.grd"))
rcp45 <- brick(file.path(wavedir, "Song_etal_2020_SSP245_sig_wave_height_annual_max_scaled.grd"))
rcp26 <- brick(file.path(wavedir, "Song_etal_2020_SSP126_sig_wave_height_annual_max_scaled.grd"))

# Build data
########################

# Subset data
yrs <- c(2021,2051,2100)
rcp85 <- rcp85[[paste0("X", yrs)]]
rcp45 <- rcp45[[paste0("X", yrs)]]
rcp26 <- rcp26[[paste0("X", yrs)]]

# Create rasters
rcp85_df <- as.data.frame(rcp85, xy=T) %>% mutate(rcp="SSP 8.5")
rcp45_df <- as.data.frame(rcp45, xy=T) %>% mutate(rcp="SSP 4.5")
rcp26_df <- as.data.frame(rcp26, xy=T) %>% mutate(rcp="SSP 2.6")

# Merge rasters
data <- rbind(rcp26_df, rcp45_df, rcp85_df) %>% 
  select(rcp, everything()) %>% 
  gather(key="year", value="value", 4:ncol(.)) %>% 
  rename(lat_dd=y, long_dd=x) %>% 
  mutate(year=gsub("X", "", year)) %>% 
  select(rcp, year, everything()) %>% 
  filter(!is.na(value) & value==1)

# data_test <- data %>% 
#   sample_frac(size=0.05)

# Plot data
########################

# Setup theme
my_theme <- theme(legend.margin=margin(0,0,0,0),
                  legend.box.margin=margin(rep(0,4)),
                  axis.text=element_blank(),
                  axis.ticks=element_blank(),
                  axis.title=element_text(size=10),
                  plot.title=element_text(size=12),
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), legend.position="bottom")

# World
world <- rnaturalearth::ne_countries(scale="small", returnclass = "sf") %>% 
  sf::st_transform(crs( rcp26 ))

# Plot data
g <- ggplot(data, aes(x=long_dd, y=lat_dd, fill=value)) +
  facet_grid(rcp ~ year) +
  # World
  geom_sf(data=world, fill="grey90", color="white", lwd=0.1, inherit.aes = F) +
  # Variable
  geom_tile() +
  # Labels
  labs(x="", y="") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position="none")

# Export figure
outfile <- "figure_wave_height_mask.png"
ggsave(g, filename=file.path(plotdir, outfile), 
       width=6.5, height=6.5, units="in", dpi=600)
  
