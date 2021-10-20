

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
datadir <- "/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/blue-paper-2/data/climate/Song_etal_2020/4rasters_scaled"

# Read data
ssp126 <- brick(file.path(datadir, "/Song_etal_2020_SSP126_sig_wave_height_annual_max_scaled.gri"))
ssp245 <- brick(file.path(datadir, "/Song_etal_2020_SSP245_sig_wave_height_annual_max_scaled.gri"))
ssp585 <- brick(file.path(datadir, "/Song_etal_2020_SSP585_sig_wave_height_annual_max_scaled.gri"))

# Wave height limit
thresh_m <- 5

# Build data
################################################################################

# Subset data
yrs <- c(2021,2051,2100)
ssp126 <- ssp126[[paste0("X", yrs)]]
ssp245 <- ssp245[[paste0("X", yrs)]]
ssp585 <- ssp585[[paste0("X", yrs)]]

# Create rasters
ssp126_df <- as.data.frame(ssp126, xy=T) %>% mutate(ssp="SSP 2.6")
ssp245_df <- as.data.frame(ssp245, xy=T) %>% mutate(ssp="SSP 4.5")
ssp585_df <- as.data.frame(ssp585, xy=T) %>% mutate(ssp="SSP 8.5")

# Merge rasters
data <- rbind(ssp126_df, ssp245_df,  ssp585_df) %>% 
  select(ssp, everything()) %>% 
  gather(key="year", value="value", 4:ncol(.)) %>% 
  rename(lat_dd=y, long_dd=x) %>% 
  mutate(year=gsub("X", "", year)) %>% 
  select(ssp, year, everything())

# Check stats
# stats <- data %>% 
#   group_by(ssp, year) %>% 
#   summarize(mean=mean(value, na.rm=T))

# Subset data for testing
# data_test <- data %>% 
#   sample_frac(size=0.1)

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
  sf::st_transform(crs( ssp126 ))

# Plot data
g <- ggplot(data, aes(x=long_dd, y=lat_dd, fill=value)) +
  facet_grid(ssp ~ year) +
  # World
  geom_sf(data=world, fill="grey90", color="white", lwd=0.1, inherit.aes = F) +
  # Variable
  geom_raster() +
  # Labels
  labs(x="", y="") +
  # Legend
  scale_fill_gradientn(name="Wave height (m)", colors=RColorBrewer::brewer.pal(9, "Blues"), na.value = NA) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + my_theme


# Export figure
outfile <- "figure_climate_proj_wave_height_m.png"
ggsave(g, filename=file.path(plotdir, outfile), 
       width=6.5, height=4.25, units="in", dpi=600)

