

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
costdir <- "data/costs/data"
eezdir <- "data/eezs"
tempdir <- "data/template"

# Read data
cdist_km <- raster(file.path(costdir, "dist_to_coast_km_10km_inhouse.grd"))
wb_costs <- read.csv(file.path(costdir, "eez_wb_diesel_income_costs.csv"), as.is=T)

# Read raster template
ras_temp <- raster(file.path(tempdir, "world_raster_template_10km.tif"))

# Read EEZ raster and key
eezs <- raster(file.path(eezdir, "eezs_v10_raster_10km.tif"))
eezs_sf <- readRDS(file.path(eezdir, "eezs_v10_polygons.Rds"))
eez_key <- read.csv(file.path(eezdir, "eezs_v10_key.csv"), as.is=T)
compareRaster(eezs, ras_temp)

# World
world <- rnaturalearth::ne_countries(scale="large", type = "countries", returnclass = "sf") %>% 
  sf::st_transform(crs(eezs))


# Plot data
################################################################################

# Setup theme
my_theme <- theme(axis.text=element_blank(),
                  axis.title=element_blank(),
                  legend.position = "bottom",
                  legend.text=element_text(size=8),
                  legend.title=element_text(size=10),
                  plot.title=element_text(size=9),
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"))

# Distance from shore
###############################

# EEZ maximum distances
eez_dist_nmi <- 200
eez_dist_km <- 370

# Clip distance from shore to EEZs
cdist_km_eezs <- mask(cdist_km, eezs)
#plot(cdist_km_eezs)
cdist_km_df <- as.data.frame(cdist_km_eezs, xy=T) %>% 
  setNames(c("x", "y", "cdist_km")) %>% 
  filter(!is.na(cdist_km)) %>% 
  mutate(cdist_km_capped=pmin(cdist_km, eez_dist_km))

# Zonal stats to make sure all EEZs have max coast distance of 200 km
cdist_eez_stats <- zonal(x=cdist_km_eezs, z=eezs, fun="max", na.rm=T) %>%
    as.data.frame() %>%
    setNames(c("eez_code", "cdist_km_max")) %>%
    left_join(select(eez_key, eez_code, eez_name, ter1_name, ter1_iso), by="eez_code") %>% 
    arrange(desc(cdist_km_max))

# Plot distance from shore#
# Alt color pallete: RColorBrewer::brewer.pal(n=9, name="Blues")
g0 <- ggplot() +
  geom_tile(cdist_km_df, mapping=aes(x=x, y=y, fill=cdist_km_capped)) +
  geom_sf(data=world, fill="grey80", lwd=0.05, col="white") +
  labs(x="", y="") +
  scale_fill_gradientn(name="Distance from shore (km)",
                       colors=rev(RColorBrewer::brewer.pal(n=9, name="RdBu")), na.value=NA) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  theme_bw() + my_theme
g0


# Hourly wages
###############################

# Create wages raster
wb_costs$wages_usd_hr <- wb_costs$income_usd_yr / (40*52)
wages_usd_hr_ras <- reclassify(eezs, rcl=select(wb_costs, eez_code, wages_usd_hr))
wages_usd_hr_df <- as.data.frame(wages_usd_hr_ras, xy=T) %>% 
  setNames(c("x", "y", "wage_usd_hr")) %>% 
  filter(!is.na(wage_usd_hr))
  
# Quick plot
# plot(wages_usd_hr_ras, main="Worker wages (USD per hour)", xaxt="n", yaxt="n",
#      col=freeR::colorpal(RColorBrewer::brewer.pal(n=9, name="RdYlBu"), 50))

# Plot wages
g1 <- ggplot() +
  geom_tile(wages_usd_hr_df, mapping=aes(x=x, y=y, fill=wage_usd_hr)) +
  geom_sf(data=world, fill="grey80", lwd=0.05, col="white") +
  labs(x="", y="") +
  scale_fill_gradientn(name="Median hourly wages\n(USD per hour)",
                       colors=RColorBrewer::brewer.pal(n=9, name="Greens"), na.value=NA) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  theme_bw() + my_theme
#g1


# Diesel cost
###############################

# Diesel cost
fuel_usd_l_ras <- reclassify(eezs, rcl=select(wb_costs, eez_code, diesel_usd_l))
fuel_usd_l_df <- as.data.frame(fuel_usd_l_ras, xy=T) %>% 
  setNames(c("x", "y", "fuel_usd_l")) %>% 
  filter(!is.na(fuel_usd_l))

# Plot diesel cost
g2 <- ggplot() +
  geom_tile(fuel_usd_l_df, mapping=aes(x=x, y=y, fill=fuel_usd_l)) +
  geom_sf(data=world, fill="grey80", lwd=0.05, col="white") +
  labs(x="", y="") +
  scale_fill_gradientn(name="Median diesel cost\n(USD per liter)",
                       colors=RColorBrewer::brewer.pal(n=9, name="YlOrBr"), na.value=NA) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  theme_bw() + my_theme
#g1


# Merge and export
##############################

# Merge
g <- grid.arrange(g0, g1, g2, nrow=3)

# Export
ggsave(g, filename=file.path(plotdir, "FigS21_costs_cdist_wages_diesel.png"), 
       width=6.5, height=11.25, units="in", dpi=600)

