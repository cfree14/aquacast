
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(raster)
library(ggplot2)
library(tidyverse)
library(countrycode)
library(rnaturalearth)
library(grid)
library(gridExtra)

# Directories
datadir <- "output/processed"
plotdir <- "figures"

# Read data
biv <- read.csv(file.path(datadir, "bivalve_mariculture_potential_by_eez_rcp.csv"))
fin <- read.csv(file.path(datadir, "finfish_mariculture_potential_by_eez_rcp_feed_scenario.csv"))

# Read current production
curr <- read.csv("data/feed_params/processed/FAO_2013_2017_maq_prod_averages_by_country.csv")

# Read EEZs
eezdir <- "data/eezs"
eezs <- readRDS(file.path(eezdir, "eezs_v10_polygons.Rds"))
eez_key <- read.csv(file.path(eezdir, "eezs_v10_key.csv"), as.is=T)

# Projections
wgs84 <- "+init=epsg:4326"
moll <- "+proj=moll"

# Create EEZ points files
eezs_pts <- eezs %>% 
  sf::st_transform(moll) %>% 
  sf::st_centroid()

# Get countries
world <- rnaturalearth::ne_countries(scale="large", type = "countries", returnclass = "sf") %>% 
  mutate(iso3=countrycode(name_long, "country.name", "iso3c"))


# Plot bivalves
################################################################################

# Add to EEZ layer
biv_sf_pts <- eezs_pts %>% 
  left_join(select(biv, rcp, year, eez_name, area_sqkm, prod_mt, profits_usd), by="eez_name") %>% 
  filter(year==2100)
biv_sf_poly <- eezs %>% 
  left_join(select(biv, rcp, year, eez_name, area_sqkm, prod_mt, profits_usd), by="eez_name") %>% 
  filter(year==2100 & !is.na(prod_mt))

# My theme
my_theme <- theme(axis.text=element_text(size=8),
                  axis.title=element_text(size=10),
                  plot.title=element_text(size=10),
                  strip.text = element_text(size=8),
                  legend.title=element_text(size=8),
                  legend.text = element_text(size=6),
                  panel.grid.major = element_line(size=0.2),
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"))

# Plot data (polygon version)
g1 <- ggplot(biv_sf_poly) +
  geom_sf(aes(fill=prod_mt/1e9), lwd=0.05, col="grey30") + # poly version
  facet_grid(~rcp) +
  geom_sf(data=world, fill="grey80", lwd=0.05, color="white") +
  labs(title="A. Bivalve mariculture") +
  scale_fill_gradientn(name="Production\npotential\n(billions of mt)",
                       colors=RColorBrewer::brewer.pal(9, "Blues"), na.value="grey80") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  theme_bw() + my_theme +
  theme(legend.position = "right")
#g1

# # Plot data (point version)
# g1 <- ggplot(biv_sf_pts) +
#   geom_sf(aes(color=prod_mt/1e9)) + # point version
#   facet_grid(~rcp) +
#   geom_sf(data=world, fill="grey80", lwd=0.05, color="white") +
#   labs(title="A. Bivalve mariculture") +
#   scale_color_gradientn(name="Production\npotential\n(billions of mt)", 
#                         colors=RColorBrewer::brewer.pal(9, "Blues"), na.value="grey80") +
#   guides(color = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
#   theme_bw() + my_theme +
#   theme(legend.position = "right")
# #g1

# Plot finfish (current)
################################################################################

# Subset current
fin_curr <- fin %>% 
  subset(feed_scen=="Business-as-usual" & dev_pattern=="Current development" & year==2100 & rcp%in%c("RCP 2.6", "RCP 8.5"))

# Add to EEZ layer
fin_curr_sf_pts <- eezs_pts %>% 
  left_join(select(fin_curr, rcp, year, eez_name, area_sqkm, prod_mt, profits_usd), by="eez_name") %>% 
  filter(year==2100)
fin_curr_sf_poly <- eezs %>% 
  left_join(select(fin_curr, rcp, year, eez_name, area_sqkm, prod_mt, profits_usd), by="eez_name") %>% 
  filter(year==2100 & !is.na(prod_mt))

# Plot data (polygons)
g2 <- ggplot(fin_curr_sf_poly) +
  geom_sf(aes(fill=prod_mt/1e6), lwd=0.05, col="grey30") + # poly version
  facet_grid(~rcp) +
  geom_sf(data=world, fill="grey80", lwd=0.05, color="white") +
  labs(title="B. Finfish mariculture without reforms") +
  scale_fill_gradientn(name="Production\npotential\n(millions of mt)",
                        colors=RColorBrewer::brewer.pal(9, "Oranges"), na.value="grey80") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  theme_bw() + my_theme +
  theme(legend.position = "right")
#g2

# # Plot data (points)
# g2 <- ggplot(fin_curr_sf_pts) +
#   geom_sf(aes(color=prod_mt/1e6)) + # point version
#   facet_grid(~rcp) +
#   geom_sf(data=world, fill="grey80", lwd=0.05, color="white") +
#   labs(title="B. Finfish mariculture without reforms") +
#   scale_color_gradientn(name="Production\npotential\n(millions of mt)", 
#                         colors=RColorBrewer::brewer.pal(9, "Oranges"), na.value="grey80") +
#   guides(color = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
#   theme_bw() + my_theme +
#   theme(legend.position = "right")
# #g2


# Plot finfish (current)
################################################################################

# Subset current
fin_reform <- fin %>% 
  subset(feed_scen=="Progressive reform" & dev_pattern=="Equal development" & year==2100 & rcp%in%c("RCP 2.6", "RCP 8.5"))

# Add to EEZ layer
fin_reform_sf_pts <- eezs_pts %>% 
  left_join(select(fin_reform, rcp, year, eez_name, area_sqkm, prod_mt, profits_usd), by="eez_name") %>% 
  filter(year==2100)
fin_reform_sf_poly <- eezs %>% 
  left_join(select(fin_reform, rcp, year, eez_name, area_sqkm, prod_mt, profits_usd), by="eez_name") %>% 
  filter(year==2100 & !is.na(prod_mt))

# Plot data (polygon)
g3 <- ggplot(fin_reform_sf_poly ) +
  geom_sf(aes(fill=prod_mt/1e6), lwd=0.05, col="grey30") + # poly version
  facet_grid(~rcp) +
  geom_sf(data=world, fill="grey80", lwd=0.05, color="white") +
  labs(title="C. Finfish mariculture with progressive reforms") +
  scale_fill_gradientn(name="Production\npotential\n(millions of mt)",
                        colors=RColorBrewer::brewer.pal(9, "Reds"), na.value="grey80") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  theme_bw() + my_theme +
  theme(legend.position = "right")
g3

# # Plot data (points)
# g3 <- ggplot(fin_reform_sf_pts) +
#   geom_sf(aes(color=prod_mt/1e6)) + # point version
#   facet_grid(~rcp) +
#   geom_sf(data=world, fill="grey80", lwd=0.05, color="white") +
#   labs(title="C. Finfish mariculture with progressive reforms") +
#   scale_color_gradientn(name="Production\npotential\n(millions of mt)", 
#                         colors=RColorBrewer::brewer.pal(9, "Reds"), na.value="grey80") +
#   guides(color = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
#   theme_bw() + my_theme +
#   theme(legend.position = "right")
# #g3


# Merge
################################################################################

# Merge plots
g <- grid.arrange(g1, g2, g3, ncol=1)

# Export merged plots
ggsave(g, filename=file.path(plotdir, "Fig2_mariculture.png"), 
       width=6.5, height=6, units="in", dpi=600)
