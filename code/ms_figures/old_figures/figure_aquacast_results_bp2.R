
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

# Read species data
load("data/species/data/aquaculture_species_key.Rdata")
spp_key <- data
rm(data_full, data)

# Read EEZ
eezdir <- "data/eezs"
eezs <- readRDS(file.path(eezdir, "eezs_v10_polygons.Rds"))
eez_key <- read.csv(file.path(eezdir, "eezs_v10_key.csv"), as.is=T)

# World
world <- rnaturalearth::ne_countries(scale="large", type = "countries", returnclass = "sf") %>% 
  mutate(iso3=countrycode(name_long, "country.name", "iso3c"))


# Plot bivalve data
#####################

# Format bivalve data for plotting
biv_plot <- biv %>% 
  filter(rcp=="RCP 8.5" & year==2100) %>% 
  group_by(sov1_name, sov1_iso) %>% 
  mutate(prod_mt=sum(prod_mt, na.rm=T))

# Add to world layer
biv_world <- world %>% 
  left_join(biv_plot, by=c("iso3"="sov1_iso"))

# Add to EEZ layer
# biv_eezs <- eezs %>% 
#   left_join(biv_plot, by="eez_code") %>% 
#   filter(!is.na(prod_mt))

# Plot maps
g1 <- ggplot() +
  # geom_sf(data=world, fill="grey80", lwd=0.05, col="white") +
  geom_sf(data=biv_world, aes(fill=prod_mt/1e9), lwd=0.05, col="grey30") +
  labs(title="A. Bivalve mariculture") +
  scale_fill_gradientn(name="Production \n(billions of mt)", 
                       colors=RColorBrewer::brewer.pal(9, "Blues"), na.value="grey80") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  theme_bw()
#g1

# Export
ggsave(g1, filename=file.path(plotdir, "figure_results_bivalve_bp2.png"), 
       width=6.5, height=2.5, units="in", dpi=600)

# Plot finfish base
#####################

# Format finfish (base case) data for plotting
fin1_plot <- fin %>% 
  filter(rcp=="RCP 8.5" & year==2100) %>% 
  filter(feed_scen=="Base case" & dev_pattern=="Equal development") %>% 
  group_by(sov1_name, sov1_iso) %>% 
  mutate(prod_mt=sum(prod_mt, na.rm=T))

# Add to world layer
fin1_world <- world %>% 
  left_join(fin1_plot, by=c("iso3"="sov1_iso"))

# Add to EEZ layer
# fin2_eezs <- eezs %>% 
#   left_join(fin2_plot, by="eez_code") %>% 
#   filter(!is.na(prod_mt))

# Plot maps
g2 <- ggplot() +
  # geom_sf(data=world, fill="grey80", lwd=0.05, col="white") +
  geom_sf(data=fin1_world, aes(fill=prod_mt/1e6), lwd=0.05, col="grey30") +
  labs(title="B. Finfish mariculture (business-as-usual)") +
  scale_fill_gradientn(name="Production \n(millions of mt)", 
                       colors=RColorBrewer::brewer.pal(9, "Reds"), na.value="grey80") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  theme_bw()

# Export
ggsave(g2, filename=file.path(plotdir, "figure_results_finfish_base_bp2.png"), 
       width=6.5, height=2.5, units="in", dpi=600)

# Plot finfish reform
#####################

# Format finfish (progressive reform) data for plotting
fin2_plot <- fin %>% 
  filter(rcp=="RCP 8.5" & year==2100) %>% 
  filter(feed_scen=="Progressive reform" & dev_pattern=="Equal development") %>% 
  group_by(sov1_name, sov1_iso) %>% 
  mutate(prod_mt=sum(prod_mt, na.rm=T))

# Add to world layer
fin2_world <- world %>% 
  left_join(fin2_plot, by=c("iso3"="sov1_iso"))

# Add to EEZ layer
# fin2_eezs <- eezs %>% 
#   left_join(fin2_plot, by="eez_code") %>% 
#   filter(!is.na(prod_mt))

# Plot maps
g3 <- ggplot() +
  # geom_sf(data=world, fill="grey80", lwd=0.05, col="white") +
  geom_sf(data=fin2_world, aes(fill=prod_mt/1e6), lwd=0.05, col="grey30") +
  labs(title="C. Finfish mariculture (progressive reforms)") +
  scale_fill_gradientn(name="Production \n(millions of mt)", 
                       colors=RColorBrewer::brewer.pal(9, "Reds"), na.value="grey80") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  theme_bw()

# Export
ggsave(g3, filename=file.path(plotdir, "figure_results_finfish_reform_bp2.png"), 
       width=6.5, height=2.5, units="in", dpi=600)

# Merge and export
##############################

# Merge plots
g <- grid.arrange(g1, g2, g3, ncol=1)

# Export merged plots
ggsave(g, filename=file.path(plotdir, "figure_results_bivalve_finfish_bp2.png"), 
       width=6.5, height=8, units="in", dpi=600)
