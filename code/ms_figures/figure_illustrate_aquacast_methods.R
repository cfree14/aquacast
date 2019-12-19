
# Clear workspace
rm(list = ls())

# Turn off scientific notation
options(scipen=999)

# Setup
################################################################################

# Packages
library(raster)
library(tidyverse)
library(grid)
library(gridExtra)

# Directories
datadir <- "data/species/data"
outputdir <- "output/raw"
plotdir <- "figures"

# Read data
load(file.path(datadir, "aquaculture_species_key.Rdata"))

# Species to do
spp <- "Salmo salar"
sdata <- filter(data, species==spp)
comm_name <- "Atlantic salmon"

# Read data
prod_mt <- raster(file.path(outputdir, paste0("RCP45_Finfish_", gsub(" ", "_", spp), "_production_mt_yr.tif")))
costs_usd <- raster(file.path(outputdir, paste0("RCP45_Finfish_", gsub(" ", "_", spp), "_costs_usd_yr.tif")))
revenues_usd <- raster(file.path(outputdir, paste0("RCP45_Finfish_", gsub(" ", "_", spp), "_revenues_usd_yr.tif")))
profits_usd <- raster(file.path(outputdir, paste0("RCP45_Finfish_", gsub(" ", "_", spp), "_profits_usd_yr.tif")))

# World
world <- rnaturalearth::ne_countries(scale="large", type = "countries", returnclass = "sf") %>% 
  sf::st_transform(crs(prod_mt))

# Values per farm
unique(prod_mt)/100
unique(revenues_usd)/100/1e6


# Plot data
################################################################################

# Setup theme
my_theme <- theme(axis.text=element_blank(),
                  axis.title=element_blank(),
                  legend.position = "right",
                  legend.text=element_text(size=8),
                  legend.title=element_text(size=10),
                  plot.title=element_text(size=9),
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"))

# Suitability
###########################

# Convert to df
prod_df <- as.data.frame(prod_mt, xy=T) %>% 
  setNames(c("x", "y", "value")) %>% 
  mutate(value=ifelse(value==0, NA, value)) %>% 
  filter(!is.na(value))

# Plot fuel costs
g1 <- ggplot() +
  geom_tile(prod_df, mapping=aes(x=x, y=y, fill=value)) +
  geom_sf(data=world, fill="grey80", lwd=0.05, col="white") +
  labs(x="", y="", title="Atlantic salmon (Salmo salar)") +
  theme_bw() + my_theme
g1

# Export figure
ggsave(g1, filename=file.path(plotdir, "figure_aquacast_methods_suitability.png"), 
       width=6.5, height=3, units="in", dpi=600)


# Costs
###########################

# Convert to df
costs_df <- as.data.frame(costs_usd, xy=T) %>% 
  setNames(c("x", "y", "value")) %>% 
  filter(!is.na(value))

# Plot fuel costs
g2 <- ggplot() +
  geom_tile(costs_df, mapping=aes(x=x, y=y, fill=value/1e6/100)) +
  geom_sf(data=world, fill="grey80", lwd=0.05, col="white") +
  labs(x="", y="", title="Atlantic salmon (Salmo salar)") +
  scale_fill_gradientn(name="Annual cost per farm\n(USD millions)",
                       colors=RColorBrewer::brewer.pal(n=9, "YlOrRd")) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  theme_bw() + my_theme
g2

# Export figure
ggsave(g2, filename=file.path(plotdir, "figure_aquacast_methods_costs.png"), 
       width=6.5, height=3, units="in", dpi=600)

# Profits
###########################

# Convert to df
profits_df <- as.data.frame(profits_usd, xy=T) %>% 
  setNames(c("x", "y", "value")) %>% 
  filter(!is.na(value)) %>% 
  mutate(profitable=ifelse(value>0, "Profitable", "Unprofitable"))

# Plot fuel costs
g3 <- ggplot() +
  geom_tile(profits_df, mapping=aes(x=x, y=y, fill=value/1e6/100)) +
  geom_sf(data=world, fill="grey80", lwd=0.05, col="white") +
  labs(x="", y="", title="Atlantic salmon (Salmo salar)") +
  # scale_fill_gradientn(name="Annual profits per farm\n(USD millions)",
  #                      colors=RColorBrewer::brewer.pal(n=9, "RdBu")) +
  scale_fill_gradient2(name="Annual profits per farm\n(USD millions)", 
                       midpoint=0) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  theme_bw() + my_theme
g3

# Export figure
ggsave(g3, filename=file.path(plotdir, "figure_aquacast_methods_profits.png"), 
       width=6.5, height=3, units="in", dpi=600)


# Profitable areas
###########################

# Plot fuel costs
g4 <- ggplot() +
  geom_tile(profits_df, mapping=aes(x=x, y=y, fill=profitable)) +
  geom_sf(data=world, fill="grey80", lwd=0.05, col="white") +
  labs(x="", y="", title="Atlantic salmon (Salmo salar)") +
  scale_fill_manual(name="", values=c("lightblue", "darkred")) +
  theme_bw() + my_theme
g4

# Export figure
ggsave(g4, filename=file.path(plotdir, "figure_aquacast_methods_profitable_yn.png"), 
       width=6.5, height=3, units="in", dpi=600)


# Priority areas
###########################

# Feed cap
feed_avail_mt_yr <- 10*1e6

# Convert to df
profits_df1 <- as.data.frame(profits_usd, xy=T) %>%
  # Rename columns
  setNames(c("x", "y", "profit_usd_yr")) %>% 
  # Reduce to suitable areas
  filter(!is.na(profit_usd_yr)) %>% 
  # Add columns
  mutate(profitable=ifelse(profit_usd_yr>0, "Profitable", "Unprofitable"),
         prod_mt_yr=1258.038,
         fcr=1.3,
         feed_mt_yr= prod_mt_yr*fcr) %>% 
  # Sort by profitability and calculate cumulative feed demand
  arrange(desc(profit_usd_yr)) %>% 
  mutate(feed_mt_yr_cum=cumsum(feed_mt_yr),
         developable=feed_mt_yr_cum<feed_avail_mt_yr) %>% 
  # Classify cell type
  mutate(type=ifelse(developable==T, "Developed",
                     ifelse(profitable=="Profitable", "Profitable but feed limited", "Unprofitable")))
  
# Plot fuel costs
g5 <- ggplot() +
  geom_tile(profits_df1, mapping=aes(x=x, y=y, fill=type)) +
  geom_sf(data=world, fill="grey80", lwd=0.05, col="white") +
  labs(x="", y="", title="Atlantic salmon (Salmo salar)") +
  scale_fill_manual(name="", values=c("blue", "darkgreen", "darkred")) +
  theme_bw() + my_theme
g5

# Export figure
ggsave(g5, filename=file.path(plotdir, "figure_aquacast_methods_possible_yn.png"), 
       width=6.5, height=3, units="in", dpi=600)










