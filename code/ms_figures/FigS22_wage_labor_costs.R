

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
datadir <- "data/costs/data"

# Read data
wages <- raster(file.path(datadir, "wage_cost_per_farm.tif"))
fuel <- raster(file.path(datadir, "fuel_cost_per_farm.tif"))

# World
world <- rnaturalearth::ne_countries(scale="large", type = "countries", returnclass = "sf") %>% 
  sf::st_transform(crs(fuel))

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

# Fuel costs
##############################

# Format fuel costs
fuel_df <- as.data.frame(fuel, xy=T) 
fuel_df1 <- sample_frac(fuel_df, size=0.05)

# Set plotting threshhold
hist(fuel_df$fuel_cost_per_farm)
abline(v=3e6)
fuel_thresh <- 3e6
fuel_df <- fuel_df %>% 
  mutate(fuel_cost_per_farm_capped=ifelse(fuel_cost_per_farm>fuel_thresh, fuel_thresh, fuel_cost_per_farm))

# Plot fuel costs
legend_breaks <- seq(0, fuel_thresh/1e6, 1)
legend_labels <- c(legend_breaks[1:(length(legend_breaks)-1)], paste0(">", legend_breaks[length(legend_breaks)]))
g1 <- ggplot() +
  geom_raster(fuel_df, mapping=aes(x=x, y=y, fill=fuel_cost_per_farm_capped/1e6)) +
  geom_sf(data=world, fill="grey80", lwd=0.05, col="white") +
  labs(x="", y="") +
  scale_fill_gradientn(name="Annual fuel cost per farm\n(USD millions per year)",
                       colors=rev(RColorBrewer::brewer.pal(n=9, name="RdYlBu")), na.value=NA,
                       breaks=legend_breaks, labels=legend_labels, limits=c(0, fuel_thresh/1e6)) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  theme_bw() + my_theme


# Labor costs
##############################

# Format fuel costs
wages_df <- as.data.frame(wages, xy=T)
wages_df1 <- sample_frac(wages_df, size=0.05)

# Set plotting threshhold
hist(wages_df$wage_cost_per_farm)
abline(v=6e6)
wage_thresh <- 6e6
wages_df <- wages_df %>% 
  mutate(wage_cost_per_farm_capped=ifelse(wage_cost_per_farm>wage_thresh, wage_thresh, wage_cost_per_farm))

# Plot labor costs
legend_breaks <- seq(0, wage_thresh/1e6, 1)
legend_labels <- c(legend_breaks[1:(length(legend_breaks)-1)], paste0(">", legend_breaks[length(legend_breaks)]))
g2 <- ggplot() +
  geom_raster(wages_df, mapping=aes(x=x, y=y, fill=wage_cost_per_farm_capped/1e6)) +
  geom_sf(data=world, fill="grey80", lwd=0.05, col="white") +
  labs(x="", y="") +
  scale_fill_gradientn(name="Annual labor cost per farm\n(USD millions per year)",
                       colors=rev(RColorBrewer::brewer.pal(n=9, name="RdYlBu")), na.value=NA,
                       breaks=legend_breaks, labels=legend_labels, limits=c(0, wage_thresh/1e6)) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  theme_bw() + my_theme


# Merge and export
##############################

# Merge
g <- grid.arrange(g1, g2, nrow=2)

# Export
ggsave(g, filename=file.path(plotdir, "FigS22_farm_wage_labor_costs.png"), 
       width=6.5, height=7.5, units="in", dpi=600)

