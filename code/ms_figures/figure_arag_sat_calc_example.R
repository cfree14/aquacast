

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

# Directories
plotdir <- "figures"
datadir <- "/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/blue-paper-2/data/climate/GFDL-ESM2G/2rasters"

# Read data
sst <- brick(file.path(datadir, "GFDL_ESM2G_rcp85_tos_degC.grd"))
salt <- brick(file.path(datadir, "GFDL_ESM2G_rcp85_so_psu.grd"))
alk <- brick(file.path(datadir, "GFDL_ESM2G_rcp85_talk_mol_kg.grd"))
dic <- brick(file.path(datadir, "GFDL_ESM2G_rcp85_dissic_mol_kg.grd"))
si <- brick(file.path(datadir, "GFDL_ESM2G_rcp85_si_mol_kg.grd"))
po4 <- brick(file.path(datadir, "GFDL_ESM2G_rcp85_po4_mol_kg.grd"))
arag <- brick(file.path(datadir, "GFDL_ESM2G_rcp85_arag_sat.grd"))


# Plot data
################################################################################

# World
world <- rnaturalearth::ne_countries(returnclass = "sf")

# Setup theme
my_theme <- theme(axis.text=element_blank(),
                  axis.title=element_blank(),
                  legend.position = "right",
                  legend.text=element_text(size=8),
                  legend.title=element_blank(),
                  plot.title=element_text(size=9),
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"))

# Temperature
######################

sst_df <- sst[[1]] %>% 
  as.data.frame(xy=T) %>% 
  setNames(c("x", "y", "value"))

sst_plot <- ggplot(sst_df, aes(x=x, y=y, fill=value)) +
  geom_raster() + 
  geom_sf(data=world, fill="grey90", color="white", lwd=0.2, inherit.aes = F) +
  labs(x="", y="", title="SST (°C)") +
  scale_fill_gradientn(name="", 
                       colors=cmocean(name="thermal")(100), na.value="white") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  theme_bw() + my_theme

# Salinity
######################

salt_df <- salt[[1]] %>% 
  as.data.frame(xy=T) %>% 
  setNames(c("x", "y", "value"))

sal_plot <- ggplot(salt_df, aes(x=x, y=y, fill=value)) +
  geom_raster() + 
  labs(x="", y="", title="Salinity (psu)") +
  scale_fill_gradientn(name="", 
                       colors=cmocean(name="haline")(100), na.value="white") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  theme_bw() + my_theme


# Silicate
######################

si_df <- si[[1]] %>% 
  as.data.frame(xy=T) %>% 
  setNames(c("x", "y", "value"))

si_plot <- ggplot(si_df, aes(x=x, y=y, fill=value)) +
  geom_raster() + 
  labs(x="", y="", title="Silicate concentration (mol/kg)") +
  scale_fill_gradientn(name="", 
                       colors=RColorBrewer::brewer.pal(9, "YlGn"), na.value="white") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  theme_bw() + my_theme


# Phosphate
######################

po4_df <- po4[[1]] %>% 
  as.data.frame(xy=T) %>% 
  setNames(c("x", "y", "value"))

po4_plot <- ggplot(po4_df, aes(x=x, y=y, fill=value)) +
  geom_raster() + 
  labs(x="", y="", title="Phosphate concentration (mol/kg)") +
  scale_fill_gradientn(name="", 
                       colors=RColorBrewer::brewer.pal(9, "RdPu"), na.value="white") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  theme_bw() + my_theme

# Alkalinity
######################

alk_df <- alk[[1]] %>% 
  as.data.frame(xy=T) %>% 
  setNames(c("x", "y", "value"))

alk_plot <- ggplot(alk_df, aes(x=x, y=y, fill=value)) +
  geom_raster() + 
  labs(x="", y="", title="Total alkalinity (mol/kg)") +
  scale_fill_gradientn(name="", 
                       colors=RColorBrewer::brewer.pal(9, "YlOrRd"), na.value="white") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  theme_bw() + my_theme

# DIC
######################

dic_df <- dic[[1]] %>% 
  as.data.frame(xy=T) %>% 
  setNames(c("x", "y", "value"))

dic_plot <- ggplot(dic_df, aes(x=x, y=y, fill=value)) +
  geom_raster() + 
  labs(x="", y="", title="DIC concentration (mol/kg)") +
  scale_fill_gradientn(name="", 
                       colors=RColorBrewer::brewer.pal(9, "Blues"), na.value="white") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  theme_bw() + my_theme


# Arag sat
######################

arag_df <- arag[[1]] %>% 
  as.data.frame(xy=T) %>% 
  setNames(c("x", "y", "value"))

arag_plot <- ggplot(arag_df, aes(x=x, y=y, fill=value)) +
  geom_raster() + 
  labs(x="", y="", title="Aragonite saturation (Ω)") +
  scale_fill_gradientn(name="", 
                      colors=RColorBrewer::brewer.pal(11, "Spectral"), na.value="white") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  theme_bw() + my_theme


# Merge
######################

# Merge plots
g <- grid.arrange(sst_plot, sal_plot, si_plot, po4_plot, alk_plot, dic_plot, arag_plot,
             layout_matrix=matrix(c(1:6,7,7), byrow=T, ncol=2), heights=c(rep(2/3/3,3), 1/3))

# Export
ggsave(g, filename=file.path(plotdir, "figure_arag_sat_calc_example.png"), 
       width=6.5, height=8, units="in", dpi=600)



