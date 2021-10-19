

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


# Function
################################################################################

# Variable to plot
# var_units <- "tos_degC_annual_mean_scaled"; color_pal="thermal"; legend_title="SST (°C)"
# var_units <- "chl_mg_m3"; color_pal <- "algae"; legend_title <- "Total chlorophyll (mg/m3)"
# var_units <- "current_speed_m_s_annual_max_scaled"; color_pal <- "speed"; legend_title <- "Current speed (m/s)"
plot_clim_proj <- function(var_units, color_pal, legend_title){
  
  # Read data
  ########################
  
  # Read data
  rcp85 <- brick(file.path(datadir, paste0("GFDL_ESM2G_rcp85_", var_units, ".grd")))
  rcp60 <- brick(file.path(datadir, paste0("GFDL_ESM2G_rcp60_", var_units, ".grd")))
  rcp45 <- brick(file.path(datadir, paste0("GFDL_ESM2G_rcp45_", var_units, ".grd")))
  rcp26 <- brick(file.path(datadir, paste0("GFDL_ESM2G_rcp26_", var_units, ".grd")))
  # layer_names <- readRDS(file.path(datadir, paste0("GFDL_ESM2MG_rcp85_", var_units, "_layer_names.Rds")))
  # names(rcp85) <- names(rcp60) <- names(rcp45) <- names(rcp26) <- layer_names
  
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
    select(rcp, year, everything())
  
  # Check stats
  stats <- data %>% 
    group_by(rcp, year) %>% 
    summarize(mean=mean(value, na.rm=T))
  
  # Subset data for testing
  data_test <- data %>% 
    sample_frac(size=0.1)
  
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
  
  # Colors
  if(color_pal=="spectral"){
    colors <- RColorBrewer::brewer.pal(11, "Spectral")
  }else{
    colors <-cmocean(color_pal)(100)
  }
  
  # World
  world <- rnaturalearth::ne_countries(scale="small", returnclass = "sf") %>% 
    sf::st_transform(crs( rcp26 ))
  
  # Plot data
  g <- ggplot(data, aes(x=long_dd, y=lat_dd, fill=value)) +
    facet_grid(rcp ~ year) +
    # World
    geom_sf(data=world, fill="grey90", color="white", lwd=0.1, inherit.aes = F) +
    # Variable
    geom_raster() +
    # Labels
    labs(x="", y="") +
    # Legend
    scale_fill_gradientn(name=legend_title, colors=colors, na.value = NA) +
    # scale_fill_gradientn(name=legend_title, colors=rev(RColorBrewer::brewer.pal(9, "RdBu")), na.value = NA) +
    guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
    # Theme
    theme_bw() + my_theme
  
  # Export figure
  outfile <- paste0("figure_climate_proj_", var_units, ".png")
  ggsave(g, filename=file.path(plotdir, outfile), 
         width=6.5, height=6.5, units="in", dpi=600)
  
}


# Run function
################################################################################

# Plot main
plot_clim_proj(var_units="tos_degC_annual_mean_scaled", color_pal="thermal", legend_title="SST (°C)")
plot_clim_proj(var_units="so_psu_annual_mean_scaled", color_pal="haline", legend_title="Salinity (psu)")
plot_clim_proj(var_units="chl_mg_m3_annual_mean_scaled", color_pal="algae", legend_title="Total chlorophyll (mg/m3)")
plot_clim_proj(var_units="o2_mol_m3_annual_mean_scaled", color_pal="solar", legend_title="Dissolved oxygen (mol/m3)")
plot_clim_proj(var_units="arag_sat_annual_mean_scaled", color_pal="spectral", legend_title="Aragonite saturation (Ω)")


# Plot secondary
plot_clim_proj(var_units="tos_degC_annual_min_scaled", color_pal="thermal", legend_title="SST (°C)")
plot_clim_proj(var_units="tos_degC_annual_max_scaled", color_pal="thermal", legend_title="SST (°C)")
plot_clim_proj(var_units="chl_mg_m3_annual_mean_minus_sd_scaled", color_pal="algae", legend_title="Mean minus standard deviation\nof ntotal chlorophyll (mg/m3)")


plot_clim_proj(var_units="current_speed_m_s_annual_max_scaled", color_pal="speed", legend_title="Current speed (m/s)")
