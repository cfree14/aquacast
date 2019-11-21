
# Clear workspace
rm(list = ls())

# Setup
#####################################################################################

# Packages
library(raster)
library(tidyverse)
library(countrycode)
library(rnaturalearth)

# World Bank API
library(wbstats)

# Distance from shore
# devtools::install_github("mdsumner/distancetocoast")
library(distancetocoast) 

# Directories
eezdir <- "data/eezs"
tempdir <- "data/template"
datadir <- "data/costs/data"
plotdir <- "data/costs/figures"

# Read raster template
ras_temp <- raster(file.path(tempdir, "world_raster_template_10km.tif"))

# Read EEZ raster and key
eezs <- raster(file.path(eezdir, "eezs_v10_raster_10km.tif"))
eezs_sf <- readRDS(file.path(eezdir, "eezs_v10_polygons.Rds"))
eez_key <- read.csv(file.path(eezdir, "eezs_v10_key.csv"), as.is=T)
compareRaster(eezs, ras_temp)

# World layer
world <- rnaturalearth::ne_countries(scale = "large", type = "countries", returnclass = "sf")


# Distance from shore layer
#####################################################################################

# Distance from coast rasters
cdist_22 <- distancetocoast::distance_to_coastline_10 # 0.222 resolution
cdist_33 <- distancetocoast::distance_to_coastline_50 # 0.333 resolution
cdist_50 <- distancetocoast::distance_to_coastline_lowres # 0.50 resolution

# Project to match template
cdist <- projectRaster(from=cdist_22, to=ras_temp)

# Export if matchs
if(compareRaster(cdist, ras_temp)){
  writeRaster(cdist, filename=file.path(datadir, "dist_to_coast_m_10km.grd"), format="raster")
}

# THIS PLOTTING TEXT DOESN'T WORK

# Plot distance from shore
# cdist_df <- as.data.frame(cdist, xy=T)
# g <- ggplot() +
#   geom_raster(cdist_df, mapping=aes(x=x, y=y, fill=layer)) +
#   geom_sf(data=world, fill="grey80", lwd=0.05, color="white") +
#   labs(x="", y="") +
#   scale_fill_continuous(name="Distance from shore (m)", na.value=NA)
#   theme_bw()
# 
# # Export
# ggsave(g, filename=file.path(plotdir, "figure_dist_to_coast.png"), 
#        width=5, height=3, units="in", dpi=600)


# Get World Bank data
#####################################################################################

# Search for World Bank cost parameters (fuel and wages)
# 12875 EP.PMP.SGAS.CD = Pump price for gasoline (US$ per liter)
# 12876 EP.PMP.DESL.CD = Pump price for diesel fuel (US$ per liter)
# 8669 NY.ADJ.NNTY.PC.CD = Adjusted net national income per capita (current US$)
wb_costs_orig <- wb(indicator = c('EP.PMP.SGAS.CD', 'EP.PMP.DESL.CD', 'NY.ADJ.NNTY.PC.CD'), mrv = 1)

# Format WB costs
wb_costs <- wb_costs_orig %>% 
  select(iso3c, iso2c, country, indicator, value) %>% 
  spread(key="indicator", value="value") %>% 
  rename(income_usd_yr="Adjusted net national income per capita (current US$)",
         diesel_usd_l="Pump price for diesel fuel (US$ per liter)",
         gas_usd_l="Pump price for gasoline (US$ per liter)")

# Calculate global means
income_usd_yr_avg <- mean(wb_costs$income_usd_yr, na.rm=T)
diesel_usd_l_avg <- mean(wb_costs$diesel_usd_l, na.rm=T)
gas_usd_l_avg <- mean(wb_costs$gas_usd_l, na.rm=T)

# Add costs to EEZ key
eez_costs <- eez_key %>% 
  # Add costs
  left_join(select(wb_costs, -c(iso2c, country)), by=c("sov1_iso"="iso3c")) %>% 
  # Fill in missing values
  mutate(income_usd_yr=ifelse(!is.na(income_usd_yr), income_usd_yr, income_usd_yr_avg),
         diesel_usd_l=ifelse(!is.na(diesel_usd_l), diesel_usd_l, diesel_usd_l_avg),
         gas_usd_l=ifelse(!is.na(gas_usd_l), gas_usd_l, gas_usd_l_avg))
  
# Export costs
write.csv(eez_costs, file.path(datadir, "eez_wb_diesel_income_costs.csv"), row.names=F)




