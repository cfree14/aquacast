

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
sppdir <- "data/species/data"
outputdir <- "output/raw"
plotdir <- "figures"

# Read data
load(file.path(sppdir, "aquaculture_species_key.Rdata"))




# Plot data
################################################################################

# Setup figure
figname <- "appendix_rcp85_profitability.pdf"
pdf(paste(plotdir, figname, sep="/"), width=8.5, height=11)
par(mfrow=c(5, 3), mar=c(2.5, 2.5, 2.5, 0.5), mgp=c(2.5,0.8,0), oma=c(3,6,3,3), lwd=0.8)

# Loop through species and plot
for(i in 1:5){
  
  # Read data
  spp <- data$species[i]
  vcell_file <- paste0("RCP85_Finfish_", gsub(" ", "_", spp), "_profits_usd_yr.tif")
  vcells <- raster::brick(file.path(outputdir, vcell_file))
  
  # Plot data
  for(j in 1:3){
    plot(vcells[[j]]/1e6/100, xaxt="n", yaxt="n")
  }
  
}
  
# Off
dev.off()
graphics.off()