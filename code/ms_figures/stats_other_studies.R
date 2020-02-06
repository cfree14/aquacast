
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Directories
gentrydir <- "/Users/cfree/Dropbox/Chris/UCSB/projects/protein_curve/ocean-protein/aquaculture/results/"

# Read data
gentry_biv <- raster::stack(file.path(gentrydir, "bivalve_layers.tif")) # suitable, phi, time, prod
gentry_fin <- raster::stack(file.path(gentrydir, "fish_layers.tif")) # suitable, phi, time, prod

# Setup
################################################################################

# Trillions of individual bivalves
sum(getValues(gentry_biv[[4]]), na.rm=T) / 1e12

# Millions of metric tons of bivalves
sum(getValues(gentry_biv[[4]]), na.rm=T) * 3.01 / 1000 / 1000 / 1e6

# Millions of metric tons of bivalves
sum(getValues(gentry_fin[[4]]), na.rm=T) / 1e9






