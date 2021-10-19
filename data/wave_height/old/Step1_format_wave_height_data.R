
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(ncdf4)
library(raster)
library(tidyverse)

# Directories
outdir <- "/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/blue-paper-2/data/climate/Song_etal_2020/1netcdfs"

# Paper: https://www.nature.com/articles/s41597-020-0566-8#Sec5

# Read data
################################################################################

# Inspect NetCDF
data_ncdf <- ncdf4::nc_open(file.path(outdir, "Hs_glob_FIO_FIO-ESM-2-0_ssp126_r1i1p1f1_mon_201501-210012.nc"))
names(data_ncdf)
data_ncdf$nvars
data_ncdf$dim
data_ncdf$var$lon
360/320
180/364

# The projection: Greenland dipole grid
# The Greenland diploe grid is a latitude/longitude grid, with the North Pole displaced to Greenland to avoid singularity problems in the model. 

# Read data
data_orig <- raster::brick(file.path(outdir, "Hs_glob_FIO_FIO-ESM-2-0_ssp126_r1i1p1f1_mon_201501-210012.nc"))
plot(data_orig[[1]])
abline(v=200)

# Setup months
# Time is months since 
# All three experiments began in 2015, initialized on Jan 1st of 2015 in the historical simulation, and ended in 2100, forced by the datasets provided by CMIP6.
months <- seq(ymd("2015-01-01"), ymd("2100-12-01"), by="1 month")
names(data_orig) <- months

# Fix latitude
extent(data_orig)
data1 <- data_orig
extent(data1) <- extent(0, 360, -90, 90)
plot(data1[[7]])
abline(h=0)
abline(v=40, lty=2)
abline(v=220, lty=2)


# Fix longitude

# Grab and shift eastern portion [0,220] to []
data_e <- crop(data1, extent(0,220, -90, 90))
plot(data_e[[7]])
data_e_shifted <- shift(data_e, dx=140)
plot(data_e_shifted[[7]])

# Grab and shift western portion [220,360] to []
data_w <- crop(data1, extent(220,360, -90, 90))
plot(data_w[[7]])
data_w_shifted <- shift(data_w, dx=-220)
plot(data_w_shifted[[7]])

# Merge
data_merged <- merge(data_w_shifted, data_e_shifted)
plot(data_merged[[7]])
data <- shift(data_merged, dx=-180)
plot(data[[7]])

# 3. Project raster
wgs84 <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
crs(data) <- wgs84

# 4. Check raster
if(F){
  
  # Example raster
  ras <- data[[10]] %>% 
    as.data.frame(xy=T) %>% 
    setNames(c("long_dd", "lat_dd", "wave_m")) %>% 
    filter(!is.na(wave_m))
  
  # Check against the world
  world <- rnaturalearth::ne_countries(returnclass = "sf", scale="small") %>% 
    sf::st_transform(wgs84)
  
  
  # Plot
  g <- ggplot() +
    # Raster
    geom_tile(data=ras, mapping=aes(x=long_dd, y=lat_dd, fill=wave_m)) +
    # World
    geom_sf(data=world, fill="grey", color="white", lwd=0.3) +
    # Theme
    theme_bw()
  g
  
  
}




