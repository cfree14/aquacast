
# Setup
################################################################################

# Packages
library(sf)
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(raster)
library(RColorBrewer)

# For testing
if(F){
  file <- "Gadus_morhua.csv"
  filedir <- "data/aquamaps"
  data <- format_aquamaps(file, filedir)
  plot_aquamaps(data)
}


# Plot AquaMaps data
plot_aquamaps <- function(data){
  
  # Get data
  pdata <- data[["pdata"]]
  pdata_mat <- data[["pdata_mat"]]
  pdata_ras <- raster(pdata_mat)

  # Get world data
  world <- fortify(map_data('world'))

  # Plot data
  p <- ggplot() 
  # Plot distribution
  p <- p + geom_raster(data = pdata, aes(x=long_dd, y = lat_dd, fill=prob)) + 
    scale_fill_gradient(name="p(occurence)", low="yellow", high="red") +
    coord_fixed(ratio = 1) +
    theme_bw() + 
    labs(x="", y="")
  # Plot world on top
  p <- p + geom_map(data=world, map=world,
                        aes(x=long, y=lat, group=group, map_id=region),
                        fill="grey80", colour="white", lwd=0.2) +
    xlim(min(pdata$long_dd), max(pdata$long_dd)) +
    ylim(min(pdata$lat_dd), max(pdata$lat_dd))
  p
  
}
