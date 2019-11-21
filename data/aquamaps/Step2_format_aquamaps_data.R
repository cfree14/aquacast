
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(ggplot2)
library(tidyverse)

# Directories
datadir <- "data/aquamaps/data"
inputdir <- "data/aquamaps/data/raw"
outputdir <- "data/aquamaps/data/processed"
codedir <- "data/aquamaps"

# Source code to Download AquaMaps data
source(file.path(codedir, "format_aquamaps.R"))


# Format AquaMaps data
################################################################################

# Raw files
files <- list.files(inputdir)

# Loop through files
for(i in 1:length(files)){
  
  # Format data
  data <- format_aquamaps(file=files[i], filedir=inputdir)
  
  # Export data
  saveRDS(data, file = file.path(outputdir, gsub(".csv", ".Rds", files[i])))
  
}


# Merge formatted data
################################################################################

# Processed files
files <- list.files(outputdir)

# Build environmental tolerance data
x <- files[1]
data <- purrr::map_df(files, function(x){
  
  # Read files
  sdata <- readRDS(file.path(outputdir, x))
  qdata <- sdata$qdata
  
})

# Export
saveRDS(data, file.path(datadir, "aquamaps_environmental_preferences.Rds"))



