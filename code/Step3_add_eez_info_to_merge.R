
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(raster)
library(ggplot2)
library(tidyverse)
library(countrycode)
library(fuzzyjoin)

# Directories
eezdir <- "data/eezs"
sppdir <- "data/species/data"
datadir <- "output/processed"

# Read EEZ data
eezs <- raster(file.path(eezdir, "eezs_v10_raster_10km.tif"))
eez_key_orig <- read.csv(file.path(eezdir, "eezs_v10_key.csv"), as.is=T)

# Format EEZ data
################################################################################

# Continent keys
south_ind <- c("Amsterdam and Saint Paul Islands", "Heard and McDonald Islands", 
               "Kerguélen", "Cocos Islands", "Chagos Archipelago", "Crozet Islands")
south_atl <- c("Ascension", "Tristan da Cunha", "South Georgia and the South Sandwich Islands", "Bouvet")
eq_pacific <- c("Howland and Baker islands", "Palmyra Atoll", "Wake Island", "Jarvis Island", "Johnston Atoll")
moz_channel <- c("Europa Island", "Juan de Nova Island", "Bassas da India")

# Build EEZ key
eez_key <- eez_key_orig %>%  
  # Add continent
  mutate(ter1_continent=countrycode(sourcevar = ter1_iso, origin="iso3c", destination = "continent"),
         sov1_continent=countrycode(sourcevar = sov1_iso, origin="iso3c", destination = "continent")) %>% 
  # Fill missing continent typ
  mutate(ter1_continent=ifelse(ter1_name %in% south_ind, "Indian Ocean", ter1_continent),
         ter1_continent=ifelse(ter1_name %in% south_atl, "S Atlantic Ocean", ter1_continent),
         ter1_continent=ifelse(ter1_name %in% eq_pacific, "Oceania", ter1_continent),
         ter1_continent=ifelse(ter1_name %in% moz_channel, "Africa", ter1_continent),
         ter1_continent=ifelse(ter1_name == "Clipperton Island", "Americas", ter1_continent),
         ter1_continent=ifelse(ter1_name == "Antarctica", "Antarctica", ter1_continent),
         sov1_continent=ifelse(is.na(sov1_continent), ter1_continent, sov1_continent))

# Convert EEZ raster to dataframe
eezs_df <- as.data.frame(eezs, xy=T) %>% 
  setNames(c("x", "y", "eez_code")) %>% 
  left_join(select(eez_key, eez_code, eez_name, eez_type, 
                   ter1_name, ter1_iso, sov1_name, sov1_iso,
                   ter1_continent, sov1_continent)) %>% 
  # Convert coords to integers to ease left_join below
  mutate(x=as.integer(x),
         y=as.integer(y))


# Add EEZ data
################################################################################

# Function
# rcp <- "rcp45"; type <- "bivalve"
add_eez_info <- function(rcp, type){
  
  # Read data
  infile <- paste(toupper(rcp), str_to_title(type), "rational.Rds", sep="_")
  data <- readRDS(file.path(datadir, infile))
  
  # Add EEZ data
  data1 <- data %>% 
    ungroup() %>% 
    # Convert coords to integers to ease left_join
    mutate(x=as.integer(x),
           y=as.integer(y)) %>% 
    left_join(eezs_df)
  
  # Inspect
  freeR::complete(data1)
  
  # Export data
  outfile <- paste(toupper(rcp), str_to_title(type), "rational_w_eez_info.Rds", sep="_")
  saveRDS(data1, file.path(datadir, outfile))
  
  
}

# Bivalves
add_eez_info(rcp="rcp26", type="bivalve")
add_eez_info(rcp="rcp45", type="bivalve")
add_eez_info(rcp="rcp60", type="bivalve")
add_eez_info(rcp="rcp85", type="bivalve")

# Finfish
add_eez_info(rcp="rcp26", type="finfish")
add_eez_info(rcp="rcp45", type="finfish")
add_eez_info(rcp="rcp60", type="finfish")
add_eez_info(rcp="rcp85", type="finfish")



