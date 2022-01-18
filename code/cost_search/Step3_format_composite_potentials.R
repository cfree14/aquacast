
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
maskdir <- "data/constraints/masks"
datadir <- "/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/blue-paper-2/data/output/cost_search_processed"

# Read EEZ data
eezs <- raster(file.path(eezdir, "eezs_v10_raster_10km.tif"))
eez_key_orig <- read.csv(file.path(eezdir, "eezs_v10_key.csv"), as.is=T)

# Read, format, and check mask
mask <- raster(file.path(maskdir, "mask_10km.grd"))
mask[mask==0] <- NA
plot(mask)

# Convert mask to data frame
mask_df <- mask %>%
  # Convert mask to dataframe
  as.data.frame(xy=T) %>% 
  setNames(c("x", "y", "use")) %>% 
  # Reduce to viable cells
  filter(!is.na(use)) %>% 
  # Fix coordinates (needed for successful merge)
  mutate(x=as.integer(x),
         y=as.integer(y))

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
# rcp <- "rcp26"; type <- "finfish"; suffix="new_costs"
format_data <- function(rcp, type, suffix=""){
  
  # Read data
  if(suffix==""){
    infile <- paste(toupper(rcp), str_to_title(type), "rational.Rds", sep="_")
    outfile <- paste(toupper(rcp), str_to_title(type), "rational_use.Rds", sep="_")
  }else{
    infile <- paste0(toupper(rcp), "_", str_to_title(type), "_rational_", suffix, ".Rds")
    outfile <- paste0(toupper(rcp), "_", str_to_title(type), "_rational_use_", suffix, ".Rds")
  }
  data_orig <- readRDS(file.path(datadir, infile))
  
  # Format data
  data <- data_orig %>% 
    # Format xy for join
    mutate(x=as.integer(x),
           y=as.integer(y)) %>% 
    # Add mask and reduce to viable areas
    left_join(mask_df, by=c("x", "y")) %>% 
    filter(use==1) %>% 
    select(-use) %>% 
    # Add EEZ information
    left_join(eezs_df, by=c("x", "y")) %>% 
    # Remove cells with out SOV1 info (pertain to an overlapping claim)
    filter(!is.na(sov1_name))
  
  # Inspect
  freeR::complete(data)
  
  # Summarize plot
  stats <- data %>% 
    group_by(period, ter1_continent) %>% 
    summarize(prod_bt_yr=sum(prod_mt_yr)/1e9)
  g <- ggplot(stats, aes(x=period, y=prod_bt_yr, fill=ter1_continent)) +
    geom_bar(stat="identity") +
    labs(x="Period", y="Production potential (billions mt)") +
    theme_bw()
  print(g)
  
  # Export data
  saveRDS(data, file.path(datadir, outfile))
  
}

# Run
cost_scalars <- c(2.1, 2.3, 2.5, 2.7)

# Loop through scalars
for(i in 1:length(cost_scalars)){
  
  # Build suffix
  suffix <- paste0("cost_search_", cost_scalars[i])
  
  # Finfish
  format_data(rcp="rcp26", type="finfish", suffix=suffix)
  format_data(rcp="rcp45", type="finfish", suffix=suffix)
  format_data(rcp="rcp60", type="finfish", suffix=suffix)
  format_data(rcp="rcp85", type="finfish", suffix=suffix)
  
  # Bivalves
  format_data(rcp="rcp26", type="bivalve", suffix=suffix)
  format_data(rcp="rcp45", type="bivalve", suffix=suffix)
  format_data(rcp="rcp60", type="bivalve", suffix=suffix)
  format_data(rcp="rcp85", type="bivalve", suffix=suffix)
  
}






