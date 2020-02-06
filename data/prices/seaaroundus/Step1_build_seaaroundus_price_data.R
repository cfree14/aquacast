
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(stringr)
library(tidyverse)

# SeaAroundUs
# install.packages("devtools")
# devtools::install_github("ropensci/seaaroundus")
library(seaaroundus)

# Build EEZ key
eezs <- seaaroundus::listregions(region = "eez")

# Directories
datadir <- "data/prices/seaaroundus"

# Read data
saup_spp <- readRDS(file="/Users/cfree/Dropbox/Chris/UCSB/projects/trawl_impacts/data/saup/processed/saup_lme_catch_data.Rds") %>% 
  select(comm_name, species) %>% 
  unique()

# Build data
################################################################################

# If scaraping
x <- 8
scraping <- F
if(scraping){
  
  # Loop through and extract
  quantity <- purrr::map_df(eezs$id, function(x) {
    eez_id <- x
    eez_name <- eezs$title[eezs$id==eez_id]
    data <- catchdata1(region = "eez", id=eez_id, measure = "tonnage", dimension = "taxon", sci_name=F) %>% 
      setNames(make.unique(colnames(.))) %>% 
      mutate(eez_id=eez_id,
             eez_name=eez_name) %>% 
      select(eez_id, eez_name, everything()) %>% 
      gather(key="comm_name", value="landings_mt", 4:ncol(.))
    
    data <- seaaroundus::catchdata(region = "eez", id=eez_id, measure = "tonnage", dimension = "taxon") %>% 
      setNames(make.unique(colnames(.))) %>% 
      mutate(eez_id=eez_id,
             eez_name=eez_name) %>% 
      select(eez_id, eez_name, everything()) %>% 
      gather(key="comm_name", value="landings_mt", 4:ncol(.))
  })
  
  # Loop through and extract
  value <- purrr::map_df(eezs$id, function(x) {
    eez_id <- x
    eez_name <- eezs$title[eezs$id==eez_id]
    data <- seaaroundus::catchdata(region = "eez", id=eez_id, measure = "value", dimension = "taxon") %>% 
      setNames(make.unique(colnames(.))) %>% 
      mutate(eez_id=eez_id,
             eez_name=eez_name) %>% 
      select(eez_id, eez_name, everything()) %>% 
      gather(key="comm_name", value="landings_usd", 4:ncol(.))
  })
  
  # Merge data
  landings <- left_join(quantity, value, by=c("eez_id", "eez_name", "comm_name", "years")) %>% 
    rename(year=years) %>% 
    mutate(price_usd_mt=landings_usd/landings_mt)
  
  # Export data
  saveRDS(landings, file.path(datadir, "1950_2014_SAUP_landings_data.Rds"))
  
}else{
  
  landings <- readRDS(file.path(datadir, "1950_2014_SAUP_landings_data.Rds"))
  
}

# Merge data
################################################################################

# Years
range(landings$year)
2010:2014

# Check landings data
landings <- landings %>% 
  mutate(landings_usd_calc=landings_mt*price_usd_mt)
plot(landings_usd_calc ~ landings_usd, landings)


# Mean ex-vessel price last 5 five years
prices <- landings %>% 
  # Filter for recent data with prices
  filter(year>=2010 & !is.na(price_usd_mt) & is.finite(price_usd_mt)) %>% 
  ungroup() %>% 
  group_by(comm_name) %>% 
  # Calculate weighted average price by landings
  summarize(price_usd_mt=weighted.mean(price_usd_mt, landings_mt, na.rm=T)) %>% 
  # Add species names
  mutate(comm_name=freeR::sentcase(comm_name),
         comm_name=gsub("atlantic", "Atlantic", comm_name),
         comm_name=gsub("pacific", "Pacific", comm_name),
         comm_name=gsub("spanish", "Spanish", comm_name), 
         comm_name=gsub("african", "African", comm_name),
         comm_name=recode(comm_name, 
                          # "Bluebarred parrotfish"=,
                          # "Cape redfish"="",
                          "Caspian sea sprat"="Caspian Sea sprat",
                          # "Helicolenus mouchezi"="",
                          "Juan fernandez rock lobster"="Juan Fernandez rock lobster",
                          # "Others"="",
                          # "Rough turban"="",
                          # "Shallow-water cape hake"="",
                          # "Snappers.1"="",
                          "South georgia icefish"="South Georgia icefish",
                          "Tristan da cunha lobster"="Tristan da Cunha lobster"
                          # "Unicorn icefish"=""
                          )) %>% 
  left_join(saup_spp, by="comm_name") %>% 
  # Reduce to species-specific values
  filter(freeR::nwords(species) > 1) %>%
  # Fix scientific names
  mutate(species=recode(species, "Clupea bentincki"="Strangomera bentincki",
                         "Diplodus sargus sargus"="Diplodus sargus",
                         "Liza aurata"="Chelon auratus",
                         "Salvelinus alpinus alpinus"="Salvelinus alpinus",
                         "Salvelinus malma malma"="Salvelinus malma",
                         "Theragra chalcogramma"="Gadus chalcogrammus"))


# Inspect
freeR::complete(prices)
freeR::check_names(prices$species)

# Export SAUP prices
write.csv(prices, file=file.path(datadir, "SAUP_2010_2014_price_averages_weighted.csv"), row.names = F)









