

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)
library(countrycode)

# Directory
datadir <- "data/coastlines"

# Read data
data_orig <- readxl::read_excel(file.path(datadir, "coastlines_wikipedia.xlsx"), na=c("—"))


# Setup
################################################################################

# Format data
data <- data_orig %>% 
  # Rename
  rename(country_orig=country) %>% 
  # Format country
  mutate(country_orig=stringr::str_trim(country_orig)) %>% 
  # Add ISOS
  mutate(iso3=countrycode(country_orig, "country.name", "iso3c"), country_orig,
         country=countrycode(iso3, "iso3c", "country.name")) %>% 
  # Format numbers
  mutate(land_area_sqkm=gsub("<|>", "", land_area_sqkm ) %>% as.numeric(),
         coast_area_ratio_twf=gsub("<|>|,", "", coast_area_ratio_twf) %>% as.numeric()) %>% 
  # Arrange
  select(country_orig, country, iso3, everything())

# Inspect
str(data)

# Export data
write.csv(data, file=file.path(datadir, "coastlines_wikipedia.csv"), row.names=F)

