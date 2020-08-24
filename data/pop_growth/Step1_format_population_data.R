

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(ggplot2)
library(tidyverse)
library(wbstats)
library(countrycode)

# Directories
indir <- "/Users/cfree/Dropbox/Chris/UCSB/projects/nutrient_endowment/data/population_growth/processed/"
outdir <- "data/pop_growth/data"

# Read data
data_orig <- readRDS(file.path(indir, "WB_UN_1960_2100_human_population_by_country.Rds"))

# Export data
saveRDS(data_orig, file.path(outdir, "WB_UN_1960_2100_human_population_by_country.Rds"))


