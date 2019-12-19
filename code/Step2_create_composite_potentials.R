
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(raster)
library(ggplot2)
library(tidyverse)

# Directories
sppdir <- "data/species/data"
inputdir <- "output/raw"
outputdir <- "output/processed"


# Read species data
load(file.path(sppdir, "aquaculture_species_key.Rdata"))


# For loop approach
################################################################################

# The goal is to produce the following three raster stacks 
# where the number of layers is equivalent to the number of years:
# 1) Identify of the most profitable species in each cell in each year
# 2) Production of the most profitable species in each cell in each year
# 3) Profitability of the most profitable species in each cell in each year

# Parameters for testing
rcp <- "rcp85"
species <- data %>% filter(class=="Bivalvia") %>% select(species)
species <- species$species
years <- c(2021, 2051, 2100)
type <- "Bivalve"

# Function to identify and map the most profitab
calc_optimal_aqprod <- function(rcp, species, years=c(2021, 2051, 2100), type){

  # Parameters
  nspp <- length(species)
  nyears <- length(years)
  
  # Loop through years
  for(i in 1:nyears){
    
    print(i)
    
    # Loop through species
    for(j in 1:nspp){
      
      print(paste("...", j))
      
      # Read profits for species j
      filename <- paste(toupper(rcp), "Bivalve", gsub(" ", "_", species[j]), "profits_usd_yr.tif", sep="_")
      profits_j <- raster::brick(file.path(inputdir,  filename))
      
      # Read production for species j
      filename <- paste(toupper(rcp), "Bivalve", gsub(" ", "_", species[j]), "production_mt_yr.tif", sep="_")
      prod_j <- raster::brick(file.path(inputdir,  filename))
      
      # Erase unprofitable cells
      profits_j_masked <- profits_j
      profits_j_masked <- reclassify(profits_j_masked, rcl=matrix(c(-Inf, 0, NA), ncol=3), right=T, include.lowest=T)
      profits_j_mask <- !is.na(profits_j_masked)
      prod_j_masked <- prod_j * profits_j_mask
      
      # Turn on to check cell counts
      if(F){
        ncells_prod <- sum(!is.na(getValues(prod_j_masked)))
        ncells_profit <- sum(!is.na(getValues(profits_j_masked)))
        ncells_prod == ncells_profit
      }

      # Create stack of species 
      prod_j_i <- prod_j_masked[[i]]
      profits_j_i <- profits_j_masked[[i]]
      if(j==1){
        prod_i <- prod_j_i
        profits_i <- profits_j_i
      }else{
        prod_i <- stack(prod_i, prod_j_i)
        profits_i <- stack(profits_i, profits_j_i)
      }
      
    }
    
    # Identify most profitable species and production/profits of most profitable species
    ras_farm_spp_i <- which.max(profits_i)
    ras_farm_prod_i <- max(prod_i, na.rm=T)
    ras_farm_profits_i <- max(profits_i, na.rm=T)
    
    # Merge values
    if(i==1){
      ras_farm_spp <- ras_farm_spp_i 
      ras_farm_prod <- ras_farm_prod_i
      ras_farm_profits <- ras_farm_profits_i
    }else{
      ras_farm_spp <- stack(ras_farm_spp, ras_farm_spp_i) 
      ras_farm_prod <- stack(ras_farm_prod, ras_farm_prod_i)
      ras_farm_profits <- stack(ras_farm_profits, ras_farm_profits_i)
    }
    
  }
  
  # Plot check
  plot(ras_farm_spp, main="Optimal species")
  plot(ras_farm_prod, main="Production (mt/yr)") # these units appear small to me
  plot(ras_farm_profits/100/1e6, main="Profits (USD millions/yr/farm)")
  
  
  # Export merged data
  outfile_basename <- paste(toupper(rcp), type, "optimal", sep="_")
  writeRaster(ras_farm_spp, file=file.path(outputdir, paste0(outfile_basename, "_species.tif")), overwrite=T)
  writeRaster(ras_farm_prod, file=file.path(outputdir, paste0(outfile_basename, "_production_mt_yr.tif")), overwrite=T)
  writeRaster(ras_farm_profits, file=file.path(outputdir, paste0(outfile_basename, "_profits_usd_yr.tif")), overwrite=T)

}


# Run function
calc_optimal_aqprod(rcp="rcp85", species=species, years=c(2021, 2051, 2100), type="Bivalve")



# Functional programming approach
################################################################################


# Parameters for testing
rcp <- "rcp85"
species <- data %>% filter(class=="Actinopterygii") %>% select(species) %>% slice(1:10)
species <- species$species
years <- c(2021, 2051, 2100)
type <- "Finfish"

# Function to read stack into R
read_stack <- function(rcp, species){
  filename <- paste(toupper(rcp), "Finfish", gsub(" ", "_", species), "profits_usd_yr.tif", sep="_")
  profit_ras <- raster::brick(file.path(inputdir,  filename))
  return(profit_ras)
}

# Function to identify and map the most profitab
calc_optimal_aqprod2 <- function(rcp, species, years=c(2021, 2051, 2100), type){
  
  years_vec <- rep(years, length(species))

  # Build key
  stack_key <- tibble(rcp=rcp,
                      species=species) %>%
    mutate(stack=purrr::map2(rcp, species, read_stack)) %>% 
    mutate(yr2021=purrr::map(stack, magrittr::extract2, 1),
           yr2051=purrr::map(stack, magrittr::extract2, 2),
           yr2100=purrr::map(stack, magrittr::extract2, 3))
  
  stack_key2 <- tibble(rcp=rcp,
                      species=species) %>%
    mutate(stack=purrr::map2(rcp, species, read_stack)) %>% 
    mutate(unstacked = purrr::map(stack, unstack)) %>% 
    select(-stack) %>% 
    unnest(cols = unstacked) %>% 
    mutate(year = years_vec) %>% 
    spread(year, unstacked) %>% 
    group_by(rcp) %>% 
    select(-species) %>% 
    summarize_all(function(x) {list(stack(x))})
  
  
  (stack_key2$`2021`[[1]]/1e6/100) %>% plot()
  
  
  
  ras <- stack_key$stack[[1]]
  
  test1 <- stack_key$yr2021[1]
  test2 <- stack_key$yr2021[2]
  
  
  %>%
    mutate(yr2021=list(magrittr::extract2(stack, 1) %>% magrittr::extract2(1)),
           yr2051=list(magrittr::extract2(stack, 1) %>% magrittr::extract2(2)),
           yr2100=list(magrittr::extract2(stack, 1) %>% magrittr::extract2(3)))
  
  # I used this code to figure out how to write the function above
  # stack_key$stack[[1]][[1]]
  # stack_key$yr2100
  
  # Build year stacks
  stack_2021 <- stack(stack_key$yr2021)
  stack_2051 <- stack(stack_key$yr2051)
  stack_2100 <- stack(stack_key$yr2100)
  
  
  # Determine which layer (species) is the most profitable
  stack_2021_wmax <- which.max(stack_2021)
  stack_2051_wmax <- which.max(stack_2051)
  stack_2100_wmax <- which.max(stack_2100)

}









