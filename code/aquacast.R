
# Read important layers

# Setup
#################################################################################

# Packages
library(raster)
library(ggplot2)
library(tidyverse)

# Directories
sstdir <- "data/climate/data/noaa/processed"
eezdir <- "data/eezs"
maskdir <- "data/constraints/masks"

# Read EEZ raster and mask
eezs <- raster(file.path(eezdir, "eezs_v10_raster_10km.tif"))
eez_key <- read.csv(file.path(eezdir, "eezs_v10_key.csv"), as.is=T)
mask <- raster(file.path(maskdir, "mask_10km.tif"))

# Read SST forecast
sst_c_min <- brick(file.path(sstdir, "RCP85_annual_sst_min.tif"))
sst_c_max <- brick(file.path(sstdir, "RCP85_annual_sst_min.tif"))


# Function
#################################################################################

# Forecast productivity under climate change
aquacast <- function(species, years=c(2020,2050,2100), outdir=F, plot=T){
  
  # 1. Extract parameters
  ####################################
  
  # Parameters
  spp <- species$species
  type <- species$type
  linf_cm <- species$linf_cm
  k <- species$k
  harvest_cm <- species$harvest_cm
  a <- species$a
  b <- species$b
  min_c <- species$temp_c_min
  max_c <- species$temp_c_max
  fcr <- species$fcr
  price_usd_mt <- species$price_usd_mt


  # 2. Calculate harvest/sqkm/yr
  ####################################
  
  # Calculate time to harvest
  harvest_yr <- -log(1 - harvest_cm / linf_cm) / k
  
  # Calculate weight at harvest
  harvest_g <- a * harvest_cm ^ b
  
  # Farm design
  if(type=="finfish"){
    # Finfish farm design
    farm_design <- tibble(type="finfish",
                          area_sqkm=1,
                          ncages=24,
                          cage_vol_m3=9000,
                          juv_m3=20) %>% 
      mutate(nstocked=ncages * cage_vol_m3 * juv_m3)
  }else{
    #Bivalve farm design
    farm_design <- tibble(type="bivalve",
                          area_sqkm=1,
                          nlines=100,
                          line_m=4000,
                          juv_ft=100) %>% 
      mutate(line_ft=measurements::conv_unit(line_m, "m", "ft"),
             nstocked=nlines * line_ft * juv_ft)
  }
  
  # Calculate yield per year (kg/yr) for 1 sqkm farm
  farm_kg_yr <- harvest_g/1000 * farm_design$nstocked / harvest_yr
  farm_mt_yr <- farm_kg_yr / 1000
  
  # 3. Identify viable cells
  ####################################
  
  # Years to evaluate
  yrs_available <- 2006:2100
  yrs_do_indices <- which(yrs_available %in% years)
  
  # Viable cells
  print("... identifying viable cells")
  sst_c_min_mask <- sst_c_min[[yrs_do_indices]] > min_c
  sst_c_max_mask <- sst_c_max[[yrs_do_indices]] < max_c
  vcells <- mask * sst_c_min_mask *  sst_c_max_mask
  vcells[is.na(vcells)] <- 0
  
  # Plot one
  # plot(vcells[[1]])
  
  # 4. Calculate production per cell
  ####################################
  
  # Calculate annual harvest per cell
  print("... mapping production potential")
  cell_sqkm <- prod(res(vcells)) / (1000^2)
  cell_mt_yr <- farm_mt_yr * cell_sqkm
  prod_mt_yr <- vcells * cell_mt_yr
  
  # Calculate annual revenue per cell
  revenue_usd_yr <- prod_mt_yr * price_usd_mt
  
  
  # 4. Calculate costs
  ####################################
  
  # Calculate costs anf profits
  print("... calculating costs and profits")
  cost_usd_yr <- calc_costs(farm_design, prod_mt_yr, fcr, vcells)
  profits_usd_yr <- revenue_usd_yr - cost_usd_yr
  
  
  # 5. Summarize by EEZ
  ####################################
  
  # Summarize by EEZ
  print("... summarizing production by EEZ")
  eez_prod_mt_yr <- zonal(x=prod_mt_yr, z=eezs, fun="sum", na.rm=T)
  
  # Build results dataframe
  eez_prod_mt_yr_df <- eez_prod_mt_yr %>% 
    as.data.frame() %>% 
    setNames(c("eez_code", years)) %>% 
    gather(key="year", value="prod_mt_yr", -1) %>% 
    mutate(year=as.numeric(year)) %>% 
    left_join(select(eez_key, eez_code, eez_name, ter1_name, ter1_iso), by="eez_code")
  
  # Build overall results
  prod_mt_yr_df <- eez_prod_mt_yr_df %>% 
    group_by(year) %>% 
    summarize(prod_mt_yr=sum(prod_mt_yr))
    
    
  # 5. Export and plot
  ####################################

  # If exporting rasters
  if(outdir!=F){
    print("... exporting production raster")
    filename <- paste0(gsub(" ", "_", spp), "_production.tif")
    writeRaster(prod_mt_yr, file=file.path(outdir, filename), overwrite=T)
  }
  
  # If plotting
  if(plot==T){
    
    # Plot production potential
    g <- ggplot(prod_mt_yr_df, aes(x=year, y=prod_mt_yr/1e6)) +
      geom_bar(stat="identity") + 
      labs(x="", y="Prodution potential\n(millions of mt)", title=spp) +
      theme_bw()
    g
    print(g)
    
  }
  
  # Return data
  return(eez_prod_mt_yr_df)
  
}






