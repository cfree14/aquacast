
# Setup
#################################################################################

# Packages
library(raster)
library(ggplot2)
library(tidyverse)

# Read EEZ raster and mask
# maskdir <- "data/constraints/masks"
# mask <- raster(file.path(maskdir, "mask_10km.tif"))

# Read EEZs
eezdir <- "data/eezs"
eezs <- raster(file.path(eezdir, "eezs_v10_raster_10km.tif"))
eez_key <- read.csv(file.path(eezdir, "eezs_v10_key.csv"), as.is=T)

# Build EEZ mask
eez_mask <- eezs
eez_mask[!is.na(eez_mask)] <- 1


# Function
#################################################################################

# Forecast productivity under climate change
aquacast <- function(species, years=c(2020,2050,2100), rcp, outdir=F, plot=T){
  
  # Read climate forecasts
  climatedir <- "data/climate/data/gfdl/GFDL-ESM2G/4rasters_scaled"
  ras_sst_c_min <- raster::brick(file.path(climatedir, paste0("GFDL_ESM2M_", rcp, "_tos_degC_annual_min_scaled.tif")))
  ras_sst_c_max <- raster::brick(file.path(climatedir, paste0("GFDL_ESM2M_", rcp, "_tos_degC_annual_max_scaled.tif")))
  ras_sal_psu_mean <- raster::brick(file.path(climatedir, paste0("GFDL_ESM2M_", rcp, "_so_psu_annual_mean_scaled.tif")))
  ras_o2_molm3_mean <- raster::brick(file.path(climatedir, paste0("GFDL_ESM2M_", rcp, "_o2_mol_m3_annual_mean_scaled.tif")))
  ras_chl_mgm3_meansubsd <- raster::brick(file.path(climatedir, paste0("GFDL_ESM2M_", rcp, "_chl_mg_m3_annual_mean_minus_sd_scaled.tif")))
  ras_arag_sat_mean <- raster::brick(file.path(climatedir, paste0("GFDL_ESM2M_", rcp, "_arag_sat_annual_mean_scaled.tif")))
  ras_layer_names <- readRDS(file.path(climatedir, "GFDL_ESM2M_rcp85_arag_sat_annual_mean_scaled_layer_names.rds"))

  # Add raster layer names
  names(ras_sst_c_min) <- names(ras_sst_c_max) <- names(ras_sal_psu_mean) <- names(ras_o2_molm3_mean) <- 
    names(ras_chl_mgm3_meansubsd) <- names(ras_arag_sat_mean) <- ras_layer_names
  
  # Check rasters
  env_ras_check <- compareRaster(eezs, ras_sst_c_min, ras_sst_c_max, ras_sal_psu_mean, 
                                 ras_o2_molm3_mean, ras_chl_mgm3_meansubsd, ras_arag_sat_mean)
  if(env_ras_check==F){print("EEZ and climate forecast rasters DO NOT have the same projection, extent, and resolution.")}

  
  # 1. Extract parameters
  ####################################
  
  # Growth and harvest parameters
  spp <- species$species
  type <- species$type
  linf_cm <- species$linf_cm
  k <- species$k
  harvest_cm <- species$harvest_cm
  harvest_g <- species$harvest_g
  harvest_yr <- species$harvest_yr
  harvest_kg_m3 <- species$harvest_kg_m3
  nstocked <- species$nstocked
  a <- species$a
  b <- species$b
  fcr <- species$fcr
  price_usd_mt <- species$price_usd_mt_isscaap
  print(spp)
  
  # Environmental tolerance parameters
  sst_c_min <- species$sst_c_min
  sst_c_max <- species$sst_c_max
  sal_psu_min <- species$sal_psu_min
  sal_psu_max <- species$sal_psu_max
  if(type=="Finfish"){
    o2_molm3_min <- 0.1378
    chl_mgm3_meansubsd_min <- NA
    arag_sat_min <- NA
  }
  if(type=="Bivalve"){
    o2_molm3_min <- 0.0622
    chl_mgm3_meansubsd_min <- 0.4
    arag_sat_min <- 1
  }
  

  # 2. Calculate harvest/sqkm/yr
  ####################################
  
  # Finfish farm design
  if(type=="Finfish"){
    # Finfish farm design
    farm_design <- tibble(type="finfish",
                          area_sqkm=1,
                          ncages=24,
                          cage_vol_m3=9000) %>% 
      # Calculate number stocked
      mutate(tot_m3=ncages * cage_vol_m3,
             tot_kg=tot_m3*harvest_kg_m3,
             nstocked= tot_kg*1000/harvest_g)
  }
  
  # Bivalve farm design
  if(type=="Bivalve"){
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
  yrs_available <- as.numeric(ras_layer_names)
  yrs_do_indices <- which(yrs_available %in% years)
  
  # Viable cells
  print("... identifying viable cells")
  sst_c_mask <- ras_sst_c_min[[yrs_do_indices]] >= sst_c_min & ras_sst_c_max[[yrs_do_indices]] <= sst_c_max
  sal_psu_mask <- ras_sal_psu_mean[[yrs_do_indices]] >= sal_psu_min & ras_sal_psu_mean[[yrs_do_indices]] <= sal_psu_max
  
  # Bivalve
  if(type=="Bivalve"){
    o2_molm3_mask <- ras_o2_molm3_mean[[yrs_do_indices]] >= o2_molm3_min
    chl_mgm3_meansubsd_mask <- ras_chl_mgm3_meansubsd[[yrs_do_indices]] >= chl_mgm3_meansubsd_min
    arag_sat_mask <- ras_arag_sat_mean[[yrs_do_indices]] >= arag_sat_min
    vcells <- eez_mask * sst_c_mask * sal_psu_mask * o2_molm3_mask * chl_mgm3_meansubsd_mask * arag_sat_mask
    NAvalue(vcells) <- 0
  }
  
  # Finfish
  if(type=="Finfish"){
    o2_molm3_mask <- ras_o2_molm3_mean[[yrs_do_indices]] >= o2_molm3_min
    vcells <- eez_mask * sst_c_mask * sal_psu_mask * o2_molm3_mask
    NAvalue(vcells) <- 0
  }
  
  
  # Plot check
  # plot(sst_c_mask[[1]], main="SST mask")
  # plot(sal_psu_mask[[1]], main="Salinity mask")
  # plot(o2_molm3_mask[[1]], main="Oxygen mask")
  # plot(chl_mgm3_meansubsd_mask[[1]], main="Chlorophyll mask")
  # plot(arag_sat_mask[[1]], main="Aragonite mask")
  # plot(vcells[[1]], main="Suitable cells")
  
  # 4. Calculate production per cell
  ####################################
  
  # Calculate annual harvest per cell
  print("... mapping production potential")
  cell_sqkm <- prod(res(vcells)) / (1000^2)
  cell_mt_yr <- farm_mt_yr * cell_sqkm
  prod_mt_yr <- vcells * cell_mt_yr
  
  # Calculate annual revenue per cell
  revenue_usd_yr <- prod_mt_yr * price_usd_mt
  
  # Plot check
  # plot(prod_mt_yr[[1]])
  # plot(revenue_usd_yr[[1]])
  
  # 4. Calculate costs
  ####################################
  
  # Calculate costs and profits
  print("... calculating costs and profits")
  cost_usd_yr <- calc_costs(farm_design, prod_mt_yr, fcr, vcells, harvest_yr)
  profits_usd_yr <- revenue_usd_yr - cost_usd_yr
  
  # Plot check
  # plot(cost_usd_yr[[1]]/1e6/100)
  # plot(profits_usd_yr[[1]]/1e6/100)
  
  # 5. Convert to dataframe
  data_df <- as.data.frame(cost_usd_yr, xy=T) %>% 
    setNames(c("x", "y", years)) %>% 
    gather(key="year", value="cost_usd_yr", 3:ncol(.)) %>% 
    filter(!is.na(cost_usd_yr)) %>% 
    mutate(viable=1,
           prod_mt_yr=cell_mt_yr,
           revenue_usd_yr=cell_mt_yr * price_usd_mt,
           profits_usd_yr=revenue_usd_yr - cost_usd_yr) %>% 
    select(x, y, viable, prod_mt_yr, revenue_usd_yr, cost_usd_yr, profits_usd_yr)
  
  # Export dataframe
  outfile_basename <- paste(toupper(rcp), type, gsub(" ", "_", spp), sep="_")
  saveRDS(data_df, file.path(outdir, paste0(outfile_basename, ".Rds")))
  

  # 5. Summarize by EEZ
  ####################################
  
  # Note: I'm turning this off because it's not plotting protiable production
  
  # # Summarize by EEZ
  # print("... summarizing production by EEZ")
  # eez_prod_mt_yr <- zonal(x=prod_mt_yr, z=eezs, fun="sum", na.rm=T)
  # 
  # # Build results dataframe
  # eez_prod_mt_yr_df <- eez_prod_mt_yr %>% 
  #   as.data.frame() %>% 
  #   setNames(c("eez_code", years)) %>% 
  #   gather(key="year", value="prod_mt_yr", -1) %>% 
  #   mutate(year=as.numeric(year)) %>% 
  #   left_join(select(eez_key, eez_code, eez_name, ter1_name, ter1_iso), by="eez_code")
  # 
  # # Build overall results
  # prod_mt_yr_df <- eez_prod_mt_yr_df %>% 
  #   group_by(year) %>% 
  #   summarize(prod_mt_yr=sum(prod_mt_yr))
    
    
  # 5. Export and plot
  ####################################

  # If exporting rasters
  if(outdir!=F){
    print("... exporting results")
    outfile_basename <- paste(toupper(rcp), type, gsub(" ", "_", spp), sep="_")
    # vcells, prod_mt_yr, revenue_usd_yr, cost_usd_yr, profits_usd_yr
    writeRaster(prod_mt_yr, file=file.path(outdir, paste0(outfile_basename, "_production_mt_yr.tif")), overwrite=T)
    writeRaster(revenue_usd_yr, file=file.path(outdir, paste0(outfile_basename, "_revenues_usd_yr.tif")), overwrite=T)
    writeRaster(cost_usd_yr, file=file.path(outdir, paste0(outfile_basename, "_costs_usd_yr.tif")), overwrite=T)
    writeRaster(profits_usd_yr, file=file.path(outdir, paste0(outfile_basename, "_profits_usd_yr.tif")), overwrite=T)
  }
  
  # # If plotting
  # if(plot==T){
  #   
  #   # Plot production potential
  #   g <- ggplot(prod_mt_yr_df, aes(x=year, y=prod_mt_yr/1e6)) +
  #     geom_bar(stat="identity") + 
  #     labs(x="", y="Prodution potential\n(millions of mt)", title=spp) +
  #     theme_bw()
  #   g
  #   print(g)
  #   
  # }
  # 
  # # Return data
  # return(eez_prod_mt_yr_df)
  
}






