
# Setup
#################################################################################

# Packages
library(raster)
library(ggplot2)
library(tidyverse)

# Read EEZs
eezdir <- "data/eezs"
eezs <- raster(file.path(eezdir, "eezs_v10_raster_10km.grd"))
eez_key <- read.csv(file.path(eezdir, "eezs_v10_key.csv"), as.is=T)

# Build EEZ mask
eez_mask <- eezs
eez_mask[!is.na(eez_mask)] <- 1


# Function
#################################################################################

# Forecast productivity under climate change
aquacast <- function(species, periods, rcp, outdir=F, plot=T){
  
  # Read climate forecasts
  climatedir <- "/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/blue-paper-2/data/climate/GFDL-ESM2G/4rasters_scaled"
  ras_sst_c_min <- raster::brick(file.path(climatedir, paste0("GFDL_ESM2G_", rcp, "_tos_degC_annual_min_scaled.grd")))
  ras_sst_c_max <- raster::brick(file.path(climatedir, paste0("GFDL_ESM2G_", rcp, "_tos_degC_annual_max_scaled.grd")))
  ras_sal_psu_mean <- raster::brick(file.path(climatedir, paste0("GFDL_ESM2G_", rcp, "_so_psu_annual_mean_scaled.grd")))
  ras_o2_molm3_mean <- raster::brick(file.path(climatedir, paste0("GFDL_ESM2G_", rcp, "_o2_mol_m3_annual_mean_scaled.grd")))
  ras_chl_mgm3_meansubsd <- raster::brick(file.path(climatedir, paste0("GFDL_ESM2G_", rcp, "_chl_mg_m3_annual_mean_minus_sd_scaled.grd")))
  ras_arag_sat_mean <- raster::brick(file.path(climatedir, paste0("GFDL_ESM2G_", rcp, "_arag_sat_annual_mean_scaled.grd")))
  ras_curr_m_s_max <- raster::brick(file.path(wavedir, paste0("GFDL_ESM2G_", rcp, "_current_speed_m_s_annual_max_scaled.grd")))
  
  # Read wave data
  wavedir <- "/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/blue-paper-2/data/climate/Song_etal_2020/4rasters_scaled"
  rcp2_ssp_key <- tibble(rcp=paste0("rcp", c("26", "45", "60", "85"))) %>% 
    mutate(ssp=recode(rcp, "rcp26"="SSP126", "rcp45"="SSP245", "rcp60"="SSP585", "rcp85"="SSP585"))
  rcp_do <- rcp
  ssp <- rcp2_ssp_key$ssp[rcp2_ssp_key$rcp==rcp_do]
  ras_waves_m_max <- raster::brick(file.path(wavedir, paste0("Song_etal_2020_", ssp, "_sig_wave_height_annual_max_scaled.grd")))
  
  # Check rasters
  env_ras_check <- compareRaster(eezs, ras_sst_c_min, ras_sst_c_max, ras_sal_psu_mean, 
                                 ras_o2_molm3_mean, ras_chl_mgm3_meansubsd, ras_arag_sat_mean, ras_curr_m_s_max, ras_waves_m_max)
  if(env_ras_check==F){stop("EEZ and climate forecast rasters DO NOT have the same projection, extent, and resolution.")}
  env_ras_check1 <- length(unique(c(nlayers(ras_sst_c_min),
                                    nlayers(ras_sst_c_max),
                                    nlayers(ras_sal_psu_mean),
                                    nlayers(ras_o2_molm3_mean),
                                    nlayers(ras_chl_mgm3_meansubsd),
                                    nlayers(ras_arag_sat_mean))))
  if(env_ras_check1==F){stop("Climate forecast rasters DO NOT have the same number of layers.")}
  
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
  harvest_cm_ft <- species$harvest_cm_ft
  nstocked <- species$nstocked
  a <- species$a
  b <- species$b
  fcr <- species$fcr
  price_usd_mt <- species$price_usd_mt_isscaap
  print(spp)
  
  # Environmental tolerance parameters
  # Species-specific
  sst_c_min <- species$sst_c_min
  sst_c_max <- species$sst_c_max
  sal_psu_min <- species$sal_psu_min
  sal_psu_max <- species$sal_psu_max
  # Category-specific
  if(type=="Finfish"){
    o2_molm3_min <- 0.1378
    chl_mgm3_meansubsd_min <- NA
    arag_sat_min <- NA
  }
  if(type=="Bivalve"){
    o2_molm3_min <- 0.0622
    chl_mgm3_meansubsd_min <- 0.20 # 0.40 used by Froehlich et al. (2018); 0.20 derived here
    arag_sat_min <- 1.75 # Barton et al. (2015)
  }
  # Generic
  curr_m_s_min <- 0.04
  curr_m_s_max <- 1.0
  waves_m_max <- 6
  
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
             nstocked=tot_kg*1000/harvest_g)
  }
  
  # Bivalve farm design
  if(type=="Bivalve"){
    farm_design <- tibble(type="bivalve",
                          area_sqkm=1,
                          nlines=species$lines_n,
                          line_rope_ft=2109,
                          harvest_cm_ft=harvest_cm_ft) %>% 
      mutate(nstocked = nlines * line_rope_ft * (harvest_cm_ft / harvest_cm))
  }
  
  # Check number of stocked
  if(all.equal(nstocked, farm_design$nstocked)!=T){
    stop("Number of stocked individuals calculated doesn't match value in key.")
  }
  
  # Calculate yield per year (kg/yr) for 1 sqkm farm
  farm_kg_yr <- harvest_g/1000 * farm_design$nstocked / harvest_yr
  farm_mt_yr <- farm_kg_yr / 1000
  
  # Check farm production
  if(all.equal(species$prod_mt_yr, farm_mt_yr)!=T){
    stop("Farm production doesn't match value in key.")
  }
  
  # 3. Identify viable cells
  ####################################
  
  # Identify years to evaluate
  yrs_df <- periods %>% purrr::map_df(function(x) {
    yr1 <- substr(x, 1, 4) %>% as.numeric()
    yr2 <- substr(x, 6, 9) %>% as.numeric()
    yrs <- yr1:yr2
    df_out <- tibble(period=x, 
                     year=yrs)
  }) %>% 
    mutate(index=1:n())
  yrs_do <- yrs_df$year
  
  # Make sure we have environmental data for all years
  yrs_available <- names(ras_sst_c_min) %>% gsub("X", "", .) %>% as.numeric()
  yrs_check <- sum(!yrs_do %in% yrs_available) == 0
  if(yrs_check==F){stop("You do not have climate data for years in the provided periods.")}
  
  # Indices for the years to evaluate
  yrs_do_indices <- which(yrs_available %in% yrs_do)
  
  # Each year's viable cells
  print("... identifying viable cells per year")
  sst_c_mask <- ras_sst_c_min[[yrs_do_indices]] >= sst_c_min & ras_sst_c_max[[yrs_do_indices]] <= sst_c_max
  sal_psu_mask <- ras_sal_psu_mean[[yrs_do_indices]] >= sal_psu_min & ras_sal_psu_mean[[yrs_do_indices]] <= sal_psu_max
  
  # Bivalve
  if(type=="Bivalve"){
    o2_molm3_mask <- ras_o2_molm3_mean[[yrs_do_indices]] >= o2_molm3_min
    chl_mgm3_meansubsd_mask <- ras_chl_mgm3_meansubsd[[yrs_do_indices]] >= chl_mgm3_meansubsd_min
    arag_sat_mask <- ras_arag_sat_mean[[yrs_do_indices]] >= arag_sat_min
    vcells_yr <- eez_mask * sst_c_mask * sal_psu_mask * o2_molm3_mask * chl_mgm3_meansubsd_mask * arag_sat_mask
    NAvalue(vcells_yr) <- 0
  }
  
  # Finfish
  if(type=="Finfish"){
    o2_molm3_mask <- ras_o2_molm3_mean[[yrs_do_indices]] >= o2_molm3_min
    vcells_yr <- eez_mask * sst_c_mask * sal_psu_mask * o2_molm3_mask
    NAvalue(vcells_yr) <- 0
  }
  
  # Each period's viable cells
  print("... identifying viable cells per period")
  yrs_per_period <- yrs_df %>% group_by(period) %>% summarize(nyrs=n()) %>% pull(nyrs) %>% unique()
  for(i in 1:length(periods)){
    indices <- yrs_df %>% filter(period==periods[i]) %>% pull(index)
    # vcells_sum <- sum(vcells_yr[[indices]], na.rm=T) # This is slower than calc()
    vcells_sum <- calc(vcells_yr[[indices]], sum, na.rm=T)
    # plot(vcells_sum) should show gradient of 0-10 cells with 10 at core
    vcells_period <- vcells_sum==yrs_per_period
    # plot(vcells_period) # should show only the core area
    if(i==1){vcells_out <- vcells_period}else{vcells_out <- stack(vcells_out, vcells_period)}
  }
  
  # Convert viable cells
  vcells <- brick(vcells_out)
  names(vcells) <- periods
  vcells[vcells==0] <- NA
  
  # Plot check
  # par(mfrow=c(3,1))
  # plot(vcells)
  
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
  cell_nfarms <- cell_sqkm / farm_design$area_sqkm
  cell_prod_mt_yr <- farm_mt_yr * cell_nfarms
  
  # Calculate annual revenue per cell
  cell_revenue_usd_yr <-  cell_prod_mt_yr * price_usd_mt
  
  # Check cell revenue
  if(all.equal(species$revenue_usd_yr*cell_nfarms, cell_revenue_usd_yr)!=T){
    stop("Cell revenue doesn't match value in key.")
  }
  
  # 4. Calculate costs
  ####################################
  
  # Calculate costs and profits
  print("... calculating costs and profits")
  cell_cost_usd_yr <- calc_costs(farm_design, cell_prod_mt_yr, fcr, vcells, harvest_yr)
  
  # Plot check
  # plot(cell_cost_usd_yr[[1]]/1e6/100)
  # plot(cell_cost_usd_yr[[1]]/cell_prod_mt_yr)
  
  
  # 4. Calculate profits
  ####################################
  
  # Build final data frame
  data_df <- as.data.frame(cell_cost_usd_yr, xy=T) %>% 
    setNames(c("x", "y", periods)) %>% 
    gather(key="period", value="cost_usd_yr", 3:ncol(.)) %>% 
    filter(!is.na(cost_usd_yr)) %>% 
    mutate(viable=1,
           prod_mt_yr=cell_prod_mt_yr,
           revenue_usd_yr=cell_revenue_usd_yr,
           profits_usd_yr=revenue_usd_yr - cost_usd_yr) %>% 
    select(period, x, y, viable, prod_mt_yr, revenue_usd_yr, cost_usd_yr, profits_usd_yr)
  
  
  # 5. Export and plot
  ####################################
  
  # If exporting
  outfile_basename <- paste(toupper(rcp), gsub(" ", "_", spp), sep="_")
  if(outdir!=F){
    saveRDS(data_df, file.path(outdir, paste0(outfile_basename, ".Rds")))
  }
  
  # If plotting
  if(plot==T){
    
    # Build stats data frame
    stats <- data_df %>% 
      filter(profits_usd_yr>0) %>% 
      group_by(period) %>% 
      summarize(area_sqkm_mil=sum(viable)*100/1e6,
                prod_mt_yr_mil=sum(prod_mt_yr)/1e6, 
                profits_usd_yr_tril=sum(profits_usd_yr)/1e12) %>% 
      ungroup()
    
    # Plot
    g <- ggplot(stats, aes(x=period, y=area_sqkm_mil)) +
      geom_bar(stat="identity") +
      labs(x="", y="Profitable area (millions of sqkm)", title=species$comm_name) +
      theme_bw() +
      theme( axis.text.y = element_text(angle = 90, hjust = 0.5) )
    print(g)
    
    
  }
  
}






