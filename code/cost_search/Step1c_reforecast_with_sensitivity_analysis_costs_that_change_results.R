
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(raster)
library(ggplot2)
library(tidyverse)

# Directories
codedir <- "code"
sppdir <- "data/species/data"
indir <- "/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/blue-paper-2/data/output/raw"
outdir <- "/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/blue-paper-2/data/output/cost_search"
plotdir <- "/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/blue-paper-2/data/output/cost_search_plots"

# Read aquacast function
source(file.path(codedir, "calc_costs_v2.R"))

# Read species data
load(file.path(sppdir, "aquaculture_species_key_20cages.Rdata"))

# Brackish ISSCAAPs
brackish_isscaaps <- c("Freshwater molluscs", "Miscellaneous diadromous fishes", 
                       "Miscellaneous freshwater fish", "River eels", "Shads", 
                       "Sturgeons, paddlefishes", "Tilapias and other cichlids")

# Not lonline bivalves
bad_bivalve_isscaaps <- c("Clams, cockles, arkshells", "Pearls, mother-of-pearl, shells", "Scallops, pectens")

# Reduce data
data <- data %>% 
  filter(!isscaap %in% c(brackish_isscaaps, bad_bivalve_isscaaps))



# Build function
################################################################################

# Function
rcp <- "rcp26"
species <- data[1,]
cost_scalar <- 1.2
periods <- c("2021-2030", "2051-2060", "2091-2100")
reforecast <- function(species, rcp, periods, price_scalar=1, cost_scalar=1, outdir=F, plot=T){
  
  # 1. Read data
  ############################
  
  # Param
  spp <- species$species
  type <- ifelse(species$class=="Bivalvia", "Bivalve", "Finfish")
  print(spp)
  
  # Read suitable cells
  print("... reading and transforming habitat suitability")
  filename_base <- paste(toupper(rcp), gsub(" ", "_", spp), sep="_")
  filename_in <- paste0(filename_base, ".Rds")
  vcells_orig <- readRDS(file.path(indir, filename_in))
  periods_avail <- sort(unique(vcells_orig$period))
  
  # Convert to raster
  vcells <- vcells_orig %>% 
    # Simplify
    select(period, x, y, viable) %>% 
    # Spread to XY-ZZZ
    spread(key="period", value="viable") %>% 
    # Convert to raster
    rasterFromXYZ(crs = crs(ras_temp)) %>% 
    # Project raster
    projectRaster(to=ras_temp)
  
  # 2. Extract data
  ############################
  
  # Growth and harvest parameters
  linf_cm <- species$linf_cm
  k <- species$k
  harvest_cm <- species$harvest_cm
  harvest_g <- species$harvest_g
  harvest_yr <- species$harvest_yr
  harvest_kg_m3 <- species$harvest_kg_m3
  harvest_cm_ft <- species$harvest_cm_ft
  nstocked <- species$nstocked
  nstocked_adj <- species$nstocked_adj
  a <- species$a
  b <- species$b
  fcr <- species$fcr
  price_usd_mt <- species$price_usd_mt_isscaap * price_scalar
  
  
  # 3. Farm design
  ############################
  
  # Finfish farm design
  if(type=="Finfish"){
    # Finfish farm design
    farm_design <- tibble(type="finfish",
                          area_sqkm=1,
                          ncages=20,
                          cage_vol_m3=9000) %>% 
      # Calculate number stocked
      mutate(tot_m3=ncages * cage_vol_m3,
             tot_kg=tot_m3*harvest_kg_m3,
             nstocked=tot_kg*1000/harvest_g,
             nstocked_adj=nstocked)
  }
  
  # Bivalve farm design
  if(type=="Bivalve"){
    farm_design <- tibble(type="bivalve",
                          area_sqkm=1,
                          nlines=species$lines_n,
                          line_rope_ft=2109,
                          harvest_cm_ft=harvest_cm_ft) %>% 
      mutate(nstocked = nlines * line_rope_ft * (harvest_cm_ft / harvest_cm),
             nstocked_adj=nstocked_adj)
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
  
  # 4. Production
  ############################
  
  # Calculate annual harvest per cell
  print("... mapping production potential")
  cell_sqkm <- 100
  cell_nfarms <- cell_sqkm / farm_design$area_sqkm
  cell_prod_mt_yr <- farm_mt_yr * cell_nfarms
  
  # Calculate annual revenue per cell
  cell_revenue_usd_yr <-  cell_prod_mt_yr * price_usd_mt
  
  # Check cell revenue
  if(all.equal(species$revenue_usd_yr*cell_nfarms*price_scalar, cell_revenue_usd_yr)!=T){
    stop("Cell revenue doesn't match value in key.")
  }
  
  
  # 5. Calculate costs
  ####################################
  
  # Calculate costs and profits
  print("... calculating costs and profits")
  cell_cost_usd_yr <- calc_costs_v2(farm_design, cell_prod_mt_yr, fcr, vcells, harvest_yr) * cost_scalar
  
  # 6. Calculate profits
  ####################################
  
  # Build final data frame
  data_df <- cell_cost_usd_yr %>% 
    # Convert to dataframe
    as.data.frame(xy=T) %>% 
    setNames(c("x", "y", periods_avail)) %>% 
    gather(key="period", value="cost_usd_yr", 3:ncol(.)) %>% 
    # Reduce to cells with costs
    filter(!is.na(cost_usd_yr)) %>% 
    # Add variables
    mutate(viable=1,
           prod_mt_yr=cell_prod_mt_yr,
           revenue_usd_yr=cell_revenue_usd_yr,
           profits_usd_yr=revenue_usd_yr - cost_usd_yr) %>% 
    # Arrange
    select(period, x, y, viable, prod_mt_yr, revenue_usd_yr, cost_usd_yr, profits_usd_yr)
  
  
  # 5. Export and plot
  ####################################
  
  # If exporting
  outfile_basename <- paste(toupper(rcp), gsub(" ", "_", spp), "cost_search", sep="_", cost_scalar)
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



# Reforecast using function
################################################################################

# Cost scalars
cost_scalars <- c(2.1, 2.7)

# Build key
spp_cost_key <-purrr::map_df(cost_scalars, function(x){
  
  # Add cost scalar
  data1 <- data %>% 
    mutate(cost_scalar=x)
  
})


# Loop through species and cost scalars
for(i in 1:nrow(spp_cost_key)){
  species <- spp_cost_key[i,]
  cost_scalar <- spp_cost_key$cost_scalar[i]
  periods <- c("2021-2030", "2051-2060", "2091-2100")
  output <- reforecast(species=species, periods=periods, rcp="rcp26", cost_scalar=cost_scalar, outdir=outdir, plot=T)
  output <- reforecast(species=species, periods=periods, rcp="rcp45", cost_scalar=cost_scalar, outdir=outdir, plot=F)
  output <- reforecast(species=species, periods=periods, rcp="rcp60", cost_scalar=cost_scalar, outdir=outdir, plot=F)
  output <- reforecast(species=species, periods=periods, rcp="rcp85", cost_scalar=cost_scalar, outdir=outdir, plot=F)
}









