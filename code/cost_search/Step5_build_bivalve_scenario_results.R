
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(raster)
library(ggplot2)
library(tidyverse)
library(countrycode)
library(rnaturalearth)

# Directories
datadir <- "/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/blue-paper-2/data/output/processed"
outputdir <- "/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/blue-paper-2/data/output/cost_search_processed"
plotdir <- "figures"
tabledir <- "tables"

# Read species data
load("data/species/data/aquaculture_species_key.Rdata")
spp_key <- data
rm(data_full, data)

# Read feed info
maq_prod <- read.csv("data/feed_params/processed/FAO_2013_2017_maq_prod_averages_by_country.csv", as.is=T)

# World
world <- rnaturalearth::ne_countries(scale="large", type = "countries", returnclass = "sf")

# Read EEZ key
eezdir <- "data/eezs"
eez_key <- read.csv(file.path(eezdir, "eezs_v10_key.csv"), as.is=T)

# Read population data
popdir <- "data/pop_growth/data"
pop <- readRDS(file.path(popdir, "WB_UN_1960_2100_human_population_by_country.Rds"))

# Read fisheries impacts
wc <- readRDS("data/capture_projections/data/Free_etal_2020_national_projections_with_pop_data.Rds")


# Function to develop demand-limited bivalve mariculture
################################################################################

# For testing
rcp <- "RCP 2.6"
dev_scenario <- "Need-based"
suffix <- "cost_search_1.3"

# Function to develop feed-constrained finfish mariculture
expand_mariculture <- function(rcp,  dev_scenario, suffix=""){
  
  # Steps
  # 1. Determine feed availability
  # 2. Determine feed demand
  # 3. Develop based on distribution pattern

  # Setup
  ###############################################################
  
  # Parameters
  rcp_do <- rcp
  dev_scenario_do <- dev_scenario
  
  # Demand limit
  prod_mt_cap <- 103 * 0.17 / 0.17 * 1e6
  
  # Read finfish forecasts
  rcp_do_short <- gsub("\\.| ", "", rcp_do)
  if(suffix==""){
    infile <- paste0( rcp_do_short, "_Bivalve_rational_use.Rds")
  }else{
    infile <- paste0( rcp_do_short, "_Bivalve_rational_use_", suffix, ".Rds")
  }
  data_orig <- readRDS(file.path(outputdir, infile))
  
  # Build forecasts period key
  periods <- data_orig %>% pull(period) %>% unique()
  period_key <- purrr::map_df(periods, function(x){
    yr1 <- substr(x,1,4) %>% as.numeric()
    yr2 <- substr(x,6,9) %>% as.numeric()
    yrs <- yr1:yr2
    yr_df <- tibble(period=x,
                    year=yrs)
  })
  
    
  
  # Develop mariculture based on development scenario
  ###############################################################
  
  # Rational development
  if(dev_scenario_do=="Rational"){
    
    # Develop profitable cells first
    data1 <- data_orig %>% 
      group_by(period) %>% 
      arrange(period, desc(profits_usd_yr)) %>% 
      # Calculate cumulative production
      mutate(cum_prod_mt_yr=cumsum(prod_mt_yr),
             developed=ifelse(cum_prod_mt_yr <= prod_mt_cap, "yes", "no")) %>% 
      filter(developed=="yes")
    
  }
  
  # Current development
  if(dev_scenario_do=="Current"){
    
    # Format current producers
    # Based on FAO 2013-2017 averages
    props <- maq_prod %>% 
      # Reduce to finfish aquaculture totals
      filter(major_group=="Mollusca") %>% 
      # Format country information
      mutate(country_use=countrycode(country_use, "country.name", "country.name"),
             iso3_use=countrycode(country_use, "country.name", "iso3c")) %>% 
      # Remove TERRITORIES not forecast to have to viable mariculutre
      filter(iso3_use %in% unique(data_orig$ter1_iso)) %>% 
      # Calculate proportion of feed dedicated to this TERRITORY
      mutate(prod_prop=prod_mt/sum(prod_mt)) # note if a country isn't present in a period some feed will go unused
    
    # Pass #1. Develop profitable cells first, in countries with production
    pass1 <- data_orig %>% 
      # Add percent of production allocated to TERRITORY-LIKE UNIT (USA (Alaska/Hawaii/Continental), Guam, Puerto Rico all seperate)
      left_join(select(props, iso3_use, prod_prop), by=c("ter1_iso"="iso3_use")) %>% 
      # Calculate production allocation
      mutate(prod_prop=ifelse(is.na(prod_prop), 0, prod_prop),
             prod_allocated_mt_yr=prod_mt_cap*prod_prop) %>% 
      # Remove places not allocated any production
      filter(prod_prop>0) %>% 
      # Rank by profitability within EEZ
      group_by(period, eez_name) %>% 
      arrange(period, eez_name, desc(profits_usd_yr)) %>% 
      mutate(rank_in_eez=1:n()) %>% 
      ungroup() %>% 
      # Arrange within TERRITORY-LIKE UNIT (USA (Alaska/Hawaii/Continental), Guam, Puerto Rico all seperate)
      group_by(period, ter1_name) %>% 
      arrange(period, ter1_name, rank_in_eez, desc(profits_usd_yr)) %>% 
      # Calculate cumulative production and see if it exceeds ALLOCATED AMOUNT
      mutate(cum_prod_mt_yr=cumsum(prod_mt_yr),
             developed=ifelse(cum_prod_mt_yr <= prod_allocated_mt_yr, "yes", "no")) %>% 
      ungroup()

    # Cells developed in pass one 
    pass1_use <- pass1 %>% 
      filter(developed=="yes")
    
    # Undeveloped production from pass 1
    pass1_leftover <- pass1_use %>% 
      group_by(period) %>% 
      summarize(prod_mt_yr=sum(prod_mt_yr)) %>% 
      ungroup() %>% 
      mutate(prod_mt_yr_fill=prod_mt_cap-prod_mt_yr)
      
    # Pass #2: Develop current countries to fill remaining demand from the pass 1
    pass2_use <- pass1 %>% 
      # Reduce to undeveloped cells
      filter(developed=="no") %>% 
      # Add remaining production available per period
      left_join(pass1_leftover %>% select(period, prod_mt_yr_fill), by="period") %>% 
      # Rank by profitability within EEZ
      group_by(period, eez_name) %>% 
      arrange(period, eez_name, desc(profits_usd_yr)) %>% 
      mutate(rank_in_eez=1:n()) %>% 
      ungroup() %>% 
      # Arrange within TERRITORY-LIKE UNIT (USA (Alaska/Hawaii/Continental), Guam, Puerto Rico all seperate) - basically distribute to current producers equally
      group_by(period) %>% 
      arrange(period, rank_in_eez) %>% 
      mutate(cum_prod_mt_yr=cumsum(prod_mt_yr),
             developed=ifelse(cum_prod_mt_yr <= prod_mt_yr_fill, "yes", "no")) %>% 
      ungroup() %>% 
      # Only developed cells
      filter(developed=="yes")
    
    # Merge the two passes
    data1 <- bind_rows(pass1_use, pass2_use)

  }
  
  # Proportional development
  if(dev_scenario_do=="Proportional"){
    
    # 2050 population
    pop55 <- pop %>% 
      filter(year==2100)
    
  # ISO key for production forecasts
   props <- data_orig %>% 
      select(sov1_iso, sov1_name, ter1_iso, ter1_name) %>% 
      unique() %>% 
      arrange(ter1_iso) %>% 
      # Add 2055 population estimates
      left_join(pop55 %>% select(iso3, pop_size_50perc), by=c("ter1_iso"="iso3")) %>% 
      # Only consider areas with population projection estimates
      filter(!is.na(pop_size_50perc)) %>% 
      # Calculate proportional allocation
      mutate(prod_prop=pop_size_50perc/sum(pop_size_50perc)) %>% 
      # Simplify
      select(ter1_iso, prod_prop) %>% 
     # Rename ISO column so that I can use the same code as in the CURRENT scenario
     rename(iso3_use=ter1_iso)
  
   # Pass #1. Develop profitable cells first, in countries with production
   pass1 <- data_orig %>% 
     # Add percent of production allocated to TERRITORY-LIKE UNIT (USA (Alaska/Hawaii/Continental), Guam, Puerto Rico all seperate)
     left_join(select(props, iso3_use, prod_prop), by=c("ter1_iso"="iso3_use")) %>% 
     # Calculate production allocation
     mutate(prod_prop=ifelse(is.na(prod_prop), 0, prod_prop),
            prod_allocated_mt_yr=prod_mt_cap*prod_prop) %>% 
     # Remove places not allocated any production
     filter(prod_prop>0) %>% 
     # Rank by profitability within EEZ
     group_by(period, eez_name) %>% 
     arrange(period, eez_name, desc(profits_usd_yr)) %>% 
     mutate(rank_in_eez=1:n()) %>% 
     ungroup() %>% 
     # Arrange within TERRITORY-LIKE UNIT (USA (Alaska/Hawaii/Continental), Guam, Puerto Rico all seperate)
     group_by(period, ter1_name) %>% 
     arrange(period, ter1_name, rank_in_eez, desc(profits_usd_yr)) %>% 
     # Calculate cumulative production and see if it exceeds ALLOCATED AMOUNT
     mutate(cum_prod_mt_yr=cumsum(prod_mt_yr),
            developed=ifelse(cum_prod_mt_yr <= prod_allocated_mt_yr, "yes", "no")) %>% 
     ungroup()
   
   # Cells developed in pass one 
   pass1_use <- pass1 %>% 
     filter(developed=="yes")
   
   # Undeveloped production from pass 1
   pass1_leftover <- pass1_use %>% 
     group_by(period) %>% 
     summarize(prod_mt_yr=sum(prod_mt_yr)) %>% 
     ungroup() %>% 
     mutate(prod_mt_yr_fill=prod_mt_cap-prod_mt_yr)
   
   # Pass #2: Develop current countries to fill remaining demand from the pass 1
   pass2_use <- pass1 %>% 
     # Reduce to undeveloped cells
     filter(developed=="no") %>% 
     # Add remaining production available per period
     left_join(pass1_leftover %>% select(period, prod_mt_yr_fill), by="period") %>% 
     # Rank by profitability within EEZ
     group_by(period, eez_name) %>% 
     arrange(period, eez_name, desc(profits_usd_yr)) %>% 
     mutate(rank_in_eez=1:n()) %>% 
     ungroup() %>% 
     # Arrange within TERRITORY-LIKE UNIT (USA (Alaska/Hawaii/Continental), Guam, Puerto Rico all seperate) - basically distribute to current producers equally
     group_by(period) %>% 
     arrange(period, rank_in_eez) %>% 
     mutate(cum_prod_mt_yr=cumsum(prod_mt_yr),
            developed=ifelse(cum_prod_mt_yr <= prod_mt_yr_fill, "yes", "no")) %>% 
     ungroup() %>% 
     # Only developed cells
     filter(developed=="yes")
   
   # Merge the two passes
   data1 <- bind_rows(pass1_use, pass2_use)
    
  }
  
  # Need-based development
  if(dev_scenario_do=="Need-based"){
    
    # Build proportions
    props <- wc %>% 
      # Reduce to climate scenario
      filter(rcp==rcp_do & scenario=="Full Adaptation") %>% 
      # Calculate amount of seafood needed to break even
      group_by(country, iso3) %>% 
      summarize(meat_mt_need = meat_kg_person[year==2012]/1000*npeople[year==2100], 
                meat_mt_avail = meat_mt[year==2100],
                meat_mt_miss = meat_mt_need - meat_mt_avail) %>% 
      ungroup() %>% 
      # Remove countries gaining seafood/capita
      filter(meat_mt_miss>0 * !is.na(meat_mt_miss)) %>% 
      # Remove countries without aquaculture potential
      filter(iso3 %in% unique(data_orig$ter1_iso)) %>% 
      # Calculate proportion of feed dedicated to this TERRITORY
      mutate(prod_prop=meat_mt_miss/sum(meat_mt_miss),
             prod_prop=1/n()) %>% 
      # Rename ISO column so that I can use the same code as in the CURRENT scenario
      rename(iso3_use=iso3)
    
    # Pass #1. Develop profitable cells first, in countries with production
    pass1 <- data_orig %>% 
      # Add percent of production allocated to TERRITORY-LIKE UNIT (USA (Alaska/Hawaii/Continental), Guam, Puerto Rico all seperate)
      left_join(select(props, iso3_use, prod_prop), by=c("ter1_iso"="iso3_use")) %>% 
      # Calculate production allocation
      mutate(prod_prop=ifelse(is.na(prod_prop), 0, prod_prop),
             prod_allocated_mt_yr=prod_mt_cap*prod_prop) %>% 
      # Remove places not allocated any production
      filter(prod_prop>0) %>% 
      # Rank by profitability within EEZ
      group_by(period, eez_name) %>% 
      arrange(period, eez_name, desc(profits_usd_yr)) %>% 
      mutate(rank_in_eez=1:n()) %>% 
      ungroup() %>% 
      # Arrange within TERRITORY-LIKE UNIT (USA (Alaska/Hawaii/Continental), Guam, Puerto Rico all seperate)
      group_by(period, ter1_name) %>% 
      arrange(period, ter1_name, rank_in_eez, desc(profits_usd_yr)) %>% 
      # Calculate cumulative production and see if it exceeds ALLOCATED AMOUNT
      mutate(cum_prod_mt_yr=cumsum(prod_mt_yr),
             developed=ifelse(cum_prod_mt_yr <= prod_allocated_mt_yr, "yes", "no")) %>% 
      ungroup()
    
    # Cells developed in pass one 
    pass1_use <- pass1 %>% 
      filter(developed=="yes")
    
    # Undeveloped production from pass 1
    pass1_leftover <- pass1_use %>% 
      group_by(period) %>% 
      summarize(prod_mt_yr=sum(prod_mt_yr)) %>% 
      ungroup() %>% 
      mutate(prod_mt_yr_fill=prod_mt_cap-prod_mt_yr)
    
    # Pass #2: Develop current countries to fill remaining demand from the pass 1
    pass2_use <- pass1 %>% 
      # Reduce to undeveloped cells
      filter(developed=="no") %>% 
      # Add remaining production available per period
      left_join(pass1_leftover %>% select(period, prod_mt_yr_fill), by="period") %>% 
      # Rank by profitability within EEZ
      group_by(period, eez_name) %>% 
      arrange(period, eez_name, desc(profits_usd_yr)) %>% 
      mutate(rank_in_eez=1:n()) %>% 
      ungroup() %>% 
      # Arrange within TERRITORY-LIKE UNIT (USA (Alaska/Hawaii/Continental), Guam, Puerto Rico all seperate) - basically distribute to current producers equally
      group_by(period) %>% 
      arrange(period, rank_in_eez) %>% 
      mutate(cum_prod_mt_yr=cumsum(prod_mt_yr),
             developed=ifelse(cum_prod_mt_yr <= prod_mt_yr_fill, "yes", "no")) %>% 
      ungroup() %>% 
      # Only developed cells
      filter(developed=="yes")
    
    # Merge the two passes
    data1 <- bind_rows(pass1_use, pass2_use)
      
    
    
  }
  
  # Perform final formatting
  ###############################################################

  # Format data
  data2 <- data1 %>% 
    # Add columns
    mutate(rcp=rcp_do, 
           dev_scenario=dev_scenario_do) %>% 
    # Arrange columns
    # BIG NOTE: you get rid of a lot of columns here
    select(rcp,  dev_scenario, period, 
           sov1_iso, sov1_name, ter1_continent, ter1_iso, ter1_name, x, y, 
           species,
           prod_mt_yr:profits_usd_yr)
    
  # Plot results
  ###############################################################
  
  # Calculate statistics
  stats <- data2 %>% 
    group_by(period, ter1_continent) %>% 
    summarize(prod_mmt_yr=sum(prod_mt_yr)/1e6)
  
  # Plot production statistics
  scen_title <- paste(rcp_do, dev_scenario_do, sep=", ")
  g <- ggplot(stats, aes(x=period, y=prod_mmt_yr, fill=ter1_continent)) +
    geom_bar(stat="identity") +
    labs(x="", y="Production (millions mt)", title=scen_title) +
    geom_hline(yintercept=prod_mt_cap/1e6, linetype="dashed") +
    theme_bw()
  print(g)
  
  # Export data
  return(data2)
  
}


# Develop demand-limited bivalve mariculture
################################################################################

# Costs scalars
cost_scalars <- c(2.1, 2.3, 2.5, 2.7)

# Build scenario key
rcps <- paste("RCP", c("2.6", "4.5", "6.0", "8.5"))
dev_scens <- c("Current", "Proportional", "Need-based")
scen_key <- expand.grid(rcp=rcps,
                        dev_scenario=dev_scens) %>% 
  arrange(rcp, dev_scenario)


# Loop through cost scalars
for(i in 1:length(cost_scalars)){
  
  # Suffix
  suffix <- paste0("cost_search_", cost_scalars[i])
  
  # Develop mariculture
  output <- purrr::map_df(1:nrow(scen_key), function(x) {
    
    # Scenario
    rcp_do <- scen_key$rcp[x]
    dev_do <- scen_key$dev_scenario[x]
    
    # Expand mariculture
    out_df <- expand_mariculture(rcp=rcp_do, dev_scenario=dev_do, suffix=suffix)
    
  })
  
  # Export output
  outfile <- paste0("bivalve_output_", suffix, ".Rds")
  saveRDS(output, file=file.path(outputdir, outfile))
  
}
  


