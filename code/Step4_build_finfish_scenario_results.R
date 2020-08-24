
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
plotdir <- "figures"
tabledir <- "tables"

# Read species data
load("data/species/data/aquaculture_species_key.Rdata")
spp_key <- data
rm(data_full, data)

# Read feed info
fifos <- read.csv("tables/TableS2_fifo_fcr_fmfo_stats.csv", as.is=T)
ffdata <- read.csv("data/feed_params/processed/forage_fish_availability.csv", as.is=T)
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


# Function to develop feed-constrained finfish mariculture
################################################################################

# For testing
rcp <- "RCP 2.6"
mgmt_scenario <- "Reformed fisheries management"
feed_scenario <- "Reformed feed use"
dev_scenario <- "Need-based"

# Function to develop feed-constrained finfish mariculture
expand_mariculture <- function(rcp, mgmt_scenario, feed_scenario, dev_scenario){
  
  # Steps
  # 1. Determine feed availability
  # 2. Determine feed demand
  # 3. Develop based on distribution pattern

  # Setup
  ###############################################################
  
  # Parameters
  rcp_do <- rcp
  mgmt_scenario_do <- mgmt_scenario
  feed_scenario_do <- feed_scenario
  dev_scenario_do <- dev_scenario
  
  # Demand limit
  prod_mt_cap <- 103 * 0.27 / 0.87 * 1e6

  # Read finfish forecasts
  rcp_do_short <- gsub("\\.| ", "", rcp_do)
  infile <- paste0( rcp_do_short, "_Finfish_rational_use.Rds")
  data_orig <- readRDS(file.path(datadir, infile))
  
  # Build forecasts period key
  periods <- data_orig %>% pull(period) %>% unique()
  period_key <- purrr::map_df(periods, function(x){
    yr1 <- substr(x,1,4) %>% as.numeric()
    yr2 <- substr(x,6,9) %>% as.numeric()
    yrs <- yr1:yr2
    yr_df <- tibble(period=x,
                    year=yrs)
  })
  
  # Determine feed availability
  ###############################################################
  
  # Determine forage fish availability given RCP, fisheries management, and feed use scenario
  ff_avail <- ffdata %>% 
    # Reduce to RCP and fisheries management scenario
    filter(rcp==rcp_do & mgmt_scenario==mgmt_scenario_do & feed_scenario==feed_scenario_do) %>% 
    # Add forecast period
    left_join(period_key, by="year") %>% 
    # Simplify and rename columns
    select(rcp, period, year, catch_ff_mt_maq) %>% 
    rename(ff_avail_mt_yr=catch_ff_mt_maq) %>% 
    # Remove years not in period
    filter(!is.na(period)) %>% 
    # Calculate mean over forecast periods 
    group_by(rcp, period) %>% 
    summarize(ff_avail_mt_yr=mean(ff_avail_mt_yr)) %>% 
    ungroup()

  # Determine feed demands
  ###############################################################
  
  # Determine which FIFO ratios to use based on feed scenario
  fifos_use <- fifos
  if(feed_scenario_do=="Reformed feed use"){
    fifos_use$fifo_use <- fifos_use$fifo2050
  }else{
    fifos_use$fifo_use <- fifos_use$fifo2030
  }
  
  # Determine feed demands of each profitable cell (based on feed use scenario)
  data1 <- data_orig %>% 
    # Add feed group and FIFO ratios
    left_join(select(spp_key, species, feed_group), by="species") %>% 
    left_join(select(fifos_use, group, fifo_use), by=c("feed_group"="group")) %>% 
    # Add forage fish availability
    left_join(ff_avail, by=c("period")) %>% 
    # Calculate forage fish demand
    mutate(ff_demand_mt_yr=prod_mt_yr*fifo_use) %>% 
    # Make sure Alaska and Hawaii have US designation
    mutate(ter1_name_use=recode(ter1_name, 
                                "Hawaii"="United States", 
                                "Alaska"="United States", 
                                "Micronesia"="Federated States of Micronesia", 
                                "Comores"="Comoros"),
           ter1_iso_use=countrycode(ter1_name_use, "country.name", "iso3c")) %>% 
    # Use the sovereign nation as the jurisdictional unit for the remaining territories without ISOs
    mutate(ter1_name_use=ifelse(is.na(ter1_iso_use), sov1_name, ter1_name_use)) %>% 
    # Run ISO3 lookup one last time and now it should be perfect
    mutate(ter1_iso_use=countrycode(ter1_name_use, "country.name", "iso3c"))
    
  
  # Develop mariculture based on development scenario
  ###############################################################
  
  # Rational development
  if(dev_scenario_do=="Rational"){
    
    # Develop profitable cells first
    data2 <- data1 %>% 
      group_by(period) %>% 
      arrange(period, desc(profits_usd_yr)) %>% 
      # Calculate cumulative forage fish
      mutate(cum_ff_demand_mt_yr=cumsum(ff_demand_mt_yr),
             cum_prod_mt_yr=cumsum(prod_mt_yr),
             developed=ifelse(cum_ff_demand_mt_yr <= ff_avail_mt_yr & cum_prod_mt_yr <= prod_mt_cap, "yes", "no")) %>% 
      filter(developed=="yes")
    
  }
  
  # Current development
  if(dev_scenario_do=="Current"){
    
    # Format current producers
    # Based on FAO 2013-2017 averages
    props <- maq_prod %>% 
      # Reduce to finfish aquaculture totals
      filter(major_group=="Pisces") %>% 
      # Format country information
      mutate(country_use=countrycode(country, "country.name", "country.name"),
             iso3_use=countrycode(country_use, "country.name", "iso3c")) %>% 
      # Remove TERRITORIES not forecast to have to viable mariculutre
      filter(iso3_use %in% unique(data1$ter1_iso_use)) %>% 
      # Calculate proportion of feed dedicated to this TERRITORY
      mutate(feed_prop=prod_mt/sum(prod_mt)) # note if a country isn't present in a period some feed will go unused
    
    # Develop profitable cells first, in countries with production
    data2 <- data1 %>% 
      # Add percent of forage fish allocated to TERRITORY-LIKE UNIT (USA (Alaska/Hawaii/Continental), Guam, Puerto Rico all seperate)
      left_join(select(props, iso3_use, feed_prop), by=c("ter1_iso_use"="iso3_use")) %>% 
      # Calculate forage fish allocation
      mutate(feed_prop=ifelse(is.na(feed_prop), 0, feed_prop),
             feed_allocated_mt_yr=ff_avail_mt_yr*feed_prop) %>% 
      # Remove places not allocated any forage fish
      filter(feed_prop>0) %>% 
      # Rank by profitability within EEZ
      group_by(period, eez_name) %>% 
      arrange(period, eez_name, desc(profits_usd_yr)) %>% 
      mutate(rank_in_eez=1:n()) %>% 
      ungroup() %>% 
      # Arrange within TERRITORY-LIKE UNIT (USA (Alaska/Hawaii/Continental), Guam, Puerto Rico all seperate)
      group_by(period, ter1_name_use) %>% 
      arrange(period, ter1_name_use, rank_in_eez, desc(profits_usd_yr)) %>% 
      # Calculate cumulative forage fish and see if it exceeds ALLOCATED AMOUNT
      mutate(cum_ff_demand_mt_yr=cumsum(ff_demand_mt_yr),
             developed=ifelse(cum_ff_demand_mt_yr<=feed_allocated_mt_yr, "yes", "no")) %>% 
      filter(developed=="yes")
    
    # If feed isn't limiting (i.e., demand is exceeded), distribute as proportion of total global demand
    # NOTE: THIS CODE IS IMPERFECT. IT WILL FAIL IF ONE PERIOD IS BELOW AND ANOTHER IS ABOVE. _ WROT E A STOP TO FLAG IF HAPPENS
    prod_my_yr_period <- data2 %>% 
      group_by(period) %>% 
      summarize(prod_mt_yr=sum(prod_mt_yr))
    cap_exceeded_yn <- sum(prod_my_yr_period$prod_mt_yr > prod_mt_cap)
    if(cap_exceeded_yn%in%c(1,2)){stop("WRONG")}
    if(cap_exceeded_yn==3){
      
      # Pass #1 
      pass1 <- data1 %>% 
        # Add percent of forage fish allocated to TERRITORY-LIKE UNIT (USA (Alaska/Hawaii/Continental), Guam, Puerto Rico all seperate)
        left_join(select(props, iso3_use, feed_prop), by=c("ter1_iso_use"="iso3_use")) %>% 
        # Calculate production allocation
        rename(prod_prop=feed_prop) %>% 
        mutate(prod_prop=ifelse(is.na(prod_prop), 0, prod_prop),
               prod_allocated_mt_yr=prod_mt_cap*prod_prop) %>% 
        # Remove places not allocated any forage fish
        filter(prod_prop>0) %>% 
        # Rank by profitability within EEZ
        group_by(period, eez_name) %>% 
        arrange(period, eez_name, desc(profits_usd_yr)) %>% 
        mutate(rank_in_eez=1:n()) %>% 
        ungroup() %>% 
        # Arrange within TERRITORY-LIKE UNIT (USA (Alaska/Hawaii/Continental), Guam, Puerto Rico all seperate)
        group_by(period, ter1_name_use) %>% 
        arrange(period, ter1_name_use, rank_in_eez, desc(profits_usd_yr)) %>% 
        # Calculate cumulative forage fish and see if it exceeds ALLOCATED AMOUNT
        mutate(cum_prod_mt_yr=cumsum(prod_mt_yr),
               developed=ifelse(cum_prod_mt_yr <= prod_allocated_mt_yr, "yes", "no"))
      
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
      data2 <- bind_rows(pass1_use, pass2_use)
      
      
    }
    
  }
  
  # Proportional development
  if(dev_scenario_do=="Proportional"){
    
    # 2050 population
    pop55 <- pop %>% 
      filter(year==2100)
    
    # ISO key for production forecasts
   props <- data1 %>% 
      select(sov1_iso, sov1_name, ter1_iso_use, ter1_name_use) %>% 
      unique() %>% 
      arrange(ter1_iso_use) %>% 
      # Add 2055 population estimates
      left_join(pop55 %>% select(iso3, pop_size_50perc), by=c("ter1_iso_use"="iso3")) %>% 
      # Only consider areas with population projection estimates
      filter(!is.na(pop_size_50perc)) %>% 
      # Calculate proportional allocation
      mutate(feed_prop=pop_size_50perc/sum(pop_size_50perc)) %>% 
      # Simplify
      select(ter1_iso_use, feed_prop)
  
    # Plot allocations  
    #hist(props$feed_prop, breaks=seq(0, 0.25, 0.005))
    #abline(v=1/nrow(props))
    
    # Develop profitable cells first, in countries with production
    data2 <- data1 %>% 
      # Add percent of forage fish allocated to TERRITORY-LIKE UNIT (USA (Alaska/Hawaii/Continental), Guam, Puerto Rico all seperate)
      left_join(props, by=c("ter1_iso_use")) %>% 
      # Calculate forage fish allocation
      mutate(feed_prop=ifelse(is.na(feed_prop), 0, feed_prop),
             feed_allocated_mt_yr=ff_avail_mt_yr*feed_prop) %>% 
      # Remove places not allocated any forage fish
      filter(feed_prop>0) %>% 
      # Rank by profitability within EEZ
      group_by(period, eez_name) %>% 
      arrange(period, eez_name, desc(profits_usd_yr)) %>% 
      mutate(rank_in_eez=1:n()) %>% 
      ungroup() %>% 
      # Arrange within TERRITORY-LIKE UNIT (USA (Alaska/Hawaii/Continental), Guam, Puerto Rico all seperate)
      group_by(period, ter1_name_use) %>% 
      arrange(period, ter1_name_use, rank_in_eez, desc(profits_usd_yr)) %>% 
      # Calculate cumulative forage fish and see if it exceeds ALLOCATED AMOUNT
      mutate(cum_ff_demand_mt_yr=cumsum(ff_demand_mt_yr),
             developed=ifelse(cum_ff_demand_mt_yr<=feed_allocated_mt_yr, "yes", "no")) %>% 
      filter(developed=="yes")
    
    # If feed isn't limiting (i.e., demand is exceeded), distribute as proportion of total global demand
    # NOTE: THIS CODE IS IMPERFECT. IT WILL FAIL IF ONE PERIOD IS BELOW AND ANOTHER IS ABOVE. _ WROT E A STOP TO FLAG IF HAPPENS
    prod_my_yr_period <- data2 %>% 
      group_by(period) %>% 
      summarize(prod_mt_yr=sum(prod_mt_yr))
    cap_exceeded_yn <- sum(prod_my_yr_period$prod_mt_yr > prod_mt_cap)
    if(cap_exceeded_yn%in%c(1,2)){stop("WRONG")}
    if(cap_exceeded_yn==3){
      
      # Pass #1 
      pass1 <- data1 %>% 
        # Add percent of forage fish allocated to TERRITORY-LIKE UNIT (USA (Alaska/Hawaii/Continental), Guam, Puerto Rico all seperate)
        left_join(props, by="ter1_iso_use") %>% 
        # Calculate production allocation
        rename(prod_prop=feed_prop) %>% 
        mutate(prod_prop=ifelse(is.na(prod_prop), 0, prod_prop),
               prod_allocated_mt_yr=prod_mt_cap*prod_prop) %>% 
        # Remove places not allocated any forage fish
        filter(prod_prop>0) %>% 
        # Rank by profitability within EEZ
        group_by(period, eez_name) %>% 
        arrange(period, eez_name, desc(profits_usd_yr)) %>% 
        mutate(rank_in_eez=1:n()) %>% 
        ungroup() %>% 
        # Arrange within TERRITORY-LIKE UNIT (USA (Alaska/Hawaii/Continental), Guam, Puerto Rico all seperate)
        group_by(period, ter1_name_use) %>% 
        arrange(period, ter1_name_use, rank_in_eez, desc(profits_usd_yr)) %>% 
        # Calculate cumulative forage fish and see if it exceeds ALLOCATED AMOUNT
        mutate(cum_prod_mt_yr=cumsum(prod_mt_yr),
               developed=ifelse(cum_prod_mt_yr <= prod_allocated_mt_yr, "yes", "no"))
      
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
      data2 <- bind_rows(pass1_use, pass2_use)
      
      
    }
    
  }
  
  # Need-based development: ATTEMPT 1
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
      mutate(feed_prop=meat_mt_miss/sum(meat_mt_miss),
             feed_prop=1/n()) 
    
    # Develop profitable cells first, in countries with production
    data2 <- data1 %>% 
      # Add percent of forage fish allocated to TERRITORY-LIKE UNIT (USA (Alaska/Hawaii/Continental), Guam, Puerto Rico all seperate)
      left_join(select(props, iso3, feed_prop), by=c("ter1_iso_use"="iso3")) %>% 
      # Calculate forage fish allocation
      mutate(feed_prop=ifelse(is.na(feed_prop), 0, feed_prop),
             feed_allocated_mt_yr=ff_avail_mt_yr*feed_prop) %>% 
      # Remove places not allocated any forage fish
      filter(feed_prop>0) %>% 
      # Rank by profitability within EEZ
      group_by(period, eez_name) %>% 
      arrange(period, eez_name, desc(profits_usd_yr)) %>% 
      mutate(rank_in_eez=1:n()) %>% 
      ungroup() %>% 
      # Arrange within TERRITORY-LIKE UNIT (USA (Alaska/Hawaii/Continental), Guam, Puerto Rico all seperate)
      group_by(period, ter1_name_use) %>% 
      arrange(period, ter1_name_use, rank_in_eez, desc(profits_usd_yr)) %>% 
      # Calculate cumulative forage fish and see if it exceeds ALLOCATED AMOUNT
      mutate(cum_ff_demand_mt_yr=cumsum(ff_demand_mt_yr),
             developed=ifelse(cum_ff_demand_mt_yr<=feed_allocated_mt_yr, "yes", "no")) %>% 
      filter(developed=="yes")
    
    # If feed isn't limiting (i.e., demand is exceeded), distribute as proportion of total global demand
    # NOTE: THIS CODE IS IMPERFECT. IT WILL FAIL IF ONE PERIOD IS BELOW AND ANOTHER IS ABOVE. _ WROT E A STOP TO FLAG IF HAPPENS
    prod_my_yr_period <- data2 %>% 
      group_by(period) %>% 
      summarize(prod_mt_yr=sum(prod_mt_yr))
    cap_exceeded_yn <- sum(prod_my_yr_period$prod_mt_yr > prod_mt_cap)
    if(cap_exceeded_yn%in%c(1,2)){stop("WRONG")}
    if(cap_exceeded_yn==3){
      
      # Pass #1 
      pass1 <- data1 %>% 
        # Add percent of forage fish allocated to TERRITORY-LIKE UNIT (USA (Alaska/Hawaii/Continental), Guam, Puerto Rico all seperate)
        left_join(select(props, iso3, feed_prop), by=c("ter1_iso_use"="iso3")) %>% 
        # Calculate production allocation
        rename(prod_prop=feed_prop) %>% 
        mutate(prod_prop=ifelse(is.na(prod_prop), 0, prod_prop),
               prod_allocated_mt_yr=prod_mt_cap*prod_prop) %>% 
        # Remove places not allocated any forage fish
        filter(prod_prop>0) %>% 
        # Rank by profitability within EEZ
        group_by(period, eez_name) %>% 
        arrange(period, eez_name, desc(profits_usd_yr)) %>% 
        mutate(rank_in_eez=1:n()) %>% 
        ungroup() %>% 
        # Arrange within TERRITORY-LIKE UNIT (USA (Alaska/Hawaii/Continental), Guam, Puerto Rico all seperate)
        group_by(period, ter1_name_use) %>% 
        arrange(period, ter1_name_use, rank_in_eez, desc(profits_usd_yr)) %>% 
        # Calculate cumulative forage fish and see if it exceeds ALLOCATED AMOUNT
        mutate(cum_prod_mt_yr=cumsum(prod_mt_yr),
               developed=ifelse(cum_prod_mt_yr <= prod_allocated_mt_yr, "yes", "no"))
      
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
      data2 <- bind_rows(pass1_use, pass2_use)
      
      
    }
  
    
  }
  
  # Perform final formatting
  ###############################################################

  # Format data
  data3 <- data2 %>% 
    # Add columns
    mutate(mgmt_scenario=mgmt_scenario, 
           feed_scenario=feed_scenario, 
           dev_scenario=dev_scenario) %>% 
    # Arrange columns
    # BIG NOTE: you get rid of a lot of columns here
    select(rcp, mgmt_scenario, feed_scenario, dev_scenario, period, 
           sov1_iso, sov1_name, ter1_continent, ter1_iso_use, ter1_name_use, x, y, 
           species, feed_group, fifo_use, 
           prod_mt_yr:profits_usd_yr, ff_demand_mt_yr) %>% 
    rename(ter1_iso=ter1_iso_use, ter1_name=ter1_name_use)
    
  # Plot results
  ###############################################################
  
  # Calculate statistics
  stats <- data3 %>% 
    group_by(period, ter1_continent) %>% 
    summarize(ff_mmt_yr=sum(ff_demand_mt_yr)/1e6,
              prod_mmt_yr=sum(prod_mt_yr)/1e6)
  
  # Plot production statistics
  scen_title <- paste(rcp_do, mgmt_scenario_do, feed_scenario_do, dev_scenario_do, sep=", ")
  g <- ggplot(stats, aes(x=period, y=prod_mmt_yr, fill=ter1_continent)) +
    geom_bar(stat="identity") +
    labs(x="", y="Production (millions mt)", title=scen_title) +
    geom_hline(yintercept=prod_mt_cap/1e6) +
    theme_bw()
  print(g)
  
  # Export data
  return(data3)
  
}

# Develop demand-limited finfish mariculture
################################################################################

# A few test scenarios
#####################################

# Reform tests
out1 <- expand_mariculture(rcp="RCP 2.6", mgmt_scenario = "Reformed fisheries management", feed_scenario = "Reformed feed use", dev_scenario="Rational")
out2 <- expand_mariculture(rcp="RCP 2.6", mgmt_scenario = "Reformed fisheries management", feed_scenario = "Reformed feed use", dev_scenario="Current")
out3 <- expand_mariculture(rcp="RCP 2.6", mgmt_scenario = "Reformed fisheries management", feed_scenario = "Reformed feed use", dev_scenario="Proportional")
out3 <- expand_mariculture(rcp="RCP 2.6", mgmt_scenario = "Reformed fisheries management", feed_scenario = "Reformed feed use", dev_scenario="Need-based")

# BAU tests
out1 <- expand_mariculture(rcp="RCP 2.6", mgmt_scenario = "BAU fisheries management", feed_scenario = "BAU feed use", dev_scenario="Rational")
out1 <- expand_mariculture(rcp="RCP 2.6", mgmt_scenario = "BAU fisheries management", feed_scenario = "BAU feed use", dev_scenario="Current")
out1 <- expand_mariculture(rcp="RCP 2.6", mgmt_scenario = "BAU fisheries management", feed_scenario = "BAU feed use", dev_scenario="Proportional")
out1 <- expand_mariculture(rcp="RCP 2.6", mgmt_scenario = "BAU fisheries management", feed_scenario = "BAU feed use", dev_scenario="Need-based")


# Do all scenarios
#####################################

# Build scenario key
rcps <- paste("RCP", c("2.6", "4.5", "6.0", "8.5"))
mgmt_scens <- c("BAU fisheries management", "Reformed fisheries management")
feed_scens <- c("BAU feed use", "Reformed feed use")
dev_scens <- c("Current", "Proportional", "Need-based")
scen_key <- expand.grid(rcp=rcps,
                        mgmt_scenario=mgmt_scens,
                        feed_scenario=feed_scens,
                        dev_scenario=dev_scens) %>% 
  arrange(rcp, mgmt_scenario, feed_scenario, dev_scenario)

# Develop mariculture
output <- purrr::map_df(1:nrow(scen_key), function(x) {
  
  # Scenario
  rcp_do <- scen_key$rcp[x]
  mgmt_do <- scen_key$mgmt_scenario[x]
  feed_do <- scen_key$feed_scenario[x]
  dev_do <- scen_key$dev_scenario[x]
  
  # Expand mariculture
  out_df <- expand_mariculture(rcp=rcp_do, mgmt_scenario = mgmt_do, feed_scenario = feed_do, dev_scenario=dev_do)
  
})

# Export output
saveRDS(output, file=file.path(datadir, "finfish_output.Rds"))

