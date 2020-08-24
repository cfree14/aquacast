
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

# Read FIFO ratios
fifos <- read.csv("tables/TableS2_fifo_fcr_fmfo_stats.csv", as.is=T)

# Read current mariculture production
maq_prod <- read.csv("data/feed_params/processed/FAO_2013_2017_maq_prod_averages_by_country.csv", as.is=T)

# Read data 
data26 <- readRDS(file.path(datadir, "RCP26_Finfish_rational_use.Rds"))
# data45 <- readRDS(file.path(datadir, "RCP45_Finfish_rational_use.Rds"))
# data60 <- readRDS(file.path(datadir, "RCP60_Finfish_rational_use.Rds"))
# data85 <- readRDS(file.path(datadir, "RCP85_Finfish_rational_use.Rds"))

# Read EEZ
eezdir <- "data/eezs"
eezs <- readRDS(file.path(eezdir, "eezs_v10_polygons.Rds"))
eez_key <- read.csv(file.path(eezdir, "eezs_v10_key.csv"), as.is=T)
eezs_remote <- read.csv(file.path(eezdir, "eezs_v10_uninhabited.csv"), as.is=T) %>% 
  mutate(eez_name=recode(eez_name, "Colombian Exclusive Economic Zone (Quitasue\x96o)"="Colombian Exclusive Economic Zone (Quitasueño)"))

# World
world <- rnaturalearth::ne_countries(scale="large", type = "countries", returnclass = "sf")

# EEZ formatting
################################################################################




# EEZ formatting
################################################################################

# Mark EEZs use
eez_key <- eez_key %>% 
  mutate(use=ifelse(eez_type=="200NM", "yes", "no")) %>% 
  left_join(eezs_remote, by="eez_name") %>% 
  mutate(use=ifelse(!is.na(reason_to_exclude), "no", use))

# EEZs to omit
eezs_omit <- sort(eez_key$eez_name[eez_key$use=="no"])


# Data formatting
################################################################################

# Merge data
data <- rbind(data26 %>% mutate(rcp="RCP 2.6"),
              data45 %>% mutate(rcp="RCP 4.5"),
              data60 %>% mutate(rcp="RCP 6.0"),
              data85 %>% mutate(rcp="RCP 8.5"))

# Correct ISO3 codes
data_use <- data %>% 
  # Remove EEZs that shouldn't be developed
  filter(!eez_name %in% eezs_omit) %>% 
  # Fix sovereign nation ISO3 codes
  mutate(sov1_name=recode(sov1_name, 
                          "Micronesia"="Federated States of Micronesia", 
                          "Comores"="Comoros"),
         sov1_iso3_use = countrycode(sov1_name, "country.name", "iso3c")) %>% 
  # Make sure Alaska and Hawaii have US designation
  mutate(ter1_name_use=recode(ter1_name, 
                              "Hawaii"="United States", 
                              "Alaska"="United States", 
                              "Micronesia"="Federated States of Micronesia", 
                              "Comores"="Comoros"),
         ter1_iso3_use=countrycode(ter1_name_use, "country.name", "iso3c")) %>% 
  # Use the sovereign nation as the jurisdictional unit for the remaining territories without ISOs
  mutate(ter1_name_use=ifelse(is.na(ter1_iso3_use), sov1_name, ter1_name_use)) %>% 
  # Run ISO3 lookup one last time and now it should be perfect
  mutate(ter1_iso3_use=countrycode(ter1_name_use, "country.name", "iso3c"))

# Subset data for testing
data_sample <- sample_frac(data, size=0.2)

# Identify proportion
props <- maq_prod %>% 
  filter(major_group=="Pisces") %>% 
  mutate(feed_prop=prod_mt/sum(prod_mt),
         iso3_use=countrycode(country, "country.name", "iso3c")) %>% 
  filter(!is.na(iso3_use))
sum(props$feed_prop)
hist(props$feed_prop)

# Approach 1: Impacts on territories (doesn't seem to work in BAU -- feed spread across too many?)
######################################################

# Read fisheries impacts
fdata <- readRDS("/Users/cfree/Dropbox/Chris/UCSB/projects/edf_climate/cc_trade/data/gaines/gaines_territory_level_results_approach1.Rds")

# Format projected fisheries impacts
fdata1 <- fdata %>%
  ungroup() %>% 
  # Remove High Seas areas
  filter(sovereign!="High Seas" & sovereign_iso3!="Disputed" & scenario=="Full Adaptation" & rcp=="RCP85" & country!="Antarctica") %>% 
  # Reclassify countries
  mutate(country_use=recode(country, 
                            "Hawaii"="United States", 
                            "Alaska"="United States", 
                            "Micronesia"="Federated States of Micronesia", 
                            "Comores"="Comoros"),
         country_iso3=countrycode(country_use, "country.name", "iso3c")) %>% 
  # Remove countries without ISOs
  filter(!is.na(country_iso3)) %>% 
  # Summarize by new COUNTRY jurisdiction
  group_by(country_use, country_iso3, rcp) %>% 
  summarize(msy_mt_2012=sum(msy_tot1, na.rm=T), 
            msy_mt_2100=sum(msy_tot2, na.rm=T),
            msy_pdiff=(msy_mt_2100-msy_mt_2012)/msy_mt_2012*100, 
            msy_pdiff_use=abs(pmin(0,msy_pdiff))) %>% 
  ungroup() %>% 
  mutate(feed_prop=msy_pdiff_use/sum(msy_pdiff_use, na.rm=T))

# Check prop
sum(fdata1$need_feed_prop)


# Approach 2: Impacts on countries (doesn't make conceptual sense -- PR/Guam get US impact, eg)
######################################################

# Read fisheries impacts
fdata_crap <- readRDS("/Users/cfree/Dropbox/Chris/UCSB/projects/edf_climate/cc_trade/data/gaines/gaines_country_level_results_approach1.Rds")

# Format projected fisheries impacts
fdata1_crap <- fdata_crap%>% 
  ungroup() %>% 
  filter(rcp=="RCP85" & scenario=="Full Adaptation") %>% 
  mutate(msy_diff=msy_tot2-msy_tot1,
         msy_diff_use=abs(pmin(0,msy_diff)),
         need_feed_prop=msy_diff_use/sum(msy_diff_use))

# Check prop
sum(fdata1_crap$need_feed_prop)


# Approach 3: Impacts on scaled catch with full adaptation
######################################################

# THIS IS STUPIDLY CIRCULAR
# I'M READING IN THE SCALED IMPACT OF CC WITH FULL ADAPTATION FROM THE X-SECTOR ANALSYSI
fdata3_orig <- readRDS(file="/Users/cfree/Dropbox/Chris/UCSB/projects/blue_papers/bluepaper2/data/fisheries/gaines_scaled_climate_impact.Rds")

# Format for feed props
fdata3 <- fdata3_orig %>% 
  # Look at RCP 8.5 impacts
  filter(rcp=="RCP 8.5") %>% 
  # Simplify dataframe
  select(sovereign:country_use_iso3, production_mt_diff_scaled) %>% 
  # Only use ISO3 in AQ dataset 
  filter(country_use_iso3 %in% unique(data_use$ter1_iso3_use)) %>% 
  mutate(msy_diff_use=abs(pmin(0, production_mt_diff_scaled)),
         feed_prop=msy_diff_use/sum(msy_diff_use))



# Plot/export function
################################################################################

# Function to plot and export results
# dataset <- data_scen4; feed_scen <- "reform"; dist_scen <- "need"
plot_export_results <- function(dataset, feed_scen, dist_scen){
  
  # Calculate stats
  data_results <- dataset %>% 
    # Summarize outcomes
    group_by(rcp, year, ter1_continent) %>% 
    summarize(ncells=n(),
              area_sqkm=ncells*100/1e3,
              prod_mt=sum(prod_mt_yr, na.rm=T)/1e6,
              profits_usd=sum(profits_usd_yr)/1e9) %>% 
    # Spread
    gather(key="metric", value="value", 4:ncol(.)) %>% 
    filter(metric!="ncells") %>% 
    # Rename columns
    mutate(metric=recode(metric, 
                         "area_sqkm"="Area developed\n(thousands of sqkm)",
                         "prod_mt"="Total production\n(millions of mt)", 
                         "profits_usd"="Total profits\n(billions of USD)")) %>% 
    # Convert year to factor
    ungroup() %>% 
    mutate(year=as.character(year))
  
  # Scenario titles
  title_key <- expand.grid(feed_scen=c("Business-as-usual", "Progressive reform"),
                           dist_scen=c("Rational development", "Current development",
                                       "Equal development", "Need-based development")) %>% 
    as_tibble() %>% 
    mutate(title=paste0(feed_scen, " (", tolower(dist_scen), ")"),
           feed_scen_short=recode(feed_scen, 
                                  "Business-as-usual"="base", 
                                  "Progressive reform"="reform"),
           dist_scen_short=recode(dist_scen, 
                                  "Rational development"="rational", 
                                  "Current development"="current",
                                  "Equal development"="equal",
                                  "Need-based development"="need")) 
  
  
  # Setup theme
  my_theme <- theme(axis.text=element_text(size=7),
                    plot.title=element_text(size=11),
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(), 
                    axis.line = element_line(colour = "black"),
                    strip.text = element_text(size=7),
                    legend.title = element_text(size=7),
                    legend.text = element_text(size=5),
                    axis.title=element_blank())
  
  # Plot continental outcomes
  title_use <- title_key$title[title_key$feed_scen_short==feed_scen & title_key$dist_scen_short==dist_scen]
  g <- ggplot(data_results, aes(x=year, y=value, fill=ter1_continent)) +
    geom_bar(stat="identity") +
    facet_wrap(rcp ~ metric, scales="free", ncol=3) +
    scale_fill_discrete(name="Continent") +
    labs(x="", y="", title=title_use) + 
    theme_bw() + my_theme
  g
  
  # Export figure
  outfile <- paste0("figure_results_finfish_cont_ts_", feed_scen, "_", dist_scen, ".png")
  ggsave(g, filename=file.path(plotdir, outfile), 
         width=6.5, height=8.5, units="in", dpi=600)
  
  # Export stats table
  feed_scen_long <- as.character(unique(title_key$feed_scen[title_key$feed_scen_short==feed_scen]))
  dev_pattern_long <- as.character(unique(title_key$dist_scen[title_key$dist_scen_short==dist_scen]))
  results_out <- dataset %>% 
    # Summarize outcomes
    group_by(rcp, year, 
             eez_code, eez_name, eez_type, 
             ter1_name, ter1_iso, ter1_continent, 
             sov1_name, sov1_iso, sov1_iso3_use, sov1_continent) %>% 
    summarize(ncells=n(),
              area_sqkm=ncells*100,
              prod_mt=sum(prod_mt_yr, na.rm=T),
              profits_usd=sum(profits_usd_yr)) %>% 
    # Add columns
    mutate(feed_scen=feed_scen_long, dev_pattern=dev_pattern_long) %>% 
    # Arrange
    select(feed_scen, dev_pattern, everything())
  return(results_out)
  
}


# Build data
################################################################################

# Feed scenarios
# 1. Base case
# 2. Progressive reform

# Development patterns
# a. Rational
# b. Current
# c. Equal
# d. Need-based

# Feed scenario to do: 
feed_scens <- c("reform", "base")

# Loop through feed scenarios
i <- 1
for(i in 1:length(feed_scens)){
  
  # Scenario
  feed_scen_do <- feed_scens[i]
  feed_scen_fullname <- ifelse(feed_scen_do=="reform", "Progressive reform", "Business-as-usual")
  
  # Format FF availability data
  ##################################
  
  # Forage fish availability in RCP, feed scenerio, and years of interest
  ffdata_use <- ffdata %>% 
    select(rcp, scenario, year, maq_ff_catch_mt) %>% 
    filter(scenario==feed_scen_fullname) %>% 
    select(rcp, year, maq_ff_catch_mt) %>% 
    rename(ff_avail_mt_yr=maq_ff_catch_mt)
  
  # Mark which FIFO to use
  # Clunky, but works fine
  fifos_use <- fifos
  if(feed_scen_do=="reform"){
    fifos_use$fifo_use <- fifos_use$fifo2050
  }else{
    fifos_use$fifo_use <- fifos_use$fifo2030
  }
  
  # Scenario 1. Rational
  ################################################################################
  
  # Build data
  ##################################
  
  # Build data
  data_scen1 <- data_use %>% 
    # Remove EEZs that shouldn't be developed
    filter(!eez_name %in% eezs_omit) %>% 
    # Add feed group and FIFO ratios
    left_join(select(spp_key, species, feed_group), by="species") %>% 
    left_join(select(fifos_use, group, fifo_use), by=c("feed_group"="group")) %>%
    # left_join(select(fifos, group, fifo, fifo1), by=c("feed_group"="group")) %>% # OLD CODE WHEN NOT ADAPTIVE TO FEED SCEN
    # Add forage fish availability
    mutate(year=as.numeric(year)) %>% 
    left_join(ffdata_use, by=c("rcp", "year")) %>% 
    # Calculate forage fish demand
    mutate(ff_demand_mt_yr=prod_mt_yr*fifo_use) %>% # OLD
    # Arrange by profitability
    group_by(rcp, year) %>% 
    arrange(rcp, year, desc(profits_usd_yr)) %>% 
    # Calculate cumulative forage fish
    mutate(cum_ff_demand_mt_yr=cumsum(ff_demand_mt_yr),
           developed=ifelse(cum_ff_demand_mt_yr<=ff_avail_mt_yr, "yes", "no")) %>% 
    filter(developed=="yes")
  
  # Plot and export data
  results_scen1 <- plot_export_results(dataset=data_scen1, feed_scen=feed_scen_do, dist_scen="rational")
  
  
  # Scenario 2. Current
  ################################################################################
  
  # Build data
  ##################################
  
  # Build data
  data_scen2 <- data_use %>% 
    # Add feed group and FIFO ratios
    left_join(select(spp_key, species, feed_group), by="species") %>% 
    left_join(select(fifos_use, group, fifo_use), by=c("feed_group"="group")) %>%
    # Add forage fish availability
    mutate(year=as.numeric(year)) %>% 
    left_join(ffdata_use, by=c("rcp", "year")) %>% 
    # Add percent of forage fish allocated to TERRITORY-LIKE UNIT (USA (Alaska/Hawaii/Continental), Guam, Puerto Rico all seperate)
    left_join(select(props, iso3_use, feed_prop), by=c("ter1_iso3_use"="iso3_use")) %>% 
    mutate(feed_prop=ifelse(is.na(feed_prop), 0, feed_prop),
           feed_allocated_mt_yr=ff_avail_mt_yr*feed_prop) %>% 
    # Calculate forage fish demand
    mutate(ff_demand_mt_yr=prod_mt_yr*fifo_use) %>% 
    # Rank by profitability within EEZ
    group_by(rcp, year, eez_name) %>% 
    arrange(rcp, year, eez_name, desc(profits_usd_yr)) %>% 
    mutate(rank_in_eez=1:n()) %>% 
    ungroup() %>% 
    # Arrange within TERRITORY-LIKE UNIT (USA (Alaska/Hawaii/Continental), Guam, Puerto Rico all seperate)
    group_by(rcp, year, ter1_name_use) %>% 
    arrange(rcp, year, ter1_name_use, rank_in_eez, desc(profits_usd_yr)) %>% 
    # Calculate cumulative forage fish and see if it exceeds ALLOCATED AMOUNT
    mutate(cum_ff_demand_mt_yr=cumsum(ff_demand_mt_yr),
           developed=ifelse(cum_ff_demand_mt_yr<=feed_allocated_mt_yr, "yes", "no")) %>% 
    filter(developed=="yes")
  
  # Plot and export data
  results_scen2 <- plot_export_results(dataset=data_scen2, feed_scen=feed_scen_do, dist_scen="current")
  
  
  # Scenario 3. Equal
  ################################################################################
  
  # Build data
  ##################################
  
  # Progressive reform - by territory
  if(i==1){
    
    # Build data
    data_scen3 <- data_use %>% 
      # Add feed group and FIFO ratios
      left_join(select(spp_key, species, feed_group), by="species") %>% 
      left_join(select(fifos_use, group, fifo_use), by=c("feed_group"="group")) %>%
      # Add forage fish availability
      mutate(year=as.numeric(year)) %>% 
      left_join(ffdata_use, by=c("rcp", "year")) %>% 
      # Add percent of forage fish allocated to TERRITORY-LIKE UNIT (USA (Alaska/Hawaii/Continental), Guam, Puerto Rico all seperate)
      mutate(feed_prop=1/n_distinct(data_use$ter1_iso3_use),
             feed_allocated_mt_yr=ff_avail_mt_yr*feed_prop) %>% 
      # Calculate forage fish demand
      mutate(ff_demand_mt_yr=prod_mt_yr*fifo_use) %>% 
      # Rank by profitability within EEZ
      group_by(rcp, year, eez_name) %>% 
      arrange(rcp, year, eez_name, desc(profits_usd_yr)) %>% 
      mutate(rank_in_eez=1:n()) %>% 
      ungroup() %>% 
      # Arrange within TERRITORY-LIKE UNIT (USA (Alaska/Hawaii/Continental), Guam, Puerto Rico all seperate)
      group_by(rcp, year, ter1_name_use) %>% 
      arrange(rcp, year, ter1_name_use, rank_in_eez, desc(profits_usd_yr)) %>% 
      # Calculate cumulative forage fish and see if it exceeds ALLOCATED AMOUNT
      mutate(cum_ff_demand_mt_yr=cumsum(ff_demand_mt_yr),
             developed=ifelse(cum_ff_demand_mt_yr<=feed_allocated_mt_yr, "yes", "no")) %>% 
      filter(developed=="yes")
    
    # Base by sovereign    
  }else{
    
    # THIS IS CRAPPY BUUT WORKS FIX IN REVISISONS
    
    # Build data #3
    data_scen3 <- data_use %>% 
      # Add feed group and FIFO ratios
      left_join(select(spp_key, species, feed_group), by="species") %>% 
      left_join(select(fifos_use, group, fifo_use), by=c("feed_group"="group")) %>%
      # Add forage fish availability
      mutate(year=as.numeric(year)) %>% 
      left_join(ffdata_use, by=c("rcp", "year")) %>% 
      # Calculate forage fish demand
      mutate(ff_demand_mt_yr=prod_mt_yr*fifo_use) %>% 
      # Arrange by profitability and add rank within SOVEREIGN
      group_by(rcp, year, sov1_name) %>% 
      arrange(rcp, year, sov1_name, desc(profits_usd_yr)) %>% 
      mutate(rank_in_sov1=1:n()) %>% 
      ungroup() %>% 
      # Arrange 
      group_by(rcp, year) %>% 
      arrange(rcp, year, rank_in_sov1, desc(profits_usd_yr)) %>% 
      # Calculate cumulative forage fish
      mutate(cum_ff_demand_mt_yr=cumsum(ff_demand_mt_yr),
             developed=ifelse(cum_ff_demand_mt_yr<=ff_avail_mt_yr, "yes", "no")) %>% 
      filter(developed=="yes")
    
    
  }
  
  # Plot and export data
  results_scen3 <- plot_export_results(dataset=data_scen3, feed_scen=feed_scen_do, dist_scen="equal")
  
  # Scenario 4. Need-based
  ################################################################################
  
  # Build data
  ##################################
  
  if(i==1){
    
    # Build data
    data_scen4_all <- data_use %>% 
      # Add feed group and FIFO ratios
      left_join(select(spp_key, species, feed_group), by="species") %>% 
      left_join(select(fifos_use, group, fifo_use), by=c("feed_group"="group")) %>%
      # Add forage fish availability
      mutate(year=as.numeric(year)) %>% 
      left_join(ffdata_use, by=c("rcp", "year")) %>% 
      # Add percent of forage fish allocated to TERRITORY-LIKE UNIT (USA (Alaska/Hawaii/Continental), Guam, Puerto Rico all seperate)
      # left_join(select(fdata1, country_iso3, feed_prop), by=c("ter1_iso3_use"="country_iso3")) %>% # approach #1
      left_join(select(fdata3, country_use_iso3, feed_prop), by=c("ter1_iso3_use"="country_use_iso3")) %>% # approach #3
      mutate(feed_prop=ifelse(is.na(feed_prop), 0, feed_prop),
             feed_allocated_mt_yr=ff_avail_mt_yr*feed_prop) %>% 
      # Calculate forage fish demand
      mutate(ff_demand_mt_yr=prod_mt_yr*fifo_use) %>% 
      # Rank by profitability within EEZ
      group_by(rcp, year, eez_name) %>% 
      arrange(rcp, year, eez_name, desc(profits_usd_yr)) %>% 
      mutate(rank_in_eez=1:n()) %>% 
      ungroup() %>% 
      # Arrange within TERRITORY-LIKE UNIT (USA (Alaska/Hawaii/Continental), Guam, Puerto Rico all seperate)
      group_by(rcp, year, ter1_name_use) %>% 
      arrange(rcp, year, ter1_name_use, rank_in_eez, desc(profits_usd_yr)) %>% 
      # Calculate cumulative forage fish and see if it exceeds ALLOCATED AMOUNT
      mutate(cum_ff_demand_mt_yr=cumsum(ff_demand_mt_yr),
             developed=ifelse(cum_ff_demand_mt_yr<=feed_allocated_mt_yr, "yes", "no"))
    
    # Cells developed in pass one
    data_scen4_pass1 <- data_scen4_all %>% 
      filter(developed=="yes")
    
    # Pass one feed stats
    pass1_feed_stats <- data_scen4_pass1 %>% 
      group_by(rcp, year) %>% 
      summarize(feed_avail_mt=unique(ff_avail_mt_yr),
                feed_used_mt=sum(ff_demand_mt_yr),
                feed_remain_mt=feed_avail_mt-feed_used_mt)
    
    # Cells developed in pass one
    data_scen4_pass2 <- data_scen4_all %>% 
      filter(developed=="no") %>% 
      ungroup() %>% 
      # Add feed remaining colum
      left_join(select(pass1_feed_stats, rcp, year, feed_remain_mt), by=c("rcp", "year")) %>% 
      # Add percent of forage fish allocated to TERRITORY-LIKE UNIT (USA (Alaska/Hawaii/Continental), Guam, Puerto Rico all seperate)
      mutate(feed_prop=1/n_distinct(ter1_iso3_use),
             feed_allocated_mt_yr=feed_remain_mt*feed_prop) %>% 
      # Calculate forage fish demand
      mutate(ff_demand_mt_yr=prod_mt_yr*fifo_use) %>% 
      # Rank by profitability within EEZ
      group_by(rcp, year, eez_name) %>% 
      arrange(rcp, year, eez_name, desc(profits_usd_yr)) %>% 
      mutate(rank_in_eez=1:n()) %>% 
      ungroup() %>% 
      # Arrange within TERRITORY-LIKE UNIT (USA (Alaska/Hawaii/Continental), Guam, Puerto Rico all seperate)
      group_by(rcp, year, ter1_name_use) %>% 
      arrange(rcp, year, ter1_name_use, rank_in_eez, desc(profits_usd_yr)) %>% 
      # Calculate cumulative forage fish and see if it exceeds ALLOCATED AMOUNT
      mutate(cum_ff_demand_mt_yr=cumsum(ff_demand_mt_yr),
             developed=ifelse(cum_ff_demand_mt_yr<=feed_allocated_mt_yr, "yes", "no")) %>% 
      filter(developed=="yes")
    
    # Merge passes one and two
    data_scen4 <- rbind(data_scen4_pass1, data_scen4_pass2)
    
    
    # Older try
    
    # # Build data
    # data_scen4 <- data_use %>% 
    #   # Add feed group and FIFO ratios
    #   left_join(select(spp_key, species, feed_group), by="species") %>% 
    #   left_join(select(fifos_use, group, fifo_use), by=c("feed_group"="group")) %>%
    #   # Add forage fish availability
    #   mutate(year=as.numeric(year)) %>% 
    #   left_join(ffdata_use, by=c("rcp", "year")) %>% 
    #   # Add percent of forage fish allocated to TERRITORY-LIKE UNIT (USA (Alaska/Hawaii/Continental), Guam, Puerto Rico all seperate)
    #   # left_join(select(fdata1, country_iso3, feed_prop), by=c("ter1_iso3_use"="country_iso3")) %>% # approach #1
    #   left_join(select(fdata3, country_use_iso3, feed_prop), by=c("ter1_iso3_use"="country_use_iso3")) %>% # approach #3
    #   mutate(feed_prop=ifelse(is.na(feed_prop), 0, feed_prop),
    #          feed_allocated_mt_yr=ff_avail_mt_yr*feed_prop) %>% 
    #   # Calculate forage fish demand
    #   mutate(ff_demand_mt_yr=prod_mt_yr*fifo_use) %>% 
    #   # Rank by profitability within EEZ
    #   group_by(rcp, year, eez_name) %>% 
    #   arrange(rcp, year, eez_name, desc(profits_usd_yr)) %>% 
    #   mutate(rank_in_eez=1:n()) %>% 
    #   ungroup() %>% 
    #   # Arrange within TERRITORY-LIKE UNIT (USA (Alaska/Hawaii/Continental), Guam, Puerto Rico all seperate)
    #   group_by(rcp, year, ter1_name_use) %>% 
    #   arrange(rcp, year, ter1_name_use, rank_in_eez, desc(profits_usd_yr)) %>% 
    #   # Calculate cumulative forage fish and see if it exceeds ALLOCATED AMOUNT
    #   mutate(cum_ff_demand_mt_yr=cumsum(ff_demand_mt_yr),
    #          developed=ifelse(cum_ff_demand_mt_yr<=feed_allocated_mt_yr, "yes", "no")) %>%
    #   # # Group by RCP and year to use up remainder of feed
    #   # ungroup() %>% 
    #   # group_by(rcp, year) %>% 
    #   # # Determine the amount of feed thats been used then allocate elsewhere
    #   # mutate(ff_used_first=sum(ff_demand_mt_yr[developed=="yes"]),
    #   #        ff_remain_first=ff_avail_mt_yr-ff_used_first) %>% 
    #   # # Rank by profitability within EEZ
    #   # ungroup() %>% 
    #   # group_by(rcp, year, developed, ter1_name_use) %>% 
    #   # arrange(rcp, year, developed, ter1_name_use, desc(profits_usd_yr)) %>% 
    #   # mutate(rank_in_2nd_round=1:n()) %>%
    #   # ungroup() %>% 
    #   # arrange(rcp, year, developed, rank_in_2nd_round) %>% 
    #   # mutate(cum_ff_demand_mt_yr2=cumsum(ff_demand_mt_yr),
    #   #        developed=ifelse(cum_ff_demand_mt_yr2<=ff_remain_first, "yes", developed)) %>% 
    # filter(developed=="yes")
    
  }else{
    
    # THIS IS GARBAGE AND NEEDS TO BE FIXED
    
    # Build data
    data_scen4 <- data_use %>% 
      # Add feed group and FIFO ratios
      left_join(select(spp_key, species, feed_group), by="species") %>% 
      left_join(select(fifos_use, group, fifo_use), by=c("feed_group"="group")) %>%
      # Add forage fish availability
      mutate(year=as.numeric(year)) %>% 
      left_join(ffdata_use, by=c("rcp", "year")) %>% 
      # Add percent of forage fish allocated to COUNTRY based on need
      left_join(select(fdata1_crap, sovereign_iso3, need_feed_prop), by=c("sov1_iso3_use"="sovereign_iso3")) %>% 
      rename(feed_prop=need_feed_prop) %>% 
      mutate(feed_prop=ifelse(is.na(feed_prop), 0, feed_prop),
             feed_allocated_mt_yr=ff_avail_mt_yr*feed_prop) %>% 
      # Calculate forage fish demand
      mutate(ff_demand_mt_yr=prod_mt_yr*fifo_use) %>% 
      # Rank by profitability within EEZ
      group_by(rcp, year, eez_name) %>% 
      arrange(rcp, year, eez_name, desc(profits_usd_yr)) %>% 
      mutate(rank_in_eez=1:n()) %>% 
      ungroup() %>% 
      # Arrange with SOVEREIGN NATION
      group_by(rcp, year, sov1_name) %>% 
      arrange(rcp, year, sov1_name, rank_in_eez, desc(profits_usd_yr)) %>% 
      # Calculate cumulative forage fish and see if it exceeds ALLOCATED AMOUNT
      mutate(cum_ff_demand_mt_yr=cumsum(ff_demand_mt_yr),
             developed=ifelse(cum_ff_demand_mt_yr<=feed_allocated_mt_yr, "yes", "no")) %>% 
      filter(developed=="yes")
    
    
  }
  
  # Plot and export data
  results_scen4 <- plot_export_results(dataset=data_scen4, feed_scen=feed_scen_do, dist_scen="need")
  
  # Merge results
  results_i <- rbind(results_scen1, results_scen2, results_scen3, results_scen4)
  if(i==1){
    results <- results_i
  }else{
    results <- rbind(results, results_i)
  }
  
}

# Export results
write.csv(results, file=file.path(datadir, "finfish_mariculture_potential_by_eez_rcp_feed_scenario.csv"), row.names=F)

# Summary table
summ_table <- results %>% 
  # Summarize
  group_by(rcp, feed_scen, dev_pattern, year) %>% 
  summarize(area_sqkm_t=sum(area_sqkm)/1e3,
            prod_mt_ml=sum(prod_mt)/1e6, 
            profits_usd_bil=sum(profits_usd)/1e9) %>% 
  # Reduce to 2100
  filter(year==2100) %>% 
  # Arrange
  arrange(feed_scen, dev_pattern, rcp) %>% 
  select(-year) %>% 
  select(feed_scen, dev_pattern, rcp, everything())

# Export summary table
write.csv(summ_table, file.path(tabledir, "TableSX_2100_faq_rcp_feed_dev_scenarios.csv"), row.names=F)



