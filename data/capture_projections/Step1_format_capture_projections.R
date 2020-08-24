
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(plyr)
library(dplyr)
library(ggplot2)
library(countrycode)

# Directories
datadir <- "/Users/cfree/Dropbox/Chris/UCSB/projects/edf_climate/cc_trade/data/gaines/raw"
outdir <- "data/capture_projections/data/chunks_eez"

# Load data
ranges_orig <- readRDS(file.path(datadir, "eez_delta_k_df.rds")) # EEZ range

# Load final results
outcomes_orig <- readRDS(file.path(datadir, "global_cc_1nation_manuscript_2019Feb12.rds"))

# Shape parameter
p <- 0.188
pdiv <- (p+1)^(1/p)


# Derive r values from Tracey's data
################################################################################

# MSY time series aren't provided
# However, in this PT parameterization, r=FMSY
# Thus, MSY = r*K/pdiv where r=FMSY

# BMSY = K/pdiv (from Gaines et al 2018 supplement)
# MSY = rK/pdiv (from Gaines et al 2018 supplement)
# FMSY = r (apparently true)

# Here is proof:
r_test <- outcomes1_orig %>% 
  mutate(r=FMSY, 
         msy=r*K/pdiv) %>% 
  filter(year==2012)
plot(msy ~ MSY2012, r_test)

# Learn about data
################################################################################

# DATA - changes in range size and carrying capacity
# --------------------------------------------------------------------
# rangekm2 = range (km2) of species(i) in EEZ(j) in year(t) 
# total_range = range (km2) of species(i) in year(t), globally
# r = rangekm2 / total_range = proportion of the range of species(i) in year(t) inside EEZ(j)
# adj_rratio = doesn't matter
# total_k = carrying capacity of species(i) in year(t), globally
# eez_k = carrying capacity of species(i) in EEZ(j) in year(t) 
# total_k0 = carrying capacity of species(i) in 2012, globally
# eez_k0 = carrying capacity of species(i) in EEZ(j) in 2012
# delta_k = eez_k - eez_k0 = change in carrying capacity relative to 2012
# delta_k_r = eez_k / eez_k0 = proportion of K in of species(i) in EEZ(j) in year(t) relative to 2012

# I want to know how biomass, catch, and profit has changed
# for species(i) in EEZ(j) in year(t) relative to 2012

# This means I have to allocate the global biomass, harvest, and profit for species(i) in year(t)
# to its constituent EEZs, EEZ(j). I'm going to assume that these are spread perfect uniformly
# across its range which means I want to multipy each value by range(EEZ)/range(total) 
# then compare difference between 2012 and the reference year.

# Inspect one scenario (RCP-EEZ-species)
sdata <- filter(ranges_orig, RCP=="RCP26" & Country=="United States" & SciName=="Gadus morhua")

# Format scenario
sdata1 <- sdata %>% 
  # Rename columns
  setNames(tolower(colnames(.))) %>% 
  rename(species=sciname,
         species_id=speciesid,
         comm_name=commname,
         eez_id=eezid,
         range_eez_km2=rangekm2,
         range_tot_km2=total_range, 
         range_prop=r, range_prop_adj=adj_rratio, 
         k_tot=total_k, k_eez=eez_k, k0_eez=eez_k0, k0_tot=total_k0) %>% 
  # Add columns
  mutate(k_prop=k_eez / k_tot, # I don't trust this because Kprops don't sum to one later
         k_prop1=range_prop)  %>% # so I trust this value
  # Rearrange columns 
  select(rcp, sovereign, country, eez_id, eez, 
         species_id, species, comm_name, year,
         range_eez_km2, range_tot_km2, range_prop,
         k_eez, k_tot, k_prop, k0_eez, k0_tot, k_prop1)
plot(k_prop ~ k_prop1, sdata1)

# Subset one species from results
sresults <- filter(outcomes_orig, SciName=="Gadus morhua" & cc_presence=="climate_change")

# Inspect scenarios
# 5 RCPs: none, 2.6, 4.5, 6.0, 8.5
# 4 scenarios: none, full, prod only, range only
# 3 discount rates
# Years: 2012-2100 (89)
scenarios <- sresults %>%
  group_by(RCP, cc_presence, scenario, HCR, discount_rate) %>% 
  summarize(n=n())

# Format results
sresults1 <- sresults %>% 
  # Rename columns
  setNames(tolower(colnames(.))) %>% 
  rename(species=sciname,
         species_id=speciesid,
         comm_name=commname,
         bbmsy=bvbmsy,
         profit_discounted=discounted_prof,
         fmort=fish_mort_rate,
         ffmsy=fvfmsy) %>% 
  # Add MSY
  mutate(r=fmsy,
         msy=r*k/pdiv) %>% 
  # Remove non-critical scenarios
  filter(cc_presence=="climate_change" & discount_rate==0)


# Build EEZ key
################################################################################

# I'm going to perform the analysis at 2-levels: sovereign nation and EEZ/territory level
# This will require changing the country field to match the EEZ field one-to-one

# Joint areas
joint_areas <- c("Australia - Papua New Guinea", "Australia/Indonesia", "Joint Development", "Joint Regime")

# FAO Major fishing areas
fao_areas <- c("Antarctica", "Arctic Sea", "Atlantic, Eastern Central", "Atlantic, Northeast", "Atlantic, Northwest",
               "Atlantic, Southeast", "Atlantic, Southwest", "Atlantic, Western-Central", "Indian Ocean, Eastern",
               "Indian Ocean, Western", "Pacific, Eastern Central", "Pacific, Northeast", "Pacific, Northwest",
               "Pacific, Southeast", "Pacific, Southwest", "Pacific, Western Central")

# Build EEZ key
eez_key <- ranges_orig %>% 
  # Lower column names
  setNames(tolower(colnames(.))) %>% 
  # Reduce data so unique() is faster
  filter(rcp=="RCP26" & year==2012 & sciname=="Gadus morhua") %>% 
  # Reduce to unique areas
  select(sovereign, country, eezid, eez) %>% 
  unique() %>% 
  # Format data
  mutate(eez=as.character(eez),
         # Fix country names
         country=as.character(country),
         country=ifelse(eez=="Antarctica", "Antarctica", country),
         country=ifelse(eez%in%fao_areas, paste("FAO", eez), country),
         country=ifelse(eez=="Colombian Exclusive Economic Zone (Quitasueño)", "Quita Sueño Bank", country),
         country=ifelse(eez=="Colombian Exclusive Economic Zone (Serrara)", "Serrana Bank", country),
         # Fix sovereign nation names
         sovereign=as.character(sovereign),
         sovereign_iso3=countrycode(sovereign, "country.name", "iso3c"), 
         sovereign_iso3=ifelse(sovereign=="High Seas", "ABNJ", sovereign_iso3),
         sovereign_iso3=ifelse(sovereign=="Disputed", "Disputed", sovereign_iso3),
         sovereign_iso3=ifelse(sovereign%in%joint_areas, "Joint", sovereign_iso3),
         sovereign_iso3=ifelse(sovereign=="Micronesia", "FSM", sovereign_iso3),
         country_iso3=countrycode(country, "country.name", "iso3c")) %>% 
  # Rearrange and rename
  rename(eez_id=eezid) %>% 
  select(sovereign_iso3, sovereign, country_iso3, country, eez_id, eez) %>% 
  arrange(sovereign, country, eez)

# Complete
freeR::complete(eez_key)

# Make sure original matches key
n_distinct(ranges_orig$EEZ) # 263 EEZs and EEZ ids
n_distinct(ranges_orig$EEZID)
n_distinct(ranges_orig$Country) # 247 countries
n_distinct(ranges_orig$Sovereign) # 159 sovereign nations
n_distinct(eez_key$eez) # 263 EEZs and EEZ ids
n_distinct(eez_key$eez_id)
n_distinct(eez_key$country) # 247 countries
n_distinct(eez_key$sovereign) # 159 sovereign nations

# Are the "country" and "eez" columns one-to-one
eez_check <- eez_key %>%
  group_by(country) %>% 
  summarize(n=n_distinct(eez)) %>% 
  desc(n)

# Country, EEZ, and EEZ id should all be unique identifiers and map to each other
anyDuplicated(eez_key$eez_id)
anyDuplicated(eez_key$eez)
anyDuplicated(eez_key$country)


# Build species key
################################################################################

# Build species key
spp_key <- ranges_orig %>% 
  # Lower column names
  setNames(tolower(colnames(.))) %>% 
  # Reduce data so unique() is faster
  filter(rcp=="RCP26" & country=="United States" & year==2012) %>% 
  # Select relevant columns
  select(speciesid, sciname, commname) %>% 
  unique() %>% 
  rename(species=sciname, comm_name=commname, species_id=speciesid) %>% 
  # Format common name
  mutate(comm_name1=gsub(" *\\(.*?\\) *", "", comm_name))

# Check species
freeR::check_names(spp_key$species)

# Confirm that species ID is unique
anyDuplicated(spp_key$species_id)


# Format range dataset
################################################################################

# Format range shift time series
ranges <- ranges_orig %>%
  # Rename columns
  setNames(tolower(colnames(.))) %>% 
  rename(species=sciname,
         species_id=speciesid,
         comm_name=commname,
         eez_id=eezid,
         range_eez_km2=rangekm2,
         range_tot_km2=total_range, 
         range_prop=r, range_prop_adj=adj_rratio, 
         k_tot=total_k, k_eez=eez_k, k0_eez=eez_k0, k0_tot=total_k0) %>% 
  # Remove and re-add geographic info
  select(-c(sovereign, country, eez)) %>% 
  left_join(eez_key, by="eez_id") %>% 
  # Add columns
  mutate(k_prop=k_eez / k_tot,
         range_prop=ifelse(range_tot_km2==0, 0, range_prop)) %>% 
  # Rearrange columns 
  select(rcp, sovereign_iso3, sovereign, country_iso3, country, eez_id, eez, 
         species_id, species, comm_name, year,
         range_eez_km2, range_tot_km2, range_prop,
         k_eez, k_tot, k_prop, k0_eez, k0_tot)


# Format outcomes dataset
################################################################################

# Format fisheries outcomes time series
outcomes <- outcomes_orig %>% 
  # Rename columns
  setNames(tolower(colnames(.))) %>% 
  rename(species=sciname,
         species_id=speciesid,
         comm_name=commname,
         bbmsy=bvbmsy,
         profit_discounted=discounted_prof,
         fmort=fish_mort_rate,
         ffmsy=fvfmsy) %>% 
  # Add formatted common name
  left_join(select(spp_key, species_id, comm_name1), by="species_id") %>% 
  # Add MSY
  mutate(r=fmsy,
         msy=r*k/pdiv,
         msy=ifelse(k==0, 0, msy),
         bmsy=ifelse(k==0, NA, bmsy),
         fmsy=ifelse(k==0, NA, fmsy),
         bbmsy=ifelse(k==0, NA, bbmsy),
         ffmsy=ifelse(k==0, NA, ffmsy),
         fmort=ifelse(k==0, NA, fmort),
         biomass=ifelse(k==0, 0, biomass),
         harvest=ifelse(k==0, 0, harvest),
         profit=ifelse(k==0, 0, profit)) %>% 
  # Remove non-critical scenarios
  filter(cc_presence=="climate_change" & discount_rate==0) %>% 
  # Arrange columns
  select(rcp, scenario, hcr, intervention_int,
         species_id, species, comm_name, comm_name1, 
         shifter, first_shift_yr, msy2012, 
         year, r, k, msy, bmsy, fmsy, bbmsy, ffmsy, 
         fmort, biomass, harvest, profit) %>% 
  arrange(rcp, scenario, species, year)

# Inspect factorial design  
factors <- outcomes %>% 
  group_by(rcp, scenario) %>% 
  summarise(nspp=n_distinct(species),
            nobs=n())

# Inspect corrections made for when K=0
# The only NAs should happen for these cases and in the intervention yr
outcomes_check <- outcomes %>%
  filter(k==0)
freeR::complete(outcomes)


# Build MSY time series
################################################################################

# Global MSY time series
# 779 * length(2012:2100) * 4
msy_ts_g <- outcomes %>%
  filter(scenario=="No Adaptation") %>% 
  select(rcp, species, comm_name1, year, msy)

# EEZ-level MSY time series
msy_ts <- ranges %>% 
  # Reduce to important columns
  select(rcp:range_prop) %>% 
  # Add K and MSY from just one scenario (since this isn't driven by scenario) 
  left_join(msy_ts_g, by=c("rcp", "species", "year")) %>% 
  # Calculate EEZ-level MSY
  mutate(msy_eez=msy*range_prop) %>% 
  select(rcp:comm_name, comm_name1, everything())

# Export
# saveRDS(msy_ts_g, file=file.path(outdir, "Free_etal_2020_msy_by_rcp_global.Rds"))
# saveRDS(msy_ts, file=file.path(outdir, "Free_etal_2020_msy_by_rcp_eez.Rds"))



# Add ISSCAAP group to species key
################################################################################

# Read FAO data
fao_orig <- readRDS("/Users/cfree/Dropbox/Chris/UCSB/data/fao/capture/processed/1950_2017_fao_landings_data.Rds")

# FAO species key
fao_spp <- fao_orig %>% 
  select(comm_name, species_orig, isscaap) %>% 
  unique() %>% 
  arrange(species_orig)

# Add ISSCAAP to species key
spp_key1 <- spp_key %>% 
  left_join(fao_spp, by=c("comm_name"="comm_name")) %>% 
  # Fill in some gaps
  mutate(isscaap=ifelse(species%in%c("Ammodytes marinus", "Ariomma indicum", "Pagrus major", "Scorpaena guttata", "Sillago flindersi"), 
                        "Miscellaneous coastal fishes", isscaap),
         isscaap=ifelse(species%in%c("Atheresthes stomias", "Glyptocephalus zachirus", "Lepidopsetta polyxystra", "Parophrys vetulus",
                                     "Platycephalus conatus", "Platycephalus richardsoni", "Pleuronectes quadrituberculatus"), "Flounders, halibuts, soles", isscaap),
         isscaap=ifelse(species%in%c("Carcharhinus sorrah","Dipturus batis", "Raja rhina"), "Sharks, rays, chimaeras", isscaap),
         isscaap=ifelse(species%in%c("Centroberyx gerrardi", "Sebastes aleutianus"), "Miscellaneous demersal fishes", isscaap),
         isscaap=ifelse(species=="Doryteuthis pealeii", "Squids, cuttlefishes, octopuses", isscaap),
         isscaap=ifelse(species%in%c("Farfantepenaeus aztecus", "Farfantepenaeus duorarum", "Litopenaeus setiferus"), "Shrimps, prawns", isscaap),
         isscaap=ifelse(species%in%c("Hilsa kelee"), "Shads", isscaap),
         isscaap=ifelse(species%in%c("Kajikia albida"), "Tunas, bonitos, billfishes", isscaap), 
         isscaap=ifelse(species%in%c("Scomber japonicus"), "Miscellaneous pelagic fishes", isscaap),
         isscaap=ifelse(grepl("Sebastes", species), "Miscellaneous demersal fishes", isscaap)) %>% 
  # Simplify
  select(species, isscaap)

# Perform checks
sum(is.na(spp_key1$isscaap))
sort(unique(spp_key1$isscaap))


# Merge ranges and outcomes dataset
################################################################################

# Reduce memory
rm(msy_ts, msy_ts_g, outcomes_orig, ranges_orig)

# Reduce outcomes to scenario of interest
outcomes_red <- outcomes %>% 
  filter(scenario %in% c("Full Adaptation", "No Adaptation"))

# Merge EEZ range shift projections and global biomass, catch, and profit projections
# Final should be nrow(ranges) * 6 scenarios : nrow(ranges) * 6
data <- ranges %>% 
  select(rcp:range_prop) %>% 
  # Add biomass, harvest, profit
  left_join(select(outcomes_red, rcp, scenario, hcr, species, comm_name1, 
                   year, r, k, msy, bmsy, fmsy, bbmsy, ffmsy, 
                   fmort, biomass, harvest, profit), by=c("rcp", "species", "year")) %>%
  # Calculate proportions in EEZ
  mutate(msy_eez=msy*range_prop,
         biomass_eez=biomass*range_prop,
         harvest_eez=harvest*range_prop,
         profit_eez=profit*range_prop,
         bbmsy_eez=ifelse(range_prop>0, bbmsy, NA),
         ffmsy_eez=ifelse(range_prop>0, ffmsy, NA)) 

# This is so fucking annoying.
# I'm getting memory errors so I have to chunk.

# Remove big stuff
rm(ranges, outcomes, outcomes_red, fao_orig)

# Loop through eezs
eezs <- sort(unique(data$eez))
eez_chunks <- split(eezs, ceiling(seq_along(eezs)/10))

for(i in 1:length(eez_chunks)){
  
  # EEZ to do
  print(i)
  eezs_do <- eez_chunks[i] %>% unlist()
  
  # Format data
  sdata <- data %>% 
    # Filter
    filter(eez%in%eezs_do) %>% 
    # Add ISSCAAP group
    left_join(spp_key1) %>% 
    # Summarize
    group_by(rcp, scenario, sovereign_iso3, sovereign, country_iso3, country, eez_id, eez, isscaap, year) %>% 
    summarize(msy_mt=sum(msy_eez),
              biomass_mt=sum(biomass_eez),
              catch_mt=sum(harvest_eez),
              profits_usd=sum(profit_eez),
              bbmsy_avg=mean(bbmsy_eez),
              ffmsy_avg=mean(ffmsy_eez))
  
  # Export data
  saveRDS(sdata, file.path(outdir, paste0("chunk", i, ".Rds")))
  
}
  

# Any chunks missing?
# eezs_done <- list.files(outdir) %>% gsub(".Rds", "", .)
# eezs_missing <- eezs[!eezs%in%eezs_done]
# which(eezs=="Disputed Kenya/Somalia" )
