
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)
library(countrycode)

# Directories
datadir <- "data/capture_projections/data"
popdir <- "data/pop_growth/data"

# Read data
data_orig <- readRDS(file.path(datadir, "Free_etal_2020_fish_proj_by_rcp_mgmt_eez_isscaap.Rds"))

# Read FAO data
fao_orig <- readRDS("/Users/cfree/Dropbox/Chris/UCSB/data/fao/capture/processed/1950_2017_fao_landings_data.Rds")

# Read population growth data
pop_orig <- readRDS(file.path(popdir, "WB_UN_1960_2100_human_population_by_country.Rds"))

# Read edible meat conversion factors
conv_factors <- readxl::read_excel("data/nutrition/isscaap_conversion_factors.xlsx") %>% 
  select(isscaap, catch2meat)


# Format data
################################################################################

# Things to do:
# 1) Scale to match 2012 catch
# 2) Calculate proportion for direct human consumption (DHC)
# 3) Calculate proportion of edible meat
# 4) Match to human population growth data

# 1) Scale to match 2012 catch
##############################################################

# Identify ISSCAAPs to scale to
isscaaps_free <- sort(unique(data_orig$isscaap))
isscaaps_all <- sort(unique(fao_orig$isscaap))
isscaaps_to_scale_to <- sort(c(isscaaps_free, "Marine fishes not identified", "Miscellaneous marine molluscs"))
isscaaps_exlcuded <- isscaaps_all[!isscaaps_all%in%isscaaps_to_scale_to]

# Confirm that all the used ISSCAAPs are in the FAO data
any(!isscaaps_to_scale_to %in% fao_orig$isscaap)

# Format FAO data
##############################################################

# FAO 2012 catch by country and ISSCAAP group
fao12 <- fao_orig %>% 
  # Reduce to 2012 marine catch (tons) for consumed ISSCAAP groups
  filter(area_type=="marine" & units=="t" & isscaap %in% isscaaps_to_scale_to & year==2012) %>% 
  # Group by country
  group_by(country_use, iso3_use) %>% 
  summarize(catch_mt=sum(quantity)) %>% 
  ungroup() %>% 
  mutate(iso3_use=ifelse(country_use=="Other nei", "Other", iso3_use),
         iso3_use=ifelse(country_use=="Channel Islands", "Channel Islands", iso3_use))

# Group FAO territories into sovereign areas to align with Free et al. (2020)
fao12a <- fao12 %>% 
  mutate(country_use=recode(country_use, 
                            "Serbia and Montenegro"="Montenegro",
                            "Hong Kong SAR China"="China", 
                            "Macau SAR China"="China",
                            "Zanzibar"="Tanzania",
                            "Channel Islands"="United Kingdom",
                            "Isle of Man"="United Kingdom",
                            "Guam"="Northern Mariana Islands and Guam",
                            "Northern Mariana Islands"="Northern Mariana Islands and Guam"),
         iso3_use=recode(iso3_use, 
                         "SCG"="MNE",
                         "HKG"="CHN", 
                         "MAC"="CHN", 
                         "EAZ"="TZA",
                         "Channel Islands"="GBR", 
                         "IMN"="GBR",
                         "GUM"="MNP/GUM",
                         "MNP"="MNP/GUM")) %>% 
  # Add difficult to match countries to Other
mutate(country_use=ifelse(country_use %in% c("Bosnia & Herzegovina", "Caribbean Netherlands", "Ethiopia", "French Southern Territories", "Jordan", "Netherlands Antilles", "Palestinian Territories", "Saint Martin (French part)", "St. Barthélemy"), "Other nei", country_use),
       iso3_use=ifelse(iso3_use %in% c("BIH", "BES", "ETH", "ATF", "JOR", "ANT", "PSE", "MAF", "BLM"), "Other", iso3_use)) %>% 
  # Regroup
  group_by(country_use, iso3_use) %>% 
  summarize(catch_mt=sum(catch_mt)) %>% 
  ungroup() %>% 
  # Remove non-existent countries with no catch
  filter(!country_use %in% c("Un. Sov. Soc. Rep.", "Yugoslavia"))

# Total FAO catch
fao12_tot <- sum(fao12$catch_mt) / 1e6
fao12a_tot <- sum(fao12a$catch_mt) / 1e6
fao12_tot == fao12a_tot

# Format Free et al data
##############################################################

# Free  et al. 2012 catch by country and ISSCAAP group
free12 <- data_orig %>% 
  filter(rcp=="RCP 2.6" & scenario=="No Adaptation" & year==2012) %>% 
  group_by(sovereign, sovereign_iso3, country, country_iso3) %>% 
  summarize(catch_mt=sum(catch_mt)) %>% 
  ungroup()

# Total Free et al. (2020) catch
sum(free12$catch_mt) / 1e6

# We are going to match the Free et al. (2020) countries to the FAO countries
cntry_key <- free12 %>% 
  # Identify unique "countries" in the Free et al. 2020 data
  select(sovereign, sovereign_iso3, country, country_iso3) %>% 
  unique() %>% 
  # Attempt #1: Use sovereign ISO3 when country ISO3 is missing
  mutate(iso3_sum_by=ifelse(!is.na(country_iso3), country_iso3, sovereign_iso3)) %>% 
  # Attempt #2: Use sovereign ISO3 over the country ISO3 for a few states
  mutate(iso3_sum_by=recode(iso3_sum_by, 
                          "ESH"="MAR", "BVT"="NOR", "GGY"="GBR", "JEY"="GBR", "SGS"="GBR", "CXR"="AUS", "CCK"="AUS", "HMD"="AUS")) %>% 
  # Attempt #3: Combine joint, disputed, high seas areas, and Antarctica into "Other"
  mutate(iso3_sum_by=ifelse(iso3_sum_by %in% c("Joint", "Disputed", "ABNJ", "ATA"), "Other", iso3_sum_by)) %>% 
  # Attempt #4: Add Azerbaijan, Kazakhstan, Turkmenistan (all on Caspian Sea which doesn't count)
  mutate(iso3_sum_by=ifelse(iso3_sum_by %in% c("AZE", "KAZ", "TKM"), "Other", iso3_sum_by)) %>% 
  # Attempt #5: Overwrite some decisions based on FAO matching
  mutate(iso3_sum_by=ifelse(country=="Puerto Rico of the United States", "PRI", iso3_sum_by),
         iso3_sum_by=ifelse(country=="Virgin Islands of the United States", "VIR", iso3_sum_by),
         iso3_sum_by=ifelse(country=="Northern Mariana Islands and Guam", "MNP/GUM", iso3_sum_by)) %>% 
  # Match to FAO data
  mutate(iso3_fao=ifelse(iso3_sum_by %in% fao12a$iso3_use, iso3_sum_by, NA)) %>% 
  # Simplify key
  select("country", "iso3_sum_by")

# Add ISO3 key to summarize by to Free et al. (2020) and perform summary
free12a <- free12 %>% 
  left_join(cntry_key) %>% 
  group_by(iso3_sum_by) %>% 
  summarize(catch_mt=sum(catch_mt))

# Merge FAO and Free et al data and calculate scalars
##############################################################

# Add Free et al. 2012 summarized catch to FAO 2012 catch
scalar_key <- fao12a %>% 
  # Rename FAO 2012 catcg
  rename(catch_mt_2012_fao=catch_mt) %>%
  # Add and rename Free 2012 catch
  left_join(free12a, by=c("iso3_use"="iso3_sum_by")) %>% 
  rename(catch_mt_2012_free=catch_mt) %>% 
  # Calculate scalar
  mutate(scalar=catch_mt_2012_fao / catch_mt_2012_free) %>% 
  # Change zeros to ones
  mutate(scalar=ifelse(scalar==0, 1, scalar))

# Scalar histogram
hist(scalar_key$scalar, breaks=seq(0,50,0.1))

# Scale Free et al 2012 catch to test
free12b <- free12a %>% 
  left_join(scalar_key %>% select(iso3_use, scalar), by=c("iso3_sum_by"="iso3_use")) %>% 
  mutate(catch_mt_scaled=catch_mt*scalar)

# Confirm
sum(free12b$catch_mt_scaled) / 1e6 # good, this is juts a bit higher than target because of changing the 0s to 1s

# Scale Free et al data
##############################################################

# Scale data
data_scaled <- data_orig %>% 
  # Add IS03 to summarize by
  left_join(cntry_key) %>% 
  # Summarize by RCP, scenario, ISO3 to summarize by, isscaap, year
  group_by(rcp, scenario, iso3_sum_by, isscaap, year) %>% 
  summarize(msy_mt=sum(msy_mt),
            biomass_mt=sum(biomass_mt),
            catch_mt=sum(catch_mt),
            profits_usd=sum(profits_usd)) %>% 
  ungroup() %>% 
  # Add scalar and perform scaling
  left_join(scalar_key %>% select(country_use, iso3_use, scalar), by=c("iso3_sum_by"="iso3_use")) %>% 
  mutate(msy_mt_scaled=msy_mt*scalar,
         biomass_mt_scaled=biomass_mt*scalar,
         catch_mt_scaled=catch_mt*scalar,
         profits_usd_scaled=profits_usd*scalar) %>% 
  # Rename and rearrange
  rename(iso3=iso3_sum_by, country=country_use) %>%
  select(rcp, scenario, country, iso3, isscaap, year, everything()) %>% 
  # Replace FULL ADAPTATION 2012 value with NO ADAPTATION 2012 VALUE
  group_by(country, iso3, isscaap) %>% 
  mutate(catch_mt_scaled=ifelse(year==2012, catch_mt_scaled[scenario=="No Adaptation"], catch_mt_scaled),
         profits_usd_scaled=ifelse(year==2012, profits_usd_scaled[scenario=="No Adaptation"], profits_usd_scaled)) %>% 
  ungroup()
    
# Check plots
##############################################################

# Calculate annual stats
stats <- data_scaled %>% 
  group_by(rcp, scenario, year) %>% 
  summarize(msy_mt_scaled=sum(msy_mt_scaled), 
            biomass_mt_scaled=sum(biomass_mt_scaled),
            catch_mt_scaled=sum(catch_mt_scaled),
            profits_usd_scaled=sum(profits_usd_scaled)) %>% 
  ungroup()

# 2012 MSY
msy2012 <- data_scaled %>% 
  filter(rcp=="RCP 2.6" & scenario=="No Adaptation" & year==2012) %>% 
  pull(msy_mt_scaled) %>% sum() / 1e6

# Plot check
g <- ggplot(stats, aes(x=year, y=catch_mt_scaled/1e6, color=rcp, linetype=scenario)) +
  geom_line() +
  # Limits
  lims(y=c(0,100)) +
  # Labels
  labs(x="", y="Catch (millions mt)") +
  scale_color_discrete(name="Emissions scenario") +
  scale_linetype_discrete(name="Managament scenario") +
  scale_x_continuous(breaks=c(2012, seq(2020, 2100, 10))) +
  # Theme
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))
g



# 2) Calculate proportion for DHC
# 3) Calculate proportion of edible meat
##############################################################

# Build
isscaaps <- sort(unique(data_scaled$isscaap))

# Add proportion for DHC and calculate edible meat
data_final <- data_scaled %>% 
  # Calculate catch for DHC
  mutate(dhc_mt_scaled=catch_mt_scaled*0.82) %>% 
  # Calculate edible meat for DHC
  left_join(conv_factors) %>% 
  mutate(meat_mt_scaled=dhc_mt_scaled*catch2meat)

# Inspect data
freeR::complete(data_final) 

# Export data
saveRDS(data_final, file=file.path(datadir, "Free_etal_2020_fish_proj_by_rcp_mgmt_cntry_isscaap_scaled.Rds"))


# Inspect stats
##############################################################

# Calculate annual stats
stats <- data_final %>% 
  group_by(rcp, scenario, year) %>% 
  summarize(catch_mt=sum(catch_mt_scaled),
            dhc_mt=sum(dhc_mt_scaled),
            meat_mt=sum(meat_mt_scaled)) %>% 
  ungroup()


# Plot check
g <- ggplot(stats, aes(x=year, y=meat_mt/1e6, color=rcp, linetype=scenario)) +
  geom_line() +
  # Limits
  lims(y=c(0,100)) +
  # Labels
  labs(x="", y="Edible meat (millions mt)") +
  scale_color_discrete(name="Emissions scenario") +
  scale_linetype_discrete(name="Managament scenario") +
  scale_x_continuous(breaks=c(2012, seq(2020, 2100, 10))) +
  # Theme
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))
g

