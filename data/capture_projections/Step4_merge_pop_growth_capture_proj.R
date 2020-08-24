
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
data_orig <- readRDS(file.path(datadir, "Free_etal_2020_fish_proj_by_rcp_mgmt_cntry_isscaap_scaled.Rds"))

# Read population growth data
pop_orig <- readRDS(file.path(popdir, "WB_UN_1960_2100_human_population_by_country.Rds"))


# Global dataset
################################################################################

# Calculate global population
gpop <- pop_orig %>% 
  group_by(year) %>% 
  summarize(npeople=sum(pop_size_50perc))

# Check plot
g <- ggplot(gpop, aes(x=year, npeople/1e9)) +
  geom_line() +
  lims(y=c(0,11)) +
  labs(x="", y="Billions of people") +
  theme_bw()
g

# Build global data
gdata <- data_orig %>% 
  # Global fish stats
  group_by(rcp, scenario, year) %>% 
  summarize(biomass_mt=sum(biomass_mt_scaled), 
            msy_mt=sum(msy_mt_scaled),
            catch_mt=sum(catch_mt_scaled),
            catch_dhc_mt=sum(dhc_mt_scaled),
            meat_mt=sum(meat_mt_scaled)) %>% 
  ungroup() %>% 
  # Add forage fish catch
  mutate(catch_ff_mt=catch_mt-catch_dhc_mt) %>% 
  select(rcp:catch_mt, catch_ff_mt, everything()) %>% 
  # Add global human population
  left_join(gpop, by="year") %>% 
  # Calculate catch and meat per capita
  mutate(catch_kg_person=catch_dhc_mt*1000/npeople, 
         meat_kg_person=meat_mt*1000/npeople)

# Plot per capita seafood
g <- ggplot(gdata %>% filter(!is.na(meat_kg_person)), aes(x=year, y=meat_kg_person, color=rcp, linetype=scenario)) +
  geom_line() +
  labs(x="", y="Annual per capita seafood supply\nfrom marine capture fisheries") +
  theme_bw()
g 

# Export
saveRDS(gdata, file=file.path(datadir, "Free_etal_2020_global_projections_with_pop_data.Rds"))



# Build national dataset
################################################################################

# Countries in population growth
pop_key <- pop_orig %>% 
  select(country, iso3) %>% 
  unique()

# Countries in fisheries projections
fish_key <- data_orig %>% 
  select(country, iso3) %>% 
  unique()

# Align keys
cntry_key <- pop_key %>% 
  full_join(fish_key, by="iso3")

# Steps required to harmonize datasets:

# Do the following to the fisheries projections:
# 1. Merge "Norway" and "Svalbard & Jan Mayen" into "Norway"
# 2. Merge "United Kingdom" and "Pitcairn Islands"
# 3. Merge "United Kingdom" and "British Indian Ocean Territory"
# 4. Merge "Australia" and "Norfolk Island" into "Australia"
# 5. Relabel "China" as "China, Hong Kong, Macau"
# 6. Relabel "Others nei" to "Other coastal nations"

# Do the following to the population projections:
# 1. Merge "Guam" (GUM) and "Northern Mariana Islands" (MNP) into "Northern Mariana Islands and Guam" (MNP/GUM)
# 2. Merge "China (CHN)", "Hong Kong SAR China" (HKG), and "Macau SAR China" (MAC) into "China, Hong Kong, Macau" (CHN/HKG/MAC)
# 3. Merge "United Kingdom", "Channel Islands", and "Isle of Man" into "United Kingdom"

# 4. Merge into "Other coastal nations": "Bosnia & Herzegovina", "Caribbean Netherlands", 
#   "Ethiopia", "French Southern Territories", "Jordan", "Netherlands Antilles", 
#   "Palestinian Territories", "Saint Martin (French part)", "St. Barthélemy"

# Other coastal nations
other_names <- c("Bosnia & Herzegovina", "Caribbean Netherlands", "Ethiopia", 
                 "Jordan",  "Palestinian Territories", "Saint Martin (French part)", "St. Barthélemy")
other_names[!other_names %in% pop_key$country]
other_isos <- pop_key$iso3[pop_key$country%in%other_names]


# Format population data for merge
pop <- pop_orig %>% 
  # Recode countries for upwards aggregation
  mutate(country=recode(country, 
                        "Guam"="Northern Mariana Islands and Guam", 
                        "Northern Mariana Islands"="Northern Mariana Islands and Guam",
                        "China"="China, Hong Kong, Macau",
                        "Hong Kong SAR China"="China, Hong Kong, Macau",
                        "Macau SAR China"="China, Hong Kong, Macau",
                        "Channel Islands"="United Kingdom", 
                        "Isle of Man"="United Kingdom"),
         country=ifelse(country%in%other_names, "Other coastal nations", country)) %>% 
  # Recode ISO3 for upwards aggregation
  mutate(iso3=recode(iso3, 
                    "GUM"="MNP/GUM", 
                    "MNP"="MNP/GUM",
                    "CHN"="CHN/HKG/MAC",
                    "HKG"="CHN/HKG/MAC",
                    "MAC"="CHN/HKG/MAC",
                    "IMN"="GBR"), 
         iso3=ifelse(country=="United Kingdom", "GBR", iso3), # this is to get the Channel Islands reclass an ISO
         iso3=ifelse(iso3%in%other_isos, "Other", iso3)) %>% 
  # Calculate new population totals
  ungroup() %>% 
  group_by(source, country, iso3, year) %>% 
  summarize(pop_size_05perc=sum(pop_size_05perc),
         pop_size_20perc=sum(pop_size_20perc),
         pop_size_50perc=sum(pop_size_50perc),
         pop_size_80perc=sum(pop_size_80perc),
         pop_size_95perc=sum(pop_size_95perc)) %>% 
  ungroup()

# Check to make sure this worked
pop_check <- pop %>% 
  group_by(iso3, year) %>% 
  summarize(n=n()) %>% 
  filter(n>1)

# Format fisheries projections for merge
data <- data_orig %>% 
  # Calculate national totals
  group_by(rcp, scenario, country, iso3, year) %>% 
  summarize(biomass_mt=sum(biomass_mt_scaled), 
            msy_mt=sum(msy_mt_scaled),
            catch_mt=sum(catch_mt_scaled),
            catch_dhc_mt=sum(dhc_mt_scaled),
            meat_mt=sum(meat_mt_scaled)) %>% 
  ungroup() %>% 
  # Recode countries for upwards aggregation
  mutate(country=recode(country, 
                        "Svalbard & Jan Mayen"="Norway",
                        "Pitcairn Islands"="United Kingdom",
                        "British Indian Ocean Territory"="United Kingdom",
                        "Norfolk Island"="Australia",
                        "China"="China, Hong Kong, Macau",
                        "Others nei"="Other coastal nations")) %>% 
  # Recode ISO3s for uprwards aggregation
  mutate(iso3=recode(iso3, 
                    "SJM"="NOR",
                    "PCN"="GBR",
                    "IOT"="GBR",
                    "NFK"="AUS",
                    "CHN"="CHN/HKG/MAC")) %>% 
  # Calculate national totals
  group_by(rcp, scenario, country, iso3, year) %>% 
  summarize(biomass_mt=sum(biomass_mt), 
            msy_mt=sum(msy_mt),
            catch_mt=sum(catch_mt),
            catch_dhc_mt=sum(catch_dhc_mt),
            meat_mt=sum(meat_mt)) %>% 
  ungroup()


# Countries in population growth
pop_key1 <- pop %>% 
  select(country, iso3) %>% 
  unique()
anyDuplicated(pop_key1$country)
anyDuplicated(pop_key1$iso3)

# Countries in fisheries projections
fish_key1 <- data %>% 
  select(country, iso3) %>% 
  unique()
anyDuplicated(fish_key1$country)
anyDuplicated(fish_key1$iso3)

# Align keys
cntry_key1 <- pop_key1 %>% 
  full_join(fish_key1, by="iso3")

# Add population data for fish data
cdata <- data %>% 
  left_join(pop %>% select(iso3, year, pop_size_50perc)) %>% 
  rename(npeople=pop_size_50perc) %>% 
  # Calculate catch and meat per capita
  mutate(catch_kg_person=catch_dhc_mt*1000/npeople, 
         meat_kg_person=meat_mt*1000/npeople)



# Perform a few checks
# Does every country have a population in 2012 and 2100
check1 <- cdata %>% 
  filter(year%in%c(2012,2100)) %>% 
  select(country, iso3, year, npeople) %>% 
  unique() %>% 
  spread(key="year", value="npeople")

# Check that is has the same amount of catch as first
sum(cdata$catch_mt) == sum(data_orig$catch_mt_scaled)


# Export
saveRDS(cdata, file=file.path(datadir, "Free_etal_2020_national_projections_with_pop_data.Rds"))


