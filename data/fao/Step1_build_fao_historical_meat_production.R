


# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)
library(countrycode)

# Directories
outdir <- "/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/blue-paper-2/data/output/processed"

# Read data
wc_orig <- readRDS("/Users/cfree/Dropbox/Chris/UCSB/data/fao/capture/processed/1950_2017_fao_landings_data.Rds")
aq_orig <- readRDS("/Users/cfree/Dropbox/Chris/UCSB/data/fao/aquaculture/processed/1950_2017_fao_aquaculture_data.Rds")
pop_orig <- readRDS("data/pop_growth/data/WB_UN_1960_2100_human_population_by_country.Rds")

# Read conversion factors
conv_factors <- readxl::read_excel("data/nutrition/isscaap_conversion_factors.xlsx")


# Plot national data
################################################################################

# Capture data
wc_nat <- wc_orig %>% 
  # Reduce to marine fisheries
  filter(area_type=="marine" & units=="t") %>% 
  filter(isscaap %in% conv_factors$isscaap) %>% 
  # Add conversion factor and calculate edible meat
  rename(prod_mt=quantity) %>% 
  left_join(conv_factors %>% select(isscaap, catch2meat)) %>% 
  mutate(meat_mt=prod_mt*catch2meat) %>% 
  # Summarize totals by country
  group_by(country_use, iso3_use, year) %>% 
  summarize(prod_mt=sum(prod_mt),
            meat_mt=sum(meat_mt)) %>% 
  rename(country=country_use, iso3=iso3_use) %>% 
  ungroup() %>% 
  # Add sector label
  mutate(sector="Capture fisheries") %>% 
  select(country, iso3, sector, year, prod_mt, meat_mt)

# Mariculture data
aq_nat <- aq_orig %>% 
  # Reduce to marine/brackish
  filter(environment %in% c("Marine", "Brackishwater")) %>% 
  # Reduce to finfish/bivalves
  filter(major_group %in% c("Pisces", "Mollusca")) %>% 
  # Remove a few freshwater species and non-finfish
  filter(!isscaap %in% c("Freshwater molluscs", "Miscellaneous freshwater fishes", "Squids, cuttlefishes, octopuses")) %>% 
  # Add conversion factor and calculate edible meat
  mutate(catch2meat=ifelse(major_group=="Pisces", 0.87, 0.17)) %>% 
  rename(prod_mt=quantity_mt) %>% 
  mutate(meat_mt=prod_mt*catch2meat) %>% 
  # Summarize totals by country
  group_by(country_use, iso3_use, major_group, year) %>% 
  summarize(prod_mt=sum(prod_mt),
            meat_mt=sum(meat_mt)) %>% 
  ungroup() %>% 
  # Clean up
  rename(sector=major_group, country=country_use, iso3=iso3_use) %>% 
  mutate(sector=recode(sector, 
                       "Pisces"="Finfish mariculture", 
                       "Mollusca"="Bivalve mariculture")) %>% 
  select(country, iso3, sector, year, prod_mt, meat_mt)

# Population data
pop_nat <- pop_orig %>% 
  # Historical data
  filter(source=="World Bank historical") %>% 
  # Simplify
  select(country, iso3, year, pop_size_50perc) %>% 
  rename(npeople=pop_size_50perc)

# Merge data
data_nat <- bind_rows(wc_nat, aq_nat) %>% 
  # Factor sectors
  mutate(sector=factor(sector, levels=c("Capture fisheries", "Finfish mariculture", "Bivalve mariculture"))) %>% 
  # Add number of people
  left_join(pop_nat) %>% 
  # Calculate per capita meat availability
  mutate(meat_kg_person=meat_mt*1000/npeople) %>% 
  # Filter
  filter(year>=1960)

# Inspect completeness
freeR::complete(data_nat)
  
# Export data
saveRDS(data_nat, file=file.path(outdir, "FAO_1950_2018_wc_aq_seafood_per_capita_national.Rds"))


# Build  global data
################################################################################

# Are all converison factor ISSCAAPS in FAO data?
sum(!conv_factors$isscaap %in% wc_orig$isscaap) # yes

# Capture data
wc <- wc_orig %>% 
  filter(area_type=="marine" & units=="t") %>% 
  filter(isscaap %in% conv_factors$isscaap) %>% 
  group_by(year, isscaap) %>% 
  summarize(prod_mt=sum(quantity)) %>% 
  ungroup() %>% 
  left_join(conv_factors %>% select(isscaap, catch2meat)) %>% 
  mutate(meat_mt=prod_mt*catch2meat) %>% 
  group_by(year) %>% 
  summarize(prod_mt=sum(prod_mt),
            meat_mt=sum(meat_mt)) %>% 
  ungroup() %>% 
  mutate(sector="Capture fisheries") %>% 
  select(sector, year, prod_mt, meat_mt)


# Mariculture data
aq <- aq_orig %>% 
  # Reduce to marine/brackish
  filter(environment %in% c("Marine", "Brackishwater")) %>% 
  # Reduce to finfish/bivalves
  filter(major_group %in% c("Pisces", "Mollusca")) %>% 
  # Remove a few FW species and non-finfish
  filter(!isscaap %in% c("Freshwater molluscs", "Miscellaneous freshwater fishes", "Squids, cuttlefishes, octopuses")) %>% 
  # Summarize
  group_by(major_group, isscaap, year) %>% 
  summarize(prod_mt=sum(quantity_mt)) %>% 
  ungroup() %>% 
  # Add conversion factor
  left_join(conv_factors %>% select(isscaap, catch2meat)) %>%
  # Fill in missing conversion factors
  mutate(catch2meat=ifelse(isscaap %in% c("River eels", "Tilapias and other cichlids", "Marine fishes not identified", 
                                          "Carps, barbels and other cyprinids", "Sturgeons, paddlefishes"), 0.87, catch2meat),
         catch2meat=ifelse(isscaap=="Miscellaneous marine molluscs", 0.17, catch2meat)) %>% 
  # Convert edible meat
  mutate(meat_mt=prod_mt*catch2meat) %>% 
  # Summarize
  group_by(major_group, year) %>% 
  summarize(prod_mt=sum(prod_mt),
            meat_mt=sum(meat_mt)) %>% 
  ungroup() %>% 
  # Clean up
  rename(sector=major_group) %>% 
  mutate(sector=recode(sector, 
                       "Pisces"="Finfish mariculture", 
                       "Mollusca"="Bivalve mariculture"))

# Population data
pop <- pop_orig %>% 
  group_by(year) %>% 
  summarize(npeople=sum(pop_size_50perc)) %>% 
  ungroup()

# Merge
data <- bind_rows(wc, aq) %>% 
  # Factor sectors
  mutate(sector=factor(sector, levels=c("Capture fisheries", "Finfish mariculture", "Bivalve mariculture"))) %>% 
  # Add number of people
  left_join(pop) %>% 
  # Calculate per capita meat availability
  mutate(meat_kg_person=meat_mt*1000/npeople)

# Plot check
g <- ggplot(data, aes(x=year, y=meat_kg_person, fill=sector)) +
  geom_area() +
  labs(x="", y="Edible meat (millions mt)") +
  theme_bw()
g

# Export data
saveRDS(data, file=file.path(outdir, "FAO_1950_2018_wc_aq_seafood_per_capita.Rds"))










