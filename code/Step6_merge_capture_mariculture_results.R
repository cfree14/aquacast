
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

# Read capture projections
wc_orig <- readRDS("data/capture_projections/data/Free_etal_2020_global_projections_with_pop_data.Rds")
 
# Read mariculture projections
faq_orig <- readRDS(file.path(datadir, "finfish_output_new_costs1.Rds"))
baq_orig <- readRDS(file.path(datadir, "bivalve_output_new_costs1.Rds"))



# Global estimates
################################################################################

# Period key
period_key <- tibble(year=c(2021:2030, 2051:2060, 2091:2100), 
                     period=sort(rep(c("2021-2030", "2051-2060", "2091-2100"), 10)))

# Calculate population growth
pop_global <- wc_orig %>% 
  # Select
  select(year, npeople, npeople05, npeople20, npeople80, npeople95) %>% 
  # Add period
  filter(year%in%c(2020, 2025, 2030,
                   2050, 2055, 2060,
                   2090, 2095, 2100)) %>% 
  unique() %>% 
  # Add period and summarize by period
  mutate(period=ifelse(year<=2030, "2021-2030",
                       ifelse(year<=2060, "2051-2060", "2091-2100"))) %>% 
  group_by(period) %>% 
  summarize(npeople=mean(npeople),
            npeople05=mean(npeople05),
            npeople20=mean(npeople20),
            npeople80=mean(npeople80),
            npeople95=mean(npeople95))

# Calculate global statistics: capture
wc_global <- wc_orig %>% 
  # Add period
  left_join(period_key) %>% 
  filter(!is.na(period)) %>% 
  # Summarize by period
  group_by(rcp, scenario, period) %>% 
  summarize(prod_mt=mean(catch_mt),
            meat_mt=mean(meat_mt)) %>% 
  ungroup %>% 
  # Simplify
  mutate(sector="Capture fisheries",
         scenario=recode(scenario, 
                         "No Adaptation"="Business-as-usual",
                         "Full Adaptation"="Progressive reforms")) %>% 
  select(rcp, scenario, period, sector, prod_mt, meat_mt)

# Calculate global statistics: finfish
faq_global <- faq_orig %>% 
  # Calculate global production
  group_by(rcp, mgmt_scenario, feed_scenario, dev_scenario, period) %>% 
  summarise(prod_mt=sum(prod_mt_yr)) %>% 
  ungroup() %>% 
  # Calculate meat production
  mutate(meat_mt=prod_mt*0.82) %>% 
  # Mark big-picture scenarios
  mutate(scenario=ifelse(mgmt_scenario=="BAU fisheries management" & feed_scenario == "BAU feed use" & dev_scenario == "Current", "Business-as-usual", NA),
         scenario=ifelse(mgmt_scenario=="Reformed fisheries management" & feed_scenario == "Reformed feed use" & dev_scenario == "Proportional", "Progressive reforms", scenario)) %>% 
  # Reduce to BAU or progressive reform
  filter(!is.na(scenario)) %>% 
  # Simplify
  mutate(sector="Finfish mariculture") %>% 
  select(rcp, scenario, period, sector, prod_mt, meat_mt)

# Calculate global statistics: bivalve
baq_global_bau <- baq_orig %>% 
  # Calculate global production
  group_by(rcp, dev_scenario, period) %>% 
  summarise(prod_mt=sum(prod_mt_yr)) %>% 
  ungroup() %>% 
  # Calculate meat production
  mutate(meat_mt=prod_mt*0.17) %>% 
  # Reduce to current development patterns
  filter(dev_scenario=="Current") %>% 
  # Simplify
  mutate(sector="Bivalve mariculture", 
         scenario="Business-as-usual") %>%
  select(rcp, scenario, period, sector, prod_mt, meat_mt)
baq_global_ref <- baq_global_bau %>% 
  mutate(scenario="Progressive reforms")
baq_global <- bind_rows(baq_global_bau, baq_global_ref)

# Merge sectors
gdata <- bind_rows(wc_global, faq_global, baq_global) %>% 
  # Add population growth
  left_join(pop_global, by="period") %>% 
  # Calculate meat per capita
  mutate(meat_kg_person=meat_mt*1000/npeople,
         meat_kg_person05=meat_mt*1000/npeople05,
         meat_kg_person20=meat_mt*1000/npeople20,
         meat_kg_person80=meat_mt*1000/npeople80,
         meat_kg_person95=meat_mt*1000/npeople95)

# Export
saveRDS(gdata, file=file.path(datadir, "global_capture_mariculture_output_merged_new_costs1.Rds"))


# Plot data
################################################################################

g <- ggplot(gdata, aes(x=period, y=meat_kg_person, fill=sector)) +
  facet_grid(rcp~scenario) +
  geom_bar(stat="identity") +
  labs(x="Period", y="Edible meat per capita (kg/person)")
g




















