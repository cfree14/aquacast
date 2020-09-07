
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
wc_orig <- readRDS("data/capture_projections/data/Free_etal_2020_national_projections_with_pop_data.Rds")
 
# Read mariculture projections
faq_orig <- readRDS(file.path(datadir, "finfish_output.Rds"))
faq_orig_opt <- readRDS(file.path(datadir, "finfish_output_optimum.Rds"))
baq_orig <- readRDS(file.path(datadir, "bivalve_output.Rds"))

# Read historical data
hist_orig <- readRDS(file.path(datadir, "FAO_1950_2018_wc_aq_seafood_per_capita_national.Rds"))

# Read EEZ key
eez_key <- read.csv(file.path("data/eezs", "eezs_v10_key.csv"), as.is=T) %>% 
  select(ter1_iso, area_sqkm) %>% 
  rename(iso3=ter1_iso) %>% 
  group_by(iso3) %>% 
  summarize(area_sqkm=sum(area_sqkm)) %>% 
  ungroup()


# Build data
################################################################################

# Historical data
###################################

# Build today's values
hist <- hist_orig %>% 
  # Recent year
  filter(year==max(year)) %>% 
  # Sum across sectors
  group_by(country, iso3, npeople) %>% 
  summarize(prod_mt=sum(prod_mt),
            meat_mt=sum(meat_mt),
            meat_kg_person_2017=sum(meat_kg_person)) %>% 
  ungroup()


# Population data
###################################

period_key_pop <- tibble(year=c(2020:2030, 2050:2060, 2090:2100),
                     period=sort(rep(c("2021-2030", "2051-2060", "2091-2100"), 11)))

pop_nat <- wc_orig %>% 
  select(country, iso3, year, npeople) %>% 
  unique() %>% 
  left_join(period_key_pop) %>% 
  filter(!is.na(period) & !is.na(npeople)) %>% 
  group_by(country, iso3, period) %>% 
  summarize(npeople=mean(npeople)) %>% 
  ungroup()

# Projected data
###################################

# Period key
period_key <- tibble(year=c(2021:2030, 2051:2060, 2091:2100),
                     period=sort(rep(c("2021-2030", "2051-2060", "2091-2100"), 10)))

# Calculate national statistics: capture
wc_nat <- wc_orig %>% 
  # Add period
  left_join(period_key) %>% 
  filter(!is.na(period)) %>% 
  # Summarize by period
  group_by(rcp, scenario, country, iso3, period) %>% 
  summarize(prod_mt=mean(catch_mt, na.rm=T),# mean is right because you're taking mean by period
            meat_mt=mean(meat_mt, na.rm=T)) %>% 
  ungroup() %>% 
  # Simplify
  mutate(sector="Capture fisheries",
         scenario=recode(scenario, 
                         "No Adaptation"="Business-as-usual",
                         "Full Adaptation"="Progressive reforms")) %>% 
  select(rcp, scenario, country, iso3, sector, period, prod_mt, meat_mt)


# Calculate national statistics: finfish AQ
faq_nat <- bind_rows(faq_orig, faq_orig_opt) %>% 
  # Add period
  left_join(period_key) %>% 
  filter(!is.na(period)) %>% 
  # Summarize by period
  group_by(rcp, mgmt_scenario, feed_scenario, dev_scenario, ter1_iso, ter1_name, period) %>% 
  summarize(prod_mt=sum(prod_mt_yr, na.rm=T), 
            ncells=n()) %>% 
  ungroup() %>% 
  # Mark big-picture scenarios
  mutate(scenario=ifelse(mgmt_scenario=="BAU fisheries management" & feed_scenario == "BAU feed use", "Business-as-usual", NA),
         scenario=ifelse(mgmt_scenario=="Reformed fisheries management" & feed_scenario == "Reformed feed use", "Progressive reforms", scenario)) %>% 
  # Reduce to BAU or progressive reform
  filter(!is.na(scenario) & dev_scenario %in% c("Current", "Proportional", "Need-based", "Optimum")) %>% 
  # Calculate meat production
  mutate(meat_mt=prod_mt*0.87) %>% 
  # Simplify
  rename(country=ter1_name, iso3=ter1_iso) %>% 
  mutate(sector="Finfish mariculture") %>% 
  select(rcp, scenario, dev_scenario, country, iso3, sector, period, prod_mt, meat_mt, ncells) %>% 
  # Calculate area
  mutate(area_dev_sqkm=ncells*100) %>% 
  left_join(eez_key) %>% 
  rename(area_eez_sqkm=area_sqkm) %>% 
  mutate(eez_prop=area_dev_sqkm/area_eez_sqkm)

# Calculate national statistics: finfish AQ
baq_nat_bau1 <- baq_orig %>% 
  # Add period
  left_join(period_key) %>% 
  filter(!is.na(period)) %>% 
  # Exclude rational development scenario
  filter(dev_scenario %in% c("Current", "Proportional", "Need-based")) %>% 
  # Summarize by period
  group_by(rcp, dev_scenario, ter1_iso, ter1_name, period) %>% 
  summarize(prod_mt=sum(prod_mt_yr),
            ncells=n()) %>% 
  ungroup() %>% 
  # Calculate meat production
  mutate(meat_mt=prod_mt*0.17) %>% 
  # Simplify
  rename(country=ter1_name, iso3=ter1_iso) %>% 
  mutate(sector="Bivalve mariculture",
         scenario="Business-as-usual") %>% 
  select(rcp, scenario, dev_scenario, country, iso3, sector, period, prod_mt, meat_mt, ncells) %>% 
  # Calculate area
  mutate(area_dev_sqkm=ncells*100) %>% 
  left_join(eez_key) %>% 
  rename(area_eez_sqkm=area_sqkm) %>% 
  mutate(eez_prop=area_dev_sqkm/area_eez_sqkm)
baq_nat_bau2 <- baq_nat_bau1 %>% 
  filter(dev_scenario=="Need-based") %>% 
  mutate(dev_scenario="Optimum")
baq_nat_bau <- bind_rows(baq_nat_bau1, baq_nat_bau2)
baq_nat_ref <- baq_nat_bau %>% 
  mutate(scenario="Progressive reforms")
baq_nat <- bind_rows(baq_nat_bau, baq_nat_ref)

# Multiply WC to have for each dev scenario
wc_nat_use <- purrr::map_df(c("Current", "Proportional", "Need-based", "Optimum"), function(x) {
  
  wc_nat_dev <- wc_nat %>% 
    mutate(dev_scenario=x) %>% 
    select(rcp, scenario, dev_scenario, country, iso3, sector, period, prod_mt, meat_mt)
  
})

# Intermediate dataset
# Merge sectors and calculate sums
data1 <- bind_rows(wc_nat_use, faq_nat, baq_nat) %>% 
  # Summarize across sectors
  group_by(rcp, scenario, dev_scenario, country, iso3, period) %>% 
  summarize(prod_mt=sum(prod_mt, na.rm=T), # if problems, remove this
            meat_mt=sum(meat_mt, na.rm=T)) %>% 
  ungroup() %>% 
  # Add population size
  left_join(pop_nat %>% select(iso3, period, npeople), by=c("iso3", "period")) %>% 
  # Calculate per capita meat production
  mutate(meat_kg_person=meat_mt*1000/npeople) %>% 
  # Add initial meat/person
  left_join(hist %>% select(iso3, meat_kg_person_2017), by="iso3") %>% 
  # Calculate difference
  mutate(meat_kg_person_diff=meat_kg_person - meat_kg_person_2017) %>% 
  # Cap difference for plotting
  mutate(meat_kg_person_diff_cap=pmin(10, meat_kg_person_diff) %>% pmax(., -10)) %>% 
  # Formatting
  mutate(dev_scenario=factor(dev_scenario, levels=c("Current", "Proportional", "Need-based", "Optimum")))

# Final dataset
# RCP, Scenario, dev scenario, country, iso3, period, delta SF
data2 <- data1 %>% 
  filter(period=="2091-2100") %>% 
  filter(!is.na(meat_kg_person_diff)) %>% 
  group_by(rcp, scenario, dev_scenario) %>% 
  summarize(npos=sum(meat_kg_person_diff>0),
            ntotal=n()) %>% 
  mutate(prop=npos/ntotal)


# Export data
save(wc_nat_use, faq_nat, baq_nat, data2, file=file.path(datadir, "national_capture_mariculture_output_merged.Rds"))


# Plot data
################################################################################

# Proportions
g1 <- ggplot(data2, aes(x=scenario, y=prop, fill=dev_scenario)) +
  facet_wrap(~rcp, ncol=4) +
  geom_bar(stat="identity", position="dodge") +
  # Labels
  labs(x="Policy scenario", y="Percent of coastal nations\nwith increasing per capita seafood supplies") +
  # Legends
  scale_fill_discrete(name="Development scenario") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  # Theme
  theme_bw() +
  theme(legend.position = "bottom")
g1


# Area data
adata <- bind_rows(faq_nat, baq_nat) %>% 
  filter(scenario=="Progressive reforms") %>% 
  mutate(eez_prop_cap=pmin(eez_prop, 1))

# Plot 
log_breaks <- c(0, 0.00025, 0.0005, 0.001, 0.0025, 0.005, 0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 1)
log_labels <- paste0(log_breaks*100, "%")
g2 <- ggplot(adata, aes(x=dev_scenario, y=eez_prop_cap, fill=sector)) +
  geom_boxplot(outlier.size = 0.5) +
  facet_wrap(~rcp, ncol=4) + 
  # Axes
  scale_y_continuous(trans="log2", breaks=log_breaks , labels = log_labels) +
  # Legend
  scale_fill_manual(name="", values=c("lightblue", "salmon")) +
  # Labels
  labs(x="Development scenarion", y="Percent of EEZ developed") +
  # Theme
  theme_bw() +
  theme(legend.position = "bottom")
g2






