
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)
library(countrycode)

# Directories
plotdir <- "figures"
outdir <- "data/feed_params/processed"

# Read data
data_orig <- readRDS("/Users/cfree/Dropbox/Chris/UCSB/data/fao/aquaculture/processed/1950_2017_fao_aquaculture_data.Rds")

# Setup
################################################################################

# Format data
data <- data_orig %>% 
  # Reduce to marine/brackishwater
  filter(environment!="Freshwater" & !grepl("Inland", area) & major_group=="Pisces") %>% 
  # Add continent
  mutate(continent=countrycode(sourcevar = iso3, origin = "iso3c", destination = "continent"),
         continent=ifelse(country=="Zanzibar", "Africa", continent),
         continent=ifelse(country=="Un. Sov. Soc. Rep.", "Europe", continent),
         continent=ifelse(country=="Yugoslavia SFR", "Europe", continent),
         continent=ifelse(country=="Serbia and Montenegro", "Europe", continent),
         continent=ifelse(country=="Channel Islands", "Europe", continent)) %>% 
  # Add region label
  mutate(region=paste(continent, area, sep=" - "))

# Missing continents
missing <- data %>% 
  filter(is.na(continent)) %>% 
  select(region, area, country, iso3, continent) %>% 
  unique()

# Regions
table(data$region)

# Format data
results <- data %>% 
  group_by(region, continent, area, year) %>% 
  summarize(prod_mt=sum(quantity_mt)) %>% 
  mutate(prod_mt_scaled=scale(prod_mt))

# More results
results1 <- results %>%
  ungroup() %>% 
  filter(year>=2008 & region!="Asia - Atlantic, Northeast") %>% 
  group_by(region, continent, area) %>% 
  summarize(slope=coef(lm(prod_mt_scaled~year))[2])
  
# Plot results
g <- ggplot(results, aes(x=year, y=prod_mt_scaled)) +
  geom_line() +
  geom_smooth(data=filter(results, year>=2008), method='lm', formula = y ~ x, color="red", se=F, size=1) +
  facet_wrap(~region, ncol=4, scales="fixed") +
  labs(x="", y="Production (millions mt)") +
  theme_bw()
g

