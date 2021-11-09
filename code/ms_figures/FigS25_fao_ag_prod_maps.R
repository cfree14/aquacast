

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(cowplot)
library(grid)
library(gridExtra)
library(countrycode)
library(tidyverse)
library(rnaturalearth)

# Read data
data <- readRDS("/Users/cfree/Dropbox/Chris/UCSB/data/fao/aquaculture/processed/1950_2017_fao_aquaculture_data.Rds")
plotdir <- "figures"
outdir <- "data/feed_params/processed"

# Read coastlines
coastlines <- read.csv("data/coastlines/coastlines_wikipedia.csv", as.is=T)

# Get world
world <- rnaturalearth::ne_countries(scale="small", type="countries", returnclass="sf") %>% 
  mutate(iso3_use=countrycode(name_long, "country.name", "iso3c"))

# Build data
################################################################################

# Map datasets
#########################

# Years
range(data$year)
yrs <- 2013:2017

# Build data
aqprod <- data %>% 
  # Recent (2013-17) marine/brackish finfish/molluscs
  filter(environment%in%c("Marine", "Brackishwater") & 
           major_group %in% c("Pisces", "Mollusca") & year%in%yrs) %>% 
  # Calculate average
  group_by(country_use, iso3_use, major_group) %>% 
  summarize(prod_mt=mean(quantity_mt, na.rm=T),
            profit_usd=mean(value_usd_t)*1000) %>% 
  ungroup()

# Number of producing countries
nproducers <- aqprod %>% 
  group_by(major_group) %>% 
  summarise(n=sum(prod_mt!=0))

# Export country average
write.csv(aqprod, file=file.path(outdir, "FAO_2013_2017_maq_prod_averages_by_country.csv"), row.names=F)

# Aquaculture 
faq_sf <- world %>% 
  left_join(filter(aqprod, major_group=="Pisces"), by=c("iso3_use"="iso3_use"))
baq_sf <- world %>% 
  left_join(filter(aqprod, major_group=="Mollusca"), by=c("iso3_use"="iso3_use"))


# Coastline datasets
#########################

# Coastline FAQ dataset
coastlines_faq <- coastlines %>% 
  # Simplify
  select(country, iso3, km_wri) %>% 
  rename(coast_km=km_wri) %>% 
  # Reduce to countries with coasts
  filter(coast_km > 0 & !is.na(iso3)) %>% 
  # Add AQ production
  left_join(aqprod %>% select(-country_use) %>% filter(major_group=="Pisces"), by=c("iso3"="iso3_use")) %>% 
  # Format sector
  rename(sector=major_group) %>% 
  mutate(sector="Finfish mariculture") %>% 
  # Fill in zeros
  mutate(prod_mt=replace_na(prod_mt, replace=list(0)) %>% as.numeric(),
         profit_usd=replace_na(profit_usd, replace=list(0)) %>% as.numeric()) %>% 
  # Categorize
  mutate(catg=ifelse(prod_mt < 10, "<10 mt", "≥10 mt"))

sum(coastlines_faq$prod_mt>0) / nrow(coastlines_faq)
median(coastlines_faq$coast_km[coastlines_faq$prod_mt==0])

# Coastline FAQ dataset
coastlines_baq <- coastlines %>% 
  # Simplify
  select(country, iso3, km_wri) %>% 
  rename(coast_km=km_wri) %>% 
  # Reduce to countries with coasts
  filter(coast_km > 0 & !is.na(iso3)) %>% 
  # Add AQ production
  left_join(aqprod %>% select(-country_use) %>% filter(major_group=="Mollusca"), by=c("iso3"="iso3_use")) %>% 
  # Format sector
  rename(sector=major_group) %>% 
  mutate(sector="Bivalve mariculture") %>% 
  # Fill in zeros
  mutate(prod_mt=replace_na(prod_mt, replace=list(0)) %>% as.numeric(),
         profit_usd=replace_na(profit_usd, replace=list(0)) %>% as.numeric()) %>% 
  # Categorize
  mutate(catg=ifelse(prod_mt < 10, "<10 mt", "≥10 mt"))

sum(coastlines_baq$prod_mt>0) / nrow(coastlines_baq)
median(coastlines_baq$coast_km[coastlines_baq$prod_mt==0])


# Plot data
################################################################################

# Setup theme
my_theme <- theme(axis.text=element_text(size=6),
                  axis.title=element_text(size=8),
                  legend.text=element_text(size=5),
                  legend.title=element_text(size=7),
                  plot.title=element_text(size=10),
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"))

# Plot finfish map
g1 <- ggplot() +
  geom_sf(faq_sf, mapping=aes(fill=prod_mt/1e3), lwd=0.2) +
  # Labels
  labs(title="Finfish mariculture") +
  # scale_fill_gradientn(name="Production (1000s mt)", colors=RColorBrewer::brewer.pal(9, "Reds"), na.value="grey80") +
  # Legend
  scale_fill_gradientn(name="Production\n(1000s mt)", 
                       trans = "log10", 
                       breaks= c(0.01, 0.1, 1, 10, 100),
                       labels = c("0.01", "0.1", "1", "10", "100"),
                       colors=RColorBrewer::brewer.pal(9, "Reds"), na.value="grey80") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position=c(0.1, 0.45), 
        legend.key.size = unit(0.3, "cm"),
        legend.background = element_rect(fill=alpha('blue', 0)))
g1

# Plot finfish coastline density
g2 <- ggplot(coastlines_faq, aes(x=coast_km, fill=catg)) +
  geom_density(alpha=0.5) +
  # Axis
  scale_x_continuous(trans="log10", 
                     limits = c(1, 1000000),
                     breaks=10^(0:6),
                     labels=c("1", "10", "100", "1,000", "10,000", "100,000", "1,000,000")) +
  # Labels
  labs(x="Coastline length (km)", y="Density", title=" ") +
  # Legend
  scale_fill_discrete("Annual\nproduction") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position=c(0.12, 0.8), 
        legend.key.size = unit(0.4, "cm"),
        legend.background = element_rect(fill=alpha('blue', 0)))
g2

# Plot
g3 <- ggplot() +
  geom_sf(baq_sf, mapping=aes(fill=prod_mt/1e3), lwd=0.2) +
  # Labels
  labs(title="Bivalve mariculture") +
  # Legend
  scale_fill_gradientn(name="Production\n(1000s mt)", 
                       trans = "log10", 
                       breaks= c(0.01, 0.1, 1, 10, 100, 1000),
                       labels = c("0.01", "0.1", "1", "10", "100", "1000"),
                       colors=RColorBrewer::brewer.pal(9, "Blues"), na.value="grey80") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position=c(0.1, 0.45), 
        legend.key.size = unit(0.3, "cm"),
        legend.background = element_rect(fill=alpha('blue', 0)))
g3

# Plot finfish coastline density
g4 <- ggplot(coastlines_baq, aes(x=coast_km, fill=catg)) +
  geom_density(alpha=0.5) +
  # Axis
  scale_x_continuous(trans="log10", 
                     limits = c(1, 1000000),
                     breaks=10^(0:6),
                     labels=c("1", "10", "100", "1,000", "10,000", "100,000", "1,000,000")) +
  # Labels
  labs(x="Coastline length (km)", y="Density", title=" ") +
  # Legend
  scale_fill_discrete("Annual\nproduction") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position=c(0.12, 0.8), 
        legend.key.size = unit(0.4, "cm"),
        legend.background = element_rect(fill=alpha('blue', 0)))
g4

# Merge time series
layout_matrix <- matrix(data=c(1,2,3,4), ncol=2, byrow=T)
g <- grid.arrange(g1, g2, g3, g4, layout_matrix=layout_matrix)

# Export plot
ggsave(g, filename=file.path(plotdir, "FigS25_fao_aq_prod_maps.png"),
       width=6.5, height=4.5, units="in", dpi=600)

