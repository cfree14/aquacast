

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


# Time series
################################################################################

# Theme
my_theme <- theme(axis.text=element_text(size=8),
                  axis.title=element_text(size=10),
                  axis.title.x=element_blank(),
                  plot.title=element_text(size=11),
                  legend.text = element_text(size=7),
                  legend.title = element_text(size=9),
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"),
                  axis.text.x = element_text(angle = 90, vjust = 0.5))

# All
#########################

# Format
sdata1 <- data %>% 
  group_by(environment, year) %>% 
  summarize(quantity_mt=sum(quantity_mt, na.rm=T),
            value_usd_t=sum(value_usd_t, na.rm=T))

# Plot
g1 <- ggplot(sdata1, aes(x=year, y=quantity_mt/1e6, fill=environment)) +
  geom_area() +
  scale_fill_discrete(name="Environment") +
  scale_x_continuous(breaks=seq(1950,2020,10)) +
  labs(x="", y="Production (millions of mt)", title="A. All aquaculture") +
  theme_bw() + my_theme +
  theme(legend.position = c(0.02,0.7), legend.justification="left")
g1

# Marine/brackish
#########################

# Format
sdata2 <- data %>% 
  filter(environment %in% c("Marine", "Brackishwater")) %>% 
  group_by(major_group, year) %>% 
  summarize(quantity_mt=sum(quantity_mt, na.rm=T),
            value_usd_t=sum(value_usd_t, na.rm=T)) %>% 
  ungroup() %>% 
  mutate(major_group=recode(major_group,
                            "Amphibia, reptilia"="Reptiles and amphibians",
                            "Crustacea"="Crustaceans",            
                            "Invertebrata aquatica"="Other aquatic invertebrates",
                            "Mollusca"="Molluscs",            
                            "Pisces"="Finfish",             
                            "Plantae aquaticae"="Aquatic plants"))

# Plot
g2 <- ggplot(sdata2, aes(x=year, y=quantity_mt/1e6, fill=major_group)) +
  geom_area() +
  scale_fill_discrete(name="Type") +
  scale_x_continuous(breaks=seq(1950,2020,10)) +
  labs(x="", y="Production (millions of mt)", title="B. Marine/brackish aquaculture") +
  theme_bw() + my_theme +
  theme(legend.position = c(0.02,0.55), legend.justification="left")
g2

# Marine/brackish finfish/molluscs
#########################

# Format
sdata3 <- data %>% 
  filter(environment %in% c("Marine", "Brackishwater") & 
           major_group %in% c("Pisces", "Mollusca")) %>% 
  mutate(isscaap=ifelse(grepl("Miscellaneous", isscaap) | isscaap=="Marine fishes not identified", 
                        "Miscellaneous fishes", isscaap)) %>% 
  group_by(major_group, isscaap, year) %>% 
  summarize(quantity_mt=sum(quantity_mt, na.rm=T),
            value_usd_t=sum(value_usd_t, na.rm=T)) %>% 
  ungroup() %>% 
  mutate(major_group=recode(major_group, 
                            "Mollusca"="Molluscs",            
                            "Pisces"="Finfish"))

# Plot
g3a <- ggplot(filter(sdata3, major_group=="Finfish"), 
              aes(x=year, y=quantity_mt/1e6, fill=isscaap)) +
  geom_area() +
  scale_fill_discrete(name="Type") +
  scale_x_continuous(breaks=seq(1950,2020,10)) +
  labs(x="", y="Production (millions of mt)", title="C. Marine/brackish finfish aquaculture") +
  theme_bw() + my_theme +
  theme(legend.position = "none")
g3a

# Plot
g3b <- ggplot(filter(sdata3, major_group=="Molluscs"), 
              aes(x=year, y=quantity_mt/1e6, fill=isscaap)) +
  geom_area() +
  scale_fill_discrete(name="Type") +
  scale_x_continuous(breaks=seq(1950,2020,10)) +
  labs(x="", y="Production (millions of mt)", title="D. Marine/brackish mollusc aquaculture") +
  theme_bw() + my_theme +
  theme(legend.position = "none")
g3b

# Merge time series
g <- grid.arrange(g1, g2, g3a, g3b, 
                  layout_matrix=matrix(c(1,1,2,2,3,4), ncol=2, byrow=T))

# Export plot
ggsave(g, filename=file.path(plotdir, "figure_fao_aq_time_series.png"), 
       width=6.5, height=8, units="in", dpi=600)


# Time series
################################################################################


# Format
range(data_orig$year)
data <- data_orig %>%
  filter(year %in% 2013:2017) %>%
  mutate(continent=countrycode(iso3, origin="iso3c", destination="continent")) %>%
  filter(continent=="Africa") %>%
  group_by(country, environment) %>%
  summarize(quantity_mt=sum(quantity_mt, na.rm=T))


# Production maps
################################################################################

# Get world
world <- rnaturalearth::ne_countries(scale="large", type="countries", returnclass="sf")

# Build data
range(data$year)
yrs <- 2013:2017
aqprod <- data %>% 
  # Recent (2013-17) marine/brackish finfish/molluscs
  filter(environment%in%c("Marine", "Brackishwater") & 
           major_group %in% c("Pisces", "Mollusca") & year%in%yrs) %>% 
  # Calculate average
  group_by(country, iso3, major_group) %>% 
  summarize(prod_mt=mean(quantity_mt, na.rm=T),
            prod_nyr=sum(!is.na(quantity_mt)))

# Aquaculture 
aqprod_sf <- world %>% 
  left_join(aqprod, by=c("sov_a3"="iso3"))

# This isn't working correctly.

# Plot
# g <- ggplot() +
#   geom_sf(aqprod_sf, mapping=aes(fill=prod_mt)) +
#   facet_wrap(~ major_group, ncol=1) +
#   theme_bw()
# 
# # Export plot
# ggsave(g, filename=file.path(plotdir, "figure_fao_aq_prod_maps.png"), 
#        width=6.5, height=6.5, units="in", dpi=600)


# Country-level time series
################################################################################

# Theme
my_theme2 <- theme(axis.text=element_text(size=8),
                   axis.title=element_text(size=10),
                   axis.title.x=element_blank(),
                   legend.text = element_text(size=8),
                   legend.title = element_blank(),
                   legend.position="bottom",
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   axis.text.x = element_text(angle = 90, vjust = 0.5))


# Format data
data_cntry <- data %>%
  group_by(country, environment, year) %>%
  summarize(quantity_mt=sum(quantity_mt, na.rm=T))

# Packages
# devtools::install_github("guiastrennec/ggplus")
library(ggplus)

# Plot all
p <- ggplot(data_cntry, aes(x=year, y=quantity_mt/1e3, fill=environment)) +
  labs(x="", y="Production (thousands of mt)") +
  geom_area() +
  scale_x_continuous(breaks=seq(1950,2020,10)) +
  scale_fill_discrete(name="") +
  theme_bw() + my_theme2

# Creat multi-page PDF
pdf(file.path(plotdir, "appendix_fao_aq_time_series_by_country.pdf"), width=8.5, height=11)
gg10 <- facet_multiple(plot=p, facets="country", ncol = 4, nrow = 5, scales="free")
dev.off()







