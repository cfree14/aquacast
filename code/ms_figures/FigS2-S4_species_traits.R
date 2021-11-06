
# Clear workspace
rm(list = ls())

# Turn off scientific notation
options(scipen=999)

# Setup
################################################################################

# Packages
library(tidyverse)
library(grid)
library(gridExtra)

# Directories
datadir <- "data/species/data"
plotdir <- "figures"

# Read data
load(file.path(datadir, "aquaculture_species_key_20cages.Rdata"))
data_orig <- data
data_full_orig <- data_full

# Format data for plotting
data_full <- data_full_orig %>% 
  mutate(class=recode(class, 
                      "Actinopterygii"="Finfish",
                      "Bivalvia"="Bivalves"), 
         class=factor(class, levels=c("Finfish", "Bivalves")))

# Brackish ISSCAAPs
brackish_isscaaps <- c("Freshwater molluscs", "Miscellaneous diadromous fishes", 
                       "Miscellaneous freshwater fishes", "River eels", "Shads", 
                       "Sturgeons, paddlefishes", "Tilapias and other cichlids")

# Not lonline bivalves
bad_bivalve_isscaaps <- c("Clams, cockles, arkshells", "Pearls, mother-of-pearl, shells", "Scallops, pectens")


# Format data for plotting
data <- data_orig %>% 
  mutate(class=recode(class, 
                      "Actinopterygii"="Finfish",
                      "Bivalvia"="Bivalves"), 
         class=factor(class, levels=c("Finfish", "Bivalves"))) %>% 
  # Remove brackish
  filter(!isscaap %in% c(brackish_isscaaps, bad_bivalve_isscaaps))

# Setup theme
my_theme <- theme(axis.text=element_text(size=7),
                  axis.title=element_text(size=9),
                  plot.title=element_text(size=11),
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"))


mean(data$lines_n, na.rm=T)
table(data$class)

# Harvest densities
################################################################################

# If I maintain current stocking densities, what does that mean for harvest densities?
harv_stats <- data %>% 
  mutate(nstocked=ifelse(class=="Bivalvia", 131200000, 4320000),
         volume=ifelse(class=="Bivalvia", 100*4000, 24*9000),
         harvest_kg_vol=nstocked*harvest_g/1000/volume)

boxplot(harvest_kg_vol ~ class, harv_stats, outline=F)


# Production values
################################################################################

# Set group order
pdata_order <- data %>% 
  group_by(class, isscaap) %>% 
  summarize(revenue_usd_yr_median=median(revenue_usd_yr)) %>% 
  arrange(class, revenue_usd_yr_median)

# Format for plotting
pdata <- data %>% 
  select(class, isscaap, species, revenue_usd_yr, prod_mt_yr, edible_mt_yr) %>% 
  mutate(revenue_usd_yr=revenue_usd_yr/1e6,
         prod_mt_yr=prod_mt_yr/1e3,
         edible_mt_yr=edible_mt_yr/1e3) %>% 
  gather(key="metric", value="value", 4:ncol(.)) %>% 
  mutate(metric=recode(metric, 
                       "edible_mt_yr"="Edible production\n(1000s mt per yr)",
                       "prod_mt_yr"="Total production\n(1000s mt per yr)",
                       "revenue_usd_yr"="Revenues\n(USD millions per yr)"),
         metric=factor(metric, levels=c("Revenues\n(USD millions per yr)",
                                        "Total production\n(1000s mt per yr)",
                                        "Edible production\n(1000s mt per yr)")),
         isscaap=factor(isscaap, levels=pdata_order$isscaap)) 

# Plot prouction values
g <- ggplot(pdata, aes(x=isscaap, y=value, fill=class)) +
  facet_wrap(~metric, scales="free_x", ncol=3) +
  geom_boxplot(lwd=0.2, outlier.size = 0.5) + 
  coord_flip() +
  theme_bw() + my_theme +
  theme(axis.title=element_blank(),
        legend.title = element_blank(),
        legend.position = "none")
g

# Export plots
ggsave(g, filename=file.path(plotdir, "FigS15_species_harvest_production.png"), 
       width=6.5, height=3.5, units="in", dpi=600)


# Sample size and Von B parameters
################################################################################

# Number of orders/families
n_distinct(data$order)
n_distinct(data$family)

# Sample size stats
table(data$class)
nstats <- data %>% 
  group_by(class, isscaap) %>% 
  summarize(n=n()) %>% 
  arrange(class, n) %>% 
  mutate(class_label=recode(class, 
                            "Finfish"="Finfish (n=122)",
                            "Bivalves"="Bivalves (n=22)"),
         class_label=factor(class_label, levels=c("Finfish (n=122)", "Bivalves (n=22)")))
nstats$isscaap_order <- factor(nstats$isscaap, level=nstats$isscaap)

# Plot sample size stats
g1 <- ggplot(nstats, aes(x=isscaap_order, y=n, fill=class_label)) +
  geom_bar(stat="identity") +
  coord_flip() +
  labs(x="", y="Number of species", tag="A") +
  scale_fill_discrete(name="") +
  theme_bw() + my_theme +
  theme(legend.position = c(0.8, 0.3))
g1

# Linf (cm) density
g2 <- ggplot(data, aes(x=linf_cm, fill=class)) +
  geom_density(alpha=0.8) +
  labs(x="Asymptotic length (Linf, cm)", y="Density", tag="B") +
  theme_bw() + my_theme +
  theme(legend.position="none")
g2

# K density
g3 <- ggplot(data, aes(x=k, fill=class)) +
  geom_density(alpha=0.8) +
  labs(x="Growth coefficient (K)", y="Density", tag="C") +
  theme_bw() + my_theme +
  theme(legend.position="none")
g3

# Natural mortality density
g4 <- ggplot(data, aes(x=m, fill=class)) +
  geom_density(alpha=0.8) +
  labs(x="Natural mortality (M)", y="Density", tag="D") +
  theme_bw() + my_theme +
  theme(legend.position="none")
g4

# Assemble plots
g <- grid.arrange(g1, g2, g3, g4, layout_matrix=matrix(c(1,1,1,2,3,4), ncol=3, byrow=T))

# Export plots
ggsave(g, filename=file.path(plotdir, "FigS2_species_growth_params.png"), 
       width=6.5, height=4.5, units="in", dpi=600)


# Trait sources
################################################################################

# Reshape for plotting
tdata <- data %>% 
  select(class, species, a_source, b_source, linf_source, k_source, m_source) %>% 
  mutate(m_source=ifelse(m_source=="FishLife", m_source, paste(gsub("-", " ", m_source), "average", sep="-"))) %>% 
  gather(key="parameter", value="source", 3:ncol(.)) %>% 
  mutate(parameter=recode(parameter, 
                   "a_source"="LW a",
                   "b_source"="LW b",
                   "linf_source"= "Linf (cm)",
                   "k_source"="K",
                   "m_source"="M"),
         class=recode(class, 
                      "Finfish"="Finfish (n=122)",
                      "Bivalves"="Bivalves (n=22)"),
         source=factor(source, levels=c("FishLife", "Gentry et al. 2017", "FB species-average", "FB genus-average", "FB family-average")))
  

# Plot
g <- ggplot(tdata, aes(x=parameter, fill=source)) +
  geom_bar(position="fill") +
  scale_fill_discrete(name="Source") +
  labs(x="Life history trait", y="Proportion") +
  facet_wrap(~class) +
  theme_bw() + my_theme
g  

# Export figure
ggsave(g, filename=file.path(plotdir, "FigS3_species_trait_sources.png"), 
       width=6.5, height=3, units="in", dpi=600)


# Environmental tolerances
################################################################################

# Identify plotting rank
spp_order_key <- data %>% 
  arrange(class, sst_c_min, sst_c_max) %>% 
  group_by(class) %>% 
  mutate(rank=1:n()) %>% 
  select(class, species, rank, sst_c_min, sst_c_max) %>% 
  ungroup()

# Reshape data for plotting
edata <- data %>% 
  # Reduce
  select(class, species, sst_c_min:sal_psu_max) %>% 
  # Wide-to-long
  gather(key="param", value="value", 3:ncol(.)) %>% 
  # Add meta-data
  mutate(range=ifelse(grepl("q", param), "Preferred", "Tolerated"),
         range=factor(range, levels=c("Tolerated", "Preferred")),
         variable=ifelse(grepl("sst", param), "Temperature (°C)", "Salinity (psu)"),
         variable=factor(variable, levels=c("Temperature (°C)", "Salinity (psu)"))) %>% 
  # Arrange
  select(class, species, param, variable, range, value) %>% 
  # Order by SST
  mutate(species=factor(species, levels=spp_order_key$species))

# Plot environmental tolerances
g <- ggplot(edata, aes(x=value, y=species, color=class, alpha=range)) +
  facet_grid(class ~ variable, scales="free", space="free_y") +
  geom_line() +
  # Axis labels and limits
  labs(x="Environmental tolerance", y="Mariculture species") +
  scale_alpha_manual(values = c(0.3, 1)) + # 1=solid, 0=transparent
  expand_limits(x=0) +
  # Theme
  theme_bw() + 
  theme(axis.text.y=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        legend.position = "none")
g

# Export figure
ggsave(g, filename=file.path(plotdir, "FigS4_species_envi_tolerances.png"), 
       width=6.5, height=6, units="in", dpi=600)


# Prices
################################################################################

# Identify plotting rank
spp_order_key <- data_full %>% 
  select(class, isscaap, price_usd_mt_isscaap) %>% 
  unique() %>% 
  arrange(desc(class), price_usd_mt_isscaap)

# Plot prices
g <- ggplot(data, aes(x=factor(isscaap, levels=spp_order_key$isscaap), y=price_usd_mt_spp, fill=class)) +
  geom_boxplot() +
  coord_flip() +
  labs(x="", y="Price (USD/mt)") +
  theme_bw() + my_theme +
  scale_fill_discrete(name="") +
  theme(legend.position = c(0.8, 0.2),
        axis.text = element_text(size=8),
        axis.text.x = element_text(size=8),
        axis.text.y = element_text(size=8),
        axis.title = element_text(size=10))
g

# Export plot
ggsave(g, filename=file.path(plotdir, "FigS9_species_prices_by_isscaap.png"), 
       width=6.5, height=3, units="in", dpi=600)


 # Harvest size
################################################################################

# Reshape for plotting
hdata <- data %>% 
  select(class, species, linf_cm, harvest_cm, harvest_g, harvest_yr) %>% 
  gather(key="parameter", value="value", 3:ncol(.)) %>% 
  mutate(parameter=recode(parameter, 
                          "harvest_cm"="Harvest size", 
                          "linf_cm"="Linf"))

# Plot
g <- ggplot(hdata, aes(x=value, fill=class)) +
  geom_density(aes(y=..scaled..)) +
  facet_grid(class ~ parameter, scales="free") +
  theme_bw() + my_theme
g

# Finfish
#############################

# Finfish lengths
g1 <- ggplot(filter(hdata, class=="Finfish" & parameter %in% c("Harvest size", "Linf")), aes(x=value, fill=parameter)) +
  geom_density(aes(y=..scaled..), alpha=0.8) +
  labs(x="Length (cm)", y="", title="Finfish") +
  theme_bw() + my_theme +
  scale_fill_discrete(name="") +
  theme(legend.position=c(0.7,0.8),
        legend.text=element_text(size=6),
        legend.background = element_rect(fill=alpha('blue', 0)))
#g1

# Finfish weights
g2 <- ggplot(filter(hdata, class=="Finfish" & parameter %in% c("harvest_g")), aes(x=value/1000)) +
  geom_density(aes(y=..scaled..), alpha=0.8) +
  labs(x="Harvest weight (kg)", y="") +
  xlim(0,25) +
  theme_bw() + my_theme
#g2

# Finfish ages
g3 <- ggplot(filter(hdata, class=="Finfish" & parameter %in% c("harvest_yr")), aes(x=value)) +
  geom_density(aes(y=..scaled..), alpha=0.8) +
  labs(x="Harvest age (yr)", y="") +
  theme_bw() + my_theme
#g3

# Bivalves
#############################

# Bivalve linf and harvest size
g4 <- ggplot(filter(hdata, class=="Bivalves" & parameter %in% c("Harvest size", "Linf")), aes(x=value, fill=parameter)) +
  geom_density(aes(y=..scaled..), alpha=0.8) +
  labs(x="Length (cm)", y="", title="Bivalves") +
  theme_bw() + my_theme +
  scale_fill_discrete(name="") +
  theme(legend.position=c(0.7,0.8),
        legend.text=element_text(size=6),
        legend.background = element_rect(fill=alpha('blue', 0)))
#g4

# Bivalve weights
g5 <- ggplot(filter(hdata, class=="Bivalves" & parameter %in% c("harvest_g")), aes(x=value)) +
  geom_density(aes(y=..scaled..), alpha=0.8) +
  labs(x="Harvest weight (g)", y="") +
  xlim(0,500) +
  theme_bw() + my_theme
#g5

# Bivalve ages
g6 <- ggplot(filter(hdata, class=="Bivalves" & parameter %in% c("harvest_yr")), aes(x=value)) +
  geom_density(aes(y=..scaled..), alpha=0.8) +
  labs(x="Harvest age (yr)", y="") +
  xlim(0,NA) +
  theme_bw() + my_theme
#g6

# Bivalves
#############################

# Merge
g <- grid.arrange(g1, g2, g3, g4, g5, g6, ncol=3)
g

# Export plot
ggsave(g, filename=file.path(plotdir, "FigS7_species_harvest_sizes_ages.png"), 
       width=6.5, height=4.5, units="in", dpi=600)

