
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
tabledir <- "tables"

# Read data
data <- read.csv(file.path(tabledir, "Free_etal_mariculture_species_database.csv"), as.is=T)


# Build data
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


# Plot data
################################################################################

# Setup theme
my_theme <- theme(axis.text=element_text(size=7),
                  axis.title=element_text(size=9),
                  plot.title=element_text(size=11),
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"))

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
ggsave(g, filename=file.path(plotdir, "FigS20_species_revenues_production.png"), 
       width=6.5, height=3.5, units="in", dpi=600)


