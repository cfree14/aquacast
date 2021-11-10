
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

# Identify plotting rank
spp_order_key <- data %>% 
  select(class, isscaap, price_usd_mt_isscaap) %>% 
  unique() %>% 
  arrange(desc(class), price_usd_mt_isscaap)


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
ggsave(g, filename=file.path(plotdir, "FigS14_species_prices_by_isscaap.png"), 
       width=6.5, height=3, units="in", dpi=600)
