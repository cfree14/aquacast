
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
plotdir <- "figures"
datadir <- "data/feed_params/processed"

# Export data
data_orig <- read.csv(file.path(datadir, "forage_fish_availability.csv"), as.is=T)

# Reduce
data <- data_orig %>% 
  select(rcp, mgmt_scenario, year, catch_ff_mt_maq) %>% 
  unique() %>% 
  mutate(mgmt_scenario=recode(mgmt_scenario, 
                              "BAU fisheries management"="Business-as-usual",
                              "Reformed fisheries management"="Climate-adaptive"))


# Plot data
################################################################################

# Setup theme
my_theme <- theme(axis.text=element_text(size=6),
                  axis.title=element_text(size=8),
                  strip.text=element_text(size=8),
                  axis.title.x = element_blank(),
                  legend.title=element_text(size=8),
                  legend.text=element_text(size=6),
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"),
                  axis.text.x = element_text(angle = 90, vjust = 0.5) )

# Plot data
g <- ggplot(data, aes(x=year, y=catch_ff_mt_maq/1e6, color=rcp, linetype=mgmt_scenario)) +
  geom_line() +
  # Axes
  lims(y=c(0,NA)) +
  scale_x_continuous(lim=c(2020,2100), breaks=seq(2020,2100,10)) +
  # Labels
  labs(x="", y="Catch available for mariculture\nfeed ingredients (millions of mt)") +
  # Legends
  scale_color_manual(name="Emissions scenario", values=RColorBrewer::brewer.pal(n=4, "RdBu") %>% rev()) +
  scale_linetype_manual(name="Fisheries management", values=c("dotted", "solid")) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position="right",
        legend.background = element_rect(fill=alpha('blue', 0)))
g

# Export plot
ggsave(g, filename=file.path(plotdir, "FigS23_forage_fish_supply.png"), 
       width=6.5, height=3.5, units="in", dpi=600)





