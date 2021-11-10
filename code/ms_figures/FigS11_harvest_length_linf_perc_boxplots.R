
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(stringr)
library(readxl)
library(tidyverse)

# Directories
plotdir <- "figures"
datadir <- "data/feed_params/processed"

# Read data
data <- read.csv(file=file.path(datadir, "FAO_harvest_size_as_prop_of_linf_data.csv"), as.is=T)

# Calculate median
stats <- data1 %>% 
  group_by(type) %>% 
  summarize(n=n(), 
            harvest_perc_linf=median(harvest_perc_linf))

# Plot data
################################################################################

# Setup theme
my_theme <- theme(axis.text=element_text(size=8),
                  axis.title=element_text(size=10),
                  plot.title=element_text(size=12),
                  legend.position = "none",
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"))

# Plot
g <- ggplot(data, aes(x=type, y=harvest_perc_linf, fill=type)) +
  geom_boxplot() +
  labs(x="", y="Harvest size as a\npercentage of asymptotic length") +
  geom_hline(yintercept=100, linetype="dotted") +
  geom_text(data=stats, mapping=aes(x=type, y=harvest_perc_linf,
                                    label=paste0(round(harvest_perc_linf), "%")), vjust=-0.5) +
  theme_bw() + my_theme
g  

# Export
ggsave(g, filename=file.path(plotdir, "FigS11_harvest_sizes_as_linf_perc.png"), 
       width=4.5, height=4.5, units="in", dpi=600)








