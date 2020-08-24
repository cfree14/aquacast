
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)
library(countrycode)

# Directories
outdir <- "/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/blue-paper-2/data/output/processed"
plotdir <- "figures"

# Read data
hist <- readRDS(file.path(outdir, "FAO_1950_2018_wc_aq_seafood_per_capita.Rds"))
proj <- readRDS(file.path(outdir, "global_capture_mariculture_output_merged.Rds"))

# Format data
hist_use <- hist %>% 
  mutate(sector=factor(sector, levels=c("Capture fisheries", "Finfish mariculture", "Bivalve mariculture")))

# Plot figure
################################################################################

# Base theme
base_theme <- theme(axis.text = element_text(size=6),
                   axis.title = element_text(size=8),
                   plot.title = element_text(size=8),
                   legend.text = element_text(size=6),
                   plot.tag = element_text(size=8, face="bold"),
                   legend.title = element_blank(),
                   legend.background = element_rect(fill=alpha('blue', 0)),
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"))

# Format
proj_use <- proj %>% 
  # Remove period
  filter(period!="2021-2030")  %>% 
  # Format RCP scenario
  mutate(rcp=recode_factor(rcp, 
                           "RCP 2.6"="2.6", 
                           "RCP 4.5"="4.5",
                           "RCP 6.0"="6.0", 
                           "RCP 8.5"="8.5"),
         sector=factor(sector, levels=c("Capture fisheries", "Finfish mariculture", "Bivalve mariculture")))

# Extract current supply
curr_kg_person <- sum(hist$meat_kg_person[hist$year==max(hist$year)])
  
# Plot historical data
g1 <- ggplot(hist_use, aes(x=year, y=meat_kg_person, fill=sector)) +
  geom_area() +
  # Axes
  scale_x_continuous(breaks=seq(1960,2020,20), lim=c(1960,2020)) +
  scale_y_continuous(lim=c(0,15)) +
  # Labels
  labs(x="Year", y="Seafood production per capita\n(kg of meat per person)", title="\nHistorical seafood production", tag="a") +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position=c(0.3, 0.78))
g1

# Plot forecast data (BAU)
g2 <- ggplot(proj_use %>% filter(scenario=="Business-as-usual"), aes(x=rcp, y=meat_kg_person, fill=sector)) +
  facet_wrap(~period) +
  geom_bar(stat="identity") +
  # Add reference line
  geom_hline(yintercept=curr_kg_person, linetype="dashed", color="grey30", lwd=0.3) +
  # Axis
  scale_y_continuous(lim=c(0,15)) +
  # Labels
  labs(x="RCP climate scenario", y="", title="Future seafood production\nin a business-as-usual scenario", tag="b") +
  geom_text(data=proj_use %>% select(scenario, period) %>% unique(), mapping=aes(x=2.5, y=15, label=period), inherit.aes = F, size=2) +
  # Theme
  theme_bw() + base_theme +
  theme(axis.title.y=element_blank(),
        legend.position="none",
        strip.background = element_blank(),
        strip.text = element_blank())
g2

# Plot forecast data (BAU)
g3 <- ggplot(proj_use %>% filter(scenario=="Progressive reforms"), aes(x=rcp, y=meat_kg_person, fill=sector)) +
  facet_wrap(~period) +
  geom_bar(stat="identity") +
  # Add reference line
  geom_hline(yintercept=curr_kg_person, linetype="dashed", color="grey30", lwd=0.3) +
  # Axis
  scale_y_continuous(lim=c(0,15)) +
  # Labels
  labs(x="RCP climate scenario", y="", title="Future seafood production\nin a progressive reform scenario", tag="c") +
  geom_text(data=proj_use %>% select(scenario, period) %>% unique(), mapping=aes(x=2.5, y=15, label=period), inherit.aes = F, size=2) +
  # Theme
  theme_bw() + base_theme +
  theme(axis.title.y=element_blank(),
        legend.position="none",
        strip.background = element_blank(),
        strip.text = element_blank())
g3

# Merge plots
g <- gridExtra::grid.arrange(g1, g2, g3, nrow=1, widths=c(0.4, 0.3, 0.3))
g

# Export plot
ggsave(g, filename=file.path(plotdir, "Fig3_global_production.png"), 
       width=6.5, height=2.5, units="in", dpi=600)
