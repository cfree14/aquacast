
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
tabledir <- "tables"

# Read data
load(file.path(outdir, "national_capture_mariculture_output_merged.Rdata"))
data_orig <- data2 
rm(data2)

# EEZ area
# Wikipedia: 137.9 millions
eezs <- readRDS(file="data/eezs/eezs_v10_polygons.Rds")
eezs_use <- eezs %>% 
  filter(use==T)
eezs_use_sqkm <- sum(eezs_use$area_sqkm)
eezs_use_sqkm / 1e6


# Format data
################################################################################

# Format data for plotting
data1 <- data_orig %>% 
  ungroup() %>% 
  mutate(scenario=recode_factor(scenario, 
                                "Business-as-usual"="Business-\nas-usual",
                                "Progressive reforms"="Progressive\nreforms"), 
         dev_scenario=recode_factor(dev_scenario, 
                                    "Current"="Current",
                                    "Proportional"="Proportional",
                                    "Need-based"="Offset-based",
                                    "Optimum"="Optimum"))

# Stats for manuscript
data1_stats <- data1 %>% 
  group_by(scenario, dev_scenario) %>% 
  summarize(range=paste(round(range(prop)*100, 1), collapse="-"))

# Area data
data2 <- bind_rows(faq_nat, baq_nat) %>% 
  filter(scenario=="Progressive reforms") %>% 
  mutate(eez_prop_cap=pmin(eez_prop, 1)) %>% 
  mutate(dev_scenario=recode_factor(dev_scenario, 
                                    "Current"="Current",
                                    "Proportional"="Proportional",
                                    "Need-based"="Offset-\nbased",
                                    "Optimum"="Optimum"))

# Compute global development stats
adj <- 0.2
area_dev_global <- data2 %>% 
  # Compute area developed by scenario
  group_by(scenario, dev_scenario, rcp, sector) %>% 
  summarize(area_dev_sqkm=sum(area_dev_sqkm)) %>% 
  ungroup() %>% 
  # Proportion of global EEZs
  mutate(area_eez_prop=area_dev_sqkm / eezs_use_sqkm) %>% 
  # Add sector adjustment for plotting
  mutate(sector_adj=ifelse(grepl("Bivalve", sector), -1*adj, adj))



# Export as table
################################################################################

# Format
data1_table <- data1 %>% 
  # Simplify
  select(scenario, dev_scenario, rcp, prop) %>% 
  # Spread
  spread(key="rcp", value="prop")

# Export
write.csv(data1_table, file=file.path(tabledir, "TableS16_pcounties_increasing_production.csv"), row.names=F)


# Plot figure
################################################################################

# Base theme
base_theme <- theme(axis.text = element_text(size=6),
                    axis.title = element_text(size=8),
                    plot.title = element_text(size=8),
                    strip.text = element_text(size=8),
                    legend.text = element_text(size=6),
                    plot.tag = element_text(size=8, face="bold"),
                    legend.title = element_text(size=8),
                    legend.background = element_rect(fill=alpha('blue', 0)),
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(), 
                    axis.line = element_line(colour = "black"),
                    legend.margin=unit(0, "cm"))

# A. Plot proportion of countries with increasing per capita seafood supplies
g1 <- ggplot(data1, aes(x=scenario, y=prop, fill=dev_scenario)) +
  facet_wrap(~rcp, ncol=4) +
  geom_bar(stat="identity", position="dodge") +
  # Labels
  labs(x="Policy scenario", y="Percent of countries with increasing\nseafood production per capita", tag="a") +
  # Legends
  # scale_fill_discrete(name="Development scenario") + 
  scale_fill_manual(name="Development scenario", values=RColorBrewer::brewer.pal(n=4, "Paired")) + 
  scale_y_continuous(breaks=seq(0,1, 0.2), labels = c("    0%",  "    20%", 
                                                      "    40%", "    60%",
                                                      "    80%", "   100%")) + # added padding to align with below
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = "bottom")
g1

# Plot proportion of EEZ developed in each development scenario
log_breaks <- c(0, 0.00025, 0.0005, 0.001, 0.0025, 0.005, 0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 1)
log_labels <- paste0(log_breaks*100, "%")
g2 <- ggplot(data2, aes(x=dev_scenario, y=eez_prop_cap, fill=sector)) +
  facet_wrap(~rcp, ncol=4) + 
  # Plot country totals - boxplots
  geom_boxplot(outlier.size = 0.5, lwd=0.3, color="black", alpha=0.7) +
  # Plot global totals - points
  geom_point(data=area_dev_global, mapping=aes(x=as.numeric(dev_scenario)+sector_adj, y=area_eez_prop, fill=sector), pch=21, size=2, inherit.aes = F) +
  # Axes
  scale_y_continuous(trans="log2", breaks=log_breaks , labels = log_labels) +
  # Legend
  scale_fill_manual(name="", values=c("navy", "salmon")) +
  # Labels
  labs(x="Development scenario", y="Percent of EEZ\ndeveloped for mariculture", tag="b") +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
g2

# Merge plots
g <- gridExtra::grid.arrange(g1, g2, nrow=2, heights=c(0.48, 0.52))

# Export data
ggsave(g, filename=file.path(plotdir, "Fig4_national_production.png"), 
       width=6.5, height=5, units="in", dpi=600)










