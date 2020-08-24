
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
plotdir <- "figures"
indir <- "data/capture_projections/data"
outdir <- "data/feed_params/processed"

# Read data
data_orig <- readRDS(file.path(indir, "Free_etal_2020_global_projections_with_pop_data.Rds"))


# Build data
################################################################################

# Build data
data <- data_orig %>% 
  # Reduce to important columns
  select(rcp, scenario, year, catch_mt, catch_ff_mt) %>% 
  # Format fisheries management column
  rename(mgmt_scenario=scenario) %>% 
  mutate(mgmt_scenario=recode(mgmt_scenario, 
                             "No Adaptation"="BAU fisheries management", 
                             "Full Adaptation"="Reformed fisheries management")) %>% 
  # Add feed policy scenario
  mutate(prop2maq_bau=0.212,
         prop2maq_ref=0.212) %>% 
         # prop2maq_ref=0.745) %>% 
  gather(key="feed_scenario", value="prop2maq", 6:7) %>% 
  mutate(feed_scenario=recode(feed_scenario, 
                              "prop2maq_bau"="BAU feed use",
                              "prop2maq_ref"="Reformed feed use"),
         catch_ff_mt_maq=catch_ff_mt*prop2maq) %>% 
  # Arrange columns
  select(rcp, mgmt_scenario, feed_scenario, prop2maq, everything())


# Plot check
g <- ggplot(data, aes(x=year, y=catch_ff_mt_maq/1e6, color=rcp)) +
  facet_grid(feed_scenario~mgmt_scenario) +
  geom_line() +
  lims(x=c(2020,2100)) +
  labs(x="", y="Catch to mAQ (millions mt)") +
  theme_bw()
g

# Export data
write.csv(data, file=file.path(outdir, "forage_fish_availability.csv"), row.names=F)


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
g <- ggplot(data, aes(x=year, y=catch_ff_mt_maq/1e6, color=rcp, linetype=feed_scenario)) +
  facet_wrap(~mgmt_scenario) +
  geom_line() +
  labs(x="", y="Catch available for mariculture\nfeed ingredients (millions of mt)") +
  scale_color_discrete(name="Emissions scenario", guide = guide_legend(title.position = "top")) +
  scale_linetype_manual(name="Feed use scenario", values=c("dotted", "solid"), guide = guide_legend(title.position = "top")) +
  scale_x_continuous(lim=c(2020,2100), breaks=seq(2020,2100,10)) +
  theme_bw() + my_theme +
  theme(legend.position="bottom",
        legend.background = element_rect(fill=alpha('blue', 0)))
g

# Export plot
ggsave(g, filename=file.path(plotdir, "figure_forage_fish_supply.png"), 
       width=6.5, height=3.5, units="in", dpi=600)





