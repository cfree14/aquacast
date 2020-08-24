
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
plotdir <- "figures"
indir <- "/Users/cfree/Dropbox/Chris/UCSB/projects/edf_climate/cc_trade/data/gaines/raw"
outdir <- "data/feed_params/processed"

# Read data
data_orig <- readRDS(file.path(indir, "global_cc_manuscript_results_1nation_20171005.rds"))

# Calc catch scalar
msy_gaines <- 68412232
msy_upsides <- 76599435
perc_reported_in_upsides <- 0.78
msy_reported <- msy_upsides / perc_reported_in_upsides # check: msy_upsides / msy_reported
perc_gaines_in_upsides <- msy_gaines / msy_upsides
perc_gaines_in_reported <- msy_gaines / msy_reported
catch_scalar <- msy_reported / msy_gaines

# Feed notes
# 18.7 million mt forage fish
# 3.9 million mt of forage fish to marine aquaculture (3,927,418 mt precisely)
# Roughly 75% to aquaculture (freshwater and marine)
3.9/(0.75*18.7)

# Revised:
# 0.22 = 0.75 * 0.2933333

# Format data
################################################################################

# Inspect
head(data)
table(data_orig$RCP)
table(data_orig$cc_presence)
table(data_orig$discount_rate)
table(data_orig$scenario)

# Format data
data <- data_orig %>% 
  # Filter data
  filter(cc_presence=="climate_change" & discount_rate==0 & scenario %in% c("Full Adaptation", "No Adaptation")) %>% 
  # Select columns
  select(RCP, scenario, SciName, CommName, SpeciesID, year, biomass, harvest, profit, BvBMSY, FvFMSY) %>% 
  rename(species_orig=SciName, comm_name=CommName, species_id=SpeciesID, rcp=RCP, bbmsy=BvBMSY, ffmsy=FvFMSY) %>% 
  # Arrange by RCP, scenario, species, year
  arrange(rcp, scenario, species_orig, year)


# Summarize data
data_sum <- data %>% 
  # Sum harvest
  group_by(rcp, scenario, year) %>% 
  summarize(catch_mt=sum(harvest, na.rm=T)) %>% 
  ungroup() %>% 
  # Format columns
  rename(mgmt_scenario=scenario) %>% 
  mutate(mgmt_scenario=recode(mgmt_scenario, 
                             "No Adaptation"="BAU fisheries management", 
                             "Full Adaptation"="Reformed fisheries management"),
         rcp=recode(rcp, 
                    "RCP26"="RCP 2.6",
                    "RCP45"="RCP 4.5", 
                    "RCP60"="RCP 6.0", 
                    "RCP85"="RCP 8.5")) %>% 
  # Add scaled catch and scaled forage fish catch
  mutate(catch_mt_scaled=catch_mt*catch_scalar,
         ff_catch_mt=catch_mt_scaled*0.18) %>% 
  # Add feed policy scenario
  mutate(prop2maq_bau=0.212,
         prop2maq_ref=0.745) %>% 
  gather(key="feed_scenario", value="prop2maq", 7:8) %>% 
  mutate(feed_scenario=recode(feed_scenario, 
                              "prop2maq_bau"="BAU feed use",
                              "prop2maq_ref"="Reformed feed use"),
         ff_catch_mt_maq=ff_catch_mt*prop2maq) %>% 
  # Arrange columns
  select(rcp, mgmt_scenario, feed_scenario, prop2maq, everything())


# Plot check
g <- ggplot(data_sum, aes(x=year, y=ff_catch_mt_maq/1e6, color=rcp)) +
  facet_grid(feed_scenario~mgmt_scenario) +
  geom_line() +
  lims(x=c(2020,2100)) +
  labs(x="", y="Catch to mAQ (millions mt)") +
  theme_bw()
g

# Export data
write.csv(data_sum, file=file.path(outdir, "forage_fish_availability.csv"), row.names=F)


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
g <- ggplot(data_sum %>% filter(year>=2020), aes(x=year, y=ff_catch_mt_maq/1e6, color=rcp, linetype=feed_scenario)) +
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


# Plot status
################################################################################

# # Subset initial statuses
# data_status <- data %>% 
#   filter(year==2012 & scenario=="No Adaptation" & rcp=="RCP45")
# 
# # Plot initial status
# g <- ggplot(data_status, aes(x=bbmsy, y=ffmsy, color=type)) +
#   geom_point() +
#   labs(x="B/BMSY", y="F/FMSY", title="2012 stock status") +
#   # Legend
#   scale_color_manual(name="Stock type", values=c("red", "grey40", "grey80")) +
#   # Mark quadrants
#   geom_hline(yintercept=1) +
#   geom_vline(xintercept=1) +
#   # Theme
#   theme_bw() +
#   theme( panel.grid.major = element_blank(), 
#          panel.grid.minor = element_blank(),
#          panel.background = element_blank(),
#          legend.position = c(0.8,0.8))
# g
# 
# 
# # Export plot
# ggsave(g, filename=file.path(plotdir, "figure_forage_fish_status.png"), 
#        width=4.5, height=4.5, units="in", dpi=600)


