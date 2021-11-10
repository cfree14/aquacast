

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
plotdir <- "figures"
datadir <- "data/capture_projections/data"
outputdir <- "/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/blue-paper-2/data/output/processed/"

# Read FAO data
fao_orig <- readRDS("/Users/cfree/Dropbox/Chris/UCSB/data/fao/aquaculture/processed/1950_2017_fao_aquaculture_data.Rds")

# Read FIFO data
load("data/feed_params/processed/fifo_trends_projections.Rdata")

# Read forage fish availability data
ffdata <- read.csv("data/feed_params/processed/forage_fish_availability.csv", as.is=T)


# Calculate ecological limits for finfish mariculture
################################################################################

# Period key
period_key <- tibble(year=c(2021:2030, 2051:2060, 2091:2100),
                     period=sort(rep(c("2021-2030", "2051-2060", "2091-2100"), 10)))

# FIFO averages
fifo_avgs <- fifo_proj_g %>% 
  group_by(year) %>% 
  summarize(fifo_avg=mean(fifo)) %>% 
  rename(scenario=year) %>% 
  mutate(scenario=recode(scenario, 
                         "2030"="Business-as-usual",
                         "2050"="Progressive reforms"))

# Calculate ecological limits of FAQ based on FF in period, climate, mgmt, scenario
faq_limits <- ffdata %>% 
  # Add period
  left_join(period_key) %>% 
  filter(!is.na(period)) %>% 
  # Summarize
  group_by(rcp, mgmt_scenario, feed_scenario, period) %>% 
  summarize(across(.cols=catch_mt:catch_ff_mt_maq, .fns = mean)) %>% 
  ungroup() %>% 
  # Add big-picture scenarios
  mutate(scenario=ifelse(mgmt_scenario=="BAU fisheries management" & feed_scenario=="BAU feed use", "Business-as-usual", NA), 
         scenario=ifelse(mgmt_scenario=="Reformed fisheries management" & feed_scenario=="Reformed feed use", "Progressive reforms", scenario)) %>% 
  filter(!is.na(scenario)) %>% 
  # Add mean FIFO for scenario
  left_join(fifo_avgs) %>% 
  mutate(meat_mt=catch_ff_mt_maq / fifo_avg) %>% 
  # Add sector
  mutate(sector="Finfish mariculture") %>% 
  mutate(sector=factor(sector, levels=c("Finfish mariculture", "Bivalve mariculture")))

  


# Setup
################################################################################

# COME BACK TO THIS
# BUILD KEY TO IDENTIFY SPECIES THAT ARE FAIR TO COUNT

# Build mariculture progress data
maq_nonzero <- fao_orig %>% 
  # Marine/brackish finfish/bivalves
  filter(environment %in% c("Marine", "Brackishwater") & major_group %in% c("Pisces", "Mollusca")) %>% 
  # Remove stragglers
  filter(!isscaap %in% c("Squids, cuttlefishes, octopuses", "Miscellaneous freshwater fishes", "Freshwater molluscs", "Pearls, mother-of-pearl, shells")) %>% 
  # Remove zero records
  filter(quantity_mt>0) 

# Inspect countries and species
sort(unique(maq_nonzero$species_orig))

# Production stats
stats <- maq_nonzero %>% 
  # Calculate annual stats
  group_by(year, major_group) %>% 
  summarise(ncountries=n_distinct(country_orig),
            prod_mt=sum(quantity_mt),
            value_usd_t=sum(value_usd_t),
            nspp=n_distinct(species_orig)) %>% 
  ungroup() %>% 
  # Reclass type
  rename(type=major_group) %>% 
  mutate(type=recode_factor(type, 
                            "Pisces"="Finfish mariculture",
                            "Mollusca"="Bivalve mariculture"))

# Plot data
################################################################################

# Small plot theme
small_plot_theme <- theme(axis.text=element_text(size=5),
                          axis.title=element_text(size=7),
                          axis.title.x=element_blank(),
                          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                          plot.title=element_blank(),
                          plot.tag=element_text(size=8, face="bold"),
                          legend.title=element_blank(),
                          legend.text = element_text(size=5),
                          legend.background = element_rect(fill=alpha('blue', 0)),
                          panel.grid.major = element_blank(), 
                          panel.grid.minor = element_blank(),
                          panel.background = element_blank(), 
                          axis.line = element_line(colour = "black"))

# Small plot theme
big_plot_theme <- theme(axis.text=element_text(size=5),
                        axis.title=element_text(size=7),
                        axis.title.x=element_blank(),
                        # axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                        plot.title=element_blank(),
                        strip.text=element_text(size=7),
                        plot.tag=element_text(size=8, face="bold"),
                        legend.title=element_text(size=7),
                        legend.text = element_text(size=5),
                        legend.background = element_rect(fill=alpha('blue', 0)),
                        panel.grid.major = element_blank(), 
                        panel.grid.minor = element_blank(),
                        panel.background = element_blank(), 
                        axis.line = element_line(colour = "black"),
                        legend.margin=unit(0, "cm"))


# Mariculture progress plots
############################################

# MAQ production
g1 <- ggplot(stats, aes(x=year, y=prod_mt/1e6, color=type)) +
  geom_line() +
  labs(x="", y="Production\n(millions of mt)", tag="a") +
  lims(y=c(0,NA)) +
  scale_x_continuous(breaks=seq(1950,2020,10)) +
  scale_color_manual(name="", values=c("salmon", "navy")) +
  annotate(geom="text", x=1950, y=13.8, hjust=0, label="Bivalve mariculture", color="navy", inherit.aes = F, size=1.9) +
  annotate(geom="text", x=1950, y=15.5, hjust=0, label="Finfish mariculture", color="salmon", inherit.aes = F, size=1.9) +
  theme_bw() + small_plot_theme +
  theme(legend.position = "none")
g1

# MAQ producing countries
g2 <- ggplot(stats, aes(x=year, y=ncountries, color=type)) +
  geom_line() +
  labs(x="", y="Number of\nproducing countries", tag="b") +
  lims(y=c(0,NA)) +
  scale_x_continuous(breaks=seq(1950,2020,10)) +
  scale_color_manual(name="", values=c("salmon", "navy")) +
  theme_bw() + small_plot_theme +
  theme(legend.position = "none")
g2

# MAQ species
g3 <- ggplot(stats, aes(x=year, y=nspp, color=type)) +
  geom_line() +
  labs(x="", y="Number of\ncultured species", tag="c") +
  lims(y=c(0,NA)) +
  scale_x_continuous(breaks=seq(1950,2020,10)) +
  scale_color_manual(name="", values=c("salmon", "navy")) +
  theme_bw() + small_plot_theme +
  theme(legend.position = "none")
g3

# FIFO progress plots
############################################

# Format data
group_order <- fifo_preds %>% 
  filter(year==2000) %>% 
  arrange(desc(fifo)) %>% 
  pull(group)
fifos_g <- fifos_g %>% 
  mutate(group=factor(group, levels=group_order))
fifo_preds <- fifo_preds %>% 
  mutate(group=factor(group, levels=group_order))

# FIFO trends
groups <-tibble(group=group_order)
g4 <- ggplot(fifos_g, aes(x=year, y=fifo, color=group)) +
  # Plot exponential decline fits
  geom_line(data=fifo_preds, mapping=aes(x=year, y=fifo, color=group), lwd=0.3) +
  # Plot points
  geom_point(size=0.3) +
  # Plot text labels
  geom_text(data=groups, mapping=aes(x=2050, y=seq(4.6, 2.4, length.out = nrow(groups)), hjust=1, label=group, color=group), size=1.9) +
  # Limits
  scale_x_continuous(breaks=seq(2000,2050, 10), limits = c(2000, 2050)) +
  # Labels
  labs(x="", y='FIFO ratio\n("fish in, fish out")', tag="d") +
  # Reference line
  geom_hline(yintercept=1, linetype="dashed", lwd=0.4) +
  # Theme
  theme_bw() + small_plot_theme +
  theme(legend.position = "none")
g4

# Number of countries
# g7 <- ggplot(pdata_use, aes(x=period, y=ncountries, fill=rcp)) +
#   facet_wrap(~sector, scales="free") +
#   labs(x="Period", y="Number of countries") +
#   geom_bar(stat="identity", position="dodge")
# g7

# Aquaculture results plots
############################################

# Build data
aqfiles <- list.files(outputdir, pattern="rational_use_new_costs1.Rds")
pdata <- purrr::map_df(aqfiles , function(x){
  
  # Read file
  sdata <- readRDS(file.path(outputdir, x))
  
  # Data info
  type <- ifelse(grepl("Bivalve", x), "Bivalve mariculture", "Finfish mariculture")
  rcp <- paste("RCP", substr(x, 4, 5))
  
  # Calculate statistics
  pstats <- sdata %>% 
    # Calc stats
    group_by(period) %>% 
    summarize(ncells=n(),
              area_sqkm=ncells*100,
              ncountries=n_distinct(ter1_name), 
              prod_mt=sum(prod_mt_yr)) %>% 
    # Add columns
    mutate(sector=type, 
           rcp=rcp,
           catch2meat=ifelse(type=="Bivalve mariculture", 0.17, 0.87),
           meat_mt=prod_mt*catch2meat) %>% 
    # Arrange
    select(sector, rcp, period, everything())
  
})

# Format
pdata_use <- pdata %>% 
  mutate(rcp=recode(rcp, 
                    "RCP 26"="RCP 2.6",
                    "RCP 45"="RCP 4.5",
                    "RCP 60"="RCP 6.0",
                    "RCP 85"="RCP 8.5"),
         sector=factor(sector, levels=c("Finfish mariculture", 
                                        "Bivalve mariculture")))

# Suitable area
g5 <- ggplot(pdata_use, aes(x=period, y=area_sqkm/1e6, fill=rcp)) +
  facet_wrap(~sector, scales="free") +
  geom_bar(stat="identity", position="dodge") +
  # Labels
  labs(x="Period", y="Profitable area\n(millions of sq. km)", tag="e") +
  # Legend
  scale_fill_manual(name="Climate scenario", values=RColorBrewer::brewer.pal(4, name="RdBu") %>% rev(), 
                    guide = guide_legend(title.position = "top")) +
  # Theme
  theme_bw() + big_plot_theme + 
  theme(legend.position = "bottom")
g5

# Demand
demand_df <- tibble(type="Demand limit", 
                    sector=c("Bivalve mariculture", "Finfish mariculture"),
                    meat_mt_demand=c(103*1e6*0.17, 103*1e6*0.27)) %>% 
  mutate(sector=factor(sector, levels=c("Finfish mariculture", "Bivalve mariculture")))

# Production potential (not log scale)
# g6 <- ggplot(pdata_use, aes(x=period, y=meat_mt/1e9, fill=rcp)) +
#   facet_wrap(~sector, scales="free") +
#   geom_bar(stat="identity", position="dodge", show.legend = F) + 
#   # Axes
#   # Reference line
#   geom_hline(data=demand_df, mapping=aes(yintercept=meat_mt_demand/1e9, linetype=type), lwd=0.5, show.legend = T) +
#   # Labels
#   labs(x="Period", y="Production potential\n(billions of mt of meat)", tag="f") +
#   # Legend
#   scale_fill_manual(name="", values=RColorBrewer::brewer.pal(4, name="RdBu") %>% rev(), guide="none") +
#   scale_linetype_manual(name="", values="dotted") +
#   # Theme
#   theme_bw() + big_plot_theme + 
#   theme(legend.position = "bottom")
# g6

# Production potential (log scale)
g6 <- ggplot(pdata_use, aes(x=period, y=meat_mt/1e6, fill=rcp)) +
  facet_wrap(~sector, scales="free") +
  geom_bar(stat="identity", position="dodge", show.legend = F) + 
  # Axes
  scale_y_log10(limit=c(1,25000), breaks=c(1, 5, 10, 50, 100, 500, 1000, 5000, 10000, 25000)) + # millions
  # scale_y_log10(limit=c(0.0001,25), breaks=c(0.5, 1, 2, 5, 10, 15, 25)) + # billions
  # Reference line
  geom_hline(data=demand_df, mapping=aes(yintercept=meat_mt_demand/1e6, linetype=type), lwd=0.4, show.legend = F) +
  # Add reference points
  geom_point(data=faq_limits, mapping=aes(x=period, y=meat_mt/1e6, shape=scenario, group=rcp), position=position_dodge(width=0.9), size=0.8) +
  # Labels
  labs(x="Period", y="Production potential\n(millions of mt of meat per year)", tag="f") +
  # Legend
  scale_fill_manual(name="", values=RColorBrewer::brewer.pal(4, name="RdBu") %>% rev(), guide="none") +
  scale_linetype_manual(name="", values="dotted") +
  scale_shape_manual(name="Feed limitation under:", guide = guide_legend(title.position = "top"), values=c(1,19)) +
  # Theme
  theme_bw() + big_plot_theme + 
  theme(legend.position = "bottom")
g6




# Merge and export
############################################

# Empty plot
gfill <- ggplot() + theme_void()


# Merge
layout_matrix <- matrix(data=c(1, 2, 3, 4, 
                               5, 5, 6, 6), ncol=4, byrow=T)
g <- gridExtra::grid.arrange(g1, g2, g3, g4, g5, g6,
                             heights=c(0.4, 0.6),
                             layout_matrix=layout_matrix)
g

# Export figure
ggsave(g, filename=file.path(plotdir, "Fig2_mariculture_results.png"), 
       width=6.5, height=3.5, units="in", dpi=600)

