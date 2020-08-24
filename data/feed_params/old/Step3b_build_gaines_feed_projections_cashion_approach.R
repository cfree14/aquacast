
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
cashion <- read.csv(file.path("/Users/cfree/Dropbox/Chris/UCSB/projects/protein_curve/ocean-protein/aquaculture/data/processed/Cashion_2016_fmfo_proportions_formatted.csv"), as.is=T)
data_orig <- readRDS(file.path(indir, "global_cc_manuscript_results_1nation_20171005.rds"))

# Calc catch scalar
msy_gaines <- 68412232
msy_upsides <- 76599435
catch_scalar <- msy_upsides/msy_gaines
perc_in_gaines <- msy_gaines/msy_upsides


# Species key
################################################################################

# Identify Cashion et al. (2016) forage fish species
ff_spp <- cashion %>% 
  filter(type=="species") %>% 
  group_by(comm_name, species) %>% 
  summarize(fmfo_perc=mean(fmfo_perc)) %>% 
  mutate(species=recode(species, 
                        "Clupea bentincki"="Strangomera bentincki",
                        "Moolgarda perusii"="Osteomugil perusii"))


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
  filter(cc_presence=="climate_change" & RCP!="RCP26" & discount_rate==0 & scenario %in% c("Full Adaptation", "No Adaptation")) %>% 
  # Select columns
  select(RCP, scenario, SciName, CommName, SpeciesID, year, biomass, harvest, profit, BvBMSY, FvFMSY) %>% 
  rename(species_orig=SciName, comm_name=CommName, species_id=SpeciesID, rcp=RCP, bbmsy=BvBMSY, ffmsy=FvFMSY) %>% 
  # Arrange by RCP, scenario, species, year
  arrange(rcp, scenario, species_orig, year) %>% 
  # Identify forage fish
  mutate(forage=ifelse(species_orig %in% ff_spp$species, "forage fish", "not forage fish"))



# Summarize data
data_sum <- data %>% 
  # Calculate sum of raw material from forage fish and from NEI stocks
  group_by(rcp, scenario, year) %>% 
  summarize(catch_mt=sum(harvest, na.rm=T),
            ff_catch_mt=sum(harvest[forage=="forage fish"], na.rm=T)) %>% 
  ungroup() %>% 
  # Calculate new variables
  mutate(red_ff_catch_mt=ff_catch_mt*0.50,
         prop_red_to_maq=ifelse(scenario=="No Adaptation", 0.75, 1.00),
         maq_red_ff_catch_mt=red_ff_catch_mt * prop_red_to_maq,
         prop_ff=ff_catch_mt/catch_mt,
         prod_red=red_ff_catch_mt/catch_mt) %>%
  # Format for plotting
  mutate(scenario=recode(scenario, 
                         "No Adaptation"="Base case", 
                         "Full Adaptation"="Progressive reform"),
         rcp=recode(rcp, 
                    "RCP45"="RCP 4.5", 
                    "RCP60"="RCP 6.0", 
                    "RCP85"="RCP 8.5"))



# # Summarize data
# data_sum <- data %>% 
#   # Calculate sum of raw material from forage fish and from NEI stocks
#   group_by(rcp, scenario, year) %>% 
#   summarize(ff_catch_mt=sum(harvest[forage=="forage fish"], na.rm=T),
#             nei_catch_mt=sum(harvest[level=="NEI"], na.rm=T)) %>% 
#   ungroup() %>% 
#   # Calculate new variables
#   mutate(reduced_ff_catch_mt=ff_catch_mt*0.90,
#          reduced_nei_catch_mt=nei_catch_mt*0.18,
#          reduced_catch_mt=reduced_ff_catch_mt+reduced_nei_catch_mt,
#          prop_reduced_catch_to_mariculture=ifelse(scenario=="No Adaptation", 0.75, 1.00),
#          maq_reduced_catch_mt=reduced_catch_mt * prop_reduced_catch_to_mariculture) %>% 
#   # Format for plotting
#   mutate(scenario=recode(scenario, 
#                          "No Adaptation"="Base case", 
#                          "Full Adaptation"="Progressive reform"),
#          rcp=recode(rcp, 
#                     "RCP45"="RCP 4.5", 
#                     "RCP60"="RCP 6.0", 
#                     "RCP85"="RCP 8.5"))


# Plot status
################################################################################

# Subset initial statuses
data_status <- data %>% 
  filter(year==2012 & scenario=="No Adaptation" & rcp=="RCP45")

# Plot initial status
g <- ggplot(data_status, aes(x=bbmsy, y=ffmsy, color=type)) +
  geom_point() +
  labs(x="B/BMSY", y="F/FMSY", title="2012 stock status") +
  # Legend
  scale_color_manual(name="Stock type", values=c("red", "grey40", "grey80")) +
  # Mark quadrants
  geom_hline(yintercept=1) +
  geom_vline(xintercept=1) +
  # Theme
  theme_bw() +
  theme( panel.grid.major = element_blank(), 
         panel.grid.minor = element_blank(),
         panel.background = element_blank(),
         legend.position = c(0.8,0.8))
g


# Export plot
ggsave(g, filename=file.path(plotdir, "figure_forage_fish_status.png"), 
       width=4.5, height=4.5, units="in", dpi=600)

# Plot data
################################################################################

# Points for years of interest
yrs <- c(2021, 2051, 2100)
pts <- data_sum %>% 
  filter(year %in% yrs)

# Setup theme
my_theme <- theme(axis.text=element_text(size=8),
                  axis.title=element_text(size=10),
                  strip.text=element_text(size=10),
                  axis.title.x = element_blank(),
                  legend.title=element_text(size=9),
                  legend.text=element_text(size=7),
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"),
                  axis.text.x = element_text(angle = 90, vjust = 0.5) )

# Plot data
g <- ggplot(data_sum, aes(x=year, y=maq_reduced_catch_mt/1e6, color=rcp)) +
  geom_line() +
  geom_point(data=pts, size=2) +
  facet_grid(~scenario) +
  labs(x="", y="Catch available for mariculture\nfeed ingredients (millions of mt)") +
  scale_color_discrete(name="Emissions scenario") +
  scale_x_continuous(breaks=c(2012, seq(2020,2100,10))) +
  theme_bw() + my_theme +
  theme(legend.position=c(0.85,0.75))
g

# Export plot
ggsave(g, filename=file.path(plotdir, "figure_forage_fish_supply.png"), 
       width=6.5, height=3.5, units="in", dpi=600)







