
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
catch_scalar <- msy_upsides/msy_gaines
perc_in_gaines <- msy_gaines/msy_upsides

# Species key
################################################################################

# Things to do
# 1) Correct taxonomy, ID finfish
# 2) Calculate maximum weight

# Build species key 
spp_key <- data_orig %>% 
  # Filter data
  filter(cc_presence=="climate_change" & discount_rate==0) %>% 
  # Identify unique species
  select(SpeciesID, SciName, CommName) %>% 
  unique() %>% 
  # Rename columns
  rename(species_orig=SciName, comm_name=CommName, species_id=SpeciesID) %>% 
  # Add level (NEI or species)
  mutate(level=ifelse(grepl("nei", comm_name), "NEI", "species")) %>% 
  # Add correct species names
  mutate(species=recode(species_orig, 
                        "Gadus ogac"="Gadus macrocephalus",   
                        "Himantura gerrardi"="Maculabatis gerrardi",
                        "Liza aurata"="Chelon auratus",
                        "Liza saliens"="Chelon saliens",       
                        "Moolgarda seheli"="Crenimugil seheli",
                        "Raja rhina"="Beringraja rhina",
                        "Rhinobatos planiceps"="Pseudobatos planiceps",
                        "Theragra chalcogramma"="Gadus chalcogrammus",
                        "Trigloporus lastoviza"="Chelidonichthys lastoviza")) %>% 
    # Arrange
    select(level, species_id, species_orig, species, comm_name) %>% 
    arrange(level, species)

# Check taxonomy
freeR::check_names(spp_key$species[spp_key$level=="species"])

# Get taxanomic info
spp_taxa <- freeR::taxa(spp_key$species[spp_key$level=="species"]) %>% 
  select(-species) %>% 
  rename(species=sciname)

# Get maximum size
spp_info <- freeR::fishbase(dataset="species", species = spp_taxa$species, cleaned=T, add_taxa=T)

# Get LW conversion info
lw_all <- freeR::fishbase(dataset="lw", species = spp_taxa$species, 
                          level="family", cleaned=T, add_taxa=T)

# Id usefule conversion info
lw_use <- lw_all %>% 
  # Remove doubtful measurements
  filter(doubtful!="yes" | is.na(doubtful)) %>% 
  # Reduce to TL (or converted) or shell length (ShL) for inverts
  filter(type=="TL" | type=="ShL" | !is.na(a_tl)) %>% 
  # Remove more unrealistic measurements
  mutate(a=ifelse(a>2, NA, a),
         a_tl=ifelse(a_tl>2, NA, a_tl)) %>% 
  # Identify final a to use
  mutate(a_use=ifelse(!is.na(a_tl), a_tl, a))

# Calculate LW species means
lw_spp <- lw_use %>% 
  group_by(species) %>% 
  summarize(a_spp=mean(a_use, na.rm=T),
            b_spp=mean(b, na.rm=T))

# Calculate LW genus means
lw_gen <- lw_use %>% 
  group_by(genus) %>% 
  summarize(a_gen=mean(a_use, na.rm=T),
            b_gen=mean(b, na.rm=T))

# Calculate LW family means
lw_fam <- lw_use %>% 
  group_by(family) %>% 
  summarize(a_fam=mean(a_use, na.rm=T),
            b_fam=mean(b, na.rm=T))

# Build species max weight dataset
spp_wmax_g <- spp_info %>% 
  # Species, lmax, wmax
  select(database, family, genus, species, lmax_cm, wmax_g) %>%
  # Add length weight params
  left_join(lw_spp, by="species") %>% 
  left_join(lw_gen, by="genus") %>%
  left_join(lw_fam, by="family") %>% 
  # Derive wmax
  mutate(wmax_g_calc_spp=a_spp*lmax_cm^b_spp,
         wmax_g_calc_gen=a_gen*lmax_cm^b_gen,
         wmax_g_calc_fam=a_fam*lmax_cm^b_fam) %>% 
  # Id final wmax and source
  mutate(wmax_g_final=ifelse(!is.na(wmax_g), wmax_g,
                             ifelse(!is.na(wmax_g_calc_spp), wmax_g_calc_spp,
                                    ifelse(!is.na(wmax_g_calc_gen), wmax_g_calc_gen,
                                           ifelse(!is.na(wmax_g_calc_fam), wmax_g_calc_fam, NA)))),
         wmax_g_source=ifelse(!is.na(wmax_g), "Species page",
                             ifelse(!is.na(wmax_g_calc_spp), "Lmax + species LW means",
                                    ifelse(!is.na(wmax_g_calc_gen), "Lmax + genus LW means",
                                           ifelse(!is.na(wmax_g_calc_fam), "Lmax + family LW means", "None"))))) %>% 
  # ID forage fish species
  mutate(forage=ifelse(wmax_g_final<=1200 & database=="FishBase", "forage fish", "not forage fish")) %>% 
  # Designate FINFISH missing forage status
  # Rastrelliger kanagurta (42 cm, Indian mackerel), 
  # Eleginops maclovinus (105 cm, Patagonian blenny, Falkland's mullet or rock cod)
  # Normanichthys crockeri (11 cm, Mote sculpin)
  mutate(forage=ifelse(species=="Eleginops maclovinus", "not forage fish", forage),
         forage=ifelse(species %in% c("Rastrelliger kanagurta", "Normanichthys crocker"), "forage fish", forage))


# Add taxanomic info and forage status to key
spp_key1 <- spp_key %>%
  left_join(spp_taxa, by="species") %>% 
  left_join(select(spp_wmax_g, species, forage), by="species")
  

# Sample size
table(spp_key$level)
table(spp_key1$forage)
table(spp_key1$type)

# Export species key
ff_stats <- spp_key1 %>% 
  filter(forage=="forage fish") %>% 
  group_by(order) %>%
  summarise(n=n()) %>% 
  arrange(n) %>% 
  mutate(order=factor(order, levels=order))

# Plot stats
g <- ggplot(ff_stats, aes(x=order, y=n)) +
  geom_bar(stat="identity") +
  labs(y="Number of forage fish species", x="") +
  coord_flip() +
  theme_bw()
g

# Export forage fish key
write.csv(spp_key1, file.path(outdir, "Gaines_etal_2018_forage_fish_key.csv"), row.names=F)


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
  # Label forage fish
  left_join(select(spp_key1, species_orig, forage))

# Summarize data
data_sum <- data %>% 
  # Calculate sum of raw material from forage fish and from NEI stocks
  group_by(rcp, scenario, year) %>% 
  summarize(catch_mt=sum(harvest, na.rm=T),
            ff_catch_mt=sum(harvest[forage=="forage fish"], na.rm=T)) %>% 
  ungroup() %>% 
  # Calculate new variables
  mutate(red_ff_catch_mt=ff_catch_mt*0.90,
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







