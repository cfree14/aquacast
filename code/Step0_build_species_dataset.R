
# Turn off scientific notation
options(scipen=999)

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
sppdir <- "data/species/data"
aqmapdir <- "data/aquamaps/data"
feeddir <- "data/feed_params/processed"
saupdir <- "data/prices/seaaroundus"

# Read data
spp_lh <- read.csv(file.path(sppdir, "aquaculture_species_lh_data.csv"), as.is=T)
spp_env <- readRDS(file.path(aqmapdir, "aquamaps_environmental_preferences.Rds"))
feed_key <- read.csv(file.path(feeddir, "Tacon_Metian_group_fcrs_and_fmfo_feed_percs.csv"), as.is=T)

# Harvest size as a proportion of linf
linf2harv_b <- 0.67
linf2harv_f <- 0.52

# Merge data
################################################################################

# Compare Von B Linf sources
# Family is definitely a last resort -- Gentry is probably second to FB species
plot(spp_lh$linf_cm_fb_spp, spp_lh$linf_cm_fb_gen)
plot(spp_lh$linf_cm_fb_spp, spp_lh$linf_cm_fb_fam)
plot(spp_lh$linf_cm_fb_spp, spp_lh$linf_cm_gentry)

# Format life history data
data1 <- spp_lh %>% 
  # Update missing common names
  mutate(comm_name=ifelse(species=="Oplegnathus fasciatus", "Striped beakfish", comm_name)) %>% 
  # Finalize LW parameters
  # Species > Genus > Family
  mutate(a=ifelse(!is.na(a_spp), a_spp,
                  ifelse(!is.na(a_gen), a_gen, 
                         ifelse(!is.na(a_fam), a_fam, NA))), 
         a_source=ifelse(!is.na(a_spp), "FB species-average",
                         ifelse(!is.na(a_gen), "FB genus-average", 
                                ifelse(!is.na(a_fam), "FB family-average", "none"))), 
         b=ifelse(!is.na(b_spp), b_spp,
                  ifelse(!is.na(b_gen), b_gen, 
                         ifelse(!is.na(b_fam), b_fam, NA))), 
         b_source=ifelse(!is.na(b_spp), "FB species-average",
                         ifelse(!is.na(b_gen), "FB genus-average", 
                                ifelse(!is.na(b_fam), "FB family-average", "none")))) %>% 
  # Finalize Von B paramaters
  # FL (spp) > FB (spp) > Gentry (spp) > FB (gen) > FB (family)
  mutate(linf_cm=ifelse(!is.na(linf_cm_fl), linf_cm_fl,
                        ifelse(!is.na(linf_cm_fb_spp), linf_cm_fb_spp,
                               ifelse(!is.na(linf_cm_gentry), linf_cm_gentry,
                                      ifelse(!is.na(linf_cm_fb_gen), linf_cm_fb_gen,
                                             ifelse(!is.na(linf_cm_fb_fam), linf_cm_fb_fam, NA))))),
         linf_source=ifelse(!is.na(linf_cm_fl), "FishLife",
                        ifelse(!is.na(linf_cm_fb_spp), "FB species-average",
                               ifelse(!is.na(linf_cm_gentry), "Gentry et al. 2017",
                                      ifelse(!is.na(linf_cm_fb_gen), "FB genus-average",
                                             ifelse(!is.na(linf_cm_fb_fam), "FB family-average", "none"))))),
         k=ifelse(!is.na(k_fl), k_fl,
                        ifelse(!is.na(k_fb_spp), k_fb_spp,
                               ifelse(!is.na(k_gentry), k_gentry,
                                      ifelse(!is.na(k_fb_gen), k_fb_gen,
                                             ifelse(!is.na(k_fb_fam), k_fb_fam, NA))))),
         k_source=ifelse(!is.na(k_fl), "FishLife",
                            ifelse(!is.na(k_fb_spp), "FB species-average",
                                   ifelse(!is.na(k_gentry), "Gentry et al. 2017",
                                          ifelse(!is.na(k_fb_gen), "FB genus-average",
                                                 ifelse(!is.na(k_fb_fam), "FB family-average", "none")))))) %>% 
  # Correct some final Von B parameters
  # The Gentry et al estimates of K for Smooth mactra abd Globnose clam result in harvest ages of 55 yrs - use family estimates instead
  mutate(k=ifelse(comm_name %in% c("Smooth mactra", "Globose clam"), k_fb_fam, k), 
         k_source=ifelse(comm_name %in% c("Smooth mactra", "Globose clam"), "FB family-average", k_source)) %>% 
  # Add feed group classifications
  # 1) Classify by ISSCAP group
  # 2) Then classify by catfish, salmon, trout, milkfish
  mutate(feed_group=recode(isscaap, 
                           "Clams, cockles, arkshells"="Non-fed",
                           "Cods, hakes, haddocks"="Misc marine fish",
                           "Flounders, halibuts, soles"="Misc marine fish",
                           "Freshwater molluscs"="Non-fed",
                           "Miscellaneous coastal fishes" ="Misc marine fish",
                           "Miscellaneous demersal fishes"="Misc marine fish",
                           "Miscellaneous diadromous fishes"="Misc freshwater fish",
                           "Miscellaneous freshwater fishes"="Misc freshwater fish",
                           "Miscellaneous pelagic fishes"="Misc marine fish",
                           "Mussels"="Non-fed",
                           "Oysters"="Non-fed",
                           "Pearls, mother-of-pearl, shells"="Non-fed",
                           "River eels"="Eel",
                           "Salmons, trouts, smelts"="more detail possible",
                           "Scallops, pectens"="Non-fed",
                           "Shads"="Misc marine fish",
                           "Sturgeons, paddlefishes"="Misc freshwater fish",
                           "Tilapias and other cichlids"="Tilapia",
                           "Tunas, bonitos, billfishes"="Tuna"),
         feed_group=ifelse(order=="Siluriformes", "Catfish", feed_group),
         feed_group=ifelse(family=="Salmonidae", "Salmon", feed_group),
         feed_group=ifelse(genus=="Salvelinus", "Trout", feed_group),
         feed_group=ifelse(species=="Chanos chanos", "Milkfish", feed_group)) %>% 
  # Add feed info
  left_join(feed_key, by="feed_group") %>% 
  # Calculate harvest size (cm and g) and time to harvest (yr)
  mutate(harvest_linf_prop = ifelse(class=="Actinopterygii", linf2harv_f, linf2harv_b),
         harvest_cm = linf_cm * harvest_linf_prop,
         harvest_g = a * harvest_cm ^ b,
         harvest_yr = -log(1 - harvest_cm / linf_cm) / k) %>% 
  # Calculate group average price
  ungroup() %>% 
  group_by(isscaap) %>% 
  rename(price_usd_mt_spp=price_usd_mt) %>% 
  mutate(price_usd_mt_isscaap=mean(price_usd_mt_spp, na.rm=T)) %>%
  ungroup()

# Inspect
freeR::complete(data1)

# Build AquaMaps environmental tolerances
spp_temp <- spp_env %>% 
  filter(variable%in%c("Temperature (C)")) %>% 
  rename(species=sci_name, sst_c_min=min, sst_c_q10=q10, sst_c_q90=q90, sst_c_max=max) %>% 
  select(-c(comm_name, variable, used))
spp_salt <- spp_env %>% 
  filter(variable%in%c("Salinity (psu)")) %>% 
  rename(species=sci_name, sal_psu_min=min, sal_psu_q10=q10, sal_psu_q90=q90, sal_psu_max=max) %>% 
  select(-c(comm_name, variable, used))
spp_env1 <- spp_temp %>% 
  left_join(spp_salt, by="species")

# Check for duplicated species
freeR::which_duplicated(spp_env1$species)

# Build full data
data_full <- data1 %>% 
  # Add AquaMaps life history data
  left_join(spp_env1, by="species") %>% 
  # Rename a few columns
  rename(sst_c_min_froehlich=temp_c_min, sst_c_max_froehlich=temp_c_max) %>% 
  # Arrange columns
  select(class:comm_name, # taxonomy
         fao, gentry, # inclusion source
         feed_group:fmfo_perc, # harvest parameters
         harvest_linf_prop:harvest_yr, # more harvest parameters
         price_usd_mt_isscaap, price_usd_mt_spp, # prices
         a_spp:b_source, # LW parameters
         linf_cm_fl:k_gentry, linf_cm:k_source, # Von B parameters
         sst_c_min_froehlich, sst_c_max_froehlich, sst_c_min:sal_psu_max,
         everything())

# Inspect
freeR::complete(data_full)


# Final data
################################################################################

# Build simple data
data <- data_full %>% 
  # Reduce columns
  select(class:comm_name, # taxonomy
         fao, gentry, # inclusion source
         feed_group:fmfo_perc, # harvest parameters
         harvest_linf_prop:harvest_yr, # more harvest parameters
         price_usd_mt_isscaap, price_usd_mt_spp, # prices
         a, a_source, b, b_source,
         linf_cm, linf_source, k, k_source, 
         sst_c_min_froehlich, sst_c_max_froehlich,
         sst_c_min:sal_psu_max) %>% 
  # Small formatting
  select(-fcr08) %>% 
  rename(fcr=fcr15) %>% 
  # Reduce to species with required data
  filter(!is.na(a) & !is.na(b) & !is.na(linf_cm) & !is.na(k) & !is.na(sst_c_min))

# Inspect
freeR::complete(data)


# Final data
################################################################################

# Export
save(data, data_full, file=file.path(sppdir, "aquaculture_species_key.Rdata"))
write.csv(data, file=file.path(sppdir, "aquaculture_species_key.csv"), row.names = F)




