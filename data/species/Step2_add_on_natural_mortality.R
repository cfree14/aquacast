
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(rio)
library(ggplot2)
library(tidyverse)
library(rfishbase)

# Directories
datadir <- "data/species/data"

# Read data
data_orig <- read.csv(file=file.path(datadir, "aquaculture_species_lh_data.csv"), as.is=T)



# Get M values
################################################################################

# Look up M values in FishLife
m_fl <- freeR::fishlife(data_orig$species)
m_fl1 <- m_fl %>% 
  rename(m_fl=m)

# Look up M values in FishBase
m_fb <- freeR::fishbase(dataset="vonb", species=data_orig$species, level="order", cleaned=T)

# FB M values by species
m_fb_spp <- m_fb %>% 
  group_by(species) %>% 
  summarise(m_fb_spp=mean(m, na.rm=T)) %>% 
  ungroup() %>% 
  filter(!is.na(species))

# FB M values by genus
m_fb_gen <- m_fb %>% 
  group_by(genus) %>% 
  summarise(m_fb_gen=mean(m, na.rm=T)) %>% 
  ungroup() %>% 
  filter(!is.na(genus))

# FB M values by family
m_fb_fam <- m_fb %>% 
  group_by(family) %>% 
  summarise(m_fb_fam=mean(m, na.rm=T)) %>% 
  ungroup() %>% 
  filter(!is.na(family))

# FB M values by order
m_fb_ord <- m_fb %>% 
  group_by(order) %>% 
  summarise(m_fb_ord=mean(m, na.rm=T)) %>% 
  ungroup() %>% 
  filter(!is.na(order))

# Build data
data <- data_orig %>% 
  # Simplify
  select(class:comm_name) %>% 
  # Add M FL
  left_join(m_fl1 %>% select(species, m_fl), by="species") %>% 
  # Add M FB species
  left_join(m_fb_spp, by="species") %>% 
  # Add M FB genus
  left_join(m_fb_gen, by="genus") %>% 
  # Add M FB family
  left_join(m_fb_fam, by="family") %>% 
  # Add M FB order
  left_join(m_fb_ord, by="order") %>% 
  # Final M
  mutate(m=ifelse(!is.na(m_fl), m_fl,
                  ifelse(!is.na(m_fb_spp), m_fb_spp,
                         ifelse(!is.na(m_fb_gen), m_fb_gen, 
                                ifelse(!is.na(m_fb_fam), m_fb_fam, m_fb_ord))))) %>% 
  # Final M source
  mutate(m_source=ifelse(!is.na(m_fl), "FishLife",
                  ifelse(!is.na(m_fb_spp), "FB-species",
                         ifelse(!is.na(m_fb_gen), "FB-genus", 
                                ifelse(!is.na(m_fb_fam), "FB-family", "FB-order"))))) %>% 
  # Arrange
  select(class:comm_name, m_source, m, everything())

# Plot data
g <- ggplot(data, mapping=aes(x=m, y=reorder(isscaap, m), fill=class)) +
  geom_boxplot() +
  # Reference line
  geom_vline(xintercept = 0.2, linetype=2) +
  # Labels
  labs(x="Natural mortality (1/yr)", y="") +
  # Theme
  theme_bw()
g

# Export data
write.csv(data, file=file.path(datadir, "aquaculture_species_nat_mort_data.csv"), row.names=F)
  





