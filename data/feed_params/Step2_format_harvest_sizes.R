
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(stringr)
library(readxl)
library(tidyverse)

# Directories
indir <- "data/feed_params/raw"
outdir <- "data/feed_params/processed"
plotdir <- "data/feed_params/figures"
sppdir <- "data/species"

# Read data
data_orig <- read_excel(file.path(indir, "fao_aq_fact_sheet_info.xlsx"))

# Read species data
# spp_key <- read.csv(file.path(sppdir, "aquaculture_species_key.csv"), as.is=T)


# Format data
################################################################################

# Formats
data <- data_orig %>% 
  # Reduce
  select(type, species, harvest_size_notes, harvest_kg, harvest_mm) %>% 
  rename(species_orig=species) %>% 
  filter(!is.na(harvest_size_notes) & harvest_size_notes!="not food") %>% 
  # Split species names
  mutate(comm_name=str_extract(species_orig, "[^\\(]+") %>% trimws(),
         species=str_extract(species_orig, "(?<=\\().*?(?=\\))"),
         species=recode(species, 
                        "Catla catla"="Gibelion catla",
                        "Ctenopharyngodon idellus"="Ctenopharyngodon idella",
                        "Morone hybrid"="Morone saxatilis",
                        "Patinopecten yessoensis"="Mizuhopecten yessoensis",
                        "Pangasius hypophthalmus"="Pangasianodon hypophthalmus",
                        "Psetta maxima"="Scophthalmus maximus",
                        "Solea spp."="Solea solea",
                        "Trachinotus spp"="Trachinotus blochii")) %>% 
  # Arrange
  select(type, species_orig, comm_name, species, everything()) %>% 
  mutate(harvest_g=harvest_kg*1000,
         harvest_cm=harvest_mm/10)
  

# Inspect
freeR::check_names(data$species)


# Get life history data
################################################################################

# FishLife
fl_vonb <- freeR::fishlife(data$species)

# FishBase
fb_vonb <- freeR::fishbase(dataset="vonb", species=data$species, level="species")
fb_lw <- freeR::fishbase(dataset="lw", species=data$species, level="species")

# FishBase species medians

fb_vonb_spp <- fb_vonb %>% 
  # Reduce to TL (or converted) or shell length (ShL) for inverts
  filter(type=="TL" | type=="ShL" | !is.na(tl_linf_cm)) %>% 
  # Identify final a to use
  mutate(linf_cm_use=ifelse(!is.na(tl_linf_cm), tl_linf_cm, linf_cm)) %>% 
  group_by(species) %>% 
  summarize(n=n(),
            fb_linf_cm=median(linf_cm_use, na.rm=T),
            fb_k=median(k, na.rm=T))

fb_lw_spp <- fb_lw %>% 
  # Reduce to TL (or converted) or shell length (ShL) for inverts
  filter(type=="TL" | type=="ShL" | !is.na(a_tl)) %>% 
  # Identify final a to use
  mutate(a_use=ifelse(!is.na(a_tl), a_tl, a)) %>% 
  group_by(species) %>% 
  summarize(n=n(),
            fb_a=median(a_use, na.rm=T),
            fb_b=median(b, na.rm=T))

# Add 
data1 <- data %>% 
  # Add FishLife Von B params
  left_join(select(fl_vonb, species, linf_cm, k), by="species") %>% 
  # Add FishBase Von B params
  left_join(select(fb_vonb_spp, -n), by="species") %>% 
  # Add FishBase LW params
  left_join(select(fb_lw_spp, -n), by="species") %>% 
  # Derive missing harvest lengths from weights
  # W = aL^b
  # L = exp( log(W/a) / b)
  mutate(harvest_cm_calc=exp(log(harvest_g/fb_a)/fb_b),
         harvest_cm_final=ifelse(!is.na(harvest_cm), harvest_cm, harvest_cm_calc),
         linf_cm_final=ifelse(!is.na(linf_cm), linf_cm, fb_linf_cm)) %>% 
  # Add percent of Linf
  mutate(harvest_perc_linf=harvest_cm_final/linf_cm_final*100) %>% 
  # Reduce to ones with percs
  filter(!is.na(harvest_perc_linf)) %>% 
  mutate(type=recode(type, "Bivalve"="Bivalves") %>% factor(levels=c("Finfish", "Bivalves")))

# Sample size
table(data1$type)


# Plot data
################################################################################

# Calculate median
stats <- data1 %>% 
  group_by(type) %>% 
  summarize(n=n(), 
            harvest_perc_linf=median(harvest_perc_linf))

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
g <- ggplot(data1, aes(x=type, y=harvest_perc_linf, fill=type)) +
  geom_boxplot() +
  labs(x="", y="Harvest size as a\npercentage of asymptotic length") +
  geom_hline(yintercept=100, linetype="dotted") +
  geom_text(data=stats, mapping=aes(x=type, y=harvest_perc_linf,
                                    label=paste0(round(harvest_perc_linf), "%")), vjust=-0.5) +
  theme_bw() + my_theme
g  

# Export
ggsave(g, filename=file.path(plotdir, "figure_harvest_sizes_as_linf_perc.png"), 
       width=4.5, height=4.5, units="in", dpi=600)








