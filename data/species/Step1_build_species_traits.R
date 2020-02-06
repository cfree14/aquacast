
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
gentrydir <- "data/gentry"

# Read data
fao_aq <- readRDS("/Users/cfree/Dropbox/Chris/UCSB/data/fao/aquaculture/processed/1950_2017_fao_aquaculture_data.Rds")

# Read Gentry et al (2016) key
gentry_orig <- read.csv(file.path(gentrydir, "Gentry_etal_2017_supp_table4.csv" ), as.is=T)

# ISSCAAP group key
key <- read.csv("/Users/cfree/Dropbox/Chris/UCSB/data/fao/capture/raw/Capture_2019.1.0/CL_FI_SPECIES_GROUPS.csv", as.is=T) %>% 
  select(Scientific_Name, ISSCAAP_Group) %>% 
  rename(species=Scientific_Name, isscaap=ISSCAAP_Group) %>% 
  # Unique species
  unique() %>% 
  arrange(species) %>% 
  # Fix scientific names
  mutate(species=recode(species, 
                       "Acanthopagrus schlegeli"="Acanthopagrus schlegelii",
                       "Anadara granosa"="Tegillarca granosa",
                       "Chlamys varia"="Mimachlamys varia",
                       "Epinephelus septemfasciatus"="Hyporthodus septemfasciatus",
                       "Larimichthys croceus"="Larimichthys crocea",
                       "Liza vaigiensis"="Ellochelon vaigiensis",
                       "Lutjanus russelli"="Lutjanus russellii",
                       "Lyropecten subnodosus"="Nodipecten subnodosus",
                       "Mactra veneriformis"="Mactra quadrangularis",
                       "Mugil soiuy"="Planiliza haematocheila",
                       "Patinopecten yessoensis"="Mizuhopecten yessoensis",
                       "Protothaca staminea"="Leukoma staminea",
                       "Psetta maxima"="Scophthalmus maximus",
                       "Saccostrea cuccullata"="Saccostrea cucullata",
                       "Saxidomus giganteus"="Saxidomus gigantea",
                       "Scapharca broughtonii"="Anadara broughtonii",
                       "Strombus gigas"="Lobatus gigas",
                       "Venerupis aurea"="Tapes aureus",     
                       "Sebastes schlegeli"="Sebastes schlegelii",
                       "Acanthopagrus schlegeli schlegelii"="Acanthopagrus schlegelii",
                       "Anadara grandis (tuberculosa)"="Larkinia grandis",
                       "Anadara granosa"="Tegillarca granosa",
                       "Chelon macrolepis"="Planiliza macrolepis",
                       "Chlamys varia"="Mimachlamys varia", 
                       "Diplodus sargus sargus"="Diplodus sargus",
                       "Evynnis japonica"="Dentex tumifrons",
                       "Liza aurata"="Chelon auratus",
                       "Liza ramada"="Chelon ramada",
                       "Liza saliens"="Chelon saliens",
                       "Lutjanus russelli"="Lutjanus russellii",
                       "Lyropecten subnodosus"="Nodipecten subnodosus",
                       "Mactra veneriformis"="Mactra quadrangularis",
                       "Mytilus galloprovinciali s"="Mytilus galloprovincialis",
                       "Panopea generosa (abrupta)"="Panopea generosa",
                       "Patinopecten yessoensis"="Mizuhopecten yessoensis",
                       "Protothaca staminea"="Leukoma staminea",
                       "Psetta maxima"="Scophthalmus maximus",
                       "Pseudopleuron ectes americanus"="Pseudopleuronectes americanus",
                       "Saccostrea cuccullata"="Saccostrea cucullata",
                       "Salvelinus alpinus alpinus"="Salvelinus alpinus",
                       "Saxidomus giganteus"="Saxidomus gigantea",
                       "Scapharca broughtonii"="Anadara broughtonii",
                       "Tilapia guineensis"="Coptodon guineensis",
                       "Venerupis aurea"="Tapes aureus")) %>% 
  # Fix more names (the original names are in FishBase but not AquaMaps)
  mutate(species=recode(species, 
                        "Crassostrea gasar"="Crassostrea tulipa",
                        "Saccostrea commercialis"="Saccostrea glomerata",
                        "Venerupis pullastra"="Venerupis corrugata")) %>% 
  unique()


# Format FAO data
################################################################################

# Years
range(fao_aq$year)
2013:2017

# Mean production of marine AQ species from 2013-2017
maq_spp <- fao_aq %>% 
  # Reduce to marine species and 2010-2016
  filter(environment!="Freshwater" & year>=2013) %>% 
  # Calculate sum each year
  group_by(major_group, isscaap, order, family, comm_name, species_orig, year) %>% 
  summarize(prod_mt=sum(quantity_mt, na.rm=T),
            value_usd_t=sum(value_usd_t, na.rm=T)) %>% 
  # Calculate ex-vessel price each year
  mutate(price_usd_mt=value_usd_t*1000/prod_mt) %>% 
  ungroup() %>% 
  # Calculate average from 2013-2017
  group_by(major_group, isscaap, order, family, comm_name, species_orig) %>% 
  summarize(prod_mt=mean(prod_mt, na.rm=T),
            value_usd_t=mean(value_usd_t, na.rm=T),
            price_usd_mt=mean(price_usd_mt, na.rm=T)) %>% 
  ungroup() %>% 
  # Only finfish and molluscs (no reptiles/amphibians, plants, crustaceans, or other inverts)
  filter(major_group %in% c("Pisces", "Mollusca")) %>% 
  filter(!grepl("nei", comm_name)) %>% 
  filter(!grepl("spp", species_orig)) %>% 
  # Fix a few scientific names
  mutate(species=recode(species_orig, 
                        "Acanthopagrus schlegeli"="Acanthopagrus schlegelii",
                        "Anadara granosa"="Tegillarca granosa",
                        "Chlamys varia"="Mimachlamys varia",
                        "Epinephelus septemfasciatus"="Hyporthodus septemfasciatus",
                        "Larimichthys croceus"="Larimichthys crocea",
                        "Liza vaigiensis"="Ellochelon vaigiensis",
                        "Lutjanus russelli"="Lutjanus russellii",
                        "Lyropecten subnodosus"="Nodipecten subnodosus",
                        "Mactra veneriformis"="Mactra quadrangularis",
                        "Mugil soiuy"="Planiliza haematocheila",
                        "Patinopecten yessoensis"="Mizuhopecten yessoensis",
                        "Protothaca staminea"="Leukoma staminea",
                        "Psetta maxima"="Scophthalmus maximus",
                        "Saccostrea cuccullata"="Saccostrea cucullata",
                        "Saxidomus giganteus"="Saxidomus gigantea",
                        "Scapharca broughtonii"="Anadara broughtonii",
                        "Strombus gigas"="Lobatus gigas",
                        "Venerupis aurea"="Tapes aureus",     
                        "Sebastes schlegeli"="Sebastes schlegelii")) %>% 
  # Fix more names (the original names are in FishBase but not AquaMaps)
  mutate(species=recode(species, 
                        "Crassostrea gasar"="Crassostrea tulipa",
                        "Saccostrea commercialis"="Saccostrea glomerata",
                        "Venerupis pullastra"="Venerupis corrugata")) %>% 
  # Add source column
  mutate(source="FAO") %>% 
  select(source, everything()) %>% 
  # Remove hybrid species
  filter(species!="Morone chrysops x M. saxatilis") %>% 
  # Remove species with no production since 2010
  filter(prod_mt>0) %>% 
  # Add production rank
  group_by(major_group) %>% 
  arrange(major_group, desc(prod_mt)) %>% 
  mutate(prod_rank=1:n()) %>% 
  ungroup()

# Check species names
freeR::check_names(maq_spp$species)
freeR::suggest_names(maq_spp$species)

# Sample size
table(maq_spp$major_group)


# Format Gentry data
################################################################################

# Format Gentry data
gentry_spp <- gentry_orig %>% 
  rename(species_orig=species) %>% 
  # Fix scientific names
  mutate(species=recode(species_orig,
                        "Acanthopagrus schlegeli schlegelii"="Acanthopagrus schlegelii",
                        "Anadara grandis (tuberculosa)"="Anadara grandis",
                        "Anadara granosa"="Tegillarca granosa",
                        "Chelon macrolepis"="Planiliza macrolepis",
                        "Chlamys varia"="Mimachlamys varia", 
                        "Diplodus sargus sargus"="Diplodus sargus",
                        "Evynnis japonica"="Dentex tumifrons",
                        "Liza aurata"="Chelon auratus",
                        "Liza ramada"="Chelon ramada",
                        "Liza saliens"="Chelon saliens",
                        "Lutjanus russelli"="Lutjanus russellii",
                        "Lyropecten subnodosus"="Nodipecten subnodosus",
                        "Mactra veneriformis"="Mactra quadrangularis",
                        "Mytilus galloprovinciali s"="Mytilus galloprovincialis",
                        "Panopea generosa (abrupta)"="Panopea generosa",
                        "Patinopecten yessoensis"="Mizuhopecten yessoensis",
                        "Protothaca staminea"="Leukoma staminea",
                        "Psetta maxima"="Scophthalmus maximus",
                        "Pseudopleuron ectes americanus"="Pseudopleuronectes americanus",
                        "Saccostrea cuccullata"="Saccostrea cucullata",
                        "Salvelinus alpinus alpinus"="Salvelinus alpinus",
                        "Saxidomus giganteus"="Saxidomus gigantea",
                        "Scapharca broughtonii"="Anadara broughtonii",
                        "Tilapia guineensis"="Coptodon guineensis",
                        "Venerupis aurea"="Tapes aureus")) %>% 
  # Fix more names (the original names are in FishBase but not AquaMaps)
  mutate(species=recode(species, 
                        "Crassostrea gasar"="Crassostrea tulipa",
                        "Saccostrea commercialis"="Saccostrea glomerata",
                        "Venerupis pullastra"="Venerupis corrugata")) %>% 
  # Remove "Evynnis japonica" because it's the same as "Dentex tumifrons" and is missing a linf value
  filter(!species_orig %in% c("Evynnis japonica", "Anadara grandis (tuberculosa)")) %>% 
  unique()
  
# Check species names
freeR::check_names(gentry_spp$species)
freeR::suggest_names(gentry_spp$species)

# Check for duplicates
gentry_spp$species[duplicated(gentry_spp$species)]


# Merge FAO and Gentry data
################################################################################

# All species
all_spp <- sort(unique(c(maq_spp$species, gentry_spp$species)))
data <- freeR::taxa(all_spp) %>% 
  # Select columns
  select(class:genus, sciname) %>% 
  rename(species=sciname) %>% 
  # Reduce finfish/bivalves
  filter(class %in% c("Actinopterygii", "Bivalvia")) %>% 
  # Check which dataset
  mutate(fao=species %in% maq_spp$species,
         gentry=species %in% gentry_spp$species) %>% 
  # Add ISSCAAP code
  left_join(key, by="species") %>% 
  mutate(isscaap = ifelse(species=="Ostrea conchaphila", "Oysters", isscaap)) %>% 
  # Add common names
  left_join(select(maq_spp, species, comm_name), by="species") %>% 
  left_join(select(gentry_spp, species, comm_name), by="species") %>% 
  mutate(comm_name=ifelse(!is.na(comm_name.x), comm_name.x, comm_name.y)) %>% 
  # Add historic production (2013-2017) and rank
  left_join(select(maq_spp, species, prod_mt, prod_rank), by="species") %>% 
  rename(fao_rank=prod_rank, fao_mt_yr=prod_mt) %>% 
  # Select final columns
  select(class, isscaap, order:species, comm_name, gentry, fao, fao_rank, fao_mt_yr)

# Stats on final sample
freeR::complete(data)
table(data$class[data$fao==T])
n_distinct(data$order)
n_distinct(data$family)


# FishLife life history
################################################################################

# Get finfish K and linf
fl <- freeR::fishlife(data$species)



# FishBase/SeaLifeBase life history
################################################################################

# Get FB data for species of interest and all species in the genera of the species of interest
lw_spp <- freeR::fishbase("lw", data$species, "species", clean=T, add_taxa = F)
lw_gen <- freeR::fishbase("lw", data$species, "genus", clean=T, add_taxa = F)
lw_fam <- freeR::fishbase("lw", data$species, "family", clean=T, add_taxa = F)
vonb_spp <- freeR::fishbase("vonb", data$species, "species", clean=T, add_taxa = F)
vonb_gen <- freeR::fishbase("vonb", data$species, "genus", clean=T, add_taxa = F)
vonb_fam <- freeR::fishbase("vonb", data$species, "family", clean=T, add_taxa = F)

# Length-weight parameters
####################################

# Calculate LW species averages
lw_spp_avg <- lw_spp %>% 
  # Remove doubtful measurements
  filter(doubtful!="yes" | is.na(doubtful)) %>% 
  # Reduce to TL (or converted) or shell length (ShL) for inverts
  filter(type=="TL" | type=="ShL" | !is.na(a_tl)) %>% 
  # Remove more unrealistic measurements
  mutate(a=ifelse(a>2, NA, a),
         a_tl=ifelse(a_tl>2, NA, a_tl)) %>% 
  # Identify final a to use
  mutate(a_use=ifelse(!is.na(a_tl), a_tl, a)) %>% 
  group_by(species) %>% 
  summarize(n=n(),
            a_avg=mean(a_use, na.rm=T),
            b_avg=mean(b, na.rm=T))

# Calculate LW genus averages
lw_gen_avg <- lw_gen %>% 
  # Remove doubtful measurements
  filter(doubtful!="yes" | is.na(doubtful)) %>% 
  # Reduce to TL (or converted) or shell length (ShL) for inverts
  filter(type=="TL" | type=="ShL" | !is.na(a_tl)) %>% 
  # Remove more unrealistic measurements
  mutate(a=ifelse(a>2, NA, a),
         a_tl=ifelse(a_tl>2, NA, a_tl)) %>% 
  # Identify final a to use
  mutate(a_use=ifelse(!is.na(a_tl), a_tl, a),
         genus=sapply(species, function(x) strsplit(x, " ")[[1]][1])) %>% 
  group_by(genus) %>% 
  summarize(n=n(),
            a_avg=mean(a_use, na.rm=T),
            b_avg=mean(b, na.rm=T))

# Calculate LW family averages
fam_key <- freeR::taxa(lw_fam$species) %>% select(family, genus) %>% unique()
lw_fam_avg <- lw_fam %>% 
  # Reduce to TL (or converted) or shell length (ShL) for inverts
  filter(type=="TL" | type=="ShL" | !is.na(a_tl)) %>% 
  # Remove more unrealistic measurements
  mutate(a=ifelse(a>2, NA, a),
         a_tl=ifelse(a_tl>2, NA, a_tl)) %>% 
  # Identify final a to use
  mutate(a_use=ifelse(!is.na(a_tl), a_tl, a),
         genus=sapply(species, function(x) strsplit(x, " ")[[1]][1])) %>% 
  # Add family
  left_join(fam_key, by="genus") %>% 
  group_by(family) %>% 
  summarize(n=n(),
            a_avg=mean(a_use, na.rm=T),
            b_avg=mean(b, na.rm=T))

# Vonn B growth parameters
####################################

# Calculate VonB species averages
vonb_spp_avg <- vonb_spp %>% 
  # Remove doubtful measurements
  filter(vonb_quality!="doubtful" | is.na(vonb_quality)) %>% 
  # Reduce to TL (or converted) or shell length (ShL) for inverts
  filter(type=="TL" | type=="ShL" | !is.na(tl_linf_cm)) %>% 
  # Identify final a to use
  mutate(linf_cm_use=ifelse(!is.na(tl_linf_cm), tl_linf_cm, linf_cm)) %>% 
  group_by(species) %>% 
  summarize(n=n(),
            linf_cm_avg=mean(linf_cm_use, na.rm=T),
            k_avg=mean(k, na.rm=T))

# Calculate VonB genus averages
vonb_gen_avg <- vonb_gen %>% 
  # Remove doubtful measurements
  filter(vonb_quality!="doubtful" | is.na(vonb_quality)) %>% 
  # Reduce to TL (or converted) or shell length (ShL) for inverts
  filter(type=="TL" | type=="ShL" | !is.na(tl_linf_cm)) %>% 
  # Identify final a to use
  mutate(linf_cm_use=ifelse(!is.na(tl_linf_cm), tl_linf_cm, linf_cm),
         genus=sapply(species, function(x) strsplit(x, " ")[[1]][1])) %>% 
  group_by(genus) %>% 
  summarize(n=n(),
            linf_cm_avg=mean(linf_cm_use, na.rm=T),
            k_avg=mean(k, na.rm=T))

# Calculate VonB family averages
fam_key <- freeR::taxa(vonb_fam$species) %>% select(family, genus) %>% unique()
vonb_fam_avg <- vonb_fam %>% 
  # Remove doubtful measurements
  filter(vonb_quality!="doubtful" | is.na(vonb_quality)) %>% 
  # Reduce to TL (or converted) or shell length (ShL) for inverts
  filter(type=="TL" | type=="ShL" | !is.na(tl_linf_cm)) %>% 
  # Identify final a to use
  mutate(linf_cm_use=ifelse(!is.na(tl_linf_cm), tl_linf_cm, linf_cm),
         genus=sapply(species, function(x) strsplit(x, " ")[[1]][1])) %>% 
  # Add family
  left_join(fam_key, by="genus") %>% 
  group_by(family) %>% 
  summarize(n=n(),
            linf_cm_avg=mean(linf_cm_use, na.rm=T),
            k_avg=mean(k, na.rm=T))


# Merge data
####################################

# Merge data
final <- data %>%
  # Add price
  left_join(select(maq_spp, species, price_usd_mt), by="species") %>% 
  # Add FishLife Von B
  left_join(select(fl, species, linf_cm, k), by="species") %>% 
  rename(linf_cm_fl=linf_cm, k_fl=k) %>% 
  # Add FishBase Von B (species-specific)
  left_join(select(vonb_spp_avg, -n), by="species") %>% 
  rename(linf_cm_fb_spp=linf_cm_avg, k_fb_spp=k_avg) %>% 
  # Add FishBase Von B (genus-specific)
  left_join(select(vonb_gen_avg, -n), by="genus") %>% 
  rename(linf_cm_fb_gen=linf_cm_avg, k_fb_gen=k_avg) %>% 
  # Add FishBase Von B (family-specific)
  left_join(select(vonb_fam_avg, -n), by="family") %>% 
  rename(linf_cm_fb_fam=linf_cm_avg, k_fb_fam=k_avg) %>% 
  # Add Gentry Von B
  left_join(select(gentry_spp, species, linf_cm, k, temp_c_min, temp_c_max), by="species") %>% 
  rename(linf_cm_gentry=linf_cm, k_gentry=k) %>% 
  # Add FishBase LW (species-specific)
  left_join(select(lw_spp_avg, -n), by="species") %>% 
  rename(a_spp=a_avg, b_spp=b_avg) %>% 
  # Add FishBase LW (genus-specific)
  left_join(select(lw_gen_avg, -n), by="genus") %>% 
  rename(a_gen=a_avg, b_gen=b_avg) %>% 
  # Add FishBase LW (family-specific)
  left_join(select(lw_fam_avg, -n), by="family") %>% 
  rename(a_fam=a_avg, b_fam=b_avg) %>% 
  # Arrange columns
  select(everything(), temp_c_min, temp_c_max)
  
# Inspect data
freeR::complete(final)


# Export
################################################################################

# Export data
write.csv(final, file=file.path(datadir, "aquaculture_species_lh_data.csv"), row.names = F)




