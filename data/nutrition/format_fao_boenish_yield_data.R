
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(rio)
library(ggplot2)
library(tidyverse)
library(readr)

# Directories
datadir <- "data/nutrition/data"

# Read FAO data
fao_orig <- read.csv(file.path(datadir, "FAO_1989_Table1_yield_from_whole.csv"), as.is=T, na.strings=c("", "-"))

# FAO species key 
fao_spp <- read.csv("/Users/cfree/Dropbox/Chris/UCSB/projects/protein_curve/ocean-protein/aquaculture/data/raw/Capture_2018.1.2/CL_FI_SPECIES_GROUPS.csv", as.is=T) %>% 
  select(ISSCAAP_Group, Major_Group, Order, Family, Scientific_Name, Name_En) %>% 
  rename(comm_name=Name_En, species=Scientific_Name, family=Family, order=Order, class=Major_Group, isscaap=ISSCAAP_Group) %>% 
  mutate(family=stringr::str_to_title(family),
         order=stringr::str_to_title(order),
         class=stringr::str_to_title(class))

# Format FAO data
################################################################################

# Footnotes from original table:
# Composition/energy: whole fish (flesh)
# Fat/energy: weighted mean of the second and third figures (Atlantic caught) (Baltic-caught)
# Meat: meat w/ roe (meat w/o roe)
# Fat/energy: wild (farmed)

# Format FAO data
fao <- fao_orig %>%
  # Remove blank
  filter(!is.na(kcalorie)) %>% 
  # Format common names
  rename(comm_name_orig=species) %>% 
  mutate(comm_name_orig=gsub("\\([^()]+\\)", "", comm_name_orig) %>%  str_trim()) %>% 
  mutate(comm_name_orig=gsub("  ", " ", comm_name_orig)) %>% 
  mutate(comm_name_orig=recode(comm_name_orig,
                          "Japanese Manila) clam ("="Japanese Manila clam",
                          "king) mackerel ("="Narrow-barred Spanish king mackerel", 
                          "Japanese claim"="Japanese clam",
                          "Northern white strimp"="Northern white shrimp")) %>% 
  mutate(comm_name=recode(comm_name_orig,
                          "American plaice"="European plaice", # based on Table 1 footnote
                          "Anchoveta"="Anchoveta(=Peruvian anchovy)",
                          "Alaska pollack"="Alaska pollock(=Walleye poll.)",
                          "Atlantic redfishes"="Atlantic redfishes nei",
                          "Black Sea sprat"="European sprat", # based on Table 1 footnote
                          "Blue whiting"="Blue whiting(=Poutassou)",
                          "Bombay duck"="Bombay-duck",
                          "Central Pacific anchoveta"="Pacific anchoveta",
                          "Characins"="Characins nei",
                          "Chichlids nei"="Cichlids nei",
                          "Chilean hake"="South Pacific hake", # based on Googling
                          "Chub mackerel"="Atlantic chub mackerel", # totally arbitray choice over Pacific chub mackerel
                          "Chum salmon"="Chum(=Keta=Dog) salmon",
                          "Clams nei"="Venus clams nei",
                          "Coho salmon"="Coho(=Silver) salmon",
                          "Common squids"="Common squids nei",
                          "Common scallop"="Scallops nei",
                          "Cuttlefishes, bobtail squids"="Cuttlefish, bobtail squids nei",
                          "Dagaas"="Dagaas (=Kapenta)",
                          "Dentex seabreams, etc nei"="Dentex nei",
                          "European pilchard"="European pilchard(=Sardine)",
                          "Flathead mullet"="Flathead grey mullet",
                          "Filefishes"="Filefishes nei",
                          "Hard clam"="Hard clams nei",
                          "Hairtails, cutlassfishes"="Hairtails, scabbardfishes nei",
                          "Japanese clam"="Japanese hard clam",
                          "Japanese Manila clam"="Japanese carpet shell",
                          "Japanese scallop"="Yesso scallop",
                          "Jellyfishes"="Jellyfishes nei",
                          "Narrow-barred Spanish king mackerel"="Narrow-barred Spanish mackerel",
                          "Northern bluefin tuna"="True tunas nei",
                          "Octopuses"="Octopuses nei",
                          "North Pacific anchovy"="Californian anchovy",
                          "Pink salmon"="Pink(=Humpback) salmon",
                          "Ponyfishes nei"="Ponyfishes(=Slipmouths) nei",
                          "Saithe"="Saithe(=Pollock)",
                          "Scads"="Scads nei",
                          "Scorpionfishes, etc nei"="Scorpionfishes, rockfishes nei",
                          "Sea scallop"="Pecten scallops nei",
                          "Short neck clams"="Short neck clams nei",
                          "Sockeye salmon"="Sockeye(=Red) salmon",
                          "Stolephorus anchovies"="Stolephorus anchovies nei",
                          "Surf clam"="Mactra surf clams nei",
                          "Squids nei"="Various squids nei",
                          "Triggerfishes, durgons"="Triggerfishes, durgons nei")) %>% 
  # Add scientific names
  left_join(fao_spp, by="comm_name") %>% 
  # Rename yield columns in prep for formatting
  rename(fillet_orig=fillet, flesh_orig=flesh, meat_orig=meat, 
         protein_orig=protein, fat_orig=fat, glycogen_orig=glycogen, kcal_orig=kcalorie, kjoules_orig=kjoules) %>% 
  # Format yield numbers
  mutate(fillet=readr::parse_number(fillet_orig),
         flesh=flesh_orig,
         meat1=readr::parse_number(meat_orig),
         meat2=ifelse(meat_orig=="[15],[6]", 6, NA),
         protein1=readr::parse_number(protein_orig), 
         protein2=ifelse(protein_orig=="[16.0],15.8", 15.8, 
                         ifelse(protein_orig=="18.0(20.2)", 20.2, NA)),
         fat1=readr::parse_number(fat_orig), 
         fat2=ifelse(fat_orig=="[0.8],0.6", 0.6, 
                         ifelse(fat_orig=="2.3(12.3)", 12.3, 
                                ifelse(fat_orig=="2.8(4.2)", 4.2, 
                                       ifelse(fat_orig=="13.8(14.2/6.1)", 14.2, NA)))),
         fat3=ifelse(fat_orig=="13.8(14.2/6.1)", 6.1, NA),
         glycogen1=readr::parse_number(glycogen_orig),
         glycogen2=ifelse(glycogen_orig=="[2.9],2.4", 2.4, NA),
         kcal1=readr::parse_number(kcal_orig),
         kcal2=ifelse(kcal_orig=="[87],83", 83, 
                       ifelse(fat_orig=="102(124)", 124, 
                              ifelse(fat_orig=="115(205)", 115, 
                                     ifelse(fat_orig=="200(204/131)", 204, NA)))),
         kcal3=ifelse(kcal_orig=="200(204/131)", 131, NA),
         kjoules1=readr::parse_number(kjoules_orig),
         kjoules2=ifelse(kjoules_orig=="[366],346", 346, 
                      ifelse(fat_orig=="427(519)", 519, 
                             ifelse(fat_orig=="480(857)", 857, 
                                    ifelse(fat_orig=="839(854/548)", 854, NA)))),
         kjoules3=ifelse(kjoules_orig=="839(854/548)", 548, NA)) %>% 
  # Add final percent edible meat column
  mutate(edible=ifelse(class=="Pisces", flesh, meat1)) %>% 
  # Arrange columns
  select(class:species, comm_name, comm_name_orig, isscaap,
         edible, 
         fillet_orig, fillet,
         flesh_orig, flesh,
         meat_orig, meat1, meat2,
         protein_orig, protein1, protein2,
         fat_orig, fat1, fat2, fat3,
         glycogen_orig, glycogen1, glycogen2, 
         kcal_orig, kcal1, kcal2, kcal3,
         kjoules_orig, kjoules1, kjoules2, kjoules3,
         everything())
  
# Inspect data
str(fao)

# Fuzzy match missing common names
missing_names <- fao$comm_name[is.na(fao$species)]
sapply(missing_names, function(x) fao_spp$comm_name[agrep(x, fao_spp$comm_name, max.distance=0.2)])

# Any duplicated scientific names?
fao$species[duplicated(fao$species)]


# Format for plotting
sdata <- fao %>%
  filter(class %in% c("Mollusca", "Pisces")) %>% 
  mutate(class=recode(class, "Mollusca"="Bivalves", "Pisces"="Finfish"))

# Boxplot yield
g <- ggplot(sdata, aes(x=class, y=edible, fill=class)) +
  geom_boxplot() +
  expand_limits(y=0) +
  labs(x="", y="Percent edible meat") +
  theme_bw() +
  theme(legend.position = "none")
g


# # Format data
# ################################################################################
# # Read data
# data_orig <- import(file.path(datadir, "Food_yield_FAO_others.xlsx"), which=1)
# data_orig[data_orig=="-"] <- NA
# 
# # Format data
# data <- data_orig %>% 
#   # Remove columns
#   select(colnames(.)[!grepl("X__", colnames(.))]) %>% 
#   # Rename columns
#   setNames(tolower(colnames(.))) %>% 
#   rename(vert="vert/invert", 
#          fillet="skinless fillet", 
#          flesh="edible flesh", 
#          meat=meats, 
#          reference=source) %>% 
#   # Remove anything in ()s in species name
#   mutate(species=gsub("\\([^()]+\\)", "", species) %>%  str_trim()) %>% 
#   # Format fillet number
#   mutate(fillet=gsub("\\[", "", fillet),
#          fillet=gsub("\\]", "", fillet),
#          fillet=as.numeric(fillet)) %>% 
#   # Format meat
#   mutate(meat=gsub("\\[", "", meat),
#          meat=gsub("\\]", "", meat))
# 
# str(data)
# 
# sort(unique(data$species))
  
  
  
