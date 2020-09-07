
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(raster)
library(ggplot2)
library(tidyverse)
library(countrycode)
library(rnaturalearth)

# Directories
eezdir <- "data/eezs"
plotdir <- "figures"
tabledir <- "tables"

# Read EEZ
eezs <- readRDS(file.path(eezdir, "eezs_v10_polygons.Rds"))
eez_key <- read.csv(file.path(eezdir, "eezs_v10_key.csv"), as.is=T)
eezs_remote <- read.csv(file.path(eezdir, "eezs_v10_uninhabited.csv"), as.is=T) %>% 
  mutate(eez_name=recode(eez_name, 
  "Colombian Exclusive Economic Zone (Quitasue\x96o)"="Colombian Exclusive Economic Zone (Quitasueño)"))

# Classify EEZs
table(eez_key$eez_type)
eezs <- eezs %>% 
  mutate(type=ifelse(eez_name %in% eezs_remote$eez_name, "Uninhabited", eez_type %>% as.character()),
         type=recode(type, "200NM"="Considered in analysis"))
table(eezs$type)

# Create EEZ points files
moll <- CRS("+proj=moll")
eezs_pts <- eezs %>% 
  sf::st_transform(moll) %>% 
  sf::st_centroid()

# World
world <- rnaturalearth::ne_countries(scale="large", type = "countries", returnclass = "sf")


# Table
################################################################################

eezs_exclude <- eez_key %>% 
  filter(eez_name %in% eezs_remote$eez_name) %>% 
  select(sov1_name, ter1_name) %>% 
  arrange(sov1_name, ter1_name) %>% 
  rename("Sovereign nation"=sov1_name, "Territory"=ter1_name) 

write.csv(eezs_exclude, file.path(tabledir, "TableS10_eezs_uninhabited.csv"), row.names=F)
  

# Plot data
################################################################################

# My theme
my_theme <- theme(axis.text=element_text(size=8),
                  axis.title=element_text(size=10),
                  plot.title=element_text(size=12),
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"))
         
# Plot data (polygon version)
g <- ggplot(eezs) +
  geom_sf(aes(fill=type), lwd=0.05, col="grey30") + # poly version
  geom_sf(data=world, fill="grey80", lwd=0.05, color="white") +
  scale_fill_discrete(name="") +
  theme_bw() + my_theme + 
  theme(legend.position = "bottom")
g

# Plot data (point version)
# g <- ggplot(eezs_pts) +
#   geom_sf(aes(color=type), size=4) + # point version
#   geom_sf(data=world, fill="grey80", lwd=0.05, color="white") +
#   scale_color_discrete(name="") +
#   theme_bw() + my_theme +
#   theme(legend.position = "bottom")
# g

# Export plot
ggsave(g, filename=file.path(plotdir, "figure_eezs_evaluated.png"), 
       width=6.5, height=3.75, units="in", dpi=600)




