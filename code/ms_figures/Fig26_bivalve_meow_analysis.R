

# Clear workspace
rm(list = ls())

# Turn off scientific notation
options(scipen=999)

# Setup
################################################################################

# Packages
library(tidyverse)
library(grid)
library(gridExtra)

# Directories
plotdir <- "figures"
tempdir <- "data/template"
datadir <- "/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/blue-paper-2/data/output/processed/"

# Read MEOW
meows <- sf::st_read("/Users/cfree/Dropbox/Chris/UCSB/data/meows/Marine_Ecoregions_Of_the_World_(MEOW)-shp/Marine_Ecoregions_Of_the_World__MEOW_.shp")

# Read data
data <- readRDS(file.path(datadir, "bivalve_output.Rds")) %>% 
  mutate(dev_scenario=as.character(dev_scenario))

# Read template
template <- raster::raster(file.path(tempdir, "world_raster_template_10km.tif"))
crs_use <- crs(template)

# Project MEOWS
meows_moll <- meows %>% 
  sf::st_transform(crs=crs_use)

# Calculate areas
area_m2 <- meows_moll %>% sf::st_area()
area_km2 <- measurements::conv_unit(area_m2, "m2", "km2") %>% as.numeric()

# Format MEOW dataframe
meows_df <- meows_moll %>% 
  sf::st_drop_geometry() %>% 
  janitor::clean_names("snake") %>% 
  mutate(area_sqkm=area_km2)

# Build data
################################################################################

# If building data
build <- T
if(build==T){
  
  # Development scenarios
  key <- data %>% 
    select(dev_scenario, period) %>% 
    unique()
  
  # Loop through development scenarios and perform analysis
  # i <- dev_scen[1]
  results <- purrr::map_df(1:nrow(key), function(i){
    
    # Convert data to raster
    # Because it is irregular, you can't use rasterfromxyz
    # Instead, you have to use rasterize, which requires an annoying set of ingredients
    dev_do <- key$dev_scenario[i]
    period_do <- key$period[i]
    xyz <- data %>% 
      filter(dev_scenario == dev_do & period==period_do) %>% 
      select(x, y, prod_mt_yr)
    xy <- xyz %>% 
      select(x, y) %>% 
      as.matrix()
    xyz_ras <- rasterize(x=xy, y=template, field=xyz$prod_mt_yr, fun=mean, background=0)
    
    # Calculate zonal stats
    vals <- raster::extract(x=xyz_ras, y=meows_moll, fun=sum, na.rm=T)
    
    # Build data
    out_df <- meows_df %>% 
      mutate(dev_scenario=dev_do,
             period=period_do, 
             biv_mt=vals, 
             biv_mt_sqkm=biv_mt/area_sqkm) %>% 
      select(dev_scenario, period, everything())
    
    # Plot results
    # hist(out_df$biv_mt_sqkm)
    
  })
  
  # Export data
  saveRDS(results, file=file.path(datadir, "bivalve_meow_mt_sqkm_by_dev_scenario.Rds"))
  
}else{
  
  # Read data
  results <- readRDS(file=file.path(datadir, "bivalve_meow_mt_sqkm_by_dev_scenario.Rds"))
  
}


# Plot data
################################################################################


# Plot data
g <- ggplot(results, aes(x=biv_mt_sqkm, fill=dev_scenario)) +
  facet_wrap(~period) +
  geom_density(alpha=0.3) +
  # Reference line
  geom_vline(xintercept=57.9, linetype="solid") +
  # Labs
  labs(x="Cultured bivalve density (mt / sqkm)", y="Density") +
  # Legend
  scale_fill_discrete(name="Development scenario") +
  # Theme
  theme_bw() +
  theme(legend.position = "bottom")
g
  
# Export plot
ggsave(g, filename=file.path(plotdir, "Fig26_bivalve_meow_analysis.png"), 
       width=6.5, height=3.5, units="in", dpi=600)



