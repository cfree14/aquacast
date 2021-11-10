
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
datadir <- "output/processed"
plotdir <- "figures"

# Read species data
load("data/species/data/aquaculture_species_key.Rdata")
spp_key <- data
rm(data_full, data)


# Read EEZ
eezdir <- "data/eezs"
eezs <- readRDS(file.path(eezdir, "eezs_v10_polygons.Rds"))
eez_key <- read.csv(file.path(eezdir, "eezs_v10_key.csv"), as.is=T)

# World
world <- rnaturalearth::ne_countries(scale="large", type = "countries", returnclass = "sf")

# Which
maq_do <- "bivalve"

# Setup theme
my_theme <- theme(axis.text=element_text(size=7),
                  axis.title=element_text(size=9),
                  plot.title=element_text(size=11),
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"))


# Function for plots
################################################################################

# Function to create figures
results_figs <- function(maq_do){
  
  # Read data
  data26 <- readRDS(file.path(datadir, paste0("RCP26_", stringr::str_to_title(maq_do), "_rational_w_eez_info.Rds")))
  data85 <- readRDS(file.path(datadir, paste0("RCP85_", stringr::str_to_title(maq_do), "_rational_w_eez_info.Rds")))

  # Merge data
  data <- rbind(data26 %>% mutate(rcp="RCP 2.6"),
                data85 %>% mutate(rcp="RCP 8.5"))

  # Continent-level outcomes
  ###############################################
  
  # Continent stats
  data_c <- data %>% 
    # Summarize outcomes
    group_by(rcp, year, ter1_continent) %>% 
    summarize(ncells=n(),
              area_sqkm=ncells*100/1e6,
              prod_mt=sum(prod_mt_yr, na.rm=T)/1e9,
              profits_usd=sum(profits_usd_yr)/1e12) %>% 
    # Spread
    gather(key="metric", value="value", 4:ncol(.)) %>% 
    filter(metric!="ncells") %>% 
    # Rename columns
    mutate(metric=recode(metric, 
                         "area_sqkm"="Area developed\n(millions of sqkm)",
                         "prod_mt"="Total production\n(billions of mt)", 
                         "profits_usd"="Total profits\n(trillions of USD)"))

  # Plot continental outcomes
  g <- ggplot(data_c, aes(x=year, y=value, fill=ter1_continent)) +
    geom_bar(stat="identity") +
    facet_wrap(rcp ~ metric, scales="free", ncol=3) +
    scale_fill_discrete(name="Continent") +
    labs(x="", y="") + 
    theme_bw() + my_theme +
    theme(strip.text = element_text(size=7),
          legend.title = element_text(size=7),
          legend.text = element_text(size=5))
  g
  
  # Export time series
  outfile <- paste0("figure_results_", maq_do, "_continent_time_series.png")
  ggsave(g, filename=file.path(plotdir, outfile), 
         width=6.5, height=4.5, units="in", dpi=600)
  
  
  # Territory-level outcomes
  ###############################################

  # # Territor-level stats
  # data_ter <- data %>% 
  #   # Summarize outcomes
  #   group_by(rcp, year, ter1_iso, ter1_name, ter1_continent) %>% 
  #   summarize(ncells=n(),
  #             area_sqkm=ncells*100/1e6,
  #             prod_mt=sum(prod_mt_yr, na.rm=T)/1e9,
  #             profits_usd=sum(profits_usd_yr)/1e12)
  # 
  # # Plot territory scatterplots: profits ~ production
  # g <- ggplot(data_ter, aes(x=prod_mt, y=profits_usd, color=ter1_continent)) +
  #   geom_point() +
  #   facet_wrap(~rcp) +
  #   scale_color_discrete(name="Continent") +
  #   labs(x="Total production\n(billions of mt)", y="Total profits\n(trillions of USD)") +
  #   theme_bw()
  # g


  # Species developed
  ###############################################

  # Build data
  spp <- data %>%
    group_by(rcp, year, species) %>% 
    summarize(prod_mt=sum(prod_mt_yr)) %>% 
    left_join(select(spp_key, species, comm_name, fao_rank)) %>% 
    mutate(spp_label=ifelse(!is.na(fao_rank), 
                            paste0(fao_rank, ". ", comm_name, "\n(", species, ")"),
                            paste0(comm_name, "\n(", species, ")")))
  
  # Plot species
  g <- ggplot(spp, aes(x=spp_label, y=prod_mt/1e9, fill=year)) +
    facet_wrap(~rcp) +
    geom_bar(stat="identity", position="dodge") +
    coord_flip() + 
    labs(y="Production (billions of mt)", x="") +
    scale_fill_discrete(name="") +
    theme_bw() + my_theme
  g
  
  # Export species
  outfile <- paste0("figure_results_", maq_do, "_species_developed.png")
  ggsave(g, filename=file.path(plotdir, outfile), 
         width=6.5, height=5.5, units="in", dpi=600)

  # Map by EEZ
  ###############################################
  
  # Calculate EEZ stats for plotting
  data_eez <- data %>% 
    group_by(rcp, year, ter1_iso, ter1_name, ter1_continent, eez_code, eez_name) %>% 
    summarize(ncells=n(),
              area_sqkm=ncells*100/1e6,
              prod_mt=sum(prod_mt_yr, na.rm=T)/1e9,
              profits_usd=sum(profits_usd_yr)/1e12)

  # Add production to EEZ shapefile
  # biv_eezs <- eezs %>% 
  #   left_join(filter(biv_eez, year==2100 & rcp=="RCP 8.5"), by="eez_code")
  data_eez_sp <- eez_key %>% 
    left_join(data_eez, by="eez_code") 

  # Plot maps
  g <- ggplot() +
    facet_grid(year ~ rcp) +
    geom_sf(data=world, fill="grey80", lwd=0.05, col="white") +
    geom_point(data=filter(data_eez_sp, !is.na(prod_mt)), 
               mapping=aes(x=long_dd, y=lat_dd, size=prod_mt)) +
    scale_size_continuous(name="Production (billions of mt)") +
    theme_bw() + my_theme +
    theme(axis.title = element_blank(),
          legend.position = "bottom")
  g
  
  # Export map
  outfile <- paste0("figure_results_", maq_do, "_map.png")
  ggsave(g, filename=file.path(plotdir, outfile), 
         width=6.5, height=5.5, units="in", dpi=600)
  
#   # Explore finfish scales
# hist(data_eez_sp$prod_mt, breaks=seq(0,10,0.25))
# data_eez_sp$prod_mt_cap <- pmin(data_eez_sp$prod_mt, 2)
# 
# # Plot finfish
# g <- ggplot() +
#   facet_grid(year ~ rcp) +
#   geom_sf(data=world, fill="grey80", lwd=0.05, col="white") +
#   geom_point(data=filter(biv_eezs, !is.na(prod_mt)), 
#              mapping=aes(x=long_dd, y=lat_dd, color=prod_mt_cap)) +
#   scale_color_gradientn(name="Production (billions of mt)",
#                         colors=rev(RColorBrewer::brewer.pal(9, "RdBu"))) +
#   theme_bw() +
#   theme(axis.title = element_blank())
# g
  
  # Calculate EEZ stats for table export
  results_out <- data %>% 
    # Summarize outcomes
    group_by(rcp, year, 
             eez_code, eez_name, eez_type, 
             ter1_name, ter1_iso, ter1_continent, 
             sov1_name, sov1_iso, sov1_continent) %>% 
    summarize(ncells=n(),
              area_sqkm=ncells*100,
              prod_mt=sum(prod_mt_yr, na.rm=T),
              profits_usd=sum(profits_usd_yr))
  
  # Export data
  write.csv(results_out, file=file.path(datadir, paste0(maq_do, "_mariculture_potential_by_eez_rcp.csv")), row.names=F)
  

}


results_figs("bivalve")
results_figs("finfish")





