
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(raster)
library(ggplot2)
library(tidyverse)

# Directories
sppdir <- "data/species/data"
inputdir <- "/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/blue-paper-2/data/output/raw"
outputdir <- "/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/blue-paper-2/data/output/processed"

# Read species data
load(file.path(sppdir, "aquaculture_species_key.Rdata"))


# Setup
################################################################################

# Function to merge results
rcp <- "rcp85"; type <- "bivalve"; outdir <- outputdir
merge_results <- function(rcp, type, outdir){
  
  # Species key
  type_do <- ifelse(type=="finfish", "Actinopterygii", "Bivalvia")
  spp_key <- data %>% 
    filter(class==type_do) %>% 
    select(species, comm_name) %>% 
    mutate(file_name=paste0(toupper(rcp), "_", gsub(" ", "_", species), ".Rds"))
  
  # Identify files to merge
  files_do <- spp_key$file_name
  
  # Chunk files into groups of 20
  # I have to do this b/c I was using up all memory on finfish and crashing R
  # x <- 1; y <- files_do_chunk[1]
  files_do_chunks <- split(files_do, ceiling(seq_along(files_do)/20))
  data <- purrr::map_df(1:length(files_do_chunks), function(x){
    
    # Chunk to do
    files_do_chunk <- files_do_chunks[[x]]
    
    # Loop through files and merge
    data_chunk <- purrr:::map_df(files_do_chunk, function(y){
      
      # Read one file
      # file_do <- files_do[1]
      file_do <- y
      spp_do <- file_do %>% gsub(".Rds", "", .) %>% gsub(paste0(toupper(rcp), "_"), "", .) %>% gsub("_", " ", .)
      sdata <- readRDS(file.path(inputdir, file_do)) %>% 
        filter(profits_usd_yr>0) %>% 
        mutate(species=spp_do) %>% 
        select(species, everything())
      
    })
    
    # Identify most profitable species for a cell in this chunk
    results_chunk <- data_chunk %>% 
      group_by(period, x, y) %>% 
      arrange(period, x, y, desc(profits_usd_yr)) %>% 
      slice(1) %>% 
      ungroup()
    
  })
  
  # Identify most profitable species for a cell across all chunks
  results <- data %>% 
    group_by(period, x, y) %>% 
    arrange(period, x, y, desc(profits_usd_yr)) %>% 
    slice(1) %>% 
    ungroup()
  
  # Calculate year stats
  ystats <- results %>%
    group_by(period) %>% 
    summarise(ncells=n(),
              area_sqkm=n()*100/1e6,
              prod_mt=sum(prod_mt_yr)/1e9,
              profits_usd=sum(profits_usd_yr)/1e12) %>% 
    ungroup()
  
  # Plot production trends
  g1 <- ggplot(ystats, aes(x=period, y=prod_mt)) +
    geom_bar(stat="identity") +
    labs(x="", y="Production (billions of mt)") +
    theme_bw()
  print(g1)

  # Plot 2100 production
  period_last <- unique(ystats$period)[length(unique(ystats$period))]
  title_text <- paste(toupper(rcp), str_to_title(type), period_last, "production")
  g2 <- ggplot(filter(results, period==period_last),
              aes(x=x, y=y, fill=prod_mt_yr/1e6)) +
    geom_tile() +
    labs(x="", y="", title=title_text) +
    scale_fill_gradientn(name="Production (millions mt)",
                         colors=rev(RColorBrewer::brewer.pal(9, "RdBu"))) +
    theme_bw()
  print(g2)
  
  # Export data
  outfile <- paste(toupper(rcp), str_to_title(type), "rational.Rds", sep="_")
  saveRDS(results, file.path(outdir, outfile))
  
}

# Finfish - 11 minutes each
merge_results(rcp="RCP26", type="finfish", outdir=outdir)
merge_results(rcp="RCP45", type="finfish", outdir=outdir)
merge_results(rcp="RCP60", type="finfish", outdir=outdir)
merge_results(rcp="RCP85", type="finfish", outdir=outdir)

# Bivalves
merge_results(rcp="RCP26", type="bivalve", outdir=outdir)
merge_results(rcp="RCP45", type="bivalve", outdir=outdir)
merge_results(rcp="RCP60", type="bivalve", outdir=outdir)
merge_results(rcp="RCP85", type="bivalve", outdir=outdir)




