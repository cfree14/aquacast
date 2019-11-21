
# Setup
################################################################################

# Packages
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)

# For testing
if(F){
  file <- "Gadus_morhua.csv"
  filedir <- "data/aquamaps/data/raw"
  data <- format_aquamaps(file, filedir)
}


# Format AquaMaps data
format_aquamaps <- function(file, filedir){
  
  # Read data
  data_orig <- read.csv(file.path(filedir, file), as.is=T, 
                   row.names=NULL, col.names=c(paste0("col", 1:15)), header=F)
  
  # Get common name
  comm_name <- gsub("[\\(\\)]", "", regmatches(data_orig[1,1], gregexpr("\\(.*?\\)", data_orig[1,1]))[[1]])
  
  # Get FAO areas
  fao_row <- which(grepl("FAOAreas:", data_orig$col1))
  fao_areas <- sort(as.numeric(unlist(strsplit(gsub(" ", "", gsub("FAOAreas: ", "", data_orig$col1[fao_row])), "\\|"))))
  
  # Bet bounding box
  bbox_row <- which(grepl("NSWE", data_orig$col1))
  bbox_coords <- as.numeric(data_orig[bbox_row, 2:5])
  names(bbox_coords) <- c("N", "S", "W", "E")
  
  # Isolate and format p(occurence data)
  # First "Genus" marks start of occurence data and second marks start of envi data
  # There is 1 row in between the occurence and envi data
  genus.rows <- which(data_orig$col1=="Genus")
  pdata1 <- genus.rows[1]+1
  pdata2 <- genus.rows[2]-2
  pdata <- data_orig %>% 
    dplyr::slice(pdata1:pdata2) %>%
    dplyr::select(paste0("col", 1:6)) %>% 
    dplyr::rename(genus=col1, species=col2, lat_dd=col3, long_dd=col4, cell_id=col5, prob=col6) %>% 
    mutate(sci_name=paste(genus, species), 
           comm_name=comm_name,
           lat_dd=as.numeric(lat_dd),
           long_dd=as.numeric(long_dd),
           prob=as.numeric(prob)) %>% 
    dplyr::select(sci_name, comm_name, everything())
  
  # Isolate and format environmental data
  edata1 <- genus.rows[2]+1
  edata2 <- nrow(data_orig)
  edata <- data_orig %>% 
    slice(edata1:edata2) %>%
    dplyr::select(paste0("col", 1:11)) %>% 
    dplyr::rename(genus=col1, species=col2, lat_dd=col3, long_dd=col4, cell_id=col5, 
           depth_m=col6, sst_c=col7, sal_psu=col8, pp_mgc=col9, ice_pc=col10, land_km=col11) %>% 
    mutate(sci_name=paste(genus, species),
           comm_name=comm_name,
           lat_dd=as.numeric(lat_dd),
           long_dd=as.numeric(long_dd),
           depth_m=as.numeric(depth_m),
           sst_c=as.numeric(sst_c),
           sal_psu=as.numeric(sal_psu),
           pp_mgc=as.numeric(pp_mgc),
           ice_pc=as.numeric(ice_pc),
           land_km=as.numeric(land_km)) %>% 
    dplyr::select(sci_name, comm_name, everything())
  
  # Get scientific name
  sci_name <- unique(pdata$sci_name)
  
  # Isolate and format envelope data
  qdata1 <- which(data_orig$col1=="Depth (m)")
  qdata2 <- which(data_orig$col1=="Distance to Land (km)")
  qdata <- data_orig %>%
    slice(qdata1:qdata2) %>% 
    dplyr::select(paste0("col", 1:6)) %>% 
    dplyr::rename(variable=col1, used=col2, min=col3, q10=col4, q90=col5, max=col6) %>% 
    mutate(sci_name=sci_name,
           comm_name=comm_name,
           variable=revalue(variable, c("Temperature (\xb0C)"="Temperature (C)",
                                        "Primary Production"="Primary production (mgC/m2/day)",
                                        "Sea Ice Concentration"="Sea ice cover (%)",
                                        "Distance to Land (km)"="Distance to land (km)")),
           used=as.numeric(used),
           min=as.numeric(min),
           q10=as.numeric(q10),
           q90=as.numeric(q90),
           max=as.numeric(max)) %>% 
    dplyr::select(sci_name, comm_name, everything())
  
  # # Transform p(occurence) data
  # pdata_mat1 <- pdata %>% 
  #   dplyr::select(lat_dd, long_dd, prob) %>% 
  #   tidyr::spread(key=long_dd, value=prob) %>% 
  #   arrange(desc(lat_dd))
  # 
  # # Convert to matrix
  # pdata_mat2 <- as.matrix(dplyr::select(pdata_mat1, -lat_dd))
  # dimnames(pdata_mat2)[[1]] <- pdata_mat1[,1]

  # Package data for export
  # pdata_mat <- pdata_mat2
  data <- list(fao_areas=fao_areas, bbox=bbox_coords, pdata=pdata, edata=edata, qdata=qdata) # pdata_mat=pdata_mat, 
  return(data)
  
}
