
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(raster)
library(tidyverse)
library(fasterize)
library(countrycode)

# Directories
eezdir <- "/Users/cfree/Dropbox/Chris/Rutgers/projects/trade_and_collapse/data/gis_data/World_EEZ_v10_20180221"
outputdir <- "data/eezs"
tempdir <- "data/template"

# Projections
moll <- CRS("+proj=moll")
wgs84 <- CRS("+init=epsg:4326")

# Read data
data_orig <- sf::st_read(dsn=eezdir, layer="eez_v10")

# Read raster template
ras_temp <- raster(file.path(tempdir, "world_raster_template_10km.tif"))


# Build data
################################################################################

# Inspect data
colnames(data_orig)

# Are MRGID and MRGID_EEZ identical? Yes!
sum(data_orig$MRGID!=data_orig$MRGID_EEZ)

# Is MRGID a unique identifier? Yes!
anyDuplicated(data_orig$MRGID)

# Format data
data <- data_orig %>% 
  # Rename columns
  rename(eez_code=MRGID, 
         eez_name=GeoName, 
         ter1_code=MRGID_Ter1, 
         eez_type=Pol_type, 
         sov1_code=MRGID_Sov1, 
         ter1_name=Territory1, 
         ter1_iso=ISO_Ter1, 
         sov1_name=Sovereign1, 
         ter2_code=MRGID_Ter2, 
         sov2_code=MRGID_Sov2, 
         ter2_name=Territory2, 
         ter2_iso=ISO_Ter2, 
         sov2_name=Sovereign2, 
         ter3_code=MRGID_Ter3, 
         sov3_code=MRGID_Sov3,
         ter3_name=Territory3,
         ter3_iso=ISO_Ter3,
         sov3_name=Sovereign3,
         long_dd=x_1,
         lat_dd=y_1,
         eez_code1=MRGID_EEZ,
         area_sqkm=Area_km2) %>% 
  # Add missing ISO3 columns
  mutate(sov1_iso=countrycode(sov1_name, "country.name", "iso3c"),
         sov2_iso=countrycode(sov2_name, "country.name", "iso3c"),
         sov3_iso=countrycode(sov3_name, "country.name", "iso3c")) %>% 
  # Rearrange columns
  select(eez_code, eez_code1, eez_name, eez_type, 
         ter1_code, ter1_name, ter1_iso,
         ter2_code, ter2_name, ter2_iso,
         ter3_code, ter3_name, ter3_iso,
         sov1_code, sov1_name, sov1_iso,
         sov2_code, sov2_name, sov2_iso,
         sov3_code, sov3_name, sov3_iso,
         long_dd, lat_dd, area_sqkm, everything()) %>% 
  select(-eez_code1) %>% 
  # Project
  sf::st_transform(crs=wgs84)

# Extract attribute data
data_df <- data %>% 
  sf::st_drop_geometry()

# Export
saveRDS(data, file=file.path(outputdir, "eezs_v10_polygons.Rds"))
write.csv(data_df, file=file.path(outputdir, "eezs_v10_key.csv"), row.names=F)


# Simplify EEZ polygons
################################################################################

# Testing
###################

# Extract EEZ for testing simplification
eez1 <- filter(data, sov1_name=="United States")
plot(eez1["eez_code"])

# Project and simplify
eez1_simple <- eez1 %>% 
  # Project
  sf::st_transform(crs=moll) %>% 
  sf::st_simplify(preserveTopology = TRUE, dTolerance = 10000) %>% 
  sf::st_transform(crs=moll) %>% 
  sf::st_transform(crs=wgs84)
  
# Plot simplified
plot(eez1_simple["eez_code"])

# The real deal
###################

# Project and simplify
eez_simple <- data %>% 
  sf::st_transform(crs=moll) %>% 
  sf::st_simplify(preserveTopology = TRUE, dTolerance = 10000) %>% 
  lwgeom::st_make_valid() %>% 
  sf::st_transform(crs=wgs84)

# Plot simplified
# plot(eez_simple["eez_code"])

# Export
saveRDS(eez_simple, file=file.path(outputdir, "eezs_v10_polygons_simplified.Rds"))

# Build raster
################################################################################

# This code is, unfortunately, purposefully convoluted 
# When I project the polygons to Mollweide and attempt to rasterize, there are band of data in the Arctic
# So, I have to rasterize in WGS84 and then project to Mollweide

# WGS84 template
wgs84_template <- raster::raster(crs=wgs84, res=0.05,
                                 xmn=-180, xmx=180, ymn=-90, ymx=90)

# Rasterise in WGS84
data_ras_wgs84 <- fasterize(data, wgs84_template, field="eez_code", background=NA, fun="first")

# Project to Mollweide
data_ras_moll <- projectRaster(data_ras_wgs84, to=ras_temp, method="ngb")

# Confirm that codes in raster match codes in key
codes_raster <- sort(unique(data_ras_moll ))
sum(!codes_raster %in% data_df$eez_code) # MUST BE ZERO

# Plot raster
# plot(data_ras_wgs84)
# plot(data_ras_moll)

# Plot raster
# data_ras_df <- raster::as.data.frame(data_ras_moll, xy=T) %>% 
#   mutate(layer=as.factor(layer))
# g <- ggplot(data_ras_df, aes(x=x, y=y, fill=layer)) +
#   scale_fill_discrete(na.value=NA) +
#   geom_raster() + theme_bw() + theme(legend.position="none")
# g

# Compare mask to raster template
raster::compareRaster(data_ras_moll, ras_temp)

# Export raster
writeRaster(data_ras_moll, file=file.path(outputdir, "eezs_v10_raster_10km.tif"), overwrite=T)










