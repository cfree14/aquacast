
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
datadir1 <- "data/gentry"
datadir2 <- "data/froehlich"


# Format Gentry data
################################################################################

# Read Gentry data
prod_orig <-  read.csv(file.path(datadir1, "CountryProdPotentialLT12.csv"), as.is=T)
fprod_orig <- read.csv(file.path(datadir1, "FishProdByCountryFAOSummaryHDen.csv"), as.is=T)
bprod_orig <- read.csv(file.path(datadir1, "BivalveProdByCountryFAOSummary.csv"), as.is=T)

# Format production area
prod <- prod_orig %>% 
  select(-X) %>% 
  rename(eez_id=ID, 
         eez_name=LABEL, 
         b_sqkm=sumareabivalve, 
         b_phi_avg=bivalvephiavg, 
         f_sqkm=sumareafish, 
         f_phi_avg=fishphiavg, 
         eez_sqkm=totalEEZarea) %>% 
  select(eez_id, eez_name, eez_sqkm, b_sqkm, b_phi_avg, f_sqkm, f_phi_avg)


# The ProdMT columns are current production and are the same in both the f/b files

# Format finfish
fprod <- fprod_orig %>% 
  # Temporarily remove columns I don't understand
  select(-c(X,
         MaxDevPerCountry,
         MaxProdPerCountry,
         OnePercentDevPerCountry,
         ProdPerCountryOnePercent)) %>% 
  rename(eez_id=ID, 
         prod_mt=CountryYieldCumSum, 
         area_sqkm=CountryAreaCumSum,
         phi_max=MaxPhi, 
         phi_avg=averagePhi,
         phi_avg_wt=averageWeightedPhi,
         eez_name=LABEL,
         fprod_mt=MarFishProdMT,
         bprod_mt=MarMolluscProdMT) %>% 
  select(eez_id, eez_name, prod_mt, area_sqkm, phi_avg, fprod_mt, bprod_mt)


# Format bivalves
bprod <- bprod_orig %>% 
  # Temporarily remove columns I don't understand
  select(-c(X,
            MaxDevPerCountry,
            MaxProdPerCountry,
            OnePercentDevPerCountry,
            ProdPerCountryOnePercent)) %>% 
  rename(eez_id=ID, 
         prod_n=CountryYieldCumSum, 
         area_sqkm=CountryAreaCumSum,
         phi_max=MaxPhi, 
         phi_avg=averagePhi,
         phi_avg_wt=averageWeightedPhi,
         eez_name=LABEL,
         fprod_mt=MarFishProdMT,
         bprod_mt=MarMolluscProdMT) %>% 
  select(eez_id, eez_name, prod_n, area_sqkm, phi_avg, fprod_mt, bprod_mt)


# Format Froehlich data
################################################################################

# Read data
f1 <-  read.csv(file.path(datadir2, "CALC_EEZmoll_Finfish_Phi_rcp85_2010-2030_noZeros.csv"), as.is=T)
f2 <- read.csv(file.path(datadir2, "CALC_EEZmoll_Finfish_Phi_rcp85_2030-2050_noZeros.csv"), as.is=T)
f3 <-  read.csv(file.path(datadir2, "CALC_EEZmoll_Finfish_Phi_rcp85_2050-2070_noZeros.csv"), as.is=T)
f4 <- read.csv(file.path(datadir2, "CALC_EEZmoll_Finfish_Phi_rcp85_2070-2090_noZeros.csv"), as.is=T)

# Format data
data <- rbind(f1 %>% mutate(year="2010-30"),
              f2 %>% mutate(year="2030-50"),
              f3 %>% mutate(year="2050-70"),
              f4 %>% mutate(year="2070-90")) %>% 
  select(-X) %>% 
  rename(eez_id=ID, eez_name=LABEL, f_sqkm=sumareafish, f_phi=fishphiavg) %>% 
  select(eez_id, eez_name, year, everything()) %>% 
  arrange(eez_name, year)

# Read data
b1 <-  read.csv(file.path(datadir2, "CALC_EEZmoll_Bivalve_Phi_rcp85_2010-2030_noZeros.csv"), as.is=T)
b2 <- read.csv(file.path(datadir2, "CALC_EEZmoll_Bivalve_Phi_rcp85_2030-2050_noZeros.csv"), as.is=T)
b3 <-  read.csv(file.path(datadir2, "CALC_EEZmoll_Bivalve_Phi_rcp85_2050-2070_noZeros.csv"), as.is=T)
b4 <- read.csv(file.path(datadir2, "CALC_EEZmoll_Bivalve_Phi_rcp85_2070-2090_noZeros.csv"), as.is=T)

# Format data
bdata <- rbind(b1 %>% mutate(year="2010-30"),
               b2 %>% mutate(year="2030-50"),
               b3 %>% mutate(year="2050-70"),
               b4 %>% mutate(year="2070-90")) %>% 
  select(-X) %>% 
  rename(eez_id=ID, eez_name=LABEL, area_sqkm=sumareafish, phi=fishphiavg) %>% 
  select(eez_id, eez_name, year, everything()) %>% 
  arrange(eez_name, year)
