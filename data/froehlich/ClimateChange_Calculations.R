##libraries useful for raster analysis

library(raster)    
library(rasterVis)  
library(maps)         
library(dplyr) 
library(rgdal)  
library(RColorBrewer)

#set working directory
#Load OHI2012 Countries
OHIcountries=raster("~/OHI2012/regions.tif")
OHIcountries

CountryData<-read.csv("~/OHI2012/country list_OHI2012.csv")
head(CountryData)

#Load list of desired layer (GPI or Proportions)   
ras<-list.files(path=getwd(), pattern="Prop.*.tif$")

#set tmp directory
  tmpdir='~/Big'
  dir.create(tmpdir, showWarnings=F)
  rasterOptions(tmpdir=tmpdir)


#loop through tifs

for(i in 1:length(ras)){
  
  print(ras[i])
  
  one_ras = raster(ras[i])
  
  #####new method to calculate area for fish or bivalve####
  AreaFish=one_ras
  AreaFish[AreaFish != (0)]<-0.87329 ##size of cell in km2 # taken by squaring .9344789
  AreaFishbyCountry=zonal(AreaFish,OHIcountries, fun="sum",na.rm=TRUE,progress=TRUE)
  AreaFishbyCountry = as.data.frame(AreaFishbyCountry)
  colnames(AreaFishbyCountry)=c("ID","sumarea")
  
  #Calculate Average Phi Prime for each country
  FishPhiAveragebyCountry=zonal(one_ras,OHIcountries,fun='mean',na.rm=TRUE)
  FishPhiAveragebyCountry = as.data.frame(FishPhiAveragebyCountry)
  colnames(FishPhiAveragebyCountry)= c("ID", "phiavg")
  
  #Combined data
  CountryPhiAreaData= left_join(CountryData,AreaFishbyCountry,by="ID", copy=TRUE)
  
  CountryPhiData=left_join(CountryPhiAreaData, FishPhiAveragebyCountry,by="ID", copy=TRUE)
  
  #Write .CSV with calculations
  write.csv(CountryPhiData, paste('CALC_',ras[i],'.csv',sep=""))

}

  
read.csv("CALC_EEZmoll_Finfish_Phi_rcp85_2070-2090_noZeros.tif.csv")

