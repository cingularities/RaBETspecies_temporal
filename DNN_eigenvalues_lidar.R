#This Rcode was written by Cynthia L. Norton, University of Arizona, 2021
#The code was used to create LiDAR model
# load libraries
library(raster)
library(lidR) 
library(rgdal) 
library(parallel)
library(MASS)
library(future)
setwd("//snow/projects/RaBET/RaBET_species/USGS_ARIZONA/")

##CHM for loop
USGS_pc = list.files("//snow/projects/RaBET/RaBET_species/USGS_ARIZONA/",pattern="*.laz$", full.names=TRUE)
LAS <- readUSGS_LPC_AZ_BrawleyRillito_2018_D19_12S_WA_2486

for(fileName in USGS_pc){
  #Bring point cloud into Rstudio
  #Identify ground points using a cloth simulation filter algorithm
  
  NEONpoints <-readLAS("//snow/projects/RaBET/RaBET_species/USGS_ARIZONA/USGS_LPC_AZ_CochiseCounty_2020_B20_12R_WA_7810.laz")
  
  #Calculate vegetation height above the ground
  NEON_DSM = normalize_height(NEONpoints, algorithm = knnidw(k = 12, p = 3), na.rm = TRUE)
  
  #Remove points that are below 0
  NEON_DSM_clean = filter_poi(NEON_DSM, Z < 45)
  
  NEON_CHM = grid_canopy(NEON_DSM_clean, res = 0.5, p2r(subcircle = 0.3, na.fill = NULL))
  
  #plot(NEON_DSM_clean)
  col  <- height.colors(50)
  eigm <- grid_metrics(NEON_DSM_clean, .stdshapemetrics, 0.5) #get grid metrics of point cloud in layers
  
  plot(eigm)
  
  NEON_CHM <- setExtent(NEON_CHM, eigm)
  NEON_CHM <- resample(NEON_CHM, eigm)
  raster <- stack(eigm,NEON_CHM)
  plot(raster$anisotropy)
  
  writeRaster(raster,   
              filename= "USGS_LPC_AZ_CochiseCounty_2020_B20_12R_WA_7810.tif",
              format="GTiff",
              overwrite=TRUE)

  #plot(NEON_DSM_clean)
  
  #Create canopy height model(raster) from the normalized canopy points
  
  #Export the Canopy Height Model to a .tif
  writeRaster(NEON_CHM,   
              filename=paste("//snow/projects/RaBET/RaBET_species/USGS_ARIZONA",substr(fileName, 49, 96), "_metrics"),
              format="GTiff",
              overwrite=TRUE)


}



for(fileName in USGS_pc){
  #Bring point cloud into Rstudio
  #Identify ground points using a cloth simulation filter algorithm
  
  NEONpoints <-readLAS(USGS_pc)
  
  col  <- height.colors(50)
  eigm <- grid_metrics(NEONpoints, .stdshapemetrics, 3) #get grid metrics of point cloud in layers
  
  
  
  #plot(NEON_DSM_clean)
  
  #Create canopy height model(raster) from the normalized canopy points
  
  #Export the Canopy Height Model to a .tif
  writeRaster(eigm,   
              filename=paste("//snow/projects/RaBET/RaBET_species/USGS_ARIZONA",substr(fileName, 49, 96), "_eigm"),
              format="GTiff",
              overwrite=TRUE)
}

