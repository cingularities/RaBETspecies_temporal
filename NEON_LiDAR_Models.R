#This Rcode was written by Cynthia L. Norton, University of Arizona, 2021
#The code was used to create LiDAR model
# load libraries
library(raster)
library(lidR) 
library(rgdal) 
library(sf) 
library(rgeos) 
library(sp)


#Bring point cloud into Rstudio
NEONpoints = readLAS("P:/RaBET/Raw/NEON/2019/Las/NEON_D14_SRER_DP1_510000_3518000_classified_point_cloud.las",filter = "-drop_z_below 1000 -drop_z_above 1100")


#Identify ground points using a cloth simulation filter algorithm
NEONpoints_classified = classify_ground(NEONpoints, algorithm = csf(sloop_smooth = TRUE, class_threshold = 0.55, cloth_resolution =  0.2, rigidness = 2))
#plot(NEONpoints_classified)
#Separate the classified point cloud into a ground point cloud and canopy point cloud
ground_NEONpoints = filter_poi(NEONpoints_classified, Classification == 2)
canopy_NEONpoints = filter_poi(NEONpoints_classified, Classification == 1)

#Create a grided digital terrain model
NEON_DTM = grid_terrain(NEONpoints, res = 0.5, algorithm = knnidw(k = 12, p = 2))
#plot(NEON_DTM)

#Create slope
NEON_slope <- terrain(NEON_DTM, opt="slope",units = "radians")
#plot(NEON_slope)

#Create aspect
NEON_aspect <- terrain(NEON_DTM, opt ="aspect", units = "radians")
#plot(NEON_aspect)

#Create aspect
NEON_hillshade <- hillShade(NEON_slope, NEON_aspect)
#plot(NEON_hillshade)

#Calculate vegetation height above the ground
NEON_DSM = normalize_height(NEONpoints, algorithm = knnidw(k = 12, p = 2))

#Remove points that are below 0
NEON_DSM_clean = filter_poi(NEON_DSM, Z < 15)

#plot(NEON_DSM_clean)

#Create canopy height model(raster) from the normalized canopy points
NEON_CHM = grid_canopy(NEON_DSM_clean, res = 1, p2r(0.25))
#windows()
#plot(NEON_CHM, col = col)



#Export the Canopy Height Model to a .tif
#Export the Canopy Height Model to a .tif
writeRaster(NEON_DTM,
            "P:/RaBET/LiDARmodels/2019/NEON_D14_SRER_DP1_510000_3518000_DTM_2019.tif",
            format="GTiff",
            overwrite=TRUE)

writeRaster(NEON_CHM,   
            "P:/RaBET/LiDARmodels/2019/NEON_D14_SRER_DP1_510000_3518000_CHM_2019.tif",
            format="GTiff",
            overwrite=TRUE)

writeRaster(NEON_slope,  
            "P:/RaBET/LiDARmodels/2019/NEON_D14_SRER_DP1_510000_3518000_slope_2019.tif",
            format="GTiff",
            overwrite=TRUE)

writeRaster(NEON_aspect,   
            "P:/RaBET/LiDARmodels/2019/NEON_D14_SRER_DP1_510000_3518000_aspect_2019.tif",
            format="GTiff",
            overwrite=TRUE)

writeRaster(NEON_hillshade,   
            "P:/RaBET/LiDARmodels/2019/NEON_D14_SRER_DP1_510000_3518000_hillshade_2019.tif",
            format="GTiff",
            overwrite=TRUE)



 




#Bulk Processing
#Bring point cloud into Rstudio
NEONpoints2017 = list.files("P:/RaBET/Subset/2017/LAS/",pattern="*.las$", full.names=TRUE)
NEONpoints2018 = list.files("P:/RaBET/Subset/2018/LAS/",pattern="*.las$", full.names=TRUE)
NEONpoints2019 = list.files("P:/RaBET/Subset/2019/LAS/",pattern="*.las$", full.names=TRUE)

outws <- '/RaBET/CLN_LiDARmodels'
path <- 'P:/RaBET/CLN_LiDARmodels/2019/'


#loop
#LiDAR2017 <- lapply(NEONpoints2017, function(fileName) )
  
for(fileName in NEONpoints2017){
#Bring point cloud into Rstudio
#Identify ground points using a cloth simulation filter algorithm
NEONpoints = readLAS(fileName)
#Identify ground points using a cloth simulation filter algorithm
NEONpoints_classified = classify_ground(NEONpoints, algorithm = csf(sloop_smooth = TRUE, class_threshold = 0.55, cloth_resolution =  0.2, rigidness = 2))
#plot(NEONpoints_classified)
#Separate the classified point cloud into a ground point cloud and canopy point cloud
ground_NEONpoints = filter_poi(NEONpoints_classified, Classification == 2)
canopy_NEONpoints = filter_poi(NEONpoints_classified, Classification == 1)

#Create a grided digital terrain model
NEON_DTM = grid_terrain(ground_NEONpoints, res = 0.5, algorithm = knnidw(k = 12, p = 2))
#plot(NEON_DTM)
projection(NEON_DTM) 
#Create slope
NEON_slope <- terrain(NEON_DTM, opt="slope",units = "radians")
#plot(NEON_slope)

#Create aspect
NEON_aspect <- terrain(NEON_DTM, opt ="aspect", units = "radians")
#plot(NEON_aspect)

#Create aspect
NEON_hillshade <- hillShade(NEON_slope, NEON_aspect)
#plot(NEON_hillshade)

#Calculate vegetation height above the ground
NEON_DSM = normalize_height(NEONpoints, algorithm = knnidw(k = 12, p = 2))

#Remove points that are below 0
NEON_DSM_clean = filter_poi(NEON_DSM, Z < 15)

#plot(NEON_DSM_clean)

#Create canopy height model(raster) from the normalized canopy points
NEON_CHM = grid_canopy(NEON_DSM_clean, res = 0.5, p2r(0.25))

#Export the Canopy Height Model to a .tif
writeRaster(NEON_DTM,
            filename=paste('P:/RaBET/LiDARmodels/2017/',substr(fileName, 26, 57), "_2017_DTM"),
            format="GTiff",
            overwrite=TRUE)

writeRaster(NEON_CHM,   
            filename=paste('P:/RaBET/LiDARmodels/2017/',substr(fileName, 26, 57), "_2017_CHM"),
            format="GTiff",
            overwrite=TRUE)

writeRaster(NEON_slope,  
            filename=paste('P:/RaBET/LiDARmodels/2017/',substr(fileName, 26, 57), "_2017_slope"),
            format="GTiff",
            overwrite=TRUE)

writeRaster(NEON_aspect,   
            filename=paste('P:/RaBET/LiDARmodels/2017/',substr(fileName, 26, 57), "_2017_aspect"),
            format="GTiff",
            overwrite=TRUE)

writeRaster(NEON_hillshade,   
            filename=paste('P:/RaBET/LiDARmodels/2017/',substr(fileName, 26, 57), "_2017_hillshade"),
            format="GTiff",
            overwrite=TRUE)

}
