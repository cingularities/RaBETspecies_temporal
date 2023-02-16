#This Rcode was written by Cynthia L. Norton, University of Arizona, 2021
#The code was used to create LiDAR model
# load libraries
library(raster)
library(lidR) 
library(rgdal) 
library(parallel)
library(MASS)
library(future)
setwd("//snow/projects/RaBET/RaBET_species/NEON_WGER/lidar/NEON_WGER_2018_ARS/")

#Bring point cloud into Rstudio
getwd()
NEONpoints = readLAS("//snow/projects/RaBET/RaBET_species/NEON_SRER/lidar/NEON_SRER_2018_ARS/NEON_D14_WGEW_DP1_597000_3507000_classified_point_cloud_colorized.laz")
#Identify ground points using a cloth simulation filter algorithm
#NEONpoints_classified = classify_ground(NEONpoints, algorithm = csf(sloop_smooth = TRUE, class_threshold = 0.55, cloth_resolution =  0.2, rigidness = 2))
#plot(NEONpoints_classified)
#Separate the classified point cloud into a ground point cloud and canopy point cloud
#ground_NEONpoints = filter_poi(NEONpoints_classified, Classification == 2)
#canopy_NEONpoints = filter_poi(NEONpoints_classified, Classification == 1)

#Create a grided digital terrain model
#NEON_DTM = grid_terrain(NEONpoints, res = 0.5, algorithm = knnidw(k = 12, p = 2))
#plot(NEON_DTM)

#Create slope
#NEON_slope <- terrain(NEON_DTM, opt="slope",units = "radians")
#plot(NEON_slope)

#Create aspect
#NEON_aspect <- terrain(NEON_DTM, opt ="aspect", units = "radians")
#plot(NEON_aspect)

#Create aspect
#NEON_hillshade <- hillShade(NEON_slope, NEON_aspect)
#plot(NEON_hillshade)

#Calculate vegetation height above the ground
NEON_DSM = normalize_height(NEONpoints, algorithm = knnidw(k = 12, p = 8), na.rm = TRUE)

#Remove points that are below 0
NEON_DSM_clean = filter_poi(NEON_DSM, Z < 45)

#plot(NEON_DSM_clean)

#Create canopy height model(raster) from the normalized canopy points
NEON_CHM = grid_canopy(NEON_DSM_clean, res = 0.5, p2r(subcircle = 0.3, na.fill = NULL))


#Export the Canopy Height Model to a .tif
#Export the Canopy Height Model to a .tif
#writeRaster(NEON_DTM,
#            "P:/RaBET/LiDARmodels/2019/NEON_D14_SRER_DP1_510000_3518000_DTM_2019.tif",
#            format="GTiff",
#            overwrite=TRUE)

writeRaster(NEON_CHM,   
            "D:/NEON_SRER/LiDAR/NEON_D14_SRER_DP1_330000_3615000_classified_point_cloud_CHM_2019.tif",
            format="GTiff",
            overwrite=TRUE)

#writeRaster(NEON_slope,  
#            "P:/RaBET/LiDARmodels/2019/NEON_D14_SRER_DP1_510000_3518000_slope_2019.tif",
#            format="GTiff",
#            overwrite=TRUE)

#writeRaster(NEON_aspect,   
#            "P:/RaBET/LiDARmodels/2019/NEON_D14_SRER_DP1_510000_3518000_aspect_2019.tif",
#            format="GTiff",
#            overwrite=TRUE)

##writeRaster(NEON_hillshade,   
#            "P:/RaBET/LiDARmodels/2019/NEON_D14_SRER_DP1_510000_3518000_hillshade_2019.tif",
#            format="GTiff",
#            overwrite=TRUE)














##CHM for loop
NEONpoints2017 = list.files("//snow/projects/RaBET/RaBET_species/NEON_SRER/2017/lidar",pattern="*.laz$", full.names=TRUE)
NEONpoints2018 = list.files("//snow/projects/RaBET/RaBET_species/NEON_WGER/lidar/NEON_WGER_2018_ARS/complete",pattern="*.laz$", full.names=TRUE)
NEONpoints2019 = list.files("//snow/projects/RaBET/RaBET_species/NEON_SRER/2019/lidar",pattern="*.laz$", full.names=TRUE)
NEONpoints2020 = list.files("//snow/projects/RaBET/RaBET_species/NEON_SRER/2020/lidar",pattern="*.laz$", full.names=TRUE)
NEONpoints2021 = list.files("//snow/projects/RaBET/RaBET_species/NEON_SRER/2021/lidar",pattern="*.laz$", full.names=TRUE)
SRER  = list.files('//snow/projects/RaBET/RaBET_species/NEON_SRER/lidar/NEON_SRER_2018_ARS/complete',pattern="*.laz$", full.names=TRUE)
SRER_22  = list.files('//snow/projects/RaBET/RaBET_species/NEON_SRER/lidar/RaBET_SRER_2022_ARS',pattern="*.las$", full.names=TRUE)


for(fileName in NEONpoints2018){
#Bring point cloud into Rstudio
#Identify ground points using a cloth simulation filter algorithm

NEONpoints <-readLAS(fileName)

#Calculate vegetation height above the ground
NEON_DSM = normalize_height(NEONpoints, algorithm = knnidw(k = 12, p = 3), na.rm = TRUE)

#Remove points that are below 0
NEON_DSM_clean = filter_poi(NEON_DSM, Z < 45)


#plot(NEON_DSM_clean)
NEON_DTM = grid_terrain(NEONpoints, res = 0.5, algorithm = knnidw(k = 12, p = 2))

#Create canopy height model(raster) from the normalized canopy points
#NEON_CHM = grid_canopy(NEON_DSM_clean, res = 0.5, p2r(subcircle = 0.3, na.fill = NULL))
#windows()


#plot(NEON_DSM_clean)

#Create canopy height model(raster) from the normalized canopy points

#Export the Canopy Height Model to a .tif
writeRaster(NEON_DTM,   
            filename=paste("//snow/projects/RaBET/RaBET_species/NEON_WGER/lidar/NEON_WGER_2018_ARS/",substr(fileName, 90, 1000), "_2018_DTM"),
            format="GTiff",
            overwrite=TRUE)
}






CHM2018 <- list.files(path = "//snow/projects/RaBET/RaBET_species/NEON_SRER/lidar", pattern = ".tif$", full.names = TRUE)
CHM2018 <- lapply(CHM2018, raster)
CHM2018final <- do.call(merge, c(CHM2018, tolerance = 1))
writeRaster(CHM2018final,
            file= "CHM2018final_SRER",
            format="GTiff",
            overwrite=TRUE)
getwd()

CHM2019 <- list.files(path = "//snow/projects/RaBET/RaBET_species/NEON_SRER/lidar/RaBET_SRER_2022_ARS", pattern = ".tif$", full.names = TRUE)
CHM2019 <- lapply(CHM2019, raster)
CHM2019final <- do.call(merge, c(CHM2019, tolerance = 1))
writeRaster(CHM2019final,
            file= "CHM2022final_SRER_rabet",
            format="GTiff",
            overwrite=TRUE)
getwd()

DTM2018 <- list.files(path = "//snow/projects/RaBET/RaBET_species/NEON_WGER/lidar/DTM", pattern = ".tif$", full.names = TRUE)
DTM2018 <- lapply(DTM2018, raster)
DTM2018final <- do.call(merge, c(DTM2018, tolerance = 1))
writeRaster(DTM2018final,
            file= "DTM2018final_WGER",
            format="GTiff",
            overwrite=TRUE)




crop <- shapefile("P:/RaBET/Subset/crop.shp")



variables <- raster('P:/RaBET/Results/CHM2017final.tif')
e <- extent(crop)
x <- crop(variables, e) 
mask <- mask(x, crop)
writeRaster(mask, filename = "crop_CHM2017final.tif")


variables <- raster('P:/RaBET/Results/CHM2018final.tif')
e <- extent(crop)
x <- crop(variables, e) 
mask <- mask(x, crop)
writeRaster(mask, filename = "crop_CHM2018final.tif")



variables <- raster('P:/RaBET/Results/CHM2019final.tif')
e <- extent(crop)
x <- crop(variables, e) 
mask <- mask(x, crop)
writeRaster(mask, filename = "crop_CHM2019final.tif")





plan(multisession, gc = TRUE, workers = 18)
#Bulk Processing
#Bring point cloud into Rstudio
NEONpoints2017 = readLAScatalog("P:/RaBET/Subset/2017/LAS/")
NEONpoints2018 = list.files("P:/RaBET/Subset/2018/LAS/",pattern="*.las$", full.names=TRUE)
NEONpoints2019 = list.files("P:/RaBET/Subset/2019/LAS/",pattern="*.las$", full.names=TRUE)

opt_output_files(NEONpoints2017) <- paste0(tempdir(), "{*}_NEON_DSM")
NEON_DSM = normalize_height(NEONpoints2017, algorithm = knnidw(k = 12, p = 2))

opt_output_files(NEON_DSM) <- paste0(tempdir(), "{*}_NEON_DSM_clean")
NEON_DSM_clean = filter_poi(NEON_DSM, Z < 15)

opt_output_files(NEON_DSM_clean) <- "P:/RaBET/LiDARmodels/2017/{ORIGINALFILENAME}_CHM"
NEON_CHM = grid_canopy(NEON_DSM_clean, res = 1, p2r(0.25))




opt_output_files(NEONpoints2017) <- paste0(tempdir(), "{*}_NEONpoints_classified")

##NEONfileName < - readLAS(NEONpoints)
NEONpoints_classified = classify_ground(NEONpoints2017, algorithm = csf(sloop_smooth = TRUE, class_threshold = 0.55, cloth_resolution =  0.2, rigidness = 2))

opt_output_files(NEONpoints_classified) <- paste0(tempdir(), "{*}_ground_NEONpoints")
ground_NEONpoints = filter_poi(NEONpoints_classified, Classification == 2)

opt_output_files(NEONpoints_classified) <- paste0(tempdir(), "{*}_canopy_NEONpoints")
canopy_NEONpoints = filter_poi(NEONpoints_classified, Classification == 1)

opt_output_files(ground_NEONpoints) <- "P:/RaBET/Subset/2018/LAS/{ORIGINALFILENAME}_DEM"
NEON_DTM = grid_terrain(ground_NEONpoints, res = 1, algorithm = knnidw(k = 12, p = 2))





