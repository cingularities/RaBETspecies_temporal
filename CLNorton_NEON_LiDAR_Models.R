#This Rcode was written by Cynthia L. Norton, University of Arizona, 2021
#The code was used to create LiDAR models
# load libraries
library(raster)
library(lidR) 
library(rgdal) 
library(sf) 
library(rgeos) 
library(sp)

#Bring point cloud into Rstudio
NEONpoints = readLAS("P:/RaBET/Raw/NEON/2019/Las/NEON_D14_SRER_DP1_516000_3519000_classified_point_cloud_colorized.las")
col <- height.colors(50)
#Identify ground points using a cloth simulation filter algorithm
NEONpoints_classified = classify_ground(NEONpoints, algorithm = csf(sloop_smooth = TRUE, class_threshold = 0.55, cloth_resolution =  0.2, rigidness = 2))

#Separate the classified point cloud into a ground point cloud and canopy point cloud
ground_NEONpoints = filter_poi(NEONpoints_classified, Classification == 2)
canopy_NEONpoints = filter_poi(NEONpoints_classified, Classification == 1)

#Create a grided digital terrain model
NEON_DTM = grid_terrain(ground_NEONpoints, res = 0.5, algorithm = knnidw(k = 12, p = 2))
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
NEON_DSM_clean = filter_poi(NEON_DSM, Z > 0.09 & Z < 15)

#plot(NEON_DSM_clean)

#Create canopy height model(raster) from the normalized canopy points
NEON_CHM = grid_canopy(NEON_DSM_clean, res = 0.5, p2r(0.25))
#windows()
plot(NEON_CHM, col = col)



#Export the Canopy Height Model to a .tif
writeRaster(NEON_DTM, "P:/RaBET/CLN_LiDARmodels/2019/516000_3519000Neon_DTM.tif", overwrite = TRUE)
writeRaster(NEON_DSM_clean, "P:/RaBET/CLN_LiDARmodels/2019/516000_3519000Neon_DSM.tif", overwrite = TRUE)
writeRaster(NEON_CHM, "P:/RaBET/CLN_LiDARmodels/2019/516000_3519000Neon_CHM_p2r(0.5)_(0.09-15)filter.tif", overwrite = TRUE)

writeRaster(NEON_slope, "P:/RaBET/CLN_LiDARmodels/2019/516000_3519000Neon_slope.tif", overwrite = TRUE)
writeRaster(NEON_aspect, "P:/RaBET/CLN_LiDARmodels/2019/516000_3519000Neon_aspect.tif", overwrite = TRUE)
writeRaster(NEON_hillshade, "P:/RaBET/CLN_LiDARmodels/2019/516000_3519000Neon_hilshade.tif", overwrite = TRUE)







#Bulk Processing
#Bring point cloud into Rstudio
NEONpoints2017 = list.files("P:/RaBET/Raw/NEON/CLNorton_NEON/NEON_D14_SRER_DP1_515000_3519000_classified_point_cloud_colorized_2019.las")
NEONpoints2018 = list.files("P:/RaBET/Raw/NEON/CLNorton_NEON/NEON_D14_SRER_DP1_515000_3519000_classified_point_cloud_colorized_2019.las")
NEONpoints2019 = list.files("P:/RaBET/Raw/NEON/CLNorton_NEON/NEON_D14_SRER_DP1_515000_3519000_classified_point_cloud_colorized_2019.las")

NEONpoints2017 <- lapply(NEONpoints2017, readLAS)
NEONpoints2018 <- lapply(NEONpoints2018, readLAS)
NEONpoints2019 <- lapply(NEONpoints2019, readLAS)

outws <- '/RaBET/CLN_LiDARmodels'

#Identify ground points using a cloth simulation filter algorithm
liDARfunction <- function(path) {
#Bring point cloud into Rstudio
NEONpoints <- readLAS(path)
#Identify ground points using a cloth simulation filter algorithm
NEONpoints_classified <- classify_ground(NEONpoints, algorithm = csf(sloop_smooth = FALSE, class_threshold = 0.4, cloth_resolution =  0.25, rigidness = 2))
ground_NEONpoints <- filter_poi(NEONpoints_classified, Classification == 2)
canopy_NEONpoints <- filter_poi(NEONpoints_classified, Classification == 1)
#Create a grided digital terrain model
NEON_DTM <- grid_terrain(ground_NEONpoints, res = 0.25, algorithm = knnidw(k = 15, p = 2))
#Calculate vegetation height above the ground
NEON_slope <- terrain(NEON_DTM, opt="slope",units = "radians")
#Create aspect
NEON_aspect <- terrain(NEON_DTM, opt ="aspect", units = "radians")
#Create aspect
NEON_hillshade <- hillShade(NEON_slope, NEON_aspect)
NEON_DSM <- normalize_height(canopy_NEONpoints, NEON_DTM, na.rm = TRUE, copy=TRUE)
#Remove points that are below 0
NEON_DSM_clean <- filter_poi(NEON_DSM, Z > 0)
#Create canopy height model(raster) from the normalized canopy points
NEON_CHM <- grid_canopy(NEON_DSM_clean, res = 0.25, algorithm = p2r(subcircle = 0, na.fill = NULL))
#Export the Canopy Height Model to a .tif
writeRaster(NEON_DTM, file.path(outws, paste(tools::file_path_sans_ext(basename(path)), '_DTM.tif', sep = "")), overwrite = T)
writeRaster(NEON_DSM,  file.path(outws, paste(tools::file_path_sans_ext(basename(path)), '_DSM.tif', sep = "")), overwrite = T)
writeRaster(NEON_CHM,  file.path(outws, paste(tools::file_path_sans_ext(basename(path)), '_CHM.tif', sep = "")), overwrite = T)
writeRaster(NEON_slope, file.path(outws, paste(tools::file_path_sans_ext(basename(path)), '_slope.tif', sep = "")), overwrite = T)
writeRaster(NEON_aspect,  file.path(outws, paste(tools::file_path_sans_ext(basename(path)), '_aspect.tif', sep = "")), overwrite = T)
writeRaster(NEON_hillshade,  file.path(outws, paste(tools::file_path_sans_ext(basename(path)), '_hillshade.tif', sep = "")), overwrite = T)
}
