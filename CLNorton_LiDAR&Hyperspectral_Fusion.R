#Code written by Cynthia L. Norton, Univserity of Arizona, 2021 
#The code was used to classify NEON scenes for species specific woody vegetation using LiDAR and Hyperspectral data.
#loadlibraries
library(doParallel)
library(raster)
library(foreach)
library(randomForest)
library(rpart)
library(tidyverse)
library(rgdal)
library(tcltk)
library(rgeos)
library(broom)
library(caret)
library(terra)
library(dismo)
#install.packages("e1071")
#library(C50)

#.libPaths()
##Import indices and Elevation Models
# set working directory
setwd("D:/projects/RaBET/NEON_NIWOT")
getwd()
#Trait variables
#area(variables)
NDVI_2017 <- raster('D:/projects/RaBET/NEON_NIWOT/NDVI2017final_NIWOT.tif')
NDWI_2017 <- raster('D:/projects/RaBET/NEON_NIWOT/NDWI2017final_NIWOT.tif')
PRI_2017 <- raster('D:/projects/RaBET/NEON_NIWOT/PRI2017final_NIWOT.tif')
SWIRI_2017 <- raster('D:/projects/RaBET/NEON_NIWOT/SWIRI2017final_NIWOT.tif')
SAVI_2017 <- raster('D:/projects/RaBET/NEON_NIWOT/SAVI2017final_NIWOT.tif')
PRI2_2017 <- raster('D:/projects/RaBET/NEON_NIWOT/PRI22017final_NIWOT.tif')
CACTI_2017 <- raster('D:/projects/RaBET/NEON_NIWOT/CACTI2017final_NIWOT.tif')
CACTI2_2017 <- raster('D:/projects/RaBET/NEON_NIWOT/CACTI22017final_NIWOT.tif')
MTCI_2017 <- raster('D:/projects/RaBET/NEON_NIWOT/MTCI2017final_NIWOT.tif')
CI_2017 <- raster('D:/projects/RaBET/NEON_NIWOT/CI2017final_NIWOT.tif')
CAI_2017 <- raster('D:/projects/RaBET/NEON_NIWOT/CAI2017final_NIWOT.tif')
CELL_2017 <- raster('D:/projects/RaBET/NEON_NIWOT/CELL2017final_NIWOT.tif')
CELL2_2017 <- raster('D:/projects/RaBET/NEON_NIWOT/CELL22017final_NIWOT.tif')
NDNI_2017 <- raster('D:/projects/RaBET/NEON_NIWOT/NDNI2017final_NIWOT.tif')
CHM_2017 <- raster('D:/projects/RaBET/NEON_NIWOT/CHM2017final_NIWOT.tif')

NDVI_2018 <- raster('D:/projects/RaBET/NEON_NIWOT/NDVI2018final_NIWOT.tif')
NDWI_2018 <- raster('D:/projects/RaBET/NEON_NIWOT/NDWI2018final_NIWOT.tif')
PRI_2018 <- raster('D:/projects/RaBET/NEON_NIWOT/PRI2018final_NIWOT.tif')
SWIRI_2018 <- raster('D:/projects/RaBET/NEON_NIWOT/SWIRI2018final_NIWOT.tif')
SAVI_2018 <- raster('D:/projects/RaBET/NEON_NIWOT/SAVI2018final_NIWOT.tif')
PRI2_2018 <- raster('D:/projects/RaBET/NEON_NIWOT/PRI22018final_NIWOT.tif')
CACTI_2018 <- raster('D:/projects/RaBET/NEON_NIWOT/CACTI2018final_NIWOT.tif')
CACTI2_2018 <- raster('D:/projects/RaBET/NEON_NIWOT/CACTI22018final_NIWOT.tif')
MTCI_2018 <- raster('D:/projects/RaBET/NEON_NIWOT/MTCI2018final_NIWOT.tif')
CI_2018 <- raster('D:/projects/RaBET/NEON_NIWOT/CI2018final_NIWOT.tif')
CAI_2018 <- raster('D:/projects/RaBET/NEON_NIWOT/CAI2018final_NIWOT.tif')
CELL_2018 <- raster('D:/projects/RaBET/NEON_NIWOT/CELL2018final_NIWOT.tif')
CELL2_2018 <- raster('D:/projects/RaBET/NEON_NIWOT/CELL22018final_NIWOT.tif')
NDNI_2018 <- raster('D:/projects/RaBET/NEON_NIWOT/NDNI2018final_NIWOT.tif')
CHM_2018 <- raster('D:/projects/RaBET/NEON_NIWOT/CHM2018final_NIWOT.tif')


NDVI_2019 <- raster('D:/projects/RaBET/NEON_NIWOT/NDVI2019final_NIWOT.tif')
NDWI_2019 <- raster('D:/projects/RaBET/NEON_NIWOT/NDWI2019final_NIWOT.tif')
PRI_2019 <- raster('D:/projects/RaBET/NEON_NIWOT/PRI2019final_NIWOT.tif')
SWIRI_2019 <- raster('D:/projects/RaBET/NEON_NIWOT/SWIRI2019final_NIWOT.tif')
SAVI_2019 <- raster('D:/projects/RaBET/NEON_NIWOT/SAVI2019final_NIWOT.tif')
PRI2_2019 <- raster('D:/projects/RaBET/NEON_NIWOT/PRI22019final_NIWOT.tif')
CACTI_2019 <- raster('D:/projects/RaBET/NEON_NIWOT/CACTI2019final_NIWOT.tif')
CACTI2_2019 <- raster('D:/projects/RaBET/NEON_NIWOT/CACTI22019final_NIWOT.tif')
MTCI_2019 <- raster('D:/projects/RaBET/NEON_NIWOT/MTCI2019final_NIWOT.tif')
CI_2019 <- raster('D:/projects/RaBET/NEON_NIWOT/CI2019final_NIWOT.tif')
CAI_2019 <- raster('D:/projects/RaBET/NEON_NIWOT/CAI2019final_NIWOT.tif')
CELL_2019 <- raster('D:/projects/RaBET/NEON_NIWOT/CELL2019final_NIWOT.tif')
CELL2_2019 <- raster('D:/projects/RaBET/NEON_NIWOT/CELL22019final_NIWOT.tif')
NDNI_2019 <- raster('D:/projects/RaBET/NEON_NIWOT/NDNI2019final_NIWOT.tif')
CHM_2019 <- raster('D:/projects/RaBET/NEON_NIWOT/CHM2019final_NIWOT.tif')

NDVI_2020 <- raster('D:/projects/RaBET/NEON_NIWOT/NDVI2020final_NIWOT.tif')
NDWI_2020 <- raster('D:/projects/RaBET/NEON_NIWOT/NDWI2020final_NIWOT.tif')
PRI_2020 <- raster('D:/projects/RaBET/NEON_NIWOT/PRI2020final_NIWOT.tif')
SWIRI_2020 <- raster('D:/projects/RaBET/NEON_NIWOT/SWIRI2020final_NIWOT.tif')
SAVI_2020 <- raster('D:/projects/RaBET/NEON_NIWOT/SAVI2020final_NIWOT.tif')
PRI2_2020 <- raster('D:/projects/RaBET/NEON_NIWOT/PRI22020final_NIWOT.tif')
CACTI_2020 <- raster('D:/projects/RaBET/NEON_NIWOT/CACTI2020final_NIWOT.tif')
CACTI2_2020 <- raster('D:/projects/RaBET/NEON_NIWOT/CACTI22020final_NIWOT.tif')
MTCI_2020 <- raster('D:/projects/RaBET/NEON_NIWOT/MTCI2020final_NIWOT.tif')
CI_2020 <- raster('D:/projects/RaBET/NEON_NIWOT/CI2020final_NIWOT.tif')
CAI_2020 <- raster('D:/projects/RaBET/NEON_NIWOT/CAI2020final_NIWOT.tif')
CELL_2020 <- raster('D:/projects/RaBET/NEON_NIWOT/CELL2020final_NIWOT.tif')
CELL2_2020 <- raster('D:/projects/RaBET/NEON_NIWOT/CELL22020final_NIWOT.tif')
NDNI_2020 <- raster('D:/projects/RaBET/NEON_NIWOT/NDNI2020final_NIWOT.tif')
CHM_2020 <- raster('D:/projects/RaBET/NEON_NIWOT/CHM2020final_NIWOT.tif')

CHM_2020 <- setExtent(CHM_2020, NDVI_2020)
CHM_2020 <- resample(CHM_2020, NDVI_2020)

CHM_2019 <- setExtent(CHM_2019, NDVI_2019)
CHM_2019 <- resample(CHM_2019, NDVI_2019)

CHM_2018 <- setExtent(CHM_2018, NDVI_2018)
CHM_2018 <- resample(CHM_2018, NDVI_2018)

CHM_2017 <- setExtent(CHM_2017, NDVI_2017)
CHM_2017 <- resample(CHM_2017, NDVI_2017)

writeRaster(CHM_2020, file = 'crop_CHM2020_NIWOT_final.tif')
writeRaster(CHM_2019, file = 'crop_CHM2019_NIWOT_final.tif')
writeRaster(CHM_2018, file = 'crop_CHM2018_NIWOT_final.tif')
writeRaster(CHM_2017, file = 'crop_CHM2017_NIWOT_final.tif')

getwd()
NEON_2020 <- stack(NDVI_2020,NDWI_2020,PRI_2020,SWIRI_2020,SAVI_2020, 
                   PRI2_2020,CACTI_2020,CACTI2_2020,MTCI_2020,CI_2020,
                   CAI_2020,CELL_2020,CELL2_2020,NDNI_2020,CHM_2020)
NEON_2019 <- stack(NDVI_2019,NDWI_2019,PRI_2019,SWIRI_2019,SAVI_2019, 
                   PRI2_2019,CACTI_2019,CACTI2_2019,MTCI_2019,CI_2019,
                   CAI_2019,CELL_2019,CELL2_2019,NDNI_2019,CHM_2019)
NEON_2018 <- stack(NDVI_2018,NDWI_2018,PRI_2018,SWIRI_2018,SAVI_2018, 
                   PRI2_2018,CACTI_2018,CACTI2_2018,MTCI_2018,CI_2018,
                   CAI_2018,CELL_2018,CELL2_2018,NDNI_2018,CHM_2018)
NEON_2017 <- stack(NDVI_2017,NDWI_2017,PRI_2017,SWIRI_2017,SAVI_2017, 
                   PRI2_2017,CACTI_2017,CACTI2_2017,MTCI_2017,CI_2017,
                   CAI_2017,CELL_2017,CELL2_2017,NDNI_2017,CHM_2017)

writeRaster(NEON_2020, file = 'NEON2020_NIWOT_final.tif')
writeRaster(NEON_2019, file = 'NEON2019_NIWOT_final.tif')
writeRaster(NEON_2018, file = 'NEON2018_NIWOT_final.tif')
writeRaster(NEON_2017, file = 'NEON2017_NIWOT_final.tif')

crop <- shapefile("D:/projects/RaBET/NEON_NIWOT/NIWOT_crop.shp")

e <- extent(crop)
NEON_2020_crop <- crop(NEON_2020, e) 
NEON_2019_crop <- crop(NEON_2019, e) 
NEON_2018_crop <- crop(NEON_2018, e) 
NEON_2017_crop <- crop(NEON_2017, e)
NEON_2020_mask <- mask(NEON_2020_crop, crop)
NEON_2019_mask <- mask(NEON_2019_crop, crop)
NEON_2018_mask <- mask(NEON_2018_crop, crop)
NEON_2017_mask <- mask(NEON_2017_crop, crop)


writeRaster(NEON_2020_mask, filename = "NEON_NIWOT_indices_2020_mask.tif",overwrite=TRUE)
writeRaster(NEON_2019_mask, filename = "NEON_NIWOT_indices_2019_mask.tif",overwrite=TRUE)
writeRaster(NEON_2018_mask, filename = "NEON_NIWOT_indices_2018_mask.tif",overwrite=TRUE)#warnings()
writeRaster(NEON_2017_mask, filename = "NEON_NIWOT_indices_2017_mask.tif",overwrite=TRUE)

mask <- mask(x, crop)


NEON_indices <- stack(NEON_2020_mask, NEON_2019_mask, NEON_2018_mask)


names(NEON_indices) <- c('NDVI_2020','NDWI_2020','PRI_2020','SWIRI_2020','SAVI_2020',
                         'PRI2_2020','CACTI_2020','CACTI2_2020','MTCI_2020','CI_2020',
                         'CAI_2020','CELL_2020','CELL2_2020','NDNI_2020','CHM_2020',
                         'NDVI_2019','NDWI_2019','PRI_2019','SWIRI_2019','SAVI_2019',
                         'PRI2_2019','CACTI_2019','CACTI2_2019','MTCI_2019','CI_2019',
                         'CAI_2019','CELL_2019','CELL2_2019','NDNI_2019','CHM_2019',
                         'NDVI_2018','NDWI_2018','PRI_2018','SWIRI_2018','SAVI_2018',
                         'PRI2_2018','CACTI_2018','CACTI2_2018','MTCI_2018','CI_2018',
                         'CAI_2018','CELL_2018','CELL2_2018','NDNI_2018','CHM_2018')

