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
setwd("//snow/projects/RaBET/RaBET_species/NEON_FINAL/SRER/FinalOutputs/")
getwd()
#Trait variables
#area(variables)
NDVI_2017 <- raster('//snow/projects/RaBET/RaBET_species/NEON_FINAL/SRER/FinalOutputs/NDVI2017final.tif')
NDWI_2017 <- raster('//snow/projects/RaBET/RaBET_species/NEON_FINAL/SRER/FinalOutputs/NDWI2017final.tif')
PRI_2017 <- raster('//snow/projects/RaBET/RaBET_species/NEON_FINAL/SRER/FinalOutputs/PRI2017final.tif')
SWIRI_2017 <- raster('//snow/projects/RaBET/RaBET_species/NEON_FINAL/SRER/FinalOutputs/SWIRI2017final.tif')
SAVI_2017 <- raster('//snow/projects/RaBET/RaBET_species/NEON_FINAL/SRER/FinalOutputs/SAVI2017final.tif')
PRI2_2017 <- raster('//snow/projects/RaBET/RaBET_species/NEON_FINAL/SRER/FinalOutputs/PRI22017final.tif')
CACTI_2017 <- raster('//snow/projects/RaBET/RaBET_species/NEON_FINAL/SRER/FinalOutputs/CACTI2017final.tif')
CACTI2_2017 <- raster('//snow/projects/RaBET/RaBET_species/NEON_FINAL/SRER/FinalOutputs/CACTI22017final.tif')
MTCI_2017 <- raster('//snow/projects/RaBET/RaBET_species/NEON_FINAL/SRER/FinalOutputs/MTCI2017final.tif')
CI_2017 <- raster('//snow/projects/RaBET/RaBET_species/NEON_FINAL/SRER/FinalOutputs/CI2017final.tif')
CAI_2017 <- raster('//snow/projects/RaBET/RaBET_species/NEON_FINAL/SRER/FinalOutputs/CAI2017final.tif')
CELL_2017 <- raster('//snow/projects/RaBET/RaBET_species/NEON_FINAL/SRER/FinalOutputs/CELL2017final.tif')
CELL2_2017 <- raster('//snow/projects/RaBET/RaBET_species/NEON_FINAL/SRER/FinalOutputs/CELL22017final.tif')
NDNI_2017 <- raster('//snow/projects/RaBET/RaBET_species/NEON_FINAL/SRER/FinalOutputs/NDNI2017final.tif')
CHM_2017 <- raster('//snow/projects/RaBET/RaBET_species/NEON_FINAL/SRER/FinalOutputs/CHM2017final.tif')
#CONIFER_2017 <- raster('//snow/projects/RaBET/RaBET_species/NEON_FINAL/SRER/FinalOutputs/CONIFER2017final.tif')
#SPRUCE_2017 <- raster('//snow/projects/RaBET/RaBET_species/NEON_FINAL/SRER/FinalOutputs/SPRUCE2017final.tif')
#NDRE_2017 <- raster('//snow/projects/RaBET/RaBET_species/NEON_FINAL/SRER/FinalOutputs/NDRE2017final.tif')


NDVI_2018 <- raster('//snow/projects/RaBET/RaBET_species/NEON_FINAL/SRER/FinalOutputs/NDVI2018final.tif')
NDWI_2018 <- raster('//snow/projects/RaBET/RaBET_species/NEON_FINAL/SRER/FinalOutputs/NDWI2018final.tif')
PRI_2018 <- raster('//snow/projects/RaBET/RaBET_species/NEON_FINAL/SRER/FinalOutputs/PRI2018final.tif')
SWIRI_2018 <- raster('//snow/projects/RaBET/RaBET_species/NEON_FINAL/SRER/FinalOutputs/SWIRI2018final.tif')
SAVI_2018 <- raster('//snow/projects/RaBET/RaBET_species/NEON_FINAL/SRER/FinalOutputs/SAVI2018final.tif')
PRI2_2018 <- raster('//snow/projects/RaBET/RaBET_species/NEON_FINAL/SRER/FinalOutputs/PRI22018final.tif')
CACTI_2018 <- raster('//snow/projects/RaBET/RaBET_species/NEON_FINAL/SRER/FinalOutputs/CACTI2018final.tif')
CACTI2_2018 <- raster('//snow/projects/RaBET/RaBET_species/NEON_FINAL/SRER/FinalOutputs/CACTI22018final.tif')
MTCI_2018 <- raster('//snow/projects/RaBET/RaBET_species/NEON_FINAL/SRER/FinalOutputs/MTCI2018final.tif')
CI_2018 <- raster('//snow/projects/RaBET/RaBET_species/NEON_FINAL/SRER/FinalOutputs/CI2018final.tif')
CAI_2018 <- raster('//snow/projects/RaBET/RaBET_species/NEON_FINAL/SRER/FinalOutputs/CAI2018final.tif')
CELL_2018 <- raster('//snow/projects/RaBET/RaBET_species/NEON_FINAL/SRER/FinalOutputs/CELL2018final.tif')
CELL2_2018 <- raster('//snow/projects/RaBET/RaBET_species/NEON_FINAL/SRER/FinalOutputs/CELL22018final.tif')
NDNI_2018 <- raster('//snow/projects/RaBET/RaBET_species/NEON_FINAL/SRER/FinalOutputs/NDNI2018final.tif')
CHM_2018 <- raster('//snow/projects/RaBET/RaBET_species/NEON_FINAL/SRER/FinalOutputs/CHM2018final.tif')
#CONIFER_2018 <- raster('//snow/projects/RaBET/RaBET_species/NEON_FINAL/SRER/FinalOutputs/CONIFER2018final.tif')
#SPRUCE_2018 <- raster('//snow/projects/RaBET/RaBET_species/NEON_FINAL/SRER/FinalOutputs/SPRUCE2018final.tif')
#NDRE_2018 <- raster('//snow/projects/RaBET/RaBET_species/NEON_FINAL/SRER/FinalOutputs/NDRE2018final.tif')

NDVI_2019 <- raster('//snow/projects/RaBET/RaBET_species/NEON_FINAL/SRER/FinalOutputs/NDVI2019final.tif')
NDWI_2019 <- raster('//snow/projects/RaBET/RaBET_species/NEON_FINAL/SRER/FinalOutputs/NDWI2019final.tif')
PRI_2019 <- raster('//snow/projects/RaBET/RaBET_species/NEON_FINAL/SRER/FinalOutputs/PRI2019final.tif')
SWIRI_2019 <- raster('//snow/projects/RaBET/RaBET_species/NEON_FINAL/SRER/FinalOutputs/SWIRI2019final.tif')
SAVI_2019 <- raster('//snow/projects/RaBET/RaBET_species/NEON_FINAL/SRER/FinalOutputs/SAVI2019final.tif')
PRI2_2019 <- raster('//snow/projects/RaBET/RaBET_species/NEON_FINAL/SRER/FinalOutputs/PRI22019final.tif')
CACTI_2019 <- raster('//snow/projects/RaBET/RaBET_species/NEON_FINAL/SRER/FinalOutputs/CACTI2019final.tif')
CACTI2_2019 <- raster('//snow/projects/RaBET/RaBET_species/NEON_FINAL/SRER/FinalOutputs/CACTI22019final.tif')
MTCI_2019 <- raster('//snow/projects/RaBET/RaBET_species/NEON_FINAL/SRER/FinalOutputs/MTCI2019final.tif')
CI_2019 <- raster('//snow/projects/RaBET/RaBET_species/NEON_FINAL/SRER/FinalOutputs/CI2019final.tif')
CAI_2019 <- raster('//snow/projects/RaBET/RaBET_species/NEON_FINAL/SRER/FinalOutputs/CAI2019final.tif')
CELL_2019 <- raster('//snow/projects/RaBET/RaBET_species/NEON_FINAL/SRER/FinalOutputs/CELL2019final.tif')
CELL2_2019 <- raster('//snow/projects/RaBET/RaBET_species/NEON_FINAL/SRER/FinalOutputs/CELL22019final.tif')
NDNI_2019 <- raster('//snow/projects/RaBET/RaBET_species/NEON_FINAL/SRER/FinalOutputs/NDNI2019final.tif')
CHM_2019 <- raster('//snow/projects/RaBET/RaBET_species/NEON_FINAL/SRER/FinalOutputs/CHM2019final.tif')
#CONIFER_2019 <- raster('//snow/projects/RaBET/RaBET_species/NEON_FINAL/SRER/FinalOutputs/CONIFER2019final.tif')
#SPRUCE_2019 <- raster('//snow/projects/RaBET/RaBET_species/NEON_FINAL/SRER/FinalOutputs/SPRUCE2019final.tif')
#NDRE_2019 <- raster('//snow/projects/RaBET/RaBET_species/NEON_FINAL/SRER/FinalOutputs/NDRE2019final.tif')

NDVI_2020 <- raster('//snow/projects/RaBET/RaBET_species/NEON_FINAL/SRER/FinalOutputs/NDVI2020final.tif')
NDWI_2020 <- raster('//snow/projects/RaBET/RaBET_species/NEON_FINAL/SRER/FinalOutputs/NDWI2020final.tif')
PRI_2020 <- raster('//snow/projects/RaBET/RaBET_species/NEON_FINAL/SRER/FinalOutputs/PRI2020final.tif')
SWIRI_2020 <- raster('//snow/projects/RaBET/RaBET_species/NEON_FINAL/SRER/FinalOutputs/SWIRI2020final.tif')
SAVI_2020 <- raster('//snow/projects/RaBET/RaBET_species/NEON_FINAL/SRER/FinalOutputs/SAVI2020final.tif')
PRI2_2020 <- raster('//snow/projects/RaBET/RaBET_species/NEON_FINAL/SRER/FinalOutputs/PRI22020final.tif')
CACTI_2020 <- raster('//snow/projects/RaBET/RaBET_species/NEON_FINAL/SRER/FinalOutputs/CACTI2020final.tif')
CACTI2_2020 <- raster('//snow/projects/RaBET/RaBET_species/NEON_FINAL/SRER/FinalOutputs/CACTI22020final.tif')
MTCI_2020 <- raster('//snow/projects/RaBET/RaBET_species/NEON_FINAL/SRER/FinalOutputs/MTCI2020final.tif')
CI_2020 <- raster('//snow/projects/RaBET/RaBET_species/NEON_FINAL/SRER/FinalOutputs/CI2020final.tif')
CAI_2020 <- raster('//snow/projects/RaBET/RaBET_species/NEON_FINAL/SRER/FinalOutputs/CAI2020final.tif')
CELL_2020 <- raster('//snow/projects/RaBET/RaBET_species/NEON_FINAL/SRER/FinalOutputs/CELL2020final.tif')
CELL2_2020 <- raster('//snow/projects/RaBET/RaBET_species/NEON_FINAL/SRER/FinalOutputs/CELL22020final.tif')
NDNI_2020 <- raster('//snow/projects/RaBET/RaBET_species/NEON_FINAL/SRER/FinalOutputs/NDNI2020final.tif')
CHM_2020 <- raster('//snow/projects/RaBET/RaBET_species/NEON_FINAL/SRER/FinalOutputs/CHM2020final.tif')
#CONIFER_2020 <- raster('//snow/projects/RaBET/RaBET_species/NEON_FINAL/SRER/FinalOutputs/CONIFER2020final.tif')
#SPRUCE_2020 <- raster('//snow/projects/RaBET/RaBET_species/NEON_FINAL/SRER/FinalOutputs/SPRUCE2020final.tif')
#NDRE_2020 <- raster('//snow/projects/RaBET/RaBET_species/NEON_FINAL/SRER/FinalOutputs/NDRE2020final.tif')

NDVI_2021 <- raster('//snow/projects/RaBET/RaBET_species/NEON_FINAL/SRER/FinalOutputs/NDVI2021final_SRER.tif')
NDWI_2021 <- raster('//snow/projects/RaBET/RaBET_species/NEON_FINAL/SRER/FinalOutputs/NDWI2021final_SRER.tif')
PRI_2021 <- raster('//snow/projects/RaBET/RaBET_species/NEON_FINAL/SRER/FinalOutputs/PRI2021final_SRER.tif')
SWIRI_2021 <- raster('//snow/projects/RaBET/RaBET_species/NEON_FINAL/SRER/FinalOutputs/SWIRI2021final_SRER.tif')
SAVI_2021 <- raster('//snow/projects/RaBET/RaBET_species/NEON_FINAL/SRER/FinalOutputs/SAVI2021final_SRER.tif')
PRI2_2021 <- raster('//snow/projects/RaBET/RaBET_species/NEON_FINAL/SRER/FinalOutputs/PRI22021final_SRER.tif')
CACTI_2021 <- raster('//snow/projects/RaBET/RaBET_species/NEON_FINAL/SRER/FinalOutputs/CACTI2021final_SRER.tif')
CACTI2_2021 <- raster('//snow/projects/RaBET/RaBET_species/NEON_FINAL/SRER/FinalOutputs/CACTI22021final_SRER.tif')
MTCI_2021 <- raster('//snow/projects/RaBET/RaBET_species/NEON_FINAL/SRER/FinalOutputs/MTCI2021final_SRER.tif')
CI_2021 <- raster('//snow/projects/RaBET/RaBET_species/NEON_FINAL/SRER/FinalOutputs/CI2021final_SRER.tif')
CAI_2021 <- raster('//snow/projects/RaBET/RaBET_species/NEON_FINAL/SRER/FinalOutputs/CAI2021final_SRER.tif')
CELL_2021 <- raster('//snow/projects/RaBET/RaBET_species/NEON_FINAL/SRER/FinalOutputs/CELL2021final_SRER.tif')
CELL2_2021 <- raster('//snow/projects/RaBET/RaBET_species/NEON_FINAL/SRER/FinalOutputs/CELL22021final_SRER.tif')
NDNI_2021 <- raster('//snow/projects/RaBET/RaBET_species/NEON_FINAL/SRER/FinalOutputs/NDNI2021final_SRER.tif')
CHM_2021 <- raster('//snow/projects/RaBET/RaBET_species/NEON_FINAL/SRER/FinalOutputs/CHM2021final_SRER.tif')
#CONIFER_2021 <- raster('//snow/projects/RaBET/RaBET_species/NEON_FINAL/SRER/FinalOutputs/CONIFER2021final_SRER.tif')
#SPRUCE_2021 <- raster('//snow/projects/RaBET/RaBET_species/NEON_FINAL/SRER/FinalOutputs/SPRUCE2021final_SRER.tif')
#NDRE_2021 <- raster('//snow/projects/RaBET/RaBET_species/NEON_FINAL/SRER/FinalOutputs/NDRE2021final_SRER.tif')



#NDRE_2018 <- setExtent(NDRE_2018, NDVI_2018)
#NDRE_2018 <- resample(NDRE_2018, NDVI_2018)

#SPRUCE_2018 <- setExtent(SPRUCE_2018, NDVI_2018)
#SPRUCE_2018 <- resample(SPRUCE_2018, NDVI_2018)


#CONIFER_2018 <- setExtent(CONIFER_2018, NDVI_2018)
#CONIFER_2018 <- resample(CONIFER_2018, NDVI_2018)

CHM_2021 <- setExtent(CHM_2021, NDVI_2021)
CHM_2021 <- resample(CHM_2021, NDVI_2021)

CHM_2020 <- setExtent(CHM_2020, NDVI_2020)
CHM_2020 <- resample(CHM_2020, NDVI_2020)

CHM_2019 <- setExtent(CHM_2019, NDVI_2019)
CHM_2019 <- resample(CHM_2019, NDVI_2019)

CHM_2018 <- setExtent(CHM_2018, NDVI_2018)
CHM_2018 <- resample(CHM_2018, NDVI_2018)

CHM_2017 <- setExtent(CHM_2017, NDVI_2017)
CHM_2017 <- resample(CHM_2017, NDVI_2017)

#writeRaster(CHM_2021, file = 'crop_CHM2021_SRER_final.tif')
#writeRaster(CHM_2020, file = 'crop_CHM2020_SRER_final.tif')
#writeRaster(CHM_2019, file = 'crop_CHM2019_SRER_final.tif')
#writeRaster(CHM_2018, file = 'crop_CHM2018_SRER_final.tif')
#writeRaster(CHM_2017, file = 'crop_CHM2017_SRER_final.tif')
plot(NDVI_2020)
plot(NDVI_2019)
plot(NDVI_2018)
getwd()
NEON_2021 <- stack(NDVI_2021,NDWI_2021,PRI_2021,SWIRI_2021,SAVI_2021, 
                   PRI2_2021,CACTI_2021,CACTI2_2021,MTCI_2021,CI_2021,
                   CAI_2021,CELL_2021,CELL2_2021,NDNI_2021, CHM_2021)
NEON_2020 <- stack(NDVI_2020,NDWI_2020,PRI_2020,SWIRI_2020,SAVI_2020, 
                   PRI2_2020,CACTI_2020,CACTI2_2020,MTCI_2020,CI_2020,
                   CAI_2020,CELL_2020,CELL2_2020,NDNI_2020, CHM_2020)
NEON_2019 <- stack(NDVI_2019,NDWI_2019,PRI_2019,SWIRI_2019,SAVI_2019, 
                   PRI2_2019,CACTI_2019,CACTI2_2019,MTCI_2019,CI_2019,
                   CAI_2019,CELL_2019,CELL2_2019,NDNI_2019,CHM_2019)
NEON_2018 <- stack(NDVI_2018,NDWI_2018,PRI_2018,SWIRI_2018,SAVI_2018, 
                   PRI2_2018,CACTI_2018,CACTI2_2018,MTCI_2018,CI_2018,
                   CAI_2018,CELL_2018,CELL2_2018,NDNI_2018,CHM_2018)
NEON_2017 <- stack(NDVI_2017,NDWI_2017,PRI_2017,SWIRI_2017,SAVI_2017, 
                   PRI2_2017,CACTI_2017,CACTI2_2017,MTCI_2017,CI_2017,
                   CAI_2017,CELL_2017,CELL2_2017,NDNI_2017,CHM_2017)



crop <- shapefile("D:/projects/RaBET/RaBET_species/NEON_FINAL/SRER/Outputs/crop.shp")
plot(crop)
e <- extent(crop)


NEON_2021_crop <- crop(NEON_2021, e) 
NEON_2020_crop <- crop(NEON_2020, e) 
NEON_2019_crop <- crop(NEON_2019, e) 
NEON_2018_crop <- crop(NEON_2018, e) 
NEON_2017_crop <- crop(NEON_2017, e)


NEON_2021_mask <- mask(NEON_2021_crop, crop)
NEON_2020_mask <- mask(NEON_2020_crop, crop)
NEON_2019_mask <- mask(NEON_2019_crop, crop)
NEON_2018_mask <- mask(NEON_2018_crop, crop)
NEON_2017_mask <- mask(NEON_2017_crop, crop)


gc()

writeRaster(NEON_2021_mask, filename = "NEON_SRER_indices_2021_mask.tif",overwrite=TRUE)
writeRaster(NEON_2020_mask, filename = "NEON_SRER_indices_2020_mask.tif",overwrite=TRUE)
writeRaster(NEON_2019_mask, filename = "NEON_SRER_indices_2019_mask.tif",overwrite=TRUE)
writeRaster(NEON_2018_mask, filename = "NEON_SRER_indices_2018_mask.tif",overwrite=TRUE)# warnings()
writeRaster(NEON_2017_mask, filename = "NEON_SRER_indices_2017_mask.tif",overwrite=TRUE)









NEON_2021_extent <- setExtent(NEON_2021_mask, e)
NEON_2021_resample <- resample(NEON_2021_mask, NDVI_2021)
NEON_2020_extent <- setExtent(NEON_2020_mask, e)
NEON_2020_resample <- resample(NEON_2020_mask, NDVI_2021)
NEON_2019_extent <- setExtent(NEON_2019_mask, e)
NEON_2019_resample <- resample(NEON_2019_mask, NDVI_2021)
NEON_2018_extent <- setExtent(NEON_2018_mask, e)
NEON_2018_resample <- resample(NEON_2018_mask, NDVI_2021)
NEON_2017_extent <- setExtent(NEON_2017_mask, e)
NEON_2017_resample <- resample(NEON_2017_mask, NDVI_2021)

names(NEON_2018_resample) <- c('NDVI_2018','NDWI_2018','PRI_2018','SWIRI_2018','SAVI_2018',
                         'PRI2_2018','CACTI_2018','CACTI2_2018','MTCI_2018','CI_2018',
                         'CAI_2018','CELL_2018','CELL2_2018','NDNI_2018','CHM_2018')

gc()
NEON_indices <- stack(NEON_2021_resample, NEON_2019_resample, NEON_2018_resample, NEON_2017_resample)





names(NEON_indices) <- c('NDVI_2020','NDWI_2020','PRI_2020','SWIRI_2020','SAVI_2020',
                         'PRI2_2020','CACTI_2020','CACTI2_2020','MTCI_2020','CI_2020',
                         'CAI_2020','CELL_2020','CELL2_2020','NDNI_2020','CHM_2020',
                         'NDVI_2019','NDWI_2019','PRI_2019','SWIRI_2019','SAVI_2019',
                         'PRI2_2019','CACTI_2019','CACTI2_2019','MTCI_2019','CI_2019',
                         'CAI_2019','CELL_2019','CELL2_2019','NDNI_2019','CHM_2019',
                         'NDVI_2018','NDWI_2018','PRI_2018','SWIRI_2018','SAVI_2018',
                         'PRI2_2018','CACTI_2018','CACTI2_2018','MTCI_2018','CI_2018',
                         'CAI_2018','CELL_2018','CELL2_2018','NDNI_2018','CHM_2018',
)

