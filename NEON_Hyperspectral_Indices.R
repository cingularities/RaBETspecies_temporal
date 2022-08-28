#This Rcode was written by Cynthia L. Norton, University of Arizona, 2021
#The code was used to create Indices models

#Load Pacakges
library(raster)
library(rgdal)
library(rgeos)
library(RColorBrewer)
library(rhdf5)
library(neonAOP)
library(tidyverse)

setwd("P:/RaBET/Indices/")

#import data
options(stringsAsFactors = FALSE)


#BULK process 
#list.files
NEON2017 <- list.files("P:/RaBET/Raw/NEON/2017/Reflectance/tif/", pattern=".tif$", full.names=T) #use pattern = '.tif$' or something else if you have multiple files in this folder
NEON2018 <- list.files("P:/RaBET/Raw/NEON/2018/Reflectance/tif/", pattern=".tif$", full.names=T) #use pattern = '.tif$' or something else if you have multiple files in this folder
NEON2019 <- list.files("P:/RaBET/Raw/NEON/2019/Reflectance/tif/", pattern=".tif$", full.names=T) # or something else if you have multiple files in this folder

outws <- '/RaBET/Indices'


#Functions for all indices
ndvifunction <- function(path, band1 = 16, band2 = 10){
  img <- stack(path)
  band1 <- img[[band1]]
  band2 <- img[[band2]]
  ndvi <- (band1-band2)/(band1+band2)
  writeRaster(ndvi, file.path(outws, paste(tools::file_path_sans_ext(basename(path)), '_NDVI.tif', sep = "")), overwrite = T)
}

ndwifunction <- function(path, band1 = 20, band2 = 16){
  img <- stack(path)
  band1 <- img[[band1]]
  band2 <- img[[band2]]
  ndwi <- (band1-band2)/(band1+band2)
  writeRaster(ndwi, file.path(outws, paste(tools::file_path_sans_ext(basename(path)), '_NDWI.tif', sep = "")), overwrite = T)
}

prifunction <- function(path, band1 = 5, band2 = 4){
  img <- stack(path)
  band1 <- img[[band1]]
  band2 <- img[[band2]]
  pri <- (band1-band2)/(band1+band2)
  writeRaster(pri, file.path(outws, paste(tools::file_path_sans_ext(basename(path)), '_PRI.tif', sep = "")), overwrite = T)
}

swirifunction <- function(path, band1 = 25, band2 = 23){
  img <- stack(path)
  band1 <- img[[band1]]
  band2 <- img[[band2]]
  swiri <- (band1-band2)/(band1+band2)
  writeRaster(swiri, file.path(outws, paste(tools::file_path_sans_ext(basename(path)), '_SWIRI.tif', sep = "")), overwrite = T)
}

savifunction <- function(path, band1 = 16, band2 = 10){
  img <- stack(path)
  band1 <- img[[band1]]
  band2 <- img[[band2]]
  savi <- (1.5*(band1-band2)/(band1+band2+5000))
  writeRaster(savi, file.path(outws, paste(tools::file_path_sans_ext(basename(path)), '_SAVI.tif', sep = "")), overwrite = T)
}

pri2function <- function(path, band1 = 4, band2 = 6){
  img <- stack(path)
  band1 <- img[[band1]]
  band2 <- img[[band2]]
  pri2 <- (band1-band2)/(band1+band2)
  writeRaster(pri2, file.path(outws, paste(tools::file_path_sans_ext(basename(path)), '_PRI2.tif', sep = "")), overwrite = T)
}

cifunction <- function(path, band1 = 14, band2 = 13){
  img <- stack(path)
  band1 <- img[[band1]]
  band2 <- img[[band2]]
  ci <- (band1)/((band1+band2)-1)
  writeRaster(ci, file.path(outws, paste(tools::file_path_sans_ext(basename(path)), '_CI.tif', sep = "")), overwrite = T)
}

mtcifunction <- function(path, band1 = 14, band2 = 13, band3 = 11){
  img <- stack(path)
  band1 <- img[[band1]]
  band2 <- img[[band2]]
  band3 <- img[[band3]]
  mtci <-  (band1-band2)/(band2-band3)
  writeRaster(mtci, file.path(outws, paste(tools::file_path_sans_ext(basename(path)), '_MTCI.tif', sep = "")), overwrite = T)
}

cactifunction <- function(path, band1 = 16, band2 = 17){
  img <- stack(path)
  band1 <- img[[band1]]
  band2 <- img[[band2]]
  ndvi <- (band1-band2)/(band1+band2)
  writeRaster(ndvi, file.path(outws, paste(tools::file_path_sans_ext(basename(path)), '_CACTI.tif', sep = "")), overwrite = T)
}

cacti2function <- function(path, band1 = 18, band2 = 17){
  img <- stack(path)
  band1 <- img[[band1]]
  band2 <- img[[band2]]
  cacti2 <- (band1-band2)/(band1+band2)
  writeRaster(cacti2, file.path(outws, paste(tools::file_path_sans_ext(basename(path)), '_CACTI2.tif', sep = "")), overwrite = T)
}

ndnifunction <- function(path, band1 = 22, band2 = 27){
  img <- stack(path)
  band1 <- img[[band1]]
  band2 <- img[[band2]]
  ndni <- (log(1/band1) - log(1/band2))/(log(1/band1) + log(1/band2))
  writeRaster(ndni, file.path(outws, paste(tools::file_path_sans_ext(basename(path)), '_NDNI.tif', sep = "")), overwrite = T)
}

caifunction <- function(path, band1 = 32, band2 = 36, band3 = 34){
  img <- stack(path)
  band1 <- img[[band1]]
  band2 <- img[[band2]]
  band3 <- img[[band3]]
  cai <- (0.5*(band1+band2)-band3)*10
  writeRaster(cai, file.path(outws, paste(tools::file_path_sans_ext(basename(path)), '_CAI.tif', sep = "")), overwrite = T)
}

cellfunction <- function(path, band1 = 35, band2 = 36){
  img <- stack(path)
  band1 <- img[[band1]]
  band2 <- img[[band2]]
  cell <- (band1-band2)/(band1+band2)
  writeRaster(cell, file.path(outws, paste(tools::file_path_sans_ext(basename(path)), '_CELL.tif', sep = "")), overwrite = T)
}

cell2function <- function(path, band1 = 37, band2 = 36){
  img <- stack(path)
  band1 <- img[[band1]]
  band2 <- img[[band2]]
  cell2 <- (band1-band2)/(band1+band2)
  writeRaster(cell2, file.path(outws, paste(tools::file_path_sans_ext(basename(path)), '_CELL2.tif', sep = "")), overwrite = T)
}


#Run functions
sapply(NEON2017, ndvifunction, simplify = FALSE, USE.NAMES = T)
sapply(NEON2017, ndwifunction, simplify = FALSE, USE.NAMES = T)
sapply(NEON2017, prifunction, simplify = FALSE, USE.NAMES = T)
sapply(NEON2017, swirifunction, simplify = FALSE, USE.NAMES = T)
sapply(NEON2017, savifunction, simplify = FALSE, USE.NAMES = T)

sapply(NEON2017, pri2function, simplify = FALSE, USE.NAMES = T)
sapply(NEON2017, cifunction, simplify = FALSE, USE.NAMES = T)
sapply(NEON2017, mtcifunction, simplify = FALSE, USE.NAMES = T)
sapply(NEON2017, cactifunction, simplify = FALSE, USE.NAMES = T)
sapply(NEON2017, cacti2function, simplify = FALSE, USE.NAMES = T)

sapply(NEON2017, ndnifunction, simplify = FALSE, USE.NAMES = T)
sapply(NEON2017, caifunction, simplify = FALSE, USE.NAMES = T)
sapply(NEON2017, cellfunction, simplify = FALSE, USE.NAMES = T)
sapply(NEON2017, cell2function, simplify = FALSE, USE.NAMES = T)

sapply(NEON2018, pri2function, simplify = FALSE, USE.NAMES = T)
sapply(NEON2018, cifunction, simplify = FALSE, USE.NAMES = T)
sapply(NEON2018, mtcifunction, simplify = FALSE, USE.NAMES = T)
sapply(NEON2018, cactifunction, simplify = FALSE, USE.NAMES = T)
sapply(NEON2018, cacti2function, simplify = FALSE, USE.NAMES = T)

sapply(NEON2018, ndnifunction, simplify = FALSE, USE.NAMES = T)
sapply(NEON2018, caifunction, simplify = FALSE, USE.NAMES = T)
sapply(NEON2018, cellfunction, simplify = FALSE, USE.NAMES = T)
sapply(NEON2018, cell2function, simplify = FALSE, USE.NAMES = T)

sapply(NEON2019, pri2function, simplify = FALSE, USE.NAMES = T)
sapply(NEON2019, cifunction, simplify = FALSE, USE.NAMES = T)
sapply(NEON2019, mtcifunction, simplify = FALSE, USE.NAMES = T)
sapply(NEON2019, cactifunction, simplify = FALSE, USE.NAMES = T)
sapply(NEON2019, cacti2function, simplify = FALSE, USE.NAMES = T)

sapply(NEON2019, ndnifunction, simplify = FALSE, USE.NAMES = T)
sapply(NEON2019, caifunction, simplify = FALSE, USE.NAMES = T)
sapply(NEON2019, cellfunction, simplify = FALSE, USE.NAMES = T)
sapply(NEON2019, cell2function, simplify = FALSE, USE.NAMES = T)


sapply(NEON2018, ndvifunction, simplify = FALSE, USE.NAMES = T)
sapply(NEON2018, ndwifunction, simplify = FALSE, USE.NAMES = T)
sapply(NEON2018, prifunction, simplify = FALSE, USE.NAMES = T)
sapply(NEON2018, swirifunction, simplify = FALSE, USE.NAMES = T)
sapply(NEON2018, savifunction, simplify = FALSE, USE.NAMES = T)

sapply(NEON2019, ndvifunction, simplify = FALSE, USE.NAMES = T)
sapply(NEON2019, ndwifunction, simplify = FALSE, USE.NAMES = T)
sapply(NEON2019, prifunction, simplify = FALSE, USE.NAMES = T)
sapply(NEON2019, swirifunction, simplify = FALSE, USE.NAMES = T)
sapply(NEON2019, savifunction, simplify = FALSE, USE.NAMES = T)


#MOSAIC and EXPORT indices
#2017
NDVI2017 <- list.files(path = "P:/RaBET/Indices/2017NDVI", pattern = ".tif$", full.names = TRUE)
NDVI2017 <- lapply(NDVI2017, raster)
NDVI2017final<- do.call(merge, c(NDVI2017, tolerance = 1))
writeRaster(NDVI2017final,
            file= "NDVI2017final",
            format="GTiff",
            overwrite=TRUE)

NDWI2017 <- list.files(path = "P:/RaBET/Indices/2017NDWI", pattern = ".tif$", full.names = TRUE)
NDWI2017 <- lapply(NDWI2017, raster)
NDWI2017final <- do.call(merge, c(NDWI2017, tolerance = 1))
writeRaster(NDWI2017final,
            file= "NDWI2017final",
            format="GTiff",
            overwrite=TRUE)

PRI2017 <- list.files(path = "P:/RaBET/Indices/2017PRI", pattern = ".tif$", full.names = TRUE)
PRI2017 <- lapply(PRI2017, raster)
PRI2017final <- do.call(merge, c(PRI2017, tolerance = 1))
writeRaster(PRI2017final,
            file= "PRI2017final",
            format="GTiff",
            overwrite=TRUE)

SWIRI2017 <- list.files(path = "P:/RaBET/Indices/2017SWIRI", pattern = ".tif$", full.names = TRUE)
SWIRI2017 <- lapply(SWIRI2017, raster)
SWIRI2017final <- do.call(merge, c(SWIRI2017, tolerance = 1))
writeRaster(SWIRI2017final,
            file= "SWIRI2017final",
            format="GTiff",
            overwrite=TRUE)

SAVI2017 <- list.files(path = "P:/RaBET/Indices/2017SAVI", pattern = ".tif$", full.names = TRUE)
SAVI2017 <- lapply(SAVI2017, raster)
SAVI2017final <- do.call(merge, c(SAVI2017, tolerance = 1))
writeRaster(SAVI2017final,
            file= "SAVI2017final",
            format="GTiff",
            overwrite=TRUE)





#2018
NDVI2018 <- list.files(path = "P:/RaBET/Indices/2018NDVI", pattern = ".tif$", full.names = TRUE)
NDVI2018 <- lapply(NDVI2018, raster)
NDVI2018final<- do.call(merge, c(NDVI2018, tolerance = 1))
writeRaster(NDVI2018final,
            file= "NDVI2018final",
            format="GTiff",
            overwrite=TRUE)

NDWI2018 <- list.files(path = "P:/RaBET/Indices/2018NDWI", pattern = ".tif$", full.names = TRUE)
NDWI2018 <- lapply(NDWI2018, raster)
NDWI2018final <- do.call(merge, c(NDWI2018, tolerance = 1))
writeRaster(NDWI2018final,
            file= "NDWI2018final",
            format="GTiff",
            overwrite=TRUE)

PRI2018 <- list.files(path = "P:/RaBET/Indices/2018PRI", pattern = ".tif$", full.names = TRUE)
PRI2018 <- lapply(PRI2018, raster)
PRI2018final <- do.call(merge, c(PRI2018, tolerance = 1))
writeRaster(PRI2018final,
            file= "PRI2018final",
            format="GTiff",
            overwrite=TRUE)

SWIRI2018 <- list.files(path = "P:/RaBET/Indices/2018SWIRI", pattern = ".tif$", full.names = TRUE)
SWIRI2018 <- lapply(SWIRI2018, raster)
SWIRI2018final <- do.call(merge, c(SWIRI2018, tolerance = 1))
writeRaster(SWIRI2018final,
            file= "SWIRI2018final",
            format="GTiff",
            overwrite=TRUE)

SAVI2018 <- list.files(path = "P:/RaBET/Indices/2018SAVI", pattern = ".tif$", full.names = TRUE)
SAVI2018 <- lapply(SAVI2018, raster)
SAVI2018final <- do.call(merge, c(SAVI2018, tolerance = 1))
writeRaster(SAVI2018final,
            file= "SAVI2018final",
            format="GTiff",
            overwrite=TRUE)





#2019
NDVI2019 <- list.files(path = "P:/RaBET/Indices/2019NDVI", pattern = ".tif$", full.names = TRUE)
NDVI2019 <- lapply(NDVI2019, raster)
NDVI2019final<- do.call(merge, c(NDVI2019, tolerance = 1))
writeRaster(NDVI2019final,
            file= "NDVI2019final",
            format="GTiff",
            overwrite=TRUE)

NDWI2019 <- list.files(path = "P:/RaBET/Indices/2019NDWI", pattern = ".tif$", full.names = TRUE)
NDWI2019 <- lapply(NDWI2019, raster)
NDWI2019final <- do.call(merge, c(NDWI2019, tolerance = 1))
writeRaster(NDWI2019final,
            file= "NDWI2019final",
            format="GTiff",
            overwrite=TRUE)

PRI2019 <- list.files(path = "P:/RaBET/Indices/2019PRI", pattern = ".tif$", full.names = TRUE)
PRI2019 <- lapply(PRI2019, raster)
PRI2019final <- do.call(merge, c(PRI2019, tolerance = 1))
writeRaster(PRI2019final,
            file= "PRI2019final",
            format="GTiff",
            overwrite=TRUE)

SWIRI2019 <- list.files(path = "P:/RaBET/Indices/2019SWIRI", pattern = ".tif$", full.names = TRUE)
SWIRI2019 <- lapply(SWIRI2019, raster)
SWIRI2019final <- do.call(merge, c(SWIRI2019, tolerance = 1))
writeRaster(SWIRI2019final,
            file= "SWIRI2019final",
            format="GTiff",
            overwrite=TRUE)

SAVI2019 <- list.files(path = "P:/RaBET/Indices/2019SAVI", pattern = ".tif$", full.names = TRUE)
SAVI2019 <- lapply(SAVI2019, raster)
SAVI2019final <- do.call(merge, c(SAVI2019, tolerance = 1))
writeRaster(SAVI2019final,
            file= "SAVI2019final",
            format="GTiff",
            overwrite=TRUE)




#MOSAIC and EXPORT elevation models
#2017
CHM2017 <- list.files(path = "P:/RaBET/Raw/NEON/2017/KyleHartfield_DiscreteLidar/CanopyHeightModelGtif", pattern = ".tif$", full.names = TRUE)
CHM2017 <- lapply(CHM2017, raster)
CHM2017final<- do.call(merge, c(CHM2017, tolerance = 1))
writeRaster(CHM2017final,
            file= "CHM2017final",
            format="GTiff",
            overwrite=TRUE)

DSM2017 <- list.files(path = "P:/RaBET/Raw/NEON/2017/KyleHartfield_DiscreteLidar/DSMGtif", pattern = ".tif$", full.names = TRUE)
DSM2017 <- lapply(DSM2017, raster)
DSM2017final <- do.call(merge, c(DSM2017, tolerance = 1))
writeRaster(DSM2017final,
            file= "DSM2017final",
            format="GTiff",
            overwrite=TRUE)

DTM2017 <- list.files(path = "P:/RaBET/Raw/NEON/2017/KyleHartfield_DiscreteLidar/DTMGtif", pattern = ".tif$", full.names = TRUE)
DTM2017 <- lapply(DTM2017, raster)
DTM2017final <- do.call(merge, c(DTM2017, tolerance = 1))
writeRaster(DTM2017final,
            file= "DTM2017final",
            format="GTiff",
            overwrite=TRUE)

Aspect2017 <- list.files(path = "P:/RaBET/Raw/NEON/2017/KyleHartfield_DiscreteLidar/AspectGtif", pattern = ".tif$", full.names = TRUE)
Aspect2017 <- lapply(Aspect2017, raster)
Aspect2017final <- do.call(merge, c(Aspect2017, tolerance = 1))
writeRaster(Aspect2017final,
            file= "Aspect2017final",
            format="GTiff",
            overwrite=TRUE)

Slope2017 <- list.files(path = "P:/RaBET/Raw/NEON/2017/KyleHartfield_DiscreteLidar/SlopeGtif", pattern = ".tif$", full.names = TRUE)
Slope2017 <- lapply(Slope2017, raster)
Slope2017final <- do.call(merge, c(Slope2017, tolerance = 1))
writeRaster(Slope2017final,
            file= "Slope2017final",
            format="GTiff",
            overwrite=TRUE)





#2018
CHM2018 <- list.files(path = "P:/RaBET/Raw/NEON/2018/KyleHartfield_DiscreteLidar/CanopyHeightModelGtif", pattern = ".tif$", full.names = TRUE)
CHM2018 <- lapply(CHM2018, raster)
CHM2018final<- do.call(merge, c(CHM2018, tolerance = 1))
writeRaster(CHM2018final,
            file= "CHM2018final",
            format="GTiff",
            overwrite=TRUE)

DSM2018 <- list.files(path = "P:/RaBET/Raw/NEON/2018/KyleHartfield_DiscreteLidar/DSMGtif", pattern = ".tif$", full.names = TRUE)
DSM2018 <- lapply(DSM2018, raster)
DSM2018final <- do.call(merge, c(DSM2018, tolerance = 1))
writeRaster(DSM2018final,
            file= "DSM2018final",
            format="GTiff",
            overwrite=TRUE)

DTM2018 <- list.files(path = "P:/RaBET/Raw/NEON/2018/KyleHartfield_DiscreteLidar/DTMGtif", pattern = ".tif$", full.names = TRUE)
DTM2018 <- lapply(DTM2018, raster)
DTM2018final <- do.call(merge, c(DTM2018, tolerance = 1))
writeRaster(DTM2018final,
            file= "DTM2018final",
            format="GTiff",
            overwrite=TRUE)

Aspect2018 <- list.files(path = "P:/RaBET/Raw/NEON/2018/KyleHartfield_DiscreteLidar/AspectGtif", pattern = ".tif$", full.names = TRUE)
Aspect2018 <- lapply(Aspect2018, raster)
Aspect2018final <- do.call(merge, c(Aspect2018, tolerance = 1))
writeRaster(Aspect2018final,
            file= "Aspect2018final",
            format="GTiff",
            overwrite=TRUE)

Slope2018 <- list.files(path = "P:/RaBET/Raw/NEON/2018/KyleHartfield_DiscreteLidar/SlopeGtif", pattern = ".tif$", full.names = TRUE)
Slope2018 <- lapply(Slope2018, raster)
Slope2018final <- do.call(merge, c(Slope2018, tolerance = 1))
writeRaster(Slope2018final,
            file= "Slope2018final",
            format="GTiff",
            overwrite=TRUE)





#2019
CHM2019 <- list.files(path = "P:/RaBET/Raw/NEON/2019/KyleHartfield_DiscreteLidar/CanopyHeightModelGtif", pattern = ".tif$", full.names = TRUE)
CHM2019 <- lapply(CHM2019, raster)
CHM2019final<- do.call(merge, c(CHM2019, tolerance = 1))
writeRaster(CHM2019final,
            file= "CHM2019final",
            format="GTiff",
            overwrite=TRUE)

DSM2019 <- list.files(path = "P:/RaBET/Raw/NEON/2019/KyleHartfield_DiscreteLidar/DSMGtif", pattern = ".tif$", full.names = TRUE)
DSM2019 <- lapply(DSM2019, raster)
DSM2019final <- do.call(merge, c(DSM2019, tolerance = 1))
writeRaster(DSM2019final,
            file= "DSM2019final",
            format="GTiff",
            overwrite=TRUE)

DTM2019 <- list.files(path = "P:/RaBET/Raw/NEON/2019/KyleHartfield_DiscreteLidar/DTMGtif", pattern = ".tif$", full.names = TRUE)
DTM2019 <- lapply(DTM2019, raster)
DTM2019final <- do.call(merge, c(DTM2019, tolerance = 1))
writeRaster(DTM2019final,
            file= "DTM2019final",
            format="GTiff",
            overwrite=TRUE)

Aspect2019 <- list.files(path = "P:/RaBET/Raw/NEON/2019/KyleHartfield_DiscreteLidar/AspectGtif", pattern = ".tif$", full.names = TRUE)
Aspect2019 <- lapply(Aspect2019, raster)
Aspect2019final <- do.call(merge, c(Aspect2019, tolerance = 1))
writeRaster(Aspect2019final,
            file= "Aspect2019final",
            format="GTiff",
            overwrite=TRUE)

Slope2019 <- list.files(path = "P:/RaBET/Raw/NEON/2019/KyleHartfield_DiscreteLidar/SlopeGtif", pattern = ".tif$", full.names = TRUE)
Slope2019 <- lapply(Slope2019, raster)
Slope2019final <- do.call(merge, c(Slope2019, tolerance = 1))
writeRaster(Slope2019final,
            file= "Slope2019final",
            format="GTiff",
            overwrite=TRUE)


#Functions
# calculate NDVI
NDVI <- function(x) {
  (x[,16]-x[,10])/(x[,16]+x[,10])
}

#Calculate NDWI
NDWI <- function(x) {
  (x[,20]-x[,16])/(x[,20]+x[,16])
}

#Calculate SWIRI
SWIRI <- function(x) {
  (x[,25]-x[,23])/(x[,25]+x[,23])
}

#Calculate PRI
PRI <- function(x) {
  (x[,5]-x[,4])/(x[,5]+x[,4])
}

#Calculate SAVI
SAVI <- function(x) {
  (1.5*(x[,16]-x[,10])/(x[,16]+x[,10]+5000))
}
