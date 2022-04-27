#This Rcode was written by Cynthia L. Norton, University of Arizona, 2021
#The code was used to create Indices models

#Load Pacakges
library(raster)
library(rgdal)
library(RColorBrewer)
library(rhdf5)
library(tidyverse)

setwd("D:/projects/RaBET/NEON_MOAB")

#import data
options(stringsAsFactors = FALSE)


#BULK process 
#list.files
NEON2017 <- list.files("D:/projects/RaBET/NEON_MOAB/HDF5/2017/", pattern=".tif$", full.names=T) #use pattern = '.tif$' or something else if you have multiple files in this folder
NEON2018 <- list.files("D:/projects/RaBET/NEON_MOAB/HDF5/2018/", pattern=".tif$", full.names=T) #use pattern = '.tif$' or something else if you have multiple files in this folder
NEON2019 <- list.files("D:/projects/RaBET/NEON_MOAB/HDF5/2019/", pattern=".tif$", full.names=T) # or something else if you have multiple files in this folder
NEON2020 <- list.files("D:/projects/RaBET/NEON_MOAB/HDF5/2020/", pattern=".tif$", full.names=T) # or something else if you have multiple files in this folder
NEON2021 <- list.files("D:/projects/RaBET/NEON_MOAB/HDF5/2021/", pattern=".tif$", full.names=T) # or something else if you have multiple files in this folder


outws <- "D:/projects/RaBET/NEON_MOAB"


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

mndwifunction <- function(path, band1 = 5, band2 = 23){
  img <- stack(path)
  band1 <- img[[band1]]
  band2 <- img[[band2]]
  mndwifunction <- (band1-band2)/(band1+band2)
  writeRaster(mndwifunction, file.path(outws, paste(tools::file_path_sans_ext(basename(path)), '_MNDWI.tif', sep = "")), overwrite = T)
}


ndiifunction <- function(path, band1 = 17, band2 = 24){
  img <- stack(path)
  band1 <- img[[band1]]
  band2 <- img[[band2]]
  ndiifunction <- (band1-band2)/(band1+band2)
  writeRaster(ndiifunction, file.path(outws, paste(tools::file_path_sans_ext(basename(path)), '_NDII.tif', sep = "")), overwrite = T)
}



#Run functions
lapply(NEON2017, ndvifunction)
lapply(NEON2017, ndwifunction)
lapply(NEON2017, prifunction)
lapply(NEON2017, swirifunction)
lapply(NEON2017, savifunction)

lapply(NEON2017, pri2function)
lapply(NEON2017, cifunction)
lapply(NEON2017, mtcifunction)
lapply(NEON2017, cactifunction)
lapply(NEON2017, cacti2function)

sapply(NEON2017, ndnifunction)
sapply(NEON2017, caifunction)
sapply(NEON2017, cellfunction)
sapply(NEON2017, cell2function)


sapply(NEON2018, ndvifunction)
sapply(NEON2018, ndwifunction)
sapply(NEON2018, prifunction)
sapply(NEON2018, swirifunction)
sapply(NEON2018, savifunction)

sapply(NEON2018, pri2function)
sapply(NEON2018, cifunction)
sapply(NEON2018, mtcifunction)
sapply(NEON2018, cactifunction)
sapply(NEON2018, cacti2function)

sapply(NEON2018, ndnifunction)
sapply(NEON2018, caifunction)
sapply(NEON2018, cellfunction)
sapply(NEON2018, cell2function)


sapply(NEON2019, ndvifunction)
sapply(NEON2019, ndwifunction)
sapply(NEON2019, prifunction)
sapply(NEON2019, swirifunction)
sapply(NEON2019, savifunction)


sapply(NEON2019, pri2function)
sapply(NEON2019, cifunction)
sapply(NEON2019, mtcifunction)
sapply(NEON2019, cactifunction)
sapply(NEON2019, cacti2function)

sapply(NEON2019, ndnifunction)
sapply(NEON2019, caifunction)
sapply(NEON2019, cellfunction)
sapply(NEON2019, cell2function)


sapply(NEON2020, ndvifunction)
sapply(NEON2020, ndwifunction)
sapply(NEON2020, prifunction)
sapply(NEON2020, swirifunction)
sapply(NEON2020, savifunction)


sapply(NEON2020, pri2function)
sapply(NEON2020, cifunction)
sapply(NEON2020, mtcifunction)
sapply(NEON2020, cactifunction)
sapply(NEON2020, cacti2function)

sapply(NEON2020, ndnifunction)
sapply(NEON2020, caifunction)
sapply(NEON2020, cellfunction)
sapply(NEON2020, cell2function)


sapply(NEON2021, ndvifunction)
sapply(NEON2021, ndwifunction)
sapply(NEON2021, prifunction)
sapply(NEON2021, swirifunction)
sapply(NEON2021, savifunction)


sapply(NEON2021, pri2function)
sapply(NEON2021, cifunction)
sapply(NEON2021, mtcifunction)
sapply(NEON2021, cactifunction)
sapply(NEON2021, cacti2function)

sapply(NEON2021, ndnifunction)
sapply(NEON2021, caifunction)
sapply(NEON2021, cellfunction)
sapply(NEON2021, cell2function)





#2017
NDVI2017 <- list.files(path = "D:/projects/RaBET/NEON_MOAB/Indices/2017/NDVI", pattern = ".tif$", full.names = TRUE)
NDVI2017 <- lapply(NDVI2017, raster)
NDVI2017final<- do.call(merge, c(NDVI2017, tolerance = 1))
writeRaster(NDVI2017final,
            file= "NDVI2017final_MOAB",
            format="GTiff",
            overwrite=TRUE)

NDWI2017 <- list.files(path = "D:/projects/RaBET/NEON_MOAB/Indices/2017/NDWI", pattern = ".tif$", full.names = TRUE)
NDWI2017 <- lapply(NDWI2017, raster)
NDWI2017final <- do.call(merge, c(NDWI2017, tolerance = 1))
writeRaster(NDWI2017final,
            file= "NDWI2017final_MOAB",
            format="GTiff",
            overwrite=TRUE)

PRI2017 <- list.files(path = "D:/projects/RaBET/NEON_MOAB/Indices/2017/PRI", pattern = ".tif$", full.names = TRUE)
PRI2017 <- lapply(PRI2017, raster)
PRI2017final <- do.call(merge, c(PRI2017, tolerance = 1))
writeRaster(PRI2017final,
            file= "PRI2017final_MOAB",
            format="GTiff",
            overwrite=TRUE)

SWIRI2017 <- list.files(path = "D:/projects/RaBET/NEON_MOAB/Indices/2017/SWIRI", pattern = ".tif$", full.names = TRUE)
SWIRI2017 <- lapply(SWIRI2017, raster)
SWIRI2017final <- do.call(merge, c(SWIRI2017, tolerance = 1))
writeRaster(SWIRI2017final,
            file= "SWIRI2017final_MOAB",
            format="GTiff",
            overwrite=TRUE)

SAVI2017 <- list.files(path = "D:/projects/RaBET/NEON_MOAB/Indices/2017/SAVI", pattern = ".tif$", full.names = TRUE)
SAVI2017 <- lapply(SAVI2017, raster)
SAVI2017final <- do.call(merge, c(SAVI2017, tolerance = 1))
writeRaster(SAVI2017final,
            file= "SAVI2017final_MOAB",
            format="GTiff",
            overwrite=TRUE)




PRI22017 <- list.files(path = "D:/projects/RaBET/NEON_MOAB/Indices/2017/PRI2", pattern = ".tif$", full.names = TRUE)
PRI22017 <- lapply(PRI22017, raster)
PRI22017final<- do.call(merge, c(PRI22017, tolerance = 1))
writeRaster(PRI22017final,
            file= "PRI22017final_MOAB",
            format="GTiff",
            overwrite=TRUE)

CI2017 <- list.files(path = "D:/projects/RaBET/NEON_MOAB/Indices/2017/CI", pattern = ".tif$", full.names = TRUE)
CI2017 <- lapply(CI2017, raster)
CI2017final <- do.call(merge, c(CI2017, tolerance = 1))
writeRaster(CI2017final,
            file= "CI2017final_MOAB",
            format="GTiff",
            overwrite=TRUE)

MTCI2017 <- list.files(path = "D:/projects/RaBET/NEON_MOAB/Indices/2017/MTCI", pattern = ".tif$", full.names = TRUE)
MTCI2017 <- lapply(MTCI2017, raster)
MTCI2017final <- do.call(merge, c(MTCI2017, tolerance = 1))
writeRaster(MTCI2017final,
            file= "MTCI2017final_MOAB",
            format="GTiff",
            overwrite=TRUE)

CACTI_2017 <- list.files(path = "D:/projects/RaBET/NEON_MOAB/Indices/2017/CACTI", pattern = ".tif$", full.names = TRUE)
CACTI_2017 <- lapply(CACTI_2017, raster)
CACTI_2017final <- do.call(merge, c(CACTI_2017, tolerance = 1))
writeRaster(CACTI_2017final,
            file= "CACTI2017final_MOAB",
            format="GTiff",
            overwrite=TRUE)

CACTI22017 <- list.files(path = "D:/projects/RaBET/NEON_MOAB/Indices/2017/CACTI2", pattern = ".tif$", full.names = TRUE)
CACTI22017 <- lapply(CACTI22017, raster)
CACTI22017final <- do.call(merge, c(CACTI22017, tolerance = 1))
writeRaster(CACTI22017final,
            file= "CACTI22017final_MOAB",
            format="GTiff",
            overwrite=TRUE)


NDNI2017 <- list.files(path = "D:/projects/RaBET/NEON_MOAB/Indices/2017/NDNI", pattern = ".tif$", full.names = TRUE)
NDNI2017 <- lapply(NDNI2017, raster)
NDNI2017final<- do.call(merge, c(NDNI2017, tolerance = 1))
writeRaster(NDNI2017final,
            file= "NDNI2017final_MOAB",
            format="GTiff",
            overwrite=TRUE)

CAI2017 <- list.files(path = "D:/projects/RaBET/NEON_MOAB/Indices/2017/CAI", pattern = ".tif$", full.names = TRUE)
CAI2017 <- lapply(CAI2017, raster)
CAI2017final <- do.call(merge, c(CAI2017, tolerance = 1))
writeRaster(CAI2017final,
            file= "CAI2017final_MOAB",
            format="GTiff",
            overwrite=TRUE)

CELL2017 <- list.files(path = "D:/projects/RaBET/NEON_MOAB/Indices/2017/CELL", pattern = ".tif$", full.names = TRUE)
CELL2017 <- lapply(CELL2017, raster)
CELL2017final <- do.call(merge, c(CELL2017, tolerance = 1))
writeRaster(CELL2017final,
            file= "CELL2017final_MOAB",
            format="GTiff",
            overwrite=TRUE)

CELL22017 <- list.files(path = "D:/projects/RaBET/NEON_MOAB/Indices/2017/CELL2", pattern = ".tif$", full.names = TRUE)
CELL22017 <- lapply(CELL22017, raster)
CELL22017final <- do.call(merge, c(CELL22017, tolerance = 1))
writeRaster(CELL22017final,
            file= "CELL22017final_MOAB",
            format="GTiff",
            overwrite=TRUE)


