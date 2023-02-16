#This Rcode was written by Cynthia L. Norton, University of Arizona, 2021
#The code was used to create Indices models
#install.packages("BiocManager")
#BiocManager::install("rhdf5")

#Load Packages
library(raster)
library(rgdal)
library(RColorBrewer)
library(rhdf5)
library(tidyverse)

setwd("//snow/projects/RaBET/RaBET_species/NEON_SRER")

#import data
options(stringsAsFactors = FALSE)


#BULK process 
#list.files
NEON2017 <- list.files("D:/projects/RaBET/RaBET_species/NEON_YELL/2017/hyperspectral/", pattern=".tif$", full.names=T) #use pattern = '.tif$' or something else if you have multiple files in this folder
NEON2018 <- list.files("//snow/projects/RaBET/RaBET_species/NEON_WGER/hyperspectral", pattern=".tif$", full.names=T) #use pattern = '.tif$' or something else if you have multiple files in this folder
NEON2019 <- list.files("D:/projects/RaBET/RaBET_species/NEON_YELL/2019/hyperspectral/", pattern=".tif$", full.names=T) # or something else if you have multiple files in this folder
NEON2020 <- list.files("D:/projects/RaBET/RaBET_species/NEON_YELL/2020/hyperspectral/", pattern=".tif$", full.names=T) # or something else if you have multiple files in this folder
NEON2021 <- list.files("D:/projects/RaBET/RaBET_species/NEON_SRER/2021/hdf5/", pattern=".tif$", full.names=T) # or something else if you have multiple files in this folder


outws <- "//snow/projects/RaBET/RaBET_species/NEON_SRER/2021/hdf5/"


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


CONIFERfunction <- function(path, band1 = 6, band2 = 13){
  img <- stack(path)
  band1 <- img[[band1]]
  band2 <- img[[band2]]
  CONIFERfunction <- (band1-band2)/(band1+band2)
  writeRaster(CONIFERfunction, file.path(outws, paste(tools::file_path_sans_ext(basename(path)), '_CONIFER.tif', sep = "")), overwrite = T)
}


SPRUCEfunction <- function(path, band1 = 6, band2 = 11){
  img <- stack(path)
  band1 <- img[[band1]]
  band2 <- img[[band2]]
  SPRUCEfunction <- (band1-band2)/(band1+band2)
  writeRaster(SPRUCEfunction, file.path(outws, paste(tools::file_path_sans_ext(basename(path)), '_SPRUCE.tif', sep = "")), overwrite = T)
}

ndrefunction <- function(path, band1 = 15, band2 = 14){
  img <- stack(path)
  band1 <- img[[band1]]
  band2 <- img[[band2]]
  ndrefunction <- (band1-band2)/(band1+band2)
  writeRaster(ndrefunction, file.path(outws, paste(tools::file_path_sans_ext(basename(path)), '_NDRE.tif', sep = "")), overwrite = T)
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



sapply(NEON2021, CONIFERfunction)
sapply(NEON2020, CONIFERfunction)
sapply(NEON2019, CONIFERfunction)
sapply(NEON2018, CONIFERfunction)
sapply(NEON2017, CONIFERfunction)



sapply(NEON2021, SPRUCEfunction)
sapply(NEON2020, SPRUCEfunction)
sapply(NEON2019, SPRUCEfunction)
sapply(NEON2018, SPRUCEfunction)
sapply(NEON2017, SPRUCEfunction)


sapply(NEON2021, ndrefunction)
sapply(NEON2020, ndrefunction)
sapply(NEON2019, ndrefunction)
sapply(NEON2018, ndrefunction)
sapply(NEON2017, ndrefunction)




CONIFER2021 <- list.files(path = "//snow/projects/RaBET/RaBET_species/NEON_SRER/2021/indices/CONIFER", pattern = ".tif$", full.names = TRUE)
CONIFER2021 <- lapply(CONIFER2021, raster)
CONIFER2021final<- do.call(merge, c(CONIFER2021, tolerance = 1))
writeRaster(CONIFER2021final,
            file= "CONIFER2021final_WGER",
            format="GTiff",
            overwrite=TRUE)

SPRUCE2021 <- list.files(path = "//snow/projects/RaBET/RaBET_species/NEON_SRER/2021/indices/SPRUCE", pattern = ".tif$", full.names = TRUE)
SPRUCE2021 <- lapply(SPRUCE2021, raster)
SPRUCE2021final <- do.call(merge, c(SPRUCE2021, tolerance = 1))
writeRaster(SPRUCE2021final,
            file= "SPRUCE2021final_WGER",
            format="GTiff",
            overwrite=TRUE)

NDRE2021 <- list.files(path = "//snow/projects/RaBET/RaBET_species/NEON_SRER/2021/indices/NDRE", pattern = ".tif$", full.names = TRUE)
NDRE2021 <- lapply(NDRE2021, raster)
NDRE2021final <- do.call(merge, c(NDRE2021, tolerance = 1))
writeRaster(NDRE2021final,
            file= "NDRE2021final_WGER",
            format="GTiff",
            overwrite=TRUE)





#2021
NDVI2021 <- list.files(path = "//snow/projects/RaBET/RaBET_species/NEON_SRER/2021/indices/NDVI", pattern = ".tif$", full.names = TRUE)
NDVI2021 <- lapply(NDVI2021, raster)
NDVI2021final<- do.call(merge, c(NDVI2021, tolerance = 1))
writeRaster(NDVI2021final,
            file= "NDVI2021final_WGER",
            format="GTiff",
            overwrite=TRUE)

NDWI2021 <- list.files(path = "//snow/projects/RaBET/RaBET_species/NEON_SRER/2021/indices/NDWI", pattern = ".tif$", full.names = TRUE)
NDWI2021 <- lapply(NDWI2021, raster)
NDWI2021final <- do.call(merge, c(NDWI2021, tolerance = 1))
writeRaster(NDWI2021final,
            file= "NDWI2021final_WGER",
            format="GTiff",
            overwrite=TRUE)

PRI2021 <- list.files(path = "//snow/projects/RaBET/RaBET_species/NEON_SRER/2021/indices/PRI", pattern = ".tif$", full.names = TRUE)
PRI2021 <- lapply(PRI2021, raster)
PRI2021final <- do.call(merge, c(PRI2021, tolerance = 1))
writeRaster(PRI2021final,
            file= "PRI2021final_WGER",
            format="GTiff",
            overwrite=TRUE)

SWIRI2021 <- list.files(path = "//snow/projects/RaBET/RaBET_species/NEON_SRER/2021/indices/SWIRI", pattern = ".tif$", full.names = TRUE)
SWIRI2021 <- lapply(SWIRI2021, raster)
SWIRI2021final <- do.call(merge, c(SWIRI2021, tolerance = 1))
writeRaster(SWIRI2021final,
            file= "SWIRI2021final_WGER",
            format="GTiff",
            overwrite=TRUE)

SAVI2021 <- list.files(path = "//snow/projects/RaBET/RaBET_species/NEON_SRER/2021/indices/SAVI", pattern = ".tif$", full.names = TRUE)
SAVI2021 <- lapply(SAVI2021, raster)
SAVI2021final <- do.call(merge, c(SAVI2021, tolerance = 1))
writeRaster(SAVI2021final,
            file= "SAVI2021final_WGER",
            format="GTiff",
            overwrite=TRUE)




PRI22021 <- list.files(path = "//snow/projects/RaBET/RaBET_species/NEON_SRER/2021/indices/PRI2", pattern = ".tif$", full.names = TRUE)
PRI22021 <- lapply(PRI22021, raster)
PRI22021final<- do.call(merge, c(PRI22021, tolerance = 1))
writeRaster(PRI22021final,
            file= "PRI22021final_WGER",
            format="GTiff",
            overwrite=TRUE)

CI2021 <- list.files(path = "//snow/projects/RaBET/RaBET_species/NEON_SRER/2021/indices/CI", pattern = ".tif$", full.names = TRUE)
CI2021 <- lapply(CI2021, raster)
CI2021final <- do.call(merge, c(CI2021, tolerance = 1))
writeRaster(CI2021final,
            file= "CI2021final_WGER",
            format="GTiff",
            overwrite=TRUE)

MTCI2021 <- list.files(path = "//snow/projects/RaBET/RaBET_species/NEON_SRER/2021/indices/MTCI", pattern = ".tif$", full.names = TRUE)
MTCI2021 <- lapply(MTCI2021, raster)
MTCI2021final <- do.call(merge, c(MTCI2021, tolerance = 1))
writeRaster(MTCI2021final,
            file= "MTCI2021final_WGER",
            format="GTiff",
            overwrite=TRUE)

CACTI_2021 <- list.files(path = "//snow/projects/RaBET/RaBET_species/NEON_SRER/2021/indices/CACTI", pattern = ".tif$", full.names = TRUE)
CACTI_2021 <- lapply(CACTI_2021, raster)
CACTI_2021final <- do.call(merge, c(CACTI_2021, tolerance = 1))
writeRaster(CACTI_2021final,
            file= "CACTI2021final_WGER",
            format="GTiff",
            overwrite=TRUE)

CACTI22021 <- list.files(path = "//snow/projects/RaBET/RaBET_species/NEON_SRER/2021/indices/CACTI2", pattern = ".tif$", full.names = TRUE)
CACTI22021 <- lapply(CACTI22021, raster)
CACTI22021final <- do.call(merge, c(CACTI22021, tolerance = 1))
writeRaster(CACTI22021final,
            file= "CACTI22021final_WGER",
            format="GTiff",
            overwrite=TRUE)


NDNI2021 <- list.files(path = "//snow/projects/RaBET/RaBET_species/NEON_SRER/2021/indices/NDNI", pattern = ".tif$", full.names = TRUE)
NDNI2021 <- lapply(NDNI2021, raster)
NDNI2021final<- do.call(merge, c(NDNI2021, tolerance = 1))
writeRaster(NDNI2021final,
            file= "NDNI2021final_WGER",
            format="GTiff",
            overwrite=TRUE)

CAI2021 <- list.files(path = "//snow/projects/RaBET/RaBET_species/NEON_SRER/2021/indices/CAI", pattern = ".tif$", full.names = TRUE)
CAI2021 <- lapply(CAI2021, raster)
CAI2021final <- do.call(merge, c(CAI2021, tolerance = 1))
writeRaster(CAI2021final,
            file= "CAI2021final_WGER",
            format="GTiff",
            overwrite=TRUE)

CELL2021 <- list.files(path = "//snow/projects/RaBET/RaBET_species/NEON_SRER/2021/indices/CELL", pattern = ".tif$", full.names = TRUE)
CELL2021 <- lapply(CELL2021, raster)
CELL2021final <- do.call(merge, c(CELL2021, tolerance = 1))
writeRaster(CELL2021final,
            file= "CELL2021final_WGER",
            format="GTiff",
            overwrite=TRUE)

CELL22021 <- list.files(path = "//snow/projects/RaBET/RaBET_species/NEON_SRER/2021/indices/CELL2", pattern = ".tif$", full.names = TRUE)
CELL22021 <- lapply(CELL22021, raster)
CELL22021final <- do.call(merge, c(CELL22021, tolerance = 1))
writeRaster(CELL22021final,
            file= "CELL22021final_WGER",
            format="GTiff",
            overwrite=TRUE)


