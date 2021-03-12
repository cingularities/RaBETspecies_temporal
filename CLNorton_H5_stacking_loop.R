#Use Tools->Install Packages... to install all packages below except rhdf5
#Install BiocManager and run BiocManager::install("rhdf5") in console to install rhdf5
#You only need to install the packages the first time you use them.
#YOU WILL NEED TO CHANGE THE NUMBERED ITEMS

#load packages - must be everytime you run the code
library(rhdf5)
library(raster)
library(plyr)
library(rgeos)
library(rgdal)
library(ggplot2)
library(devtools)

# 1. set wd - this is where data without a full path is pulled from and where outputs are placed.
setwd("P:\\\\RaBET\\Raw\\NEON\\2018\\Reflectance\\h5")

# 2. list bands to process.Input band numbers you would like to stack.
#bands <- list(3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,256,257,258,259,260,261,262,263,264,265,266,267,268,269,270,271,272,273,274,275,276,277,278,279,280,281,282,283,316,317,318,319,320,321,322,323,324,325,326,327,328,329,330,331,332,333,334,335,336,337,338,339,340,341,342,343,344,345,346,347,348,349,350,351,352,353,354,355,356,357,358,359,360,361,362,363,364,365,366,367,368,369,370,371,372,373,374,375,376,377,378,379,380,381,382,383,384,385,386,387,388,389,390,391,392,393,394,395,396,397,398,399,400,401,402,403,404,405,406,407,408,409,410,411,412,413,414,415,416,417,418)
#bands <- list(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,256,257,258,259,260,261,262,263,264,265,266,267,268,269,270,271,272,273,274,275,276,277,278,279,280,281,282,283,284,285,286,287,288,289,290,291,292,293,294,295,296,297,298,299,300,301,302,303,304,305,306,307,308,309,310,311,312,313,314,315,316,317,318,319,320,321,322,323,324,325,326,327,328,329,330,331,332,333,334,335,336,337,338,339,340,341,342,343,344,345,346,347,348,349,350,351,352,353,354,355,356,357,358,359,360,361,362,363,364,365,366,367,368,369,370,371,372,373,374,375,376,377,378,379,380,381,382,383,384,385,386,387,388,389,390,391,392,393,394,395,396,397,398,399,400,401,402,403,404,405,406,407,408,409,410,411,412,413,414,415,416,417,418,419,420,421,422,423,424,425,426)
bands <- list(6,15,25,31,35,38,42,49,55,57,59,65,67,75,81,97,119,139,164,173,177,226,234,248,254,257,260,271,275,314,318,324,326,351,354,366,369,397)
bandstring <- paste(bands, collapse='_')

#list h5 files in folder
imglist <- list.files(pattern='h5$', full.names = TRUE)

# 3. define the file name as an object
#imglist <- list("\\\\snow\\2019\\FullSite\\D14\\2019_SRER_3\\L3\\Spectrometer\\Reflectance\\NEON_D14_SRER_DP3_515000_3519000_reflectance.h5")

# look at the HDF5 file structure 
#h5ls(f,all=T)

for(f in imglist){
  # get attributes for the Reflectance dataset
  reflInfo <- h5readAttributes(f,"/SRER/Reflectance/Reflectance_Data")
  
  # note that we can grab the dimensions of the dataset from the attributes
  # we can then use that information to slice out our band data
  nRows <- reflInfo$Dimensions[1]
  nCols <- reflInfo$Dimensions[2]
  nBands <- reflInfo$Dimensions[3]
  
  #create empty stack
  rasterstack <- stack()
  
  for(b in bands) {
    
    # Extract or "slice" data for specific band from the HDF5 file
    band <- h5read(f,"/SRER/Reflectance/Reflectance_Data",index=list(b,1:nCols,1:nRows))
    
    # convert from array to matrix
    band <- band[1,,]
    
    # there is a no data value in our raster - let's define it
    myNoDataValue <- as.numeric(reflInfo$`Data_Ignore_Value`)
    
    # set all values greater than -9999 to NA
    band[band == myNoDataValue] <- NA
    
    # We need to transpose x and y values in order for our 
    # final image to plot properly
    band<-t(band)
    
    # Populate the raster image extent value. 
    # get the map info, split out elements
    mapInfo<-h5read(f,"/SRER/Reflectance/Metadata/Coordinate_System/Map_Info")
    
    # Extract each element of the map info information 
    # so we can extract the lower left hand corner coordinates.
    mapInfo<-unlist(strsplit(mapInfo, ","))
    
    # define the CRS in EPGS format for the file
    epsg <- 32612
    
    # define final raster with projection info 
    bandr <- raster(band, 
                    crs=CRS(paste0("+init=epsg:", epsg)))
    
    # grab resolution of raster as an object
    res <- as.numeric(mapInfo[2])
    
    # Grab the UTM coordinates of the upper left hand corner of the raster
    #grab the left side x coordinate (xMin)
    xMin <- as.numeric(mapInfo[4]) 
    #grab the top corner coordinate (yMax)
    yMax <- as.numeric(mapInfo[5])
    
    # Calculate the lower right hand corner to define the full extent of the 
    # raster. To do this we need the number of columns and rows in the raster
    # and the resolution of the raster.
    # note that you need to multiple the columns and rows by the resolution of 
    # the data to calculate the proper extent!
    xMax <- (xMin + (ncol(band))*res)
    yMin <- (yMax - (nrow(band))*res) 
    
    # define the extent (left, right, top, bottom)
    rasExt <- extent(xMin,xMax,yMin,yMax)
    
    # assign the spatial extent to the raster
    extent(bandr) <- rasExt
    
    #add raster to raster stack
    rasterstack <- stack( rasterstack, bandr)}
  
  #band_names = c('band19', 'band35', 'band53')
  
  #names(rasterstack) <- band_names
  
  #4. write out the raster as a geotiff. Change file name to what you want.
  # writeRaster(rasterstack,
  #             file= paste(substr(f,1,nchar(f)-3), bandstring, sep='_'),
  #             format="GTiff",
  #             overwrite=TRUE)
  
  # writeRaster(rasterstack,
  #             file= paste(substr(f,1,nchar(f)-3), "bands1to426_2019", sep='_'),
  #             format="GTiff",
  #             overwrite=TRUE)
  
  writeRaster(rasterstack,
              file= paste(substr(f,1,nchar(f)-3), "bands6to397_nonoise_2017", sep='_'),
              format="GTiff",
              overwrite=TRUE)
  
  print(paste(substr(f,1,nchar(f)-3), bandstring, sep='_'))}

#It's always good practice to close the H5 connection before moving on!
#close the H5 file
H5close()

