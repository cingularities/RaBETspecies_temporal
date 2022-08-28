#Code written by Cynthia L. Norton, Univserity of Arizona, 2021 
#The code was used to classify NEON scenes for species specific woody vegetation using LiDAR and Hyperspectral data.
# load libraries
library(raster)
library(rhdf5)
library(rgdal)
library(neonAOP)
require(sp)
require(randomForest)
library(caret)
library(terra)
library(dismo)
library(rpart)
library(tidyverse)

##Import indices and Elevation Models
# set working directory
setwd("C:P:/RaBET/Results")
getwd()
#Trait variables
#2019
##Indices 
ndvi_2019 <- raster("P:/RaBET/Subset/2019/2019oneNDVI.tif")
crs(ndvi_2019)
extent(ndvi_2019)
ndwi_2019 <- raster("P:/RaBET/Subset/2019/2019oneNDWI.tif")
pri_2019 <- raster("P:/RaBET/Subset/2019/2019onePRI.tif")
swiri_2019 <- raster("P:/RaBET/Subset/2019/2019oneSWIRI.tif")
savi_2019 <- raster("P:/RaBET/Subset/2019/2019oneSAVI.tif")
pri2_2019 <- raster("P:/RaBET/Subset/2019/2019onePRI2.tif")
cacti_2019 <- raster("P:/RaBET/Subset/2019/2019oneCACTI.tif")
cacti2_2019 <- raster("P:/RaBET/Subset/2019/2019oneCACTI2.tif")
mtci_2019 <- raster("P:/RaBET/Subset/2019/2019oneMTCI.tif")
ci_2019 <- raster("P:/RaBET/Subset/2019/2019oneCI.tif")
cell_2019 <- raster("P:/RaBET/Subset/2019/2019oneCELL.tif")
cell2_2019 <- raster("P:/RaBET/Subset/2019/2019oneCELL2.tif")
ndni_2019 <- raster("P:/RaBET/Subset/2019/2019oneNDNI.tif")
cai_2019 <- raster("P:/RaBET/Subset/2019/2019oneCAI.tif")

#Hyperspectral
#NEON_2019 <- raster("P:/RaBET/Subset/2019/NEON_D14_SRER_DP3_515000_3519000_reflectance_bands1to426_nonoise_2019.tif")

# import digital surface model (dsm) 
#dsm_2019 <- raster("P:/RaBET/Subset/2019/")

# import  digital terrain model (dtm), elevation
#dtm_2019 <- raster("P:/RaBET/Subset/2019/KyleHartfield_DiscreteLidar/DTMGtif/NEON_D14_SRER_DP3_515000_3519000_DTM.tif") 

# import canopy height model (height of vegetation) 
chm_2019 <- raster("P:/RaBET/Subset/2019/CHMone2019.tif")
crs(chm_2019) <- "+proj=utm +zone=12 +datum=WGS84 +units=m +no_defs"
chm_2019 <- setExtent(chm_2019, ndvi_2019)
chm_2019 <- resample(chm_2019,ndvi_2019)


#import aspect 
#aspect_2019 <- raster("P:/RaBET/Subset/2019/KyleHartfield_DiscreteLidar/AspectGtif/NEON_D14_SRER_DP3_515000_3519000_aspect.tif")

#2018
##Indices 
ndvi_2018 <- raster("P:/RaBET/Subset/2018/2018oneNDVI.tif")
ndwi_2018 <- raster("P:/RaBET/Subset/2018/2018oneNDWI.tif")
pri_2018 <- raster("P:/RaBET/Subset/2018/2018onePRI.tif")
swiri_2018 <- raster("P:/RaBET/Subset/2018/2018oneSWIRI.tif")
savi_2018 <- raster("P:/RaBET/Subset/2018/2018oneSAVI.tif")
pri2_2018 <- raster("P:/RaBET/Subset/2018/2018onePRI2.tif")
cacti_2018 <- raster("P:/RaBET/Subset/2018/2018oneCACTI.tif")
cacti2_2018 <- raster("P:/RaBET/Subset/2018/2018oneCACTI2.tif")
mtci_2018 <- raster("P:/RaBET/Subset/2018/2018oneMTCI.tif")
ci_2018 <- raster("P:/RaBET/Subset/2018/2018oneCI.tif")
cell_2018 <- raster("P:/RaBET/Subset/2018/2018oneCELL.tif")
cell2_2018 <- raster("P:/RaBET/Subset/2018/2018oneCELL2.tif")
ndni_2018 <- raster("P:/RaBET/Subset/2018/2018oneNDNI.tif")
cai_2018 <- raster("P:/RaBET/Subset/2018/2018oneCAI.tif")

#Hyperspectral
#NEON_2018 <- raster("P:/RaBET/Subset/2018/NEON_D14_SRER_DP3_515000_3519000_reflectance_bands1to426_nonoise_2018.tif")


# import digital surface model (dsm) 
#dsm_2018 <- raster("P:/RaBET/Subset/2018/")

# import  digital terrain model (dtm), elevation
#dtm_2018 <- raster("P:/RaBET/Subset/2018/KyleHartfield_DiscreteLidar/DTMGtif/NEON_D14_SRER_DP3_515000_3519000_DTM.tif") 

# import canopy height model (height of vegetation) 
chm_2018 <- raster("P:/RaBET/Subset/2018/CHMone2018.tif")
crs(chm_2018) <- "+proj=utm +zone=12 +datum=WGS84 +units=m +no_defs"
chm_2018 <- setExtent(chm_2018, ndvi_2018)
chm_2018 <- resample(chm_2018,ndvi_2018)


#import aspect 
#aspect_2018 <- raster("P:/RaBET/Subset/2018/KyleHartfield_DiscreteLidar/AspectGtif/NEON_D14_SRER_DP3_515000_3519000_aspect.tif")

#2017
##Indices 
ndvi_2017 <- raster("P:/RaBET/Subset/2017/2017oneNDVI.tif")
ndwi_2017 <- raster("P:/RaBET/Subset/2017/2017oneNDWI.tif")
pri_2017 <- raster("P:/RaBET/Subset/2017/2017onePRI.tif")
swiri_2017 <- raster("P:/RaBET/Subset/2017/2017oneSWIRI.tif")
savi_2017 <- raster("P:/RaBET/Subset/2017/2017oneSAVI.tif")
pri2_2017 <- raster("P:/RaBET/Subset/2017/2017onePRI2.tif")
cacti_2017 <- raster("P:/RaBET/Subset/2017/2017oneCACTI.tif")
cacti2_2017 <- raster("P:/RaBET/Subset/2017/2017oneCACTI2.tif")
mtci_2017 <- raster("P:/RaBET/Subset/2017/2017oneMTCI.tif")
ci_2017 <- raster("P:/RaBET/Subset/2017/2017oneCI.tif")
cell_2017 <- raster("P:/RaBET/Subset/2017/2017oneCELL.tif")
cell2_2017 <- raster("P:/RaBET/Subset/2017/2017oneCELL2.tif")
ndni_2017 <- raster("P:/RaBET/Subset/2017/2017oneNDNI.tif")
cai_2017 <- raster("P:/RaBET/Subset/2017/2017oneCAI.tif")

#Hyperspectral
#NEON_2017 <- raster("P:/RaBET/Subset/2017/NEON_D14_SRER_DP3_515000_3519000_reflectance_bands1to426_nonoise_2017.tif")

# import digital surface model (dsm) 
#dsm_2017 <- raster("P:/RaBET/Subset/2017/")

# import  digital terrain model (dtm), elevation
#dtm_2017 <- raster("P:/RaBET/Subset/2017/KyleHartfield_DiscreteLidar/DTMGtif/NEON_D14_SRER_DP3_515000_3519000_DTM.tif") 

# import canopy height model (height of vegetation) 
chm_2017 <- raster("P:/RaBET/Subset/2017/CHMone2017.tif")
crs(chm_2017) <- "+proj=utm +zone=12 +datum=WGS84 +units=m +no_defs"
chm_2017 <- setExtent(chm_2017, ndvi_2017)
chm_2017 <- resample(chm_2017,ndvi_2017)
chm_2017
ndvi_2017
#import aspect 
#aspect_2017 <- raster("P:/RaBET/Subset/2017/KyleHartfield_DiscreteLidar/AspectGtif/NEON_D14_SRER_DP3_515000_3519000_aspect.tif")


#stack data
#NEON_lidar_brick <- brick(chm_2017,chm_2018,chm_2019)
#NEON_lidar_brick <-spTransform(NEON_lidar_brick, crs(cai_2017)) #making sure scenes overlap crs, crs(NEON.alldata)) #making sure scenes overlap crs
NEON_2017 <-  brick(chm_2017, ndvi_2017, ndwi_2017, pri_2017, swiri_2017,savi_2017, pri2_2017,cacti_2017,cacti2_2017,mtci_2017, ci_2017, cai_2017, cell_2017, cell2_2017,ndni_2017)
NEON_2018 <-  brick(chm_2018, ndvi_2018, ndwi_2018, pri_2018, swiri_2018,savi_2018, pri2_2018,cacti_2018,cacti2_2018,mtci_2018, ci_2018, cai_2018, cell_2018, cell2_2018,ndni_2018)
NEON_2019 <-  brick(chm_2019, ndvi_2019, ndwi_2019, pri_2019, swiri_2019,savi_2019, pri2_2019,cacti_2019,cacti2_2019,mtci_2019, ci_2019, cai_2019, cell_2019, cell2_2019,ndni_2019)


NEON.alldata <- stack(NEON_2017, NEON_2018, NEON_2019)

#plot(NEON.alldata)

# Check out the height distribution - do the values seem reasonable?
#plot(chm,
#     main="Canopy Height NEON 515000_3519000 scene") 

#hist(chm,
#     main="Canopy Height NEON 515000_3519000 scene",
#     xlab="Tree Height (m)", 
#     col="springgreen")

## Warning in .hist1(x, maxpixels = maxpixels, main = main, plot = plot, ...):
## 32% of the raster cells were used. 100000 values used.


# view chm mean and max
#cellStats(chm, max)

## [1] 8.86

#cellStats(chm, mean)

## [1] 0.3415337


###TRAINING DATA
trainPoly <- shapefile("P:/RaBET/Subset/onetrainingpoints.shp") #when creating polygons, make sure to create a code column as a double.
trainPoly <- spTransform(trainPoly, crs(NEON.alldata)) #making sure scenes overlap crs
trainPoly$Code
#make column numeric for rasterization
trainPoly$Code <- as.numeric(trainPoly$Code) 
#creating a raster of classes with similar resolution as training rasters
classes <- rasterize(trainPoly,NEON.alldata, field = "Code") 
plot(classes)
#mask training rasters with rasterized training polygons
NEON.alldatamasked <- mask(NEON.alldata, classes) 
names(classes) <- "class"
#add classes layer to brick
trainingstack <- addLayer(NEON.alldatamasked, classes) 
plot(trainingstack)

trainData <- getValues(trainingstack)
trainData <- na.omit(trainData)
trainData <- as.data.frame(trainData)
head(trainData, n = 10)
tail(trainData, n = 10)
trainData

#desert vegetation
val_bareground <- subset(trainData, class == 1)
val_grass <- subset(trainData, class == 2)
val_mesquite <- subset(trainData, class == 3)
val_cactus <- subset(trainData, class == 4)
val_catclaw <- subset(trainData, class == 5)
val_graythorn <- subset(trainData, class == 6)

#val_cactus <- subset(trainData, class == 7)
#val_catclaw <- subset(trainData, class == 8)
#val_graythorn <- subset(trainData, class == 9)


#riparian vegetation
#val_cottonwood
#val_willow

#higher elevation vegetation
#val_pine
#val_cypress
#val_aspen
#val_juniper

trainData$class
## NDVI
#par(mfrow = c(4, 1))
#hist(val_pricklypear$ndvi_rast_neon_19, main = "pricklypear", xlab = "NDVI", xlim = c(0, 1), ylim = c(0, 30), col = "orange")

#par(mfrow = c(1, 1))
## 3. Bands 3 and 4 (scatterplots)
#plot(band4 ~ band3, data = valcrop, pch = ".", col = "orange", xlim = c(0, 0.2), ylim = c(0, 0.5))
trainData$class <- as.character(trainData$class)
trainData$class <- as.factor(trainData$class)

trainData



#RANDOM FOREST CLASSIFICATON
modelRF <- randomForest(x=trainData[ ,c(1:6)], y=trainData$class, importance = TRUE)
modelRF
class(modelRF)
str(modelRF)
names(modelRF)
speciesspecific_rf <- predict(NEON.alldata, model=modelRF, na.rm=TRUE)

## Plot the results
# recall: 1 = pricklypear, 2 = grass, 3 = bareground, 4 = mesquite, 5 = cholla
cols <- c("orange", "dark green", "light blue","red","black","yellow")
plot(speciesspecific_rf, col=cols, legend=FALSE)
legend("bottomright",
       legend=c("bareground", "grass", "mesquite","cactus","catclaw","graythorn"),
       fill=cols, bg="white")

writeRaster(speciesspecific_rf, 
            filename="speciesspecific03112021.tif",
            format="GTiff",
            options="COMPRESS=LZW",
            overwrite = TRUE,
            NAflag = -9999)

#RF MODEL EVALUATION
## Inspect the confusion matrix of the OOB error assessment
modelRF$confusion
# to make the confusion matrix more readable
colnames(modelRF$confusion) <- c("bareground", "grass", "mesquite","cactus","catclaw","graythorn", "class.error")
rownames(modelRF$confusion) <- c("bareground", "grass", "mesquite","cactus","catclaw","graythorn")
matrix <- modelRF$confusion
varImp(modelRF)
varImpPlot(modelRF)
matrix

accmat <- as.table(matrix)
accmat
accmat.ext <- addmargins(accmat)
accmat.ext <- rbind(accmat.ext, "Users" = c(PA, NA))
accmat.ext <- cbind(accmat.ext, "Producers" = c(UA, NA, OA))
colnames(accmat.ext) <- c(levels(as.factor(trainData$class)), "Sum", "PA")
rownames(accmat.ext) <- c(levels(as.factor(trainData$class)), "Sum", "UA")
accmat.ext <- round(accmat.ext, digits = 1)
dimnames(accmat.ext) <- list("Prediction" = colnames(accmat.ext),
                             "Reference" = rownames(accmat.ext))
class(accmat.ext) <- "table"
accmat.ext



#CART CLASSIFICATION
modelCART <- rpart(class~., data=trainData, method = 'class',minsplit = 5)
speciesspecific_cart <- predict(NEON.alldata, modelCART, type='class', na.rm=TRUE)
plot(modelCART)
text(modelCART, digits = 3)
neonclass <- c("bareground", "grass", "mesquite","cactus","catclaw","graythorn")
classdf <- data.frame(classvalue1 = c(1,2,3,4,5,6), classnames1 = neonclass)
varImp(modelCART)

#PLOT
plot(speciesspecific_cart)

#Export
writeRaster(speciesspecific_cart, 
            filename="speciesspecific03112021cart.tif",
            format="GTiff",
            options="COMPRESS=LZW",
            overwrite = TRUE,
            NAflag = -9999)



#CART MODEL EVALUATION
set.seed(99)
j <- kfold(trainData, k = 6, by=trainData$class)
table(j)
## j
##   1   2   3   4   5
## 320 320 320 320 320

x <- list()
for (k in 1:6) {
  train <- trainData[j!= k, ]
  test <- trainData[j == k, ]
  cart <- rpart(as.factor(class)~., data=trainData, method = 'class', minsplit = 5)
  pclass <- predict(cart, test, type='class')
  # create a data.frame using the reference and prediction
  x[[k]] <- cbind(test$class, as.integer(pclass))
}

y <- do.call(rbind, x)
y <- data.frame(y)
colnames(y) <- c('observed', 'predicted')
conmat <- table(y)
# change the name of the classes
colnames(conmat) <- classdf$classnames
rownames(conmat) <- classdf$classnames
conmat

accmat <- as.table(conmat)
accmat
accmat.ext <- addmargins(accmat)
accmat.ext <- rbind(accmat.ext, "Users" = c(PA, NA))
accmat.ext <- cbind(accmat.ext, "Producers" = c(UA, NA, OA))
colnames(accmat.ext) <- c(levels(as.factor(trainData$class)), "Sum", "PA")
rownames(accmat.ext) <- c(levels(as.factor(trainData$class)), "Sum", "UA")
accmat.ext <- round(accmat.ext, digits = 1)
dimnames(accmat.ext) <- list("Prediction" = colnames(accmat.ext),
                             "Reference" = rownames(accmat.ext))
class(accmat.ext) <- "table"
accmat.ext


# number of cases
n <- sum(conmat)
n
## [1] 1600
# number of correctly classified cases per class
diag <- diag(conmat)
# Overall Accuracy
OA <- sum(diag) / n
OA
## [1] 0.4975

# observed (true) cases per class
rowsums <- apply(conmat, 1, sum)
p <- rowsums / n
# predicted cases per class
colsums <- apply(conmat, 2, sum)
q <- colsums / n
expAccuracy <- sum(p*q)
kappa <- (OA - expAccuracy) / (1 - expAccuracy)
kappa
## [1] 0.4257143

# Producer accuracy
PA <- diag / colsums
# User accuracy
UA <- diag / rowsums
outAcc <- data.frame(producerAccuracy = PA, userAccuracy = UA)
outAcc


####ANOTHER APPROACH
#import training polygons
trainData <- shapefile("C:/data/landsat/shps/UTM18N_32618/training_15.shp")
responseCol <- "class"

#Extract training pixel values
dfAll = data.frame(matrix(vector(), nrow = 0, ncol = length(names(NEON.alldata)) + 1))   
for (i in 1:length(unique(trainData[[responseCol]]))){
  category <- unique(trainData[[responseCol]])[i]
  categorymap <- trainData[trainData[[responseCol]] == category,]
  dataSet <- extract(NEON.alldata, categorymap)
  if(is(trainData, "SpatialPointsDataFrame")){
    dataSet <- cbind(dataSet, class = as.numeric(rep(category, nrow(dataSet))))
    dfAll <- rbind(dfAll, dataSet[complete.cases(dataSet),])
  }
  if(is(trainData, "SpatialPolygonsDataFrame")){
    dataSet <- dataSet[!unlist(lapply(dataSet, is.null))]
    dataSet <- lapply(dataSet, function(x){cbind(x, class = as.numeric(rep(category, nrow(x))))})
    df <- do.call("rbind", dataSet)
    dfAll <- rbind(dfAll, df)
  }
}

nsamples <- 1000
sdfAll <- dfAll[sample(1:nrow(dfAll), nsamples), ]


#Model fit and image classification
modFit_rf <- train(as.factor(class) ~ B3 + B4 + B5, method = "rf", data = sdfAll)
beginCluster()
preds_rf <- clusterR(NEON.alldata, raster::predict, args = list(model = modFit_rf))
endCluster()
