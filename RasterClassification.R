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
library(cubeview)
library(e1071)
#install.packages("e1071")
#library(C50)

#.libPaths()
##Import indices and Elevation Models
# set working directory
setwd("D:/projects/RaBET/NEON_JORN/")
getwd()
#Trait variables
#2019

NEON_2017_indices <-stack("P:/RaBET/Results/NEON_2017_indices_JORN_resampled.tif") 
NEON_2018_indices <-stack("D:/projects/RaBET/NEON_JORN/NEON_2018_mask_JORN.tif") 
NEON_2019_indices <-stack("D:/projects/RaBET/NEON_JORN/NEON_2019_mask_JORN.tif") 


NEON_indices <- stack(NEON_2019_indices,NEON_2017_indices)

names(NEON_indices) <- c('NDVI_2019','NDWI_2019','PRI_2019','SWIRI_2019','SAVI_2019',
                         'PRI2_2019','CACTI_2019','CACTI2_2019','MTCI_2019','CI_2019',
                         'CAI_2019','CELL_2019','CELL2_2019','NDNI_2019','CHM_2019',
                         'NDVI_2017','NDWI_2017','PRI_2017','SWIRI_2017','SAVI_2017',
                         'PRI2_2017','CACTI_2017','CACTI2_2017','MTCI_2017','CI_2017',
                         'CAI_2017','CELL_2017','CELL2_2017','NDNI_2017','CHM_2019')

#warnings()

#TRAINING
trainData_indices <- read.csv('P:/RaBET/Results/JORN_trainingindices_041122.csv')  %>% dplyr::select(-X)



#REMOVE BAD LAYER
#CHM nolidar
#CART CLASSIFICATION
ptm <- proc.time()
modelCART <- rpart(class~., data=trainData_indices, method = 'class',minsplit = 5)



#Predict
#speciesspecific_cart <- predict(NEON_indices, modelCART, type='class', na.rm=TRUE)
beginCluster(15)

speciesspecific_cart_indices <- clusterR(NEON_indices, predict, args=list(modelCART, na.rm=T,type="class"))


endCluster()



JORN_masked <- mask(speciesspecific_cart_indices, crop)

#plot(modelCART)
#text(modelCART, digits = 3)
#neonclass <- c("bareground", "grass", "mesquite","cactus","lotebush", "paloverde", "creosote")
#classdf <- data.frame(classvalue1 = c(1,2,3,4,5,6,7), classnames1 = neonclass)
### Stops timer.
proc.time() - ptm

#PLOT
#plot(speciesspecific_cart)

#Export
#writeRaster(JORN_masked, 
#            filename="speciesspecific_cart_102821_naomit_outliers.rm_5minsplit.tif",
#            format="GTiff",
#            options="COMPRESS=LZW",
#            overwrite = TRUE,
#            NAflag = -9999)

gc()
#plot(JORN_masked)







setwd("P:/RaBET/Results")








#RANDOMFOREST

### Starts timer.
ptm <- proc.time()

#RANDOM FOREST CLASSIFICATON
modelRF<-randomForest(as.factor(class)~.,data=trainData_indices,ntree=500,importance=TRUE, na.action = na.exclude)



#Predict
#speciesspecific_rf <- predict(NEON_indices, modelRF, type='class', na.rm=TRUE)
beginCluster(10)

speciesspecific_rf_JORN <- clusterR(NEON_indices, predict, args=list(modelRF, na.rm=T,type="class"))


endCluster()



#JORN_masked <- mask(speciesspecific_rf_JORN, crop)

#plot(modelRF)
#text(modelRF, digits = 3)
#neonclass <- c("bareground", "grass", "mesquite","cactus","lotebush", "paloverde", "creosote")
#classdf <- data.frame(classvalue1 = c(1,2,3,4,5,6,7), classnames1 = neonclass)

### Stops timer.
proc.time() - ptm
#PLOT
#plot(speciesspecific_RF)

#Export
writeRaster(speciesspecific_rf_JORN, 
            filename="speciesspecific_RF_041122_JORN.tif",
            format="GTiff",
            options="COMPRESS=LZW",
            overwrite = TRUE,
            NAflag = -9999)

gc()
#plot(JORN_masked)





















### Starts timer.
ptm <- proc.time()



#SVM CLASSIFICATION
modelSVM <- svm(class~., data=trainData_indices, kernel ="linear", type = 'C-classification',  cost = 0.125, gamma = 128,scale = TRUE, na.action = na.exclude)

#Predict
#speciesspecific_svm <- predict(NEON_indices, modelSVM)
beginCluster(15)

speciesspecific_svm_indices <- clusterR(NEON_indices, raster::predict, args=list(modelSVM))


endCluster()


JORN_masked <- mask(speciesspecific_svm_indices, crop)
### Stops timer.
proc.time() - ptm

neonclass <- c("bareground", "grass", "mesquite","cactus","lotebush", "paloverde", "creosote")
classdf <- data.frame(classvalue1 = c(1,2,3,4,5,6,7), classnames1 = neonclass)

#PLOT
#plot(speciesspecific_cart)

#Export
#writeRaster(JORN_masked, 
#            filename="speciesspecific_SVM_102821_naomit_indices_outliers.rm_cost0.125_gamm128.tif",
#            format="GTiff",
#            options="COMPRESS=LZW",
#            overwrite = TRUE,
 #           NAflag = -9999)

gc()
          #plot(JORN_masked)


summary(modelSVM)

