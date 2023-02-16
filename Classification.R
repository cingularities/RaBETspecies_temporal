#Code written by Cynthia L. Norton, Univserity of Arizona, 2020 
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
library(dplyr)
library(snow)
#library(C50)

#.libPaths()
##Import indices and Elevation Models
#Trait variables
#2019

#setting working directory
setwd("//snow/projects/RaBET/RaBET_species/RESULTS/WGEW/") 

#input raster stack
NEON_indices <-stack("//snow/projects/RaBET/RaBET_species/RESULTS/WGEW/WGER_indices_mask_2018.tif") 

#renaming raster layers to match training points
names(NEON_indices) <- c('NDVI_2018','NDWI_2018','PRI_2018','SWIRI_2018','SAVI_2018',
                         'PRI2_2018','CACTI_2018','CACTI2_2018','MTCI_2018','CI_2018',
                         'CAI_2018','CELL_2018','CELL2_2018','NDNI_2018','CONIER_2018','SPRUCE_2018', 'NDRE_2018','CHM_2018') #renaming raster layers to match training points

#if more than oneyear, stack all years 
NEON_indices <- stack(NEON_2021_indices,NEON_2018_indices,NEON_2017_indices)

#rename all years layers
names(NEON_indices) <- c('NDVI_2021','NDWI_2021','PRI_2021','SWIRI_2021','SAVI_2021',
                         'PRI2_2021','CACTI_2021','CACTI2_2021','MTCI_2021','CI_2021',
                         'CAI_2021','CELL_2021','CELL2_2021','NDNI_2021','CHM_2021',
                         'NDVI_2018','NDWI_2018','PRI_2018','SWIRI_2018','SAVI_2018',
                         'PRI2_2018','CACTI_2018','CACTI2_2018','MTCI_2018','CI_2018',
                         'CAI_2018','CELL_2018','CELL2_2018','NDNI_2018','CHM_2018',
                         'NDVI_2017','NDWI_2017','PRI_2017','SWIRI_2017','SAVI_2017',
                         'PRI2_2017','CACTI_2017','CACTI2_2017','MTCI_2017','CI_2017',
                         'CAI_2017','CELL_2017','CELL2_2017','NDNI_2017','CHM_2017')


#TRAINING
#read in traindata CSV removing ID column
trainData_indices <- read.csv("//snow/projects/RaBET/RaBET_species/RESULTS/WGEW/WGEW_trainingindices_02152023_2018.csv") %>% select(-c(X)) #read in traindata CSV removing ID column



#RANDOMFOREST
### Starts timer.
ptm <- proc.time()

#RANDOM FOREST CLASSIFICATON
#creating the model
modelRF<-randomForest(as.factor(class)~.,data=trainData_indices,ntree=500,importance=TRUE, na.action = na.exclude) 


#Predict
#starting a cluster for faster processing
beginCluster(10)

#starting a cluster and applying the classification model to 
speciesspecific_rf_SRER <- clusterR(NEON_indices, predict, args=list(modelRF, na.rm=T,type="class")) 

#end cluster
endCluster()


### Stops timer.
proc.time() - ptm

#Export raster
writeRaster(speciesspecific_rf_SRER, 
            filename="speciesspecific_RF_021623_WGEW_2018.tif",
            format="GTiff",
            options="COMPRESS=LZW",
            overwrite = TRUE,
            NAflag = -9999)
#plot(speciesspecific_rf_SRERÛ)



































#CART CLASSIFICATION
ptm <- proc.time()
modelCART <- rpart(class~., data=trainData_indices, method = 'class',minsplit = 5)



#Predict
#speciesspecific_cart <- predict(NEON_indices, modelCART, type='class', na.rm=TRUE)
beginCluster(15)

speciesspecific_cart_indices <- clusterR(NEON_indices, predict, args=list(modelCART, na.rm=T,type="class"))


endCluster()



SRER_masked <- mask(speciesspecific_cart_indices, crop)

#plot(modelCART)
#text(modelCART, digits = 3)
#neonclass <- c("bareground", "grass", "mesquite","cactus","lotebush", "paloverde", "creosote")
#classdf <- data.frame(classvalue1 = c(1,2,3,4,5,6,7), classnames1 = neonclass)
### Stops timer.
proc.time() - ptm

#PLOT
#plot(speciesspecific_cart)

#Export
#writeRaster(SRER_masked, 
#            filename="speciesspecific_cart_102821_naomit_outliers.rm_5minsplit.tif",
#            format="GTiff",
#            options="COMPRESS=LZW",
#            overwrite = TRUE,
#            NAflag = -9999)

gc()
#plot(SRER_masked)







setwd("P:/RaBET/Results")
























### Starts timer.
ptm <- proc.time()



#SVM CLASSIFICATION
modelSVM <- svm(class~., data=trainData_indices, kernel ="linear", type = 'C-classification',  cost = 0.125, gamma = 128,scale = TRUE, na.action = na.exclude)

#Predict
#speciesspecific_svm <- predict(NEON_indices, modelSVM)
beginCluster(15)

speciesspecific_svm_indices <- clusterR(NEON_indices, raster::predict, args=list(modelSVM))


endCluster()


SRER_masked <- mask(speciesspecific_svm_indices, crop)
### Stops timer.
proc.time() - ptm

neonclass <- c("bareground", "grass", "mesquite","cactus","lotebush", "paloverde", "creosote")
classdf <- data.frame(classvalue1 = c(1,2,3,4,5,6,7), classnames1 = neonclass)

#PLOT
#plot(speciesspecific_cart)

#Export
#writeRaster(SRER_masked, 
#            filename="speciesspecific_SVM_102821_naomit_indices_outliers.rm_cost0.125_gamm128.tif",
#            format="GTiff",
#            options="COMPRESS=LZW",
#            overwrite = TRUE,
 #           NAflag = -9999)

gc()
          #plot(SRER_masked)


summary(modelSVM)

