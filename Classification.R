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
#install.packages("e1071")
#library(C50)

#.libPaths()
##Import indices and Elevation Models
# set working directory
#Trait variables
#2019

setwd("//snow/projects/RaBET/RaBET_species/NEON_FINAL/SRER/FinalOutputs")
NEON_2020_indices <-stack("//snow/projects/RaBET/RaBET_species/NEON_FINAL/SRER/FinalOutputs/NEON_SRER_indices_2020_resample.tif") 
NEON_2019_indices <-stack("//snow/projects/RaBET/RaBET_species/NEON_FINAL/SRER/FinalOutputs/NEON_SRER_indices_2019_resample.tif") 
NEON_2018_indices <-stack("//snow/projects/RaBET/RaBET_species/NEON_FINAL/SRER/FinalOutputs/NEON_SRER_indices_2018_mask.tif") 
NEON_2017_indices <-stack("//snow/projects/RaBET/RaBET_species/NEON_FINAL/SRER/FinalOutputs/NEON_SRER_indices_2017_mask.tif") 
NEON_2019_indices <-stack("//snow/projects/RaBET/RaBET_species/NEON_FINAL/SRER/FinalOutputs/NEON_SRER_indices_2019_mask.tif") 
NEON_indices <-stack("//snow/projects/RaBET/RaBET_species/NEON_FINAL/SRER/FinalOutputs/NEON_SRER_indices_2021_mask.tif") 

NEON_indices <- stack(NEON_2021_indices,NEON_2018_indices,NEON_2017_indices)
names(NEON_indices) <- c('NDVI_2021','NDWI_2021','PRI_2021','SWIRI_2021','SAVI_2021',
                         'PRI2_2021','CACTI_2021','CACTI2_2021','MTCI_2021','CI_2021',
                         'CAI_2021','CELL_2021','CELL2_2021','NDNI_2021','CHM_2021')

names(NEON_indices) <- c('NDVI_2021','NDWI_2021','PRI_2021','SWIRI_2021','SAVI_2021',
                         'PRI2_2021','CACTI_2021','CACTI2_2021','MTCI_2021','CI_2021',
                         'CAI_2021','CELL_2021','CELL2_2021','NDNI_2021','CHM_2021',
                         'NDVI_2018','NDWI_2018','PRI_2018','SWIRI_2018','SAVI_2018',
                         'PRI2_2018','CACTI_2018','CACTI2_2018','MTCI_2018','CI_2018',
                         'CAI_2018','CELL_2018','CELL2_2018','NDNI_2018','CHM_2018',
                         'NDVI_2017','NDWI_2017','PRI_2017','SWIRI_2017','SAVI_2017',
                         'PRI2_2017','CACTI_2017','CACTI2_2017','MTCI_2017','CI_2017',
                         'CAI_2017','CELL_2017','CELL2_2017','NDNI_2017','CHM_2017')
gc()

#warnings()

#TRAINING
trainData_2020 <- read.csv("//snow/projects/RaBET/RaBET_species/NEON_FINAL/SRER/FinalOutputs/SRER_2020_trainingindices_112222.csv") %>% na.omit()
trainData_2019 <- read.csv("//snow/projects/RaBET/RaBET_species/NEON_FINAL/SRER/FinalOutputs/SRER_2019_trainingindices_112122.csv") %>% na.omit()
trainData_2017 <- read.csv("//snow/projects/RaBET/RaBET_species/NEON_FINAL/SRER/FinalOutputs/SRER_2017_trainingindices_112222.csv") %>% na.omit()
trainData_indices <- trainData_2020 %>% left_join(trainData_2019) %>% select(-c(X))
#write.csv(trainData_indices, file = "SRER_trainingdata_112822_no2017.csv")

trainData_indices <- read.csv("//snow/projects/RaBET/RaBET_species/NEON_FINAL/final_results/Training/SRERtrainingindices_012823_with2018_no2019.csv") %>% select(-c(X))

#  dplyr::select(c(NDVI_2021,NDWI_2021,PRI_2021,SWIRI_2021,SAVI_2021,PRI2_2021,CACTI_2021,CACTI2_2021,MTCI_2021,CI_2021,CAI_2021,NDNI_2021,CHM_2021, class))

gc()


#RANDOMFOREST

### Starts timer.
ptm <- proc.time()

#RANDOM FOREST CLASSIFICATON
modelRF<-randomForest(as.factor(class)~.,data=trainData_indices,ntree=500,importance=TRUE, na.action = na.exclude)



#Predict
#speciesspecific_rf <- predict(NEON_indices, modelRF, type='class', na.rm=TRUE)
beginCluster(10)

speciesspecific_rf_SRER <- clusterR(NEON_indices, predict, args=list(modelRF, na.rm=T,type="class"))


endCluster()



#SRER_masked <- mask(speciesspecific_rf_SRER, crop)

#plot(modelRF)
#text(modelRF, digits = 3)
#neonclass <- c("bareground", "grass", "mesquite","cactus","lotebush", "paloverde", "creosote")
#classdf <- data.frame(classvalue1 = c(1,2,3,4,5,6,7), classnames1 = neonclass)

### Stops timer.
proc.time() - ptm
#PLOT
#plot(speciesspecific_RF)

#Export
writeRaster(speciesspecific_rf_SRER, 
            filename="speciesspecific_RF_012823_SRER_no2019.tif",
            format="GTiff",
            options="COMPRESS=LZW",
            overwrite = TRUE,
            NAflag = -9999)

gc()
#plot(SRER_masked)
















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

