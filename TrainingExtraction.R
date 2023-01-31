#Code written by Cynthia L. Norton, Univserity of Arizona, 2021 
#The code was used to classify NEON scenes for species specific woody vegetation using LiDAR and Hyperspectral data.
#loadlibraries
library(doParallel)
library(raster)
library(foreach)
#library(randomForest)
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
setwd("//snow/projects/RaBET/RaBET_species/NEON_FINAL/SRER/")
setwd("//snow/projects/RaBET/RaBET_species/NEON_FINAL/SRER/FinalOutputs")
NEON_2017_indices <-stack("//snow/projects/RaBET/RaBET_species/NEON_FINAL/SRER/FinalOutputs/NEON_SRER_indices_2017_mask.tif") 
NEON_2019_indices <-stack("//snow/projects/RaBET/RaBET_species/NEON_FINAL/SRER/FinalOutputs/NEON_SRER_indices_2019_mask.tif") 
NEON_2021_indices <-stack("//snow/projects/RaBET/RaBET_species/NEON_FINAL/SRER/FinalOutputs/NEON_SRER_indices_2021_mask.tif") 
NEON_2018_indices <-stack("//snow/projects/RaBET/RaBET_species/NEON_FINAL/SRER/FinalOutputs/NEON_SRER_indices_2018_mask.tif") 

NEON_indices <- stack(NEON_2021_indices,NEON_2018_indices,NEON_2017_indices)

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
gc()

#POINTS
memory.limit(size=2620330)
memory.size()
##TRAINING DATA
#trainPoints <- shapefile("P:/RaBET/Subset/training/trainPoints_071321.shp")
trainPoints <- shapefile("//snow/projects/RaBET/RaBET_species/NEON_FINAL/SRER/Training/TrainData0913_PlusAC/TrainData0913PlusAC.shp") #when creating Pointsgons, make sure to create a code column as a double.
###TRAINING DATA
trainPoints <- spTransform(trainPoints, crs(NEON_indices)) #making sure scenes overlap crs
trainPoints$Code
#make column numeric for rasterization
trainPoints$Code <- as.numeric(trainPoints$Code)
#creating a raster of classes with similar resolution as training rasters

ptm <- proc.time()

classesPoints <- rasterize(trainPoints, NEON_indices, field = "Code") 

proc.time() - ptm

names(classesPoints) <- "class"

gc()


###MASK PARALLEL
#mask training rasters with rasterized training Pointsgons
ptm <- proc.time()
# Start by defining the function that does the work 
mask_layer <- function(layer, mask) {
  masked_layer <- raster::mask(layer, mask)
  return(masked_layer)
}

# We need the RasterStack to be a list of RasterLayers
layer_list <- list()
for (i in 1:nlayers(NEON_indices)) {
  layer_list[[i]] <- NEON_indices[[i]]
}

# Set up parallel processing
num_cores = 4

# Start the cluster
cl <- makeCluster(num_cores)

# Load required libraries
clusterEvalQ(cl, {
  library(sp)
  library(raster)
})

# Run the function in parallel
masked_list <- parallel::parLapply(cl = cl,
                                   X = layer_list,
                                   fun = mask_layer,
                                   mask = classesPoints) 


gc()

#NEON_indices_mask <- raster::stack(masked_list)
#writeRaster(x = NEON_indices_mask, filename = "neon_stack.grd", overwrite=TRUE)
#neon_mask <- raster(x = "neon_stack.grd")




NEON_indices_mask <- raster::stack(masked_list)
gc()
trainingstack_indices <- addLayer(NEON_indices_mask, classesPoints) 
gc()


trainData_indices <- getValues(trainingstack_indices)
stopCluster(cl)


trainData_indices <- as.data.frame(trainData_indices) %>% na.omit()
gc()
                                              
trainData_indices$class <- as.factor(trainData_indices$class)
gc()

trainData_indices$class
#writeRaster(trainingstack_indices, filename ="trainingstack_indices_MNDWI.tif" )

write.csv(trainData_indices, file= "SRERtrainingindices_012823_with2018_no2019.csv")
# Shut the cluster off 




getwd()






















proc.time() - ptm
getwd()

rm(list = c('trainingstack_indices','NEON_2018_subset_mask','masked_list'))
gc()

##Clean dataset
trainData_indices <- read.csv("P:/RaBET/Results/trainingstack_indices_101221.csv")

val_bareground <- subset(trainData_indices, class == 1)

val_grass <- subset(trainData_indices, class == 2)%>%
  filter(CHM_2018 < 1.5)


val_mesquite <- subset(trainData_indices, class == 3)


val_cactus <- subset(trainData_indices, class == 4)%>%
  filter(CACTI_2019 > 0.105)%>%
  filter(CHM_2018 > 0)


val_lotebush <- subset(trainData_indices, class == 5)


val_paloverde <- subset(trainData_indices, class == 6)

val_creosote <- subset(trainData_indices, class == 7)%>%
  filter(CACTI_2019 < 0)%>%
  filter(CACTI_2018 < 0)%>%
  filter(CACTI_2017 < 0)%>%
  filter(CHM_2018 > 1)

trainData_indices <- rbind(val_bareground,val_grass,val_mesquite,val_cactus,val_lotebush,val_paloverde,val_creosote)
write.csv(trainData_indices, file= "trainData_10042121_clean_ac.csv")

#write.csv(val_bareground, file = "val_bareground.csv")
#write.csv(val_grass, file = "val_grass.csv")
#write.csv(val_mesquite, file = "val_mesquite.csv")
#write.csv(val_cactus, file = "val_cactus.csv")
#write.csv(val_catclaw, file = "val_catclaw.csv")
#write.csv(val_lotebush, file = "val_lotebush.csv")
#write.csv(val_paloverde, file = "val_paloverde.csv")
#write.csv(val_creosote, file = "val_creosote.csv")

#riparian vegetation
#val_cottonwood
#val_willow

#higher elevation vegetation
#val_pine
#val_cypress
#val_aspen






































###GET TRAINING DATA
ptm <- proc.time()

NEON_indices_mask <- mask(NEON_indices, classesPoints) 
trainingstack_indices <- addLayer(NEON_indices_mask, classesPoints) 
trainData_indices <- getValues(trainingstack_indices)
trainData_indices <- as.data.frame(trainData_indices)
trainData_indices <- na.omit(trainData_indices)
trainData_indices$Class <- as.factor(trainData_indices$Class)
#writeRaster(trainingstack_indices, filename ="trainingstack_indices_NDVI.tif" )

write.csv(trainData_indices, file= "SRER_training_012222.csv")


proc.time() - ptm



###GET TRAINING DATA
ptm <- proc.time()

NEON_indices_mask <- mask(NEON_2018_indices, classesPoints) 
trainingstack_indices <- addLayer(NEON_indices_mask, classesPoints) 
trainData_indices <- getValues(trainingstack_indices)
trainData_indices <- as.data.frame(trainData_indices)
trainData_indices <- na.omit(trainData_indices)
trainData_indices$class <- as.factor(trainData_indices$class)
#writeRaster(trainingstack_indices, filename ="trainingstack_indices_NDVI.tif" )

write.csv(trainData_indices, file= "SRER_2018_training_012122.csv")


proc.time() - ptm




###GET TRAINING DATA
ptm <- proc.time()

NEON_indices_mask <- mask(NEON_2019_indices, classesPoints) 
trainingstack_indices <- addLayer(NEON_indices_mask, classesPoints) 
trainData_indices <- getValues(trainingstack_indices)
trainData_indices <- as.data.frame(trainData_indices)
trainData_indices <- na.omit(trainData_indices)
trainData_indices$class <- as.factor(trainData_indices$class)
#writeRaster(trainingstack_indices, filename ="trainingstack_indices_NDVI.tif" )

write.csv(trainData_indices, file= "SRER_2019_training_012122.csv")


proc.time() - ptm


###GET TRAINING DATA
ptm <- proc.time()

NEON_indices_mask <- mask(NEON_2018_subset, classesPoints) 
trainingstack_indices <- addLayer(NEON_indices_mask, classesPoints) 
trainData_indices <- getValues(trainingstack_indices)
trainData_indices <- as.data.frame(trainData_indices)
trainData_indices <- na.omit(trainData_indices)
trainData_indices$class <- as.factor(trainData_indices$class)
#writeRaster(trainingstack_indices, filename ="trainingstack_indices_NDVI.tif" )

write.csv(trainData_indices, file= "SRER_training_012122.csv")


proc.time() - ptm



###GET TRAINING DATA
ptm <- proc.time()

NEON_indices_mask <- mask(NEON_2017_subset, classesPoints) 
trainingstack_indices <- addLayer(NEON_indices_mask, classesPoints) 
trainData_indices <- getValues(trainingstack_indices)
trainData_indices <- as.data.frame(trainData_indices)
trainData_indices <- na.omit(trainData_indices)
trainData_indices$class <- as.factor(trainData_indices$class)
#writeRaster(trainingstack_indices, filename ="trainingstack_indices_NDVI.tif" )

write.csv(trainData_indices, file= "Neon_2017_hyperspectral_subset.csv")


proc.time() - ptm




###GET TRAINING DATA
ptm <- proc.time()
NEON_indices <- stack(NDVI_2019, NDVI_2018, NDVI_2017)
names(NEON_indices) <- c('NDVI_2019','NDVI_2018','NDVI_2017')

NEON_indices_mask <- mask(NEON_indices, classesPoints) 
trainingstack_indices <- addLayer(NEON_indices_mask, classesPoints) 
trainData_indices <- getValues(trainingstack_indices)
trainData_indices <- as.data.frame(trainData_indices)
trainData_indices <- na.omit(trainData_indices)
trainData_indices$class <- as.factor(trainData_indices$class)
#writeRaster(trainingstack_indices, filename ="trainingstack_indices_NDVI.tif" )

write.csv(trainData_indices, file= "trainingstack_indices_NDVI.csv")


proc.time() - ptm


ptm <- proc.time()
NEON_indices <- stack(CACTI_2019, CACTI_2018, CACTI_2017)
names(NEON_indices) <- c('CACTI_2019','CACTI_2018','CACTI_2017')

NEON_indices_mask <- mask(NEON_indices, classesPoints) 
trainingstack_indices <- addLayer(NEON_indices_mask, classesPoints) 
trainData_indices <- getValues(trainingstack_indices)
trainData_indices <- as.data.frame(trainData_indices)
trainData_indices <- na.omit(trainData_indices)
trainData_indices$class <- as.factor(trainData_indices$class)
#writeRaster(trainingstack_indices, filename ="trainingstack_indices_CACTI.tif" )

write.csv(trainData_indices, file= "trainingstack_indices_CACTI.csv")


proc.time() - ptm



ptm <- proc.time()
NEON_indices <- stack(CACTI2_2019, CACTI2_2018, CACTI2_2017)
names(NEON_indices) <- c('CACTI2_2019','CACTI2_2018','CACTI2_2017')

NEON_indices_mask <- mask(NEON_indices, classesPoints) 
trainingstack_indices <- addLayer(NEON_indices_mask, classesPoints) 
trainData_indices <- getValues(trainingstack_indices)
trainData_indices <- as.data.frame(trainData_indices)
trainData_indices <- na.omit(trainData_indices)
trainData_indices$class <- as.factor(trainData_indices$class)
#writeRaster(trainingstack_indices, filename ="trainingstack_indices_CACTI2.tif" )

write.csv(trainData_indices, file= "trainingstack_indices_CACTI2.csv")


proc.time() - ptm



ptm <- proc.time()
NEON_indices <- stack(CAI_2019, CAI_2018, CAI_2017)
names(NEON_indices) <- c('CAI_2019','CAI_2018','CAI_2017')

NEON_indices_mask <- mask(NEON_indices, classesPoints) 
trainingstack_indices <- addLayer(NEON_indices_mask, classesPoints) 
trainData_indices <- getValues(trainingstack_indices)
trainData_indices <- as.data.frame(trainData_indices)
trainData_indices <- na.omit(trainData_indices)
trainData_indices$class <- as.factor(trainData_indices$class)
#writeRaster(trainingstack_indices, filename ="trainingstack_indices_CAI.tif" )

write.csv(trainData_indices, file= "trainingstack_indices_CAI.csv")


proc.time() - ptm

ptm <- proc.time()
NEON_indices <- stack(CELL_2019, CELL_2018, CELL_2017)
names(NEON_indices) <- c('CELL_2019','CELL_2018','CELL_2017')

NEON_indices_mask <- mask(NEON_indices, classesPoints) 
trainingstack_indices <- addLayer(NEON_indices_mask, classesPoints) 
trainData_indices <- getValues(trainingstack_indices)
trainData_indices <- as.data.frame(trainData_indices)
trainData_indices <- na.omit(trainData_indices)
trainData_indices$class <- as.factor(trainData_indices$class)
#writeRaster(trainingstack_indices, filename ="trainingstack_indices_CELL.tif" )

write.csv(trainData_indices, file= "trainingstack_indices_CELL.csv")


proc.time() - ptm



ptm <- proc.time()
NEON_indices <- stack(CELL2_2019, CELL2_2018, CELL2_2017)
names(NEON_indices) <- c('CELL2_2019','CELL2_2018','CELL2_2017')

NEON_indices_mask <- mask(NEON_indices, classesPoints) 

trainingstack_indices <- addLayer(NEON_indices_mask, classesPoints) 
trainData_indices <- getValues(trainingstack_indices)
trainData_indices <- as.data.frame(trainData_indices)
trainData_indices <- na.omit(trainData_indices)
trainData_indices$class <- as.factor(trainData_indices$class)
#writeRaster(trainingstack_indices, filename ="trainingstack_indices_CELL2.tif" )

write.csv(trainData_indices, file= "trainingstack_indices_CELL2.csv")

ptm <- proc.time()
NEON_indices <- stack(CHM_2019, CHM_2018, CHM_2017)
names(NEON_indices) <- c('CHM_2019','CHM_2018','CHM_2017')

NEON_indices_mask <- mask(NEON_indices, classesPoints) 
trainingstack_indices <- addLayer(NEON_indices_mask, classesPoints) 
trainData_indices <- getValues(trainingstack_indices)
trainData_indices <- as.data.frame(trainData_indices)
trainData_indices <- na.omit(trainData_indices)
trainData_indices$class <- as.factor(trainData_indices$class)
#writeRaster(trainingstack_indices, filename ="trainingstack_indices_CHM.tif" )

write.csv(trainData_indices, file= "trainingstack_indices_CHM.csv")


proc.time() - ptm

ptm <- proc.time()
NEON_indices <- stack(CI_2019, CI_2018, CI_2017)
names(NEON_indices) <- c('CI_2019','CI_2018','CI_2017')

NEON_indices_mask <- mask(NEON_indices, classesPoints) 
trainingstack_indices <- addLayer(NEON_indices_mask, classesPoints) 
trainData_indices <- getValues(trainingstack_indices)
trainData_indices <- as.data.frame(trainData_indices)
trainData_indices <- na.omit(trainData_indices)
trainData_indices$class <- as.factor(trainData_indices$class)
#writeRaster(trainingstack_indices, filename ="trainingstack_indices_CI.tif" )

write.csv(trainData_indices, file= "trainingstack_indices_CI.csv")


ptm <- proc.time()
NEON_indices <- stack(MTCI_2019, MTCI_2018, MTCI_2017)
names(NEON_indices) <- c('MTCI_2019','MTCI_2018','MTCI_2017')

NEON_indices_mask <- mask(NEON_indices, classesPoints) 
trainingstack_indices <- addLayer(NEON_indices_mask, classesPoints) 
trainData_indices <- getValues(trainingstack_indices)
trainData_indices <- as.data.frame(trainData_indices)
trainData_indices <- na.omit(trainData_indices)
trainData_indices$class <- as.factor(trainData_indices$class)
#writeRaster(trainingstack_indices, filename ="trainingstack_indices_MTCI.tif" )

write.csv(trainData_indices, file= "trainingstack_indices_MTCI.csv")


proc.time() - ptm



ptm <- proc.time()
NEON_indices <- stack(NDNI_2019, NDNI_2018, NDNI_2017)
names(NEON_indices) <- c('NDNI_2019','NDNI_2018','NDNI_2017')

NEON_indices_mask <- mask(NEON_indices, classesPoints) 
trainingstack_indices <- addLayer(NEON_indices_mask, classesPoints) 
trainData_indices <- getValues(trainingstack_indices)
trainData_indices <- as.data.frame(trainData_indices)
trainData_indices <- na.omit(trainData_indices)
trainData_indices$class <- as.factor(trainData_indices$class)
#writeRaster(trainingstack_indices, filename ="trainingstack_indices_NDNI.tif" )

write.csv(trainData_indices, file= "trainingstack_indices_NDNI.csv")


proc.time() - ptm


ptm <- proc.time()
NEON_indices <- stack(NDWI_2019, NDWI_2018, NDWI_2017)
names(NEON_indices) <- c('NDWI_2019','NDWI_2018','NDWI_2017')

NEON_indices_mask <- mask(NEON_indices, classesPoints) 
trainingstack_indices <- addLayer(NEON_indices_mask, classesPoints) 
trainData_indices <- getValues(trainingstack_indices)
trainData_indices <- as.data.frame(trainData_indices)
trainData_indices <- na.omit(trainData_indices)
trainData_indices$class <- as.factor(trainData_indices$class)
#writeRaster(trainingstack_indices, filename ="trainingstack_indices_NDWI.tif" )

write.csv(trainData_indices, file= "trainingstack_indices_NDWI.csv")


proc.time() - ptm


ptm <- proc.time()
NEON_indices <- stack(PRI_2019, PRI_2018, PRI_2017)
names(NEON_indices) <- c('PRI_2019','PRI_2018','PRI_2017')

NEON_indices_mask <- mask(NEON_indices, classesPoints) 
trainingstack_indices <- addLayer(NEON_indices_mask, classesPoints) 
trainData_indices <- getValues(trainingstack_indices)
trainData_indices <- as.data.frame(trainData_indices)
trainData_indices <- na.omit(trainData_indices)
trainData_indices$class <- as.factor(trainData_indices$class)
#writeRaster(trainingstack_indices, filename ="trainingstack_indices_PRI.tif" )

write.csv(trainData_indices, file= "trainingstack_indices_PRI.csv")


proc.time() - ptm

ptm <- proc.time()
NEON_indices <- stack(PRI2_2019, PRI2_2018, PRI2_2017)
names(NEON_indices) <- c('PRI2_2019','PRI2_2018','PRI2_2017')

NEON_indices_mask <- mask(NEON_indices, classesPoints) 
trainingstack_indices <- addLayer(NEON_indices_mask, classesPoints) 
trainData_indices <- getValues(trainingstack_indices)
trainData_indices <- as.data.frame(trainData_indices)
trainData_indices <- na.omit(trainData_indices)
trainData_indices$class <- as.factor(trainData_indices$class)
#writeRaster(trainingstack_indices, filename ="trainingstack_indices_PRI2.tif" )

write.csv(trainData_indices, file= "trainingstack_indices_PRI2.csv")


proc.time() - ptm

ptm <- proc.time()
NEON_indices <- stack(SAVI_2019, SAVI_2018, SAVI_2017)
names(NEON_indices) <- c('SAVI_2019','SAVI_2018','SAVI_2017')

NEON_indices_mask <- mask(NEON_indices, classesPoints) 
trainingstack_indices <- addLayer(NEON_indices_mask, classesPoints) 
trainData_indices <- getValues(trainingstack_indices)
trainData_indices <- as.data.frame(trainData_indices)
trainData_indices <- na.omit(trainData_indices)
trainData_indices$class <- as.factor(trainData_indices$class)
#writeRaster(trainingstack_indices, filename ="trainingstack_indices_SAVI.tif" )

write.csv(trainData_indices, file= "trainingstack_indices_SAVI.csv")


proc.time() - ptm


ptm <- proc.time()
NEON_indices <- stack(SWIRI_2019, SWIRI_2018, SWIRI_2017)
names(NEON_indices) <- c('SWIRI_2019','SWIRI_2018','SWIRI_2017')

NEON_indices_mask <- mask(NEON_indices, classesPoints) 
trainingstack_indices <- addLayer(NEON_indices_mask, classesPoints) 
trainData_indices <- getValues(trainingstack_indices)
trainData_indices <- as.data.frame(trainData_indices)
trainData_indices <- na.omit(trainData_indices)
trainData_indices$class <- as.factor(trainData_indices$class)
#writeRaster(trainingstack_indices, filename ="trainingstack_indices_SWIRI.tif" )

write.csv(trainData_indices, file= "trainingstack_indices_SWIRI.csv")


ptm <- proc.time()
NEON_indices <- stack(MNDWI_2019, MNDWI_2018, MNDWI_2017)
names(NEON_indices) <- c('MNDWI_2019','MNDWI_2018','MNDWI_2017')

NEON_indices_mask <- mask(NEON_indices, classesPoints) 
trainingstack_indices <- addLayer(NEON_indices_mask, classesPoints) 
trainData_indices <- getValues(trainingstack_indices)
trainData_indices <- as.data.frame(trainData_indices)
trainData_indices <- na.omit(trainData_indices)
trainData_indices$class <- as.factor(trainData_indices$class)
#writeRaster(trainingstack_indices, filename ="trainingstack_indices_MNDWI.tif" )

write.csv(trainData_indices, file= "trainingstack_indices_MNDWI.csv")


proc.time() - ptm




ptm <- proc.time()
NEON_indices <- stack(NDII_2019, NDII_2018, NDII_2017)
names(NEON_indices) <- c('NDII_2019','NDII_2018','NDII_2017')

NEON_indices_mask <- mask(NEON_indices, classesPoints) 
trainingstack_indices <- addLayer(NEON_indices_mask, classesPoints) 
trainData_indices <- getValues(trainingstack_indices)
trainData_indices <- as.data.frame(trainData_indices)
trainData_indices <- na.omit(trainData_indices)
trainData_indices$class <- as.factor(trainData_indices$class)
#writeRaster(trainingstack_indices, filename ="trainingstack_indices_NDII.tif" )

write.csv(trainData_indices, file= "trainingstack_indices_NDII.csv")


proc.time() - ptm




#area(variables)
NDVI_training <- read.csv('P:/RaBET/Results/trainingstack_indices_NDVI.csv')
NDWI_training <- read.csv('P:/RaBET/Results/trainingstack_indices_NDWI.csv')
PRI_training <- read.csv('P:/RaBET/Results/trainingstack_indices_PRI.csv')
SWIRI_training <- read.csv('P:/RaBET/Results/trainingstack_indices_SWIRI.csv')
SAVI_training <- read.csv('P:/RaBET/Results/trainingstack_indices_SAVI.csv')
PRI2_training <- read.csv('P:/RaBET/Results/trainingstack_indices_PRI2.csv')
CACTI_training <- read.csv('P:/RaBET/Results/trainingstack_indices_CACTI.csv')
CACTI2_training <- read.csv('P:/RaBET/Results/trainingstack_indices_CACTI2.csv')
MTCI_training <- read.csv('P:/RaBET/Results/trainingstack_indices_MTCI.csv')
CI_training <- read.csv('P:/RaBET/Results/trainingstack_indices_CI.csv')
CAI_training <- read.csv('P:/RaBET/Results/trainingstack_indices_CAI.csv')
CELL_training <- read.csv('P:/RaBET/Results/trainingstack_indices_CELL.csv')
CELL2_training <- read.csv('P:/RaBET/Results/trainingstack_indices_CELL2.csv')
NDNI_training <- read.csv('P:/RaBET/Results/trainingstack_indices_NDNI.csv')
CHM_training <- read.csv('P:/RaBET/Results/trainingstack_indices_CHM.csv')
MNDWI_training <- read.csv('P:/RaBET/Results/trainingstack_indices_MNDWI.csv')
NDII_training <- read.csv('P:/RaBET/Results/trainingstack_indices_NDII.csv')


trainData_101421 <- merge(NDVI_training,NDWI_training,PRI_training,SWIRI_training,SAVI_training, 
                          PRI2_training,CACTI_training,CACTI2_training,MTCI_training,CI_training,
                          CAI_training,CELL_training,CELL2_training,NDNI_training,CHM_training, MNDWI_training,NDII_training)

trainData_102021 <-list(NDVI_training,NDWI_training,PRI_training,SWIRI_training,SAVI_training, 
                        PRI2_training,CACTI_training,CACTI2_training,MTCI_training,CI_training,
                        CAI_training,CELL_training,CELL2_training,NDNI_training,CHM_training,MNDWI_training,NDII_training) %>% reduce(left_join, by = "X") %>% select(-c(5,9,13,17,21,25,29,33,37,41,45,49,53,57,61,65))

write.csv(trainData_101421, file = 'trainData_102021.csv')





#removeoutliers
library(tidyverse)

trainData_indices <- read.csv("P:/RaBET/Results/trainData_101421_newindices.csv")

val_bareground <- subset(trainData_indices, class == 1)
val_bareground_outlier.rm <- val_bareground %>%
  gather(class, value) %>%
  group_by(class) %>%
  mutate(q95 = quantile(value, 0.95), row = row_number())%>%
  #mutate(q05 = quantile(value, 0.05), row = row_number())%>%
  filter(value < q95)%>%
  #filter(value > q05)%>%
  select(c(class,value, row))%>%
  spread(class, value)%>%
  mutate(class = "1")%>%
  select(-row)


boxplot(val_bareground_outlier.rm$NDVI_2019)
boxplot(val_bareground_outlier.rm$CAI_2019)


val_grass <- subset(trainData_indices, class == 2)
val_grass_outlier.rm <- val_grass %>%
  gather(class, value) %>%
  group_by(class) %>%
  mutate(q95 = quantile(value, 0.95), row = row_number())%>%
  #mutate(q05 = quantile(value, 0.05), row = row_number())%>%
  filter(value < q95)%>%
  #filter(value > q05)%>%
  select(c(class,value, row))%>%
  spread(class, value)%>%
  mutate(class = "2")%>%
  select(-row)


boxplot(val_grass_outlier.rm$NDVI_2019)
boxplot(val_grass_outlier.rm$CAI_2019)



val_mesquite <- subset(trainData_indices, class == 3)
val_mesquite_outlier.rm <- val_mesquite %>%
  gather(class, value) %>%
  group_by(class) %>%
  mutate(q95 = quantile(value, 0.95), row = row_number())%>%
  mutate(q05 = quantile(value, 0.05), row = row_number())%>%
  filter(value < q95)%>%
  filter(value > q05)%>%
  select(c(class,value, row))%>%
  spread(class, value)%>%
  mutate(class = "3")%>%
  select(-row)


boxplot(val_mesquite_outlier.rm$NDVI_2019)
boxplot(val_mesquite_outlier.rm$CAI_2019)



val_cactus <- subset(trainData_indices, class == 4)
val_cactus_outlier.rm <- val_cactus %>%
  gather(class, value) %>%
  group_by(class) %>%
  mutate(q95 = quantile(value, 0.95), row = row_number())%>%
  mutate(q05 = quantile(value, 0.05), row = row_number())%>%
  filter(value < q95)%>%
  filter(value > q05)%>%
  select(c(class,value, row))%>%
  spread(class, value)%>%
  mutate(class = "4")%>%
  select(-row)


boxplot(val_cactus_outlier.rm$NDVI_2019)
boxplot(val_cactus_outlier.rm$CAI_2019)


val_lotebush <- subset(trainData_indices, class == 5)
val_lotebush_outlier.rm <- val_lotebush %>%
  gather(class, value) %>%
  group_by(class) %>%
  mutate(q95 = quantile(value, 0.95), row = row_number())%>%
  mutate(q05 = quantile(value, 0.05), row = row_number())%>%
  filter(value < q95)%>%
  filter(value > q05)%>%
  select(c(class,value, row))%>%
  spread(class, value)%>%
  mutate(class = "5")%>%
  select(-row)


boxplot(val_lotebush_outlier.rm$NDVI_2019)
boxplot(val_lotebush_outlier.rm$CAI_2019)


val_paloverde <- subset(trainData_indices, class == 6)
val_paloverde_outlier.rm <- val_paloverde %>%
  gather(class, value) %>%
  group_by(class) %>%
  mutate(q95 = quantile(value, 0.95), row = row_number())%>%
  mutate(q05 = quantile(value, 0.05), row = row_number())%>%
  filter(value < q95)%>%
  filter(value > q05)%>%
  select(c(class,value, row))%>%
  spread(class, value)%>%
  mutate(class = "6")%>%
  select(-row)


boxplot(val_paloverde_outlier.rm$NDVI_2019)
boxplot(val_paloverde_outlier.rm$CAI_2019)


val_creosote <- subset(trainData_indices, class == 7)
val_creosote_outlier.rm <- val_creosote %>%
  gather(class, value) %>%
  group_by(class) %>%
  mutate(q95 = quantile(value, 0.95), row = row_number())%>%
  mutate(q05 = quantile(value, 0.05), row = row_number())%>%
  filter(value < q95)%>%
  filter(value > q05)%>%
  select(c(class,value, row))%>%
  spread(class, value)%>%
  mutate(class = "7")%>%
  select(-row)


boxplot(val_creosote_outlier.rm$NDVI_2019)
boxplot(val_creosote_outlier.rm$CAI_2019)




trainData_indices <- rbind(val_bareground,val_grass,val_mesquite_outlier.rm,val_cactus,val_lotebush_outlier.rm,val_paloverde_outlier.rm,val_creosote)
write.csv(trainData_indices, file= "trainData_101421_ouliersrm_cln_ac_newindices.csv")