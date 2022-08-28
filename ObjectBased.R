#CLNorton Object Based

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}


packages <- c("rgdal","raster","sp","RStoolbox","RColorBrewer","remotes","sf","stars")
ipak(packages)

install_github("joaofgoncalves/SegOptim")
library(SegOptim)


# Path to raster data used for image segmentation
# In SEGM_FEAT directory
inputSegFeat.path <- "test/data/SEGM_FEAT/SegmFeat_WV2_b532.tif"

# Path to training raster data
# [0] Non-invaded areas [1] Acacia dealbata invaded areas
# In TRAIN_AREAS directory
trainData.path <- "test/data/TRAIN_AREAS/TrainAreas.tif"

# Path to raster data used as classification features
# In CLASSIF_FEAT directory
classificationFeatures.path <- c("test/data/CLASSIF_FEAT/ClassifFeat_WV2_NDIs.tif",
                                 "test/data/CLASSIF_FEAT/ClassifFeat_WV2_SpectralBands.tif")

# Path to Orfeo Toolbox binaries 
otbPath <- "D:\\SW\\OTB-7.2.0-Win64\\bin"

## Output file from OTB image segmentation     
outSegmRst.path <- "test/results3/segmRaster.tif"

# The final output file containing the distribution of the target species
outClassRst.path <- "test/results3/WV2_VilarVeiga_AcaciaDealbata_v1.tif"


# Check if the files and folders exist! 
if(!file.exists(inputSegFeat.path))
  print("Could not find data for image segmentation!")

if(!file.exists(trainData.path))
  print("Could not find training data!")

if(!(file.exists(classificationFeatures.path[1]) | file.exists(classificationFeatures.path[2])))
  print("Could not find data for classification!")

if(!dir.exists(otbPath))
  print("Could not find Orfeo Toolbox binaries!")


## Run the segmentation
outSegmRst <- segmentation_OTB_LSMS(
  # Input raster with features/bands to segment
  inputRstPath = inputSegFeat.path, 
  # Algorithm params
  SpectralRange = 3.1, 
  SpatialRange = 4.5, 
  MinSize = 21,
  # Output
  outputSegmRst = outSegmRst.path,
  verbose = TRUE,
  otbBinPath = otbPath,
  lsms_maxiter = 50)


# Check the file paths with outputs
print(outSegmRst)


# Load the segmented raster and plot it
segmRst <- raster(outSegmRst$segm)

x11()
plot(segmRst)


# Train data
trainDataRst <- raster(trainData.path)


# Classification features
classificationFeatures <- stack(classificationFeatures.path)
# Change the names for each layer
names(classificationFeatures) <- c(paste("NDI_",1:28,sep=""),paste("SpecBand_",1:8,sep=""))


calData <- prepareCalData(rstSegm = segmRst,
                          trainData = trainDataRst,
                          rstFeatures = classificationFeatures,
                          thresh = 0.5,
                          funs = "mean",
                          minImgSegm = 30,
                          verbose = TRUE)

# Calibrate/evaluate the RF classifier
classifObj <- calibrateClassifier( calData                    = calData,
                                   classificationMethod       = "RF",
                                   balanceTrainData           = FALSE,
                                   balanceMethod              = "ubOver",
                                   evalMethod                 = "10FCV",
                                   evalMetric                 = "Kappa",
                                   minTrainCases              = 30,
                                   minCasesByClassTrain       = 10,
                                   minCasesByClassTest        = 5,
                                   runFullCalibration         = TRUE)



# Get more evaluation measures
evalMatrix <- evalPerformanceClassifier(classifObj)

print(round(evalMatrix,2))

# Finally, predict the class label for the entire image (i.e., outside the training set)
# and also save the classified image
rstPredSegmRF <- predictSegments(classifierObj = classifObj,
                                 calData = calData,
                                 rstSegm = segmRst,
                                 predictFor = "all",
                                 filename = outClassRst.path)
print(rstPredSegmRF)

# Variable importance
varImpPlot(classifObj$ClassObj$FULL)

