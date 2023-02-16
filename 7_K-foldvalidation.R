library(tidyverse)
library(e1071)
library(randomForest)
library(caret)
library(rpart)
library(dismo)
library(dplyr)
library(rlang)
install.packages('tidyverse')
install.packages('rlang')

getwd()
gc()
NEON_2017_indices <-read.csv("//snow/projects/RaBET/RaBET_species/NEON_FINAL/NIWOT/FinalOutputs/NIWOT_2017_trainingindices_112222.csv") %>% na.omit()
NEON_2020_indices <-read.csv("//snow/projects/RaBET/RaBET_species/NEON_FINAL/NIWOT/FinalOutputs/NIWOT_2020_trainingindices_112222.csv") %>% na.omit()
NEON_2019_indices <-read.csv("//snow/projects/RaBET/RaBET_species/NEON_FINAL/NIWOT/FinalOutputs/NIWOT_2019_trainingindices_112122.csv") %>% na.omit()
#dataset
trainData_indices <- read.csv("//gaea/projects/RaBET/RaBET_species/Outputs/final_results/JORN_trainingdata_030122.csv") %>% dplyr::select(-X)
trainData_indices <- read.csv("//gaea/projects/RaBET/RaBET_species/Outputs/final_results/CLBJ_trainingdata_122021.csv") %>% dplyr::select(-X)
trainData_indices <- read.csv("//gaea/projects/RaBET/RaBET_species/Outputs/final_results/ONAQ_trainingdata_071922.csv") %>% dplyr::select(-X)
trainData_indices <- read.csv("//snow/projects/RaBET/RaBET_species/NEON_FINAL/final_results/Training/SRERtrainingindices_012723.csv") %>% select(-c(X)) 
trainData_indices <- read.csv("//snow/projects/RaBET/RaBET_species/NEON_FINAL/final_results/Training/SRERtrainingindices_012823_with2018_no2019.csv") %>% select(-c(X)) 

names(trainData_indices) <- c('NDVI_2021','NDWI_2021','PRI_2021','SWIRI_2021','SAVI_2021',
                              'PRI2_2021','CACTI_2021','CACTI2_2021','MTCI_2021','CI_2021',
                              'CAI_2021','CELL_2021','CELL2_2021','NDNI_2021','CHM_2021',
                              'NDVI_2018','NDWI_2018','PRI_2018','SWIRI_2018','SAVI_2018',
                              'PRI2_2018','CACTI_2018','CACTI2_2018','MTCI_2018','CI_2018',
                              'CAI_2018','CELL_2018','CELL2_2018','NDNI_2018','CHM_2018', 
                              'NDVI_2017','NDWI_2017','PRI_2017','SWIRI_2017','SAVI_2017',
                              'PRI2_2017','CACTI_2017','CACTI2_2017','MTCI_2017','CI_2017',
                              'CAI_2017','CELL_2017','CELL2_2017','NDNI_2017','CHM_2017', 'class')

names(trainData_indices) <- c('NDVI_2021','NDWI_2021','PRI_2021','SWIRI_2021','SAVI_2021',
                              'PRI2_2021','CACTI_2021','CACTI2_2021','MTCI_2021','CI_2021',
                              'CAI_2021','CELL_2021','CELL2_2021','NDNI_2021', 'CONIFER_2021','SPRUCE_2021','NDRE_2021','CHM_2021',
                              'NDVI_2019','NDWI_2019','PRI_2019','SWIRI_2019','SAVI_2019',
                              'PRI2_2019','CACTI_2019','CACTI2_2019','MTCI_2019','CI_2019',
                              'CAI_2019','CELL_2019','CELL2_2019','NDNI_2019','CONIFER_2019','SPRUCE_2019','NDRE_2019','CHM_2019', 
                              'NDVI_2018','NDWI_2018','PRI_2018','SWIRI_2018','SAVI_2018',
                              'PRI2_2018','CACTI_2018','CACTI2_2018','MTCI_2018','CI_2018',
                              'CAI_2018','CELL_2018','CELL2_2018','NDNI_2018','CONIFER_2018','SPRUCE_2018','NDRE_2018','CHM_2018',
                              'NDVI_2017','NDWI_2017','PRI_2017','SWIRI_2017','SAVI_2017',
                              'PRI2_2017','CACTI_2017','CACTI2_2017','MTCI_2017','CI_2017',
                              'CAI_2017','CELL_2017','CELL2_2017','NDNI_2017','CONIFER_2017','SPRUCE_2017','NDRE_2017','CHM_2017', 'class')
gc()


trainData_indices <- read.csv("//snow/projects/RaBET/RaBET_species/NEON_FINAL/SRER/SRER_trainingdata_021622.csv") %>% select(-c(X.1,X))
trainData_indices <- read.csv("//snow/projects/RaBET/RaBET_species/NEON_FINAL/MOAB/FinalOutputs/MOAB_trainingindices_072922.csv") 
trainData_indices <- read.csv("//snow/projects/RaBET/RaBET_species/NEON_FINAL/ONAQ/FinalOutputs/ONAQtrainingindices_112922_nosaltbush.csv") %>% select(-c(X)) 
trainData_indices <- read.csv("//snow/projects/RaBET/RaBET_species/NEON_FINAL/CLBJ/FinalOutputs/CLBJ_trainingdata_122021.csv") 
trainData_indices <- read.csv("//snow/projects/RaBET/RaBET_species/NEON_FINAL/JORN/FinalOutputs/JORN_trainingdata_030122.csv") 
trainData_indices <- read.csv("//snow/projects/RaBET/RaBET_species/NEON_FINAL/NIWOT/FinalOutputs/NIWOT_trainingdata_112822_no2017.csv")
trainData_nolidar <- trainData_indices %>% dplyr::select(-c(CHM_2019, CHM_2018, CHM_2017))

trainData_indices <- trainData_indices %>% dplyr::select(c(NDVI_2021,NDWI_2021,PRI_2021,SWIRI_2021,SAVI_2021,
                                                 PRI2_2021,CACTI_2021,CACTI2_2021,MTCI_2021,CI_2021,
                                                 CAI_2021,NDNI_2021,CHM_2021, class))

trainData_2018 <- trainData_indices %>% dplyr::select(c(NDVI_2018,NDWI_2018,PRI_2018,SWIRI_2018,SAVI_2018,
                                                 PRI2_2018,CACTI_2018,CACTI2_2018,MTCI_2018,CI_2018,
                                                 CAI_2018,NDNI_2018,CHM_2018, class))

trainData_2017 <- trainData_indices %>% dplyr::select(c(NDVI_2017,NDWI_2017,PRI_2017,SWIRI_2017,SAVI_2017,
                                                 PRI2_2017,CACTI_2017,CACTI2_2017,MTCI_2017,CI_2017,
                                                 CAI_2017,NDNI_2017,CHM_2017, class))

trainData_2019_nolidar <- trainData_nolidar %>% dplyr::select(c(NDVI_2019,NDWI_2019,PRI_2019,SWIRI_2019,SAVI_2019,
                                                 PRI2_2019,CACTI_2019,CACTI2_2019,MTCI_2019,CI_2019,
                                                 CAI_2019,NDNI_2019, class))

trainData_2018_nolidar <- trainData_nolidar %>% dplyr::select(c(NDVI_2018,NDWI_2018,PRI_2018,SWIRI_2018,SAVI_2018,
                                                 PRI2_2018,CACTI_2018,CACTI2_2018,MTCI_2018,CI_2018,
                                                 CAI_2018,NDNI_2018, class))

trainData_2017_nolidar <- trainData_nolidar %>% dplyr::select(c(NDVI_2017,NDWI_2017,PRI_2017,SWIRI_2017,SAVI_2017,
                                                 PRI2_2017,CACTI_2017,CACTI2_2017,MTCI_2017,CI_2017,
                                                 CAI_2017,NDNI_2017, class))


trainData_indices_outliers.rm <- read.csv("P:/RaBET/Results/trainData_102021_outliersrm_cln_ac.csv") %>% dplyr::select(-c(MNDWI_2019,MNDWI_2018,MNDWI_2017,NDII_2019,NDII_2018,NDII_2017,X.1,X, X.2))
trainData_nolidar_outliers.rm <- trainData_indices_outliers.rm %>% dplyr::select(-c(1,CHM_2019, CHM_2018, CHM_2017)) %>% as.data.frame()

trainData_2019_outliers.rm <- trainData_indices_outliers.rm %>% dplyr::select(c(NDVI_2019,NDWI_2019,PRI_2019,SWIRI_2019,SAVI_2019,
                                                 PRI2_2019,CACTI_2019,CACTI2_2019,MTCI_2019,CI_2019,
                                                 CAI_2019,CELL_2019,CELL2_2019,NDNI_2019,CHM_2019,class))

trainData_2018_outliers.rm <- trainData_indices_outliers.rm %>% dplyr::select(c(NDVI_2018,NDWI_2018,PRI_2018,SWIRI_2018,SAVI_2018,
                                                 PRI2_2018,CACTI_2018,CACTI2_2018,MTCI_2018,CI_2018,
                                                 CAI_2018,CELL_2018,CELL2_2018,NDNI_2018,CHM_2018, class))

trainData_2017_outliers.rm <- trainData_indices_outliers.rm %>% dplyr::select(c(NDVI_2017,NDWI_2017,PRI_2017,SWIRI_2017,SAVI_2017,
                                                 PRI2_2017,CACTI_2017,CACTI2_2017,MTCI_2017,CI_2017,
                                                 CAI_2017,CELL_2017,CELL2_2017,NDNI_2017,CHM_2017,  class))

trainData_2019_nolidar_outliers.rm <- trainData_nolidar_outliers.rm %>% dplyr::select(c(NDVI_2019,NDWI_2019,PRI_2019,SWIRI_2019,SAVI_2019,
                                                         PRI2_2019,CACTI_2019,CACTI2_2019,MTCI_2019,CI_2019,
                                                         CAI_2019,CELL_2019,CELL2_2019,NDNI_2019, class))

trainData_2018_nolidar_outliers.rm <- trainData_nolidar_outliers.rm %>% dplyr::select(c(NDVI_2018,NDWI_2018,PRI_2018,SWIRI_2018,SAVI_2018,
                                                         PRI2_2018,CACTI_2018,CACTI2_2018,MTCI_2018,CI_2018,
                                                         CAI_2018,CELL_2018,CELL2_2018,NDNI_2018,  class))

trainData_2017_nolidar_outliers.rm <- trainData_nolidar_outliers.rm %>% dplyr::select(c(NDVI_2017,NDWI_2017,PRI_2017,SWIRI_2017,SAVI_2017,
                                                         PRI2_2017,CACTI_2017,CACTI2_2017,MTCI_2017,CI_2017,
                                                         CAI_2017,CELL_2017,CELL2_2017,NDNI_2017,  class))



trainData_2018_2019 <- trainData_indices %>% dplyr::select(NDVI_2019,NDWI_2019,PRI_2019,SWIRI_2019,SAVI_2019,
                                                                    PRI2_2019,CACTI_2019,CACTI2_2019,MTCI_2019,CI_2019,
                                                                    CAI_2019,NDNI_2019,CHM_2019, 
                                                                    NDVI_2018,NDWI_2018,PRI_2018,SWIRI_2018,SAVI_2018,
                                                                    PRI2_2018,CACTI_2018,CACTI2_2018,MTCI_2018,CI_2018,
                                                                    CAI_2018,NDNI_2018,CHM_2018,
                                                                    class)



trainData_2017_2018 <- trainData_indices %>% dplyr::select(NDVI_2017,NDWI_2017,PRI_2017,SWIRI_2017,SAVI_2017,
                                                                PRI2_2017,CACTI_2017,CACTI2_2017,MTCI_2017,CI_2017,
                                                                CAI_2017,NDNI_2017,CHM_2017, 
                                                                NDVI_2018,NDWI_2018,PRI_2018,SWIRI_2018,SAVI_2018,
                                                                PRI2_2018,CACTI_2018,CACTI2_2018,MTCI_2018,CI_2018,
                                                                CAI_2018,NDNI_2018,CHM_2018,
                                                                class)



trainData_2017_2019 <- trainData_indices %>% dplyr::select(NDVI_2019,NDWI_2019,PRI_2019,SWIRI_2019,SAVI_2019,
                                                                PRI2_2019,CACTI_2019,CACTI2_2019,MTCI_2019,CI_2019,
                                                                CAI_2019,NDNI_2019,CHM_2019, 
                                                                NDVI_2017,NDWI_2017,PRI_2017,SWIRI_2017,SAVI_2017,
                                                                PRI2_2017,CACTI_2017,CACTI2_2017,MTCI_2017,CI_2017,
                                                                CAI_2017,NDNI_2017,CHM_2017,
                                                                class)


trainData_varImp <- trainData_indices_outliers.rm %>% dplyr::select(NDVI_2019,NDWI_2019,PRI_2019,SWIRI_2019,SAVI_2019,
                                                                PRI2_2019,MTCI_2019,CI_2019,
                                                                CAI_2019,CELL_2019,CELL2_2019,NDNI_2019,CHM_2019, 
                                                                NDVI_2018,NDWI_2018,PRI_2018,SWIRI_2018,SAVI_2018,
                                                                PRI2_2018,MTCI_2018,CI_2018,
                                                                CAI_2018,CELL_2018,CELL2_2018,NDNI_2018,CHM_2018,
                                                                NDVI_2017,NDWI_2017,PRI_2017,SWIRI_2017,SAVI_2017,
                                                                PRI2_2017,MTCI_2017,CI_2017,
                                                                CAI_2017,CELL_2017,CELL2_2017,NDNI_2017,CHM_2017,
                                                                class)





#CART MODEL EVALUATION
modelCART <- rpart(class~., data=trainData_indices, method = 'class',minsplit = 5)
set.seed(99)
j <- kfold(trainData_indices, k = 7, by=trainData_indices$class)
table(j)

x <- list()
for (k in 1:5) {
  train <- trainData_indices[j!= k, ]
  test <- trainData_indices[j == k, ]
  cart <- rpart(as.factor(class)~., data=train, method = 'class', minsplit = 5)
  pclass <- predict(cart, test, type='class')
  # create a data.frame using the reference and prediction
  x[[k]] <- cbind(test$class, as.integer(pclass))
}

y <- do.call(rbind, x)
y <- data.frame(y)
colnames(y) <- c('observed', 'predicted')
conmat <- table(y)
# change the name of the classes
colnames(conmat) <- classdf$classnames1
rownames(conmat) <- classdf$classnames1
accmat <- as.table(conmat)
accmat.ext <- addmargins(accmat)
accmat.ext <- rbind(accmat.ext, "Users" = c(PA, NA))
accmat.ext <- cbind(accmat.ext, "Producers" = c(UA, NA, OA))
colnames(accmat.ext) <- c(levels(as.factor(trainData_indices$class)), "Sum", "PA")
rownames(accmat.ext) <- c(levels(as.factor(trainData_indices$class)), "Sum", "UA")
accmat.ext <- round(accmat.ext, digits = 1)
dimnames(accmat.ext) <- list("Prediction" = colnames(accmat.ext),
                             "Reference" = rownames(accmat.ext))
class(accmat.ext) <- "table"


# number of cases
n <- sum(conmat)
# number of correctly classified cases per class
diag <- diag(conmat)
# Overall Accuracy
OA <- sum(diag) / n
# observed (true) cases per class
rowsums <- apply(conmat, 1, sum)
p <- rowsums / n
# predicted cases per class
colsums <- apply(conmat, 2, sum)
q <- colsums / n
expAccuracy <- sum(p*q)
kappa <- (OA - expAccuracy) / (1 - expAccuracy)
# Producer accuracy
PA <- diag / colsums
# User accuracy
UA <- diag / rowsums
outAcc <- data.frame(producerAccuracy = PA, userAccuracy = UA)

accmat.ext
varImp(modelCART,scale = FALSE,surrogates = FALSE,competes = TRUE)
OA
kappa
outAcc









































#RF MODEL EVALUATION
set.seed(99)
j <- kfold(trainData_indices, k = 5, by=trainData_indices$class)
table(j)

x <- list()
for (k in 1:5) {
  train <- trainData_indices[j!= k, ]
  test <- trainData_indices[j == k, ]
  modelRF <- randomForest(as.factor(class)~.,data=train,ntree=500,importance=TRUE,  na.action=na.omit)
  pclass <- predict(modelRF, test, type='class')
  # create a data.frame using the reference and prediction
  x[[k]] <- cbind(test$class, as.integer(pclass))
}

y <- do.call(rbind, x)
y <- data.frame(y)

colnames(y) <- c('observed', 'predicted')
conmat <- table(y)
# change the name of the classes
colnames(conmat) <- classdf$classnames1
rownames(conmat) <- classdf$classnames1
accmat <- as.table(conmat)
accmat.ext <- addmargins(accmat)
accmat.ext <- rbind(accmat.ext, "Users" = c(PA, NA))
accmat.ext <- cbind(accmat.ext, "Producers" = c(UA, NA, OA))
colnames(accmat.ext) <- c(levels(as.factor(trainData_indices$class)), "Sum", "PA")
rownames(accmat.ext) <- c(levels(as.factor(trainData_indices$class)), "Sum", "UA")
accmat.ext <- round(accmat.ext, digits = 1)
dimnames(accmat.ext) <- list("Prediction" = colnames(accmat.ext),
                             "Reference" = rownames(accmat.ext))
class(accmat.ext) <- "table"


# number of cases
n <- sum(conmat)
# number of correctly classified cases per class
diag <- diag(conmat)
# Overall Accuracy
OA <- sum(diag) / n
# observed (true) cases per class
rowsums <- apply(conmat, 1, sum)
p <- rowsums / n
# predicted cases per class
colsums <- apply(conmat, 2, sum)
q <- colsums / n
expAccuracy <- sum(p*q)
kappa <- (OA - expAccuracy) / (1 - expAccuracy)
# Producer accuracy
PA <- diag / colsums
# User accuracy
UA <- diag / rowsums
outAcc <- data.frame(producerAccuracy = PA, userAccuracy = UA)

accmat.ext
importance(modelRF,scale = TRUE,surrogates = TRUE,competes = TRUE)
OA
kappa
outAcc











#SVM MODEL EVALUATION
modelSVM <- svm(class~., data=trainData_indices, kernel ="linear", type = 'C-classification',cost = 0.125, gamma = 128)

#trainData_indices <- na.omit(trainData_indices_outliers.rm)

set.seed(99)
j <- kfold(trainData_indices, k = 5, by=trainData_indices$class)
table(j)

x <- list()
for (k in 1:5) {
  train <- trainData_indices[j!= k, ]
  test <- trainData_indices[j == k, ]
  modelSVM <- svm(as.factor(class)~.,data=train, kernel ="linear", type = 'C-classification',  cost = 0.125, gamma = 128,scale = TRUE, na.action = na.exclude)
  pclass <- predict(modelSVM, test, na.rm=T, type="class")
  # create a data.frame using the reference and prediction
  x[[k]] <- cbind(test$class, as.integer(pclass))
}


y <- do.call(rbind, x)
y <- data.frame(y)
y
colnames(y) <- c('observed', 'predicted')
conmat <- table(y)
# change the name of the classes
colnames(conmat) <- classdf$classnames1
rownames(conmat) <- classdf$classnames1
accmat <- as.table(conmat)
accmat.ext <- addmargins(accmat)
accmat.ext <- rbind(accmat.ext, "Users" = c(PA, NA))
accmat.ext <- cbind(accmat.ext, "Producers" = c(UA, NA, OA))
colnames(accmat.ext) <- c(levels(as.factor(trainData_indices$class)), "Sum", "PA")
rownames(accmat.ext) <- c(levels(as.factor(trainData_indices$class)), "Sum", "UA")
accmat.ext <- round(accmat.ext, digits = 1)
dimnames(accmat.ext) <- list("Prediction" = colnames(accmat.ext),
                             "Reference" = rownames(accmat.ext))
class(accmat.ext) <- "table"


# number of cases
n <- sum(conmat)
# number of correctly classified cases per class
diag <- diag(conmat)
# Overall Accuracy
OA <- sum(diag) / n
# observed (true) cases per class
rowsums <- apply(conmat, 1, sum)
p <- rowsums / n
# predicted cases per class
colsums <- apply(conmat, 2, sum)
q <- colsums / n
expAccuracy <- sum(p*q)
kappa <- (OA - expAccuracy) / (1 - expAccuracy)
# Producer accuracy
PA <- diag / colsums
# User accuracy
UA <- diag / rowsums
outAcc <- data.frame(producerAccuracy = PA, userAccuracy = UA)

accmat.ext
varImp(modelSVM,scale = FALSE,surrogates = FALSE,competes = TRUE)
OA
kappa
outAcc






#CART MODEL EVALUATION
###naomit
modelCART <- rpart(class~., data=trainData_indices_outliers.rm, method = 'class',minsplit = 5)
set.seed(99)
j <- kfold(trainData_indices_outliers.rm, k = 5, by=trainData_indices_outliers.rm$class)
table(j)

x <- list()
for (k in 1:5) {
  train <- trainData_indices_outliers.rm[j!= k, ]
  test <- trainData_indices_outliers.rm[j == k, ]
  cart <- rpart(as.factor(class)~., data=train, method = 'class', minsplit = 5)
  pclass <- predict(cart, test, type='class')
  # create a data.frame using the reference and prediction
  x[[k]] <- cbind(test$class, as.integer(pclass))
}

y <- do.call(rbind, x)
y <- data.frame(y)
colnames(y) <- c('observed', 'predicted')
conmat <- table(y)
# change the name of the classes
colnames(conmat) <- classdf$classnames1
rownames(conmat) <- classdf$classnames1
accmat <- as.table(conmat)
accmat.ext <- addmargins(accmat)
accmat.ext <- rbind(accmat.ext, "Users" = c(PA, NA))
accmat.ext <- cbind(accmat.ext, "Producers" = c(UA, NA, OA))
colnames(accmat.ext) <- c(levels(as.factor(trainData_indices_outliers.rm$class)), "Sum", "PA")
rownames(accmat.ext) <- c(levels(as.factor(trainData_indices_outliers.rm$class)), "Sum", "UA")
accmat.ext <- round(accmat.ext, digits = 1)
dimnames(accmat.ext) <- list("Prediction" = colnames(accmat.ext),
                             "Reference" = rownames(accmat.ext))
class(accmat.ext) <- "table"


# number of cases
n <- sum(conmat)
# number of correctly classified cases per class
diag <- diag(conmat)
# Overall Accuracy
OA <- sum(diag) / n
# observed (true) cases per class
rowsums <- apply(conmat, 1, sum)
p <- rowsums / n
# predicted cases per class
colsums <- apply(conmat, 2, sum)
q <- colsums / n
expAccuracy <- sum(p*q)
kappa <- (OA - expAccuracy) / (1 - expAccuracy)
# Producer accuracy
PA <- diag / colsums
# User accuracy
UA <- diag / rowsums
outAcc <- data.frame(producerAccuracy = PA, userAccuracy = UA)

accmat.ext
varImp(modelCART,scale = FALSE,surrogates = FALSE,competes = TRUE)
OA
kappa
outAcc









































#RF MODEL EVALUATION
set.seed(99)
j <- kfold(trainData_indices_outliers.rm, k = 5, by=trainData_indices_outliers.rm$class)
table(j)

x <- list()
for (k in 1:5) {
  train <- trainData_indices_outliers.rm[j!= k, ]
  test <- trainData_indices_outliers.rm[j == k, ]
  modelRF <- randomForest(as.factor(class)~.,data=train,ntree=500,importance=TRUE,  na.action=na.omit)
  pclass <- predict(modelRF, test, type='class')
  # create a data.frame using the reference and prediction
  x[[k]] <- cbind(test$class, as.integer(pclass))
}

y <- do.call(rbind, x)
y <- data.frame(y)
y
colnames(y) <- c('observed', 'predicted')
conmat <- table(y)
# change the name of the classes
colnames(conmat) <- classdf$classnames1
rownames(conmat) <- classdf$classnames1
accmat <- as.table(conmat)
accmat.ext <- addmargins(accmat)
accmat.ext <- rbind(accmat.ext, "Users" = c(PA, NA))
accmat.ext <- cbind(accmat.ext, "Producers" = c(UA, NA, OA))
colnames(accmat.ext) <- c(levels(as.factor(trainData_indices_outliers.rm$class)), "Sum", "PA")
rownames(accmat.ext) <- c(levels(as.factor(trainData_indices_outliers.rm$class)), "Sum", "UA")
accmat.ext <- round(accmat.ext, digits = 1)
dimnames(accmat.ext) <- list("Prediction" = colnames(accmat.ext),
                             "Reference" = rownames(accmat.ext))
class(accmat.ext) <- "table"


# number of cases
n <- sum(conmat)
# number of correctly classified cases per class
diag <- diag(conmat)
# Overall Accuracy
OA <- sum(diag) / n
# observed (true) cases per class
rowsums <- apply(conmat, 1, sum)
p <- rowsums / n
# predicted cases per class
colsums <- apply(conmat, 2, sum)
q <- colsums / n
expAccuracy <- sum(p*q)
kappa <- (OA - expAccuracy) / (1 - expAccuracy)
# Producer accuracy
PA <- diag / colsums
# User accuracy
UA <- diag / rowsums
outAcc <- data.frame(producerAccuracy = PA, userAccuracy = UA)

accmat.ext
varImp(modelRF,scale = FALSE,surrogates = FALSE,competes = TRUE)
OA
kappa
outAcc















#SVM MODEL EVALUATION
modelSVM <- svm(class~., data=trainData_indices, kernel ="linear", type = 'C-classification',cost = 0.125, gamma = 128)

#trainData_indices_outliers.rm <- na.omit(trainData_indices_outliers.rm)

set.seed(99)
j <- kfold(trainData_indices_outliers.rm, k = 5, by=trainData_indices_outliers.rm$class)
table(j)

x <- list()
for (k in 1:5) {
  train <- trainData_indices_outliers.rm[j!= k, ]
  test <- trainData_indices_outliers.rm[j == k, ]
  modelSVM <- svm(as.factor(class)~.,data=train, kernel ="linear", type = 'C-classification',  cost = 0.125, gamma = 128,scale = TRUE, na.action = na.exclude)
  pclass <- predict(modelSVM, test, na.rm=T, type="class")
  # create a data.frame using the reference and prediction
  x[[k]] <- cbind(test$class, as.integer(pclass))
}


y <- do.call(rbind, x)
y <- data.frame(y)
y
colnames(y) <- c('observed', 'predicted')
conmat <- table(y)
# change the name of the classes
colnames(conmat) <- classdf$classnames1
rownames(conmat) <- classdf$classnames1
accmat <- as.table(conmat)
accmat.ext <- addmargins(accmat)
accmat.ext <- rbind(accmat.ext, "Users" = c(PA, NA))
accmat.ext <- cbind(accmat.ext, "Producers" = c(UA, NA, OA))
colnames(accmat.ext) <- c(levels(as.factor(trainData_indices_outliers.rm$class)), "Sum", "PA")
rownames(accmat.ext) <- c(levels(as.factor(trainData_indices_outliers.rm$class)), "Sum", "UA")
accmat.ext <- round(accmat.ext, digits = 1)
dimnames(accmat.ext) <- list("Prediction" = colnames(accmat.ext),
                             "Reference" = rownames(accmat.ext))
class(accmat.ext) <- "table"


# number of cases
n <- sum(conmat)
# number of correctly classified cases per class
diag <- diag(conmat)
# Overall Accuracy
OA <- sum(diag) / n
# observed (true) cases per class
rowsums <- apply(conmat, 1, sum)
p <- rowsums / n
# predicted cases per class
colsums <- apply(conmat, 2, sum)
q <- colsums / n
expAccuracy <- sum(p*q)
kappa <- (OA - expAccuracy) / (1 - expAccuracy)
# Producer accuracy
PA <- diag / colsums
# User accuracy
UA <- diag / rowsums
outAcc <- data.frame(producerAccuracy = PA, userAccuracy = UA)

accmat.ext
#varImp(modelSVM,scale = FALSE,surrogates = FALSE,competes = TRUE)
OA
kappa
outAcc

#write.csv(trainData_naomit, file = "trainData_naomit_ouliersrm_95CI_102821_cln_ac.csv")







####YEARLY BREAKDOWN CLASSIFICATION
s