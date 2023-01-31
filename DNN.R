library(rgdal)
library(raster)
library(dplyr)
library(plyr)
library(keras)         # karas api in R
library(tfruns)        # Create and manage unique directories for each 'TensorFlow' training run. 
library(tfestimators)  # Interface to 'TensorFlow' Estimators 

dataFolder<-"D:\\Dropbox\\Spatial Data Analysis and Processing in R\\DATA_RS_DNN"
point<-read.csv(paste0(dataFolder,".\\Sentinel_2\\point_data.csv"), header = T)
grid<-read.csv(paste0(dataFolder,".\\Sentinel_2\\prediction_grid_data.csv"), header = T)
#Creat data frames
point.df<-cbind(point[c(3:13)])
grid.df<-grid[c(4:13)]
grid.xy<-grid[c(3,1:2)]

#Convert Class to dummy variables
point.df[,11] <- as.numeric(point.df[,11]) -1 

#Convert data as matrix
point.df<- as.matrix(point.df)
grid.df <- as.matrix(grid.df)

#Set dimnames to NULL
dimnames(point.df) <- NULL
dimnames(grid.df) <- NULL

#Standardize_the data: ((x-mean(x))/sd(x))
point.df[, 1:10] = scale(point.df[, 1:10])
grid.df[, 1:10] = scale(grid.df[, 1:10])

#Split data
##  Determine sample size
ind <- sample(2, nrow(point.df), replace=TRUE, prob=c(0.80, 0.20))
# Split the `Split data
training <- point.df[ind==1, 1:10]
test <- point.df[ind==2, 1:10]
# Split the class attribute
trainingtarget <- point.df[ind==1, 11]
testtarget <- point.df[ind==2, 11]
#Hyperparameter flag
FLAGS <- flags(
  flag_numeric('dropout_1', 0.2, 'First dropout'),
  flag_numeric('dropout_2', 0.2, 'Second dropout'),
  flag_numeric('dropout_3', 0.1, 'Third dropout'),
  flag_numeric('dropout_4', 0.1, 'Forth dropout')
)
#Define model parameters with 4 hidden layers with 200 neurons
model <- keras_model_sequential()
model %>% 
  # Imput layer
  layer_dense(units = 200, activation = 'relu', 
              kernel_regularizer =regularizer_l1_l2(l1 = 0.00001, l2 = 0.00001),input_shape = c(10)) %>% 
  layer_dropout(rate = FLAGS$dropout_1,seed = 1) %>% 
  # Hidden layers
  layer_dense(units = 200, activation = 'relu',
              kernel_regularizer = regularizer_l1_l2(l1 = 0.00001, l2 = 0.00001)) %>%
  layer_dropout(rate = FLAGS$dropout_2,seed = 1) %>%
  layer_dense(units = 200, activation = 'relu',
              kernel_regularizer = regularizer_l1_l2(l1 = 0.00001, l2 = 0.00001)) %>%
  layer_dropout(rate = FLAGS$dropout_3,seed = 1) %>%
  layer_dense(units = 200, activation = 'relu',
              kernel_regularizer = regularizer_l1_l2(l1 = 0.0001, l2 = 0.00001)) %>%
  layer_dropout(rate = FLAGS$dropout_4) %>%
  # Output layer
  layer_dense(units = 5, activation = 'softmax')
summary(model)



#Define an optimizer (Stochastic gradient descent optimizer)
optimizer <- optimizer_sgd(lr=0.01)
#Compile the model:
  model %>% compile(
    loss = 'sparse_categorical_crossentropy',
    optimizer = optimizer,
    metrics = 'accuracy'
  )
#Fit the model to the data
history<-model %>% fit(
  training, trainingtarget, 
  epochs = 10, 
  batch_size = 100, 
  shuffle = TRUE,
  validation_split = 0.2
)
#Plot history:
  plot(history)



score <- model %>% evaluate(test, testtarget, batch_size = 100)
cat('Test loss:', score[[1]], '\n')
## Test loss: 0.6054224
cat('Test accuracy:', score[[2]], '\n')
## Test accuracy: 0.8741097

#Prediction & confusion matrix - test data
class.test <- model %>%
  predict_classes(test, batch_size = 100)
# Confusion matrix
table(testtarget,class.test)

#Predicted Class Probability
prob.test <- model %>%
  predict_proba(test, batch_size = 100)

#Prediction at grid locations:
  Class.grid <- model %>%
  predict_classes(grid.df, batch_size = 100)
  
#Detach keras, tfruns, tftestimators
detach(package:keras, unload=TRUE)
detach(package:tfruns, unload=TRUE)
detach(package:tfestimators, unload=TRUE)
  
#Change column name:
  class<-as.data.frame(Class.grid)
new.grid<-cbind(x=grid.xy$x, y=grid.xy$y,Class_ID=class )
colnames(new.grid)[3]<-"Class_ID"
new.grid.na<-na.omit(new.grid)

#Load landuse ID file
#### Join Class Id Column
ID<-read.csv(paste0(dataFolder,".\\Sentinel_2\\Landuse_ID_keras.csv"), header=TRUE)
ID

#Convert to raster
#### Convert to raster
x<-SpatialPointsDataFrame(as.data.frame(new.grid.na)[, c("x", "y")], data = new.grid.na)
r <- rasterFromXYZ(as.data.frame(x)[, c("x", "y", "Class_ID")])

#Plot map:
  # Create color palette
  myPalette <- colorRampPalette(c("light grey","burlywood4", "forestgreen","light green", "dodgerblue"))
# Plot Map
LU<-spplot(r,"Class_ID", main="Supervised Image Classification: DNN keras-R" , 
           colorkey = list(space="right",tick.number=1,height=1, width=1.5,
                           labels = list(at = seq(0,3.8,length=5),cex=1.0,
                                         lab = c("Road/parking/pavement" ,"Building", "Tree/buses", "Grass", "Water"))),
           col.regions=myPalette,cut=4)
LU