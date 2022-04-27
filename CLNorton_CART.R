#CART CLASSIFICATION
modelCART <- rpart(class~., data=trainData_indices, method = 'class',minsplit = 5)

### Starts timer.
ptm <- proc.time()

#Predict
#speciesspecific_cart <- predict(NEON_indices, modelCART, type='class', na.rm=TRUE)
beginCluster(15)

speciesspecific_cart_indices <- clusterR(NEON_indices, predict, args=list(modelCART, na.rm=T,type="class"))


endCluster()
