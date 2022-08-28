#RANDOM FOREST CLASSIFICATON
modelRF <- randomForest(x=trainData[ ,c(1:7)], y=trainData$class, importance = TRUE)
modelRF
class(modelRF)
str(modelRF)
names(modelRF)
speciesspecific_rf <- predict(NEON_all, model=modelRF, na.rm=TRUE)

## Plot the results
# recall: 1 = bareground, 2 = grass, 3 = mesquite, 4 = cactus, 5 = catclaw
cols <- c("orange", "dark green", "blue","yellow","black")
plot(speciesspecific_rf, col=cols, legend=FALSE)
legend("bottomright",
       legend=c("bareground", "grass", "mesquite","cactus","catclaw", "loetbush", "paloverde", "creosote"),
       fill=cols, bg="white")

writeRaster(speciesspecific_rf, 
            filename="speciesspecific040121.tif",
            format="GTiff",
            options="COMPRESS=LZW",
            overwrite = TRUE,
            NAflag = -9999)












#RF MODEL EVALUATION
## Inspect the confusion matrix of the OOB error assessment
modelRF$confusion
# to make the confusion matrix more readable
colnames(modelRF$confusion) <- c("bareground", "grass", "mesquite","cactus","catclaw", "loetbush", "paloverde", "creosote", "class.error")
rownames(modelRF$confusion) <- c("bareground", "grass", "mesquite","cactus","catclaw", "loetbush", "paloverde", "creosote")
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

