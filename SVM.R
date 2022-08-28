svm <- svm(class~, data=NEON_indices, kernel ="linear")
beginCluster(15)
speciesspecific_svm_indices <- clusterR(NEON_indices, raster::predict, arcgs = (model = svm))
endCluster()
