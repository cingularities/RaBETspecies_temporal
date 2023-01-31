library(raster)
setwd("//gaea/Projects/RaBET/RaBET_species/Outputs/final_results/percentages")
JORN_rabet <- raster('//gaea/Projects/RaBET/RaBET_species/Outputs/final_results//speciesspecific_RF_041122_JORN.tif')
SRER_rabet <- raster('//gaea/Projects/RaBET/RaBET_species/Outputs/final_results//RF_allyr_SRER.tif')
CLBJ_rabet <- raster('//gaea/Projects/RaBET/RaBET_species/Outputs/final_results//speciesspecific_RF_122021_CLBJ_96OA.tif')
MOAB_rabet <- raster('//gaea/Projects/RaBET/RaBET_species/Outputs/final_results//speciesspecific_RF_072522_MOAB.tif')
ONAQ_rabet <- raster('//gaea/Projects/RaBET/RaBET_species/Outputs/final_results//speciesspecific_RF_071922_ONAQ.tif')




CLBJ_Species5 <- CLBJ_rabet
CLBJ_Species5[CLBJ_Species5 != 7] = 0
CLBJ_rabet_Species5 <- reclassify(CLBJ_Species5, cbind(7, 1))
#plot(CLBJ_rabet_Species5)
CLBJ_rabet_Species5_percent <- aggregate(CLBJ_rabet_Species5, 10, mean)
CLBJ_rabet_Species5_percent[CLBJ_rabet_Species5_percent <= 0] <- NA
plot(CLBJ_rabet_Species5_percent)
writeRaster(CLBJ_rabet_Species5_percent, filename = "CLBJ_rabet_Species5_percent.tif")


CLBJ_juniper <- CLBJ_rabet
CLBJ_juniper[CLBJ_juniper != 6] = 0
CLBJ_rabet_juniper <- reclassify(CLBJ_juniper, cbind(6, 1))
#plot(CLBJ_rabet_juniper)
CLBJ_rabet_juniper_percent <- aggregate(CLBJ_rabet_juniper, 10, mean)
CLBJ_rabet_juniper_percent[CLBJ_rabet_juniper_percent <= 0] <- NA
plot(CLBJ_rabet_juniper_percent)
writeRaster(CLBJ_rabet_juniper_percent, filename = "CLBJ_rabet_juniper_percent.tif")


CLBJ_blackjackoak <- CLBJ_rabet
CLBJ_blackjackoak[CLBJ_blackjackoak != 5] = 0
CLBJ_rabet_blackjackoak <- reclassify(CLBJ_blackjackoak, cbind(5, 1))
#plot(CLBJ_rabet_blackjackoak)
CLBJ_rabet_blackjackoak_percent <- aggregate(CLBJ_rabet_blackjackoak, 10, mean)
CLBJ_rabet_blackjackoak_percent[CLBJ_rabet_blackjackoak_percent <= 0] <- NA
plot(CLBJ_rabet_blackjackoak_percent)
writeRaster(CLBJ_rabet_blackjackoak_percent, filename = "CLBJ_rabet_blackjackoak_percent.tif")



CLBJ_elm <- CLBJ_rabet
CLBJ_elm[CLBJ_elm != 4] = 0
CLBJ_rabet_elm <- reclassify(CLBJ_elm, cbind(4, 1))
#plot(CLBJ_rabet_elm)
CLBJ_rabet_elm_percent <- aggregate(CLBJ_rabet_elm, 10, mean)
CLBJ_rabet_elm_percent[CLBJ_rabet_elm_percent <= 0] <- NA
plot(CLBJ_rabet_elm_percent)
writeRaster(CLBJ_rabet_elm_percent, filename = "CLBJ_rabet_elm_percent.tif")


CLBJ_postoak <- CLBJ_rabet
CLBJ_postoak[CLBJ_postoak != 3] = 0
CLBJ_rabet_postoak <- reclassify(CLBJ_postoak, cbind(3, 1))
#plot(CLBJ_rabet_postoak)
CLBJ_rabet_postoak_percent <- aggregate(CLBJ_rabet_postoak, 10, mean)
CLBJ_rabet_postoak_percent[CLBJ_rabet_postoak_percent <= 0] <- NA
plot(CLBJ_rabet_postoak_percent)
writeRaster(CLBJ_rabet_postoak_percent, filename = "CLBJ_rabet_postoak_percent.tif")


CLBJ_grass <- CLBJ_rabet
CLBJ_grass[CLBJ_grass != 2] = 0
CLBJ_rabet_grass <- reclassify(CLBJ_grass, cbind(2, 1))
#plot(CLBJ_rabet_grass)
CLBJ_rabet_grass_percent <- aggregate(CLBJ_rabet_grass, 10, mean)
plot(CLBJ_rabet_grass_percent)
writeRaster(CLBJ_rabet_grass_percent, filename = "CLBJ_rabet_grass_percent.tif")


CLBJ_bareground <- CLBJ_rabet
CLBJ_bareground[CLBJ_bareground != 1] = 0
CLBJ_rabet_bareground <- reclassify(CLBJ_bareground, cbind(1, 1))
#plot(CLBJ_rabet_bareground)
CLBJ_rabet_bareground_percent <- aggregate(CLBJ_rabet_bareground, 10, mean)
plot(CLBJ_rabet_bareground_percent)
writeRaster(CLBJ_rabet_bareground_percent, filename = "CLBJ_rabet_bareground_percent.tif")



CLBJ_stack <- stack(CLBJ_rabet_Species5_percent,
                    CLBJ_rabet_cliffrose_percent,
                    CLBJ_rabet_juniper_percent, 
                    CLBJ_rabet_saltbrush_percent,
                    CLBJ_rabet_sagebrush_percent)

names(CLBJ_stack) <- c("Species5","cliffrose","juniper","saltbrush","sagebrush")

plot(CLBJ_stack)
writeRaster(CLBJ_stack, filename = "CLBJ_rabet_percent_stacked.tif",overwrite=TRUE)














###FUNCTION
species_percentage <- function(classified_image, class_num,aggregate_num) {
class <- classified_image
class[class != class_num] = 0
class_reclass <- reclassify(class, cbind(class_num, 1))
#plot(JORN_rabet_grass)
class_reclass_aggregate <- class_reclassaggregate(JORN_rabet_saltbrush, aggregate_num, mean)
plot(class_reclass_aggregate)
return(class_reclass_aggregate)
}

CLBJ_mesquite_percent <- species_percentage(CLBJ_rabet, 3,10)


writeRaster(class_reclass_aggregate, filename = "class_reclass_aggregate")













CLBJ_grass <- CLBJ_rabet
CLBJ_grass[CLBJ_grass != 2] = 0
CLBJ_rabet_grass <- reclassify(CLBJ_grass, cbind(2, 1))
#plot(CLBJ_rabet_grass)
CLBJ_rabet_grass_percent <- aggregate(CLBJ_rabet_grass, 10, mean)
plot(CLBJ_rabet_grass_percent)
writeRaster(CLBJ_rabet_grass_percent, filename = "CLBJ_rabet_grass_percent.tif")


CLBJ_bareground <- CLBJ_rabet
CLBJ_bareground[CLBJ_bareground != 1] = 0
CLBJ_rabet_bareground <- reclassify(CLBJ_bareground, cbind(1, 1))
#plot(CLBJ_rabet_bareground)
CLBJ_rabet_bareground_percent <- aggregate(CLBJ_rabet_bareground, 10, mean)
plot(CLBJ_rabet_bareground_percent)
writeRaster(CLBJ_rabet_bareground_percent, filename = "CLBJ_rabet_bareground_percent.tif")


