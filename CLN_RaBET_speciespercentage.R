library(raster)
setwd("P:/RaBET/Outputs/final_results/percentages/")
JORN_rabet <- raster('P:/RaBET/Outputs/final_results/speciesspecific_RF_041122_JORN.tif')
SRER_rabet <- raster('P:/RaBET/Outputs/final_results/RF_allyr_SRER.tif')
CLBJ_rabet <- raster('P:/RaBET/Outputs/final_results/speciesspecific_RF_122021_CLBJ_96OA.tif')



SRER_woody <- SRER_rabet
SRER_woody[SRER_woody ==  1] = 0
SRER_woody[SRER_woody ==  2] = 0
SRER_woody[SRER_woody ==  3] = 1
SRER_woody[SRER_woody ==  4] = 1
SRER_woody[SRER_woody ==  5] = 1
SRER_woody[SRER_woody ==  6] = 1
SRER_woody[SRER_woody ==  7] = 1
plot(SRER_woody)
writeRaster(SRER_woody, file = "SRER_woody.tif")

SRER_rabet_woody <- reclassify(SRER_woody, cbind(7, 1))
#plot(SRER_rabet_woody)
SRER_rabet_woody_percent <- aggregate(SRER_rabet_woody, 10, mean)
SRER_rabet_woody_percent[SRER_rabet_woody_percent <= 0] <- NA
plot(SRER_rabet_woody_percent)
#writeRaster(SRER_rabet_woody_percent, filename = "SRER_rabet_woody_percent.tif")


SRER_paloverde <- SRER_rabet
SRER_paloverde[SRER_paloverde != 7] = 0
SRER_rabet_paloverde <- reclassify(SRER_paloverde, cbind(7, 1))
#plot(SRER_rabet_paloverde)
SRER_rabet_paloverde_percent <- aggregate(SRER_rabet_paloverde, 10, mean)
SRER_rabet_paloverde_percent[SRER_rabet_paloverde_percent <= 0] <- NA
plot(SRER_rabet_paloverde_percent)
#writeRaster(SRER_rabet_paloverde_percent, filename = "SRER_rabet_paloverde_percent.tif")


SRER_catclaw <- SRER_rabet
SRER_catclaw[SRER_catclaw != 6] = 0
SRER_rabet_catclaw <- reclassify(SRER_catclaw, cbind(6, 1))
#plot(SRER_rabet_catclaw)
SRER_rabet_catclaw_percent <- aggregate(SRER_rabet_catclaw, 10, mean)
SRER_rabet_catclaw_percent[SRER_rabet_catclaw_percent <= 0] <- NA
plot(SRER_rabet_catclaw_percent)
#writeRaster(SRER_rabet_catclaw_percent, filename = "SRER_rabet_catclaw_percent.tif")


SRER_lotebush <- SRER_rabet
SRER_lotebush[SRER_lotebush != 5] = 0
SRER_rabet_lotebush <- reclassify(SRER_lotebush, cbind(5, 1))
#plot(SRER_rabet_lotebush)
SRER_rabet_lotebush_percent <- aggregate(SRER_rabet_lotebush, 10, mean)
SRER_rabet_lotebush_percent[SRER_rabet_lotebush_percent <= 0] <- NA
plot(SRER_rabet_lotebush_percent)
#writeRaster(SRER_rabet_lotebush_percent, filename = "SRER_rabet_lotebush_percent.tif")



SRER_cactus <- SRER_rabet
SRER_cactus[SRER_cactus != 4] = 0
SRER_rabet_cactus <- reclassify(SRER_cactus, cbind(4, 1))
#plot(SRER_rabet_cactus)
SRER_rabet_cactus_percent <- aggregate(SRER_rabet_cactus, 10, mean)
SRER_rabet_cactus_percent[SRER_rabet_cactus_percent <= 0] <- NA
plot(SRER_rabet_cactus_percent)
#writeRaster(SRER_rabet_cactus_percent, filename = "SRER_rabet_cactus_percent.tif")


SRER_mesquite <- SRER_rabet
SRER_mesquite[SRER_mesquite != 3] = 0
SRER_rabet_mesquite <- reclassify(SRER_mesquite, cbind(3, 1))
#plot(SRER_rabet_mesquite)
SRER_rabet_mesquite_percent <- aggregate(SRER_rabet_mesquite, 10, mean)
SRER_rabet_mesquite_percent[SRER_rabet_mesquite_percent <= 0] <- NA
plot(SRER_rabet_mesquite_percent)
#writeRaster(SRER_rabet_mesquite_percent, filename = "SRER_rabet_mesquite_percent.tif")


SRER_grass <- SRER_rabet
SRER_grass[SRER_grass != 2] = 0
SRER_rabet_grass <- reclassify(SRER_grass, cbind(2, 1))
#plot(SRER_rabet_grass)
SRER_rabet_grass_percent <- aggregate(SRER_rabet_species2, 10, mean)
plot(SRER_rabet_grass_percent)
#writeRaster(SRER_rabet_grass_percent, filename = "SRER_rabet_grass_percent.tif")


SRER_bareground <- SRER_rabet
SRER_bareground[SRER_bareground != 1] = 0
SRER_rabet_bareground <- reclassify(SRER_bareground, cbind(1, 1))
#plot(SRER_rabet_bareground)
SRER_rabet_bareground_percent <- aggregate(SRER_rabet_species1, 10, mean)
plot(SRER_rabet_bareground_percent)
#writeRaster(SRER_rabet_bareground_percent, filename = "SRER_rabet_bareground_percent.tif")



SRER_stack <- stack(SRER_rabet_paloverde_percent,
                    SRER_rabet_catclaw_percent,
                    SRER_rabet_lotebush_percent, 
                    SRER_rabet_cactus_percent,
                    SRER_rabet_mesquite_percent)

names(SRER_stack) <- c("paloverde","catclaw","lotebush","cactus","mesquite")

plot(SRER_stack)
writeRaster(SRER_stack, filename = "SRER_rabet_percent_stacked.tif",overwrite=TRUE)














###FUNCTION
species_percentage <- function(classified_image, class_num,aggregate_num) {
class <- classified_image
class[class != class_num] = 0
class_reclass <- reclassify(class, cbind(class_num, 1))
#plot(JORN_rabet_grass)
class_reclass_aggregate <- class_reclassaggregate(JORN_rabet_species2, aggregate_num, mean)
plot(class_reclass_aggregate)
return(class_reclass_aggregate)
}

SRER_mesquite_percent <- species_percentage(SRER_rabet, 3,10)


writeRaster(class_reclass_aggregate, filename = "class_reclass_aggregate")













SRER_grass <- SRER_rabet
SRER_grass[SRER_grass != 2] = 0
SRER_rabet_grass <- reclassify(SRER_grass, cbind(2, 1))
#plot(SRER_rabet_grass)
SRER_rabet_grass_percent <- aggregate(SRER_rabet_grass, 10, mean)
plot(SRER_rabet_grass_percent)
writeRaster(SRER_rabet_grass_percent, filename = "SRER_rabet_grass_percent.tif")


SRER_bareground <- SRER_rabet
SRER_bareground[SRER_bareground != 1] = 0
SRER_rabet_bareground <- reclassify(SRER_bareground, cbind(1, 1))
#plot(SRER_rabet_bareground)
SRER_rabet_bareground_percent <- aggregate(SRER_rabet_bareground, 10, mean)
plot(SRER_rabet_bareground_percent)
writeRaster(SRER_rabet_bareground_percent, filename = "SRER_rabet_bareground_percent.tif")


