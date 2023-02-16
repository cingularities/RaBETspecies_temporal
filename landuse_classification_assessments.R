#Script written by Cynthia Norton
# load the raster and rgdal libraries
library(raster)
library(rgdal)
library(dplyr)
library(exactextractr)
library(sf)
library(tidyr)
library(terra)
library(tibble)
library(reshape2)
library(ggplot2)
library(ExtractTrainData)
library(rLiDAR)
library("ForestTools")


setwd("//gaea/projects/RaBET/RaBET_species/landuse/")
options(scipen = 100, digits = 4)

# read data    
shp <- shapefile("//gaea/projects/RaBET/RaBET_species/landuse/results/SRER_pastures.shp")
shp_df <-shp@polygons 
csv <- read.csv("//gaea/projects/RaBET/RaBET_species/landuse/datasets/pasture_summaries.csv")
tif <- raster("//snow/projects/RaBET/RaBET_species/RESULTS/final_results/Classification/RF_allyr_SRER.tif")
stocking_csv <- read.csv("//gaea/projects/RaBET/RaBET_species/landuse/datasets/Yearly Utilization.csv") %>% 
  group_by(Pasture) %>% 
  summarise_all(sum) %>%  
  mutate(sumVar = rowSums(.[2:14], na.rm = TRUE))%>%
  as.data.frame()%>%
  dplyr::select(Pasture, sumVar)
CHM <- raster("//snow/projects/RaBET/RaBET_species/RESULTS/SRER/NEON_CHM_2018_mask_SRER.tif")
DTM<-raster("//snow/projects/RaBET/RaBET_species/RESULTS/SRER/FinalOutputs/NEON_DTM_2021_mask.tif")
pasture <- shapefile("//gaea/projects/RaBET/RaBET_species/landuse/datasets/pasture.shp")
pasture_elev_SRER <- shapefile("//gaea/projects/RaBET/RaBET_species/landuse/datasets/pasture_elev_srer_union.shp")
dtm_classified<-rast("//gaea/projects/RaBET/RaBET_species/landuse/results/DTM_classified_2021_SRER.tif")
pasture_elev_SRER_df <- pasture_elev_SRER %>% as.data.frame() %>% rownames_to_column() %>% rename(FID = rowname)%>% mutate_at('FID', as.integer)

# merge on common variable, here called 'key'
merge <- merge(shp, csv, by='pasture')

# perhaps save as shapefile again
#e <- extract(tif, shp)


# create classification matrix
reclass_df <- c(864, 950, 1,
                950, 1000, 2,
                1000, 1100, 3,
                1100,1200, 4,
                1200,1300,5,
                1300,1400,6,
                1400,Inf,7)


# reshape the object into a matrix with columns and rows
reclass_m <- matrix(reclass_df,
                    ncol = 3,
                    byrow = TRUE)
reclass_m


# reclassify the raster using the reclass object - reclass_m
dtm_classified <- reclassify(DTM,
                             reclass_m)


p<- as.polygons(dtm_classified == 4)
writeVector(p, "//gaea/projects/RaBET/RaBET_species/grazing/Temp/highelevation_srer.gpkg", filetype="ESRI Shapefile",
            overwrite=TRUE, options="ENCODING=UTF-8")






##Species frequencies###
#extract pixel frequencies
freq_species <- exact_extract(tif, pasture_elev_SRER, function(value, coverage_fraction) {
  data.frame(value = value,
             frac = sum(coverage_fraction)) %>%
    group_by(value) %>%
    summarize(freq = sum(frac), .groups = 'drop') %>%
    pivot_wider(names_from = 'value',
                names_prefix = 'freq_',
                values_from = 'freq')
}) %>% 
  mutate(across(starts_with('freq'), replace_na, 0)) 
##rename rows and columns

freq_species_fin <- freq_species %>% rownames_to_column() %>% rename(FID = rowname, 
                                                                          bareground=freq_1, 
                                                                          grass = freq_2, 
                                                                          mesquite = freq_3,
                                                                          cactus = freq_4,
                                                                          lotebush = freq_5,
                                                                          paloverde = freq_6,
                                                                          creosote = freq_7,
                                                                          noclass = freq_NA) %>% mutate_at('FID', as.integer) %>% 
  mutate(total = rowSums(.[2:8]))%>%
  transform(bareground = (bareground/total)*100,
            grass = (grass/total)*100,
            mesquite = (mesquite/total)*100,
            cactus = (cactus/total)*100,
            lotebush = (lotebush/total)*100,
            paloverde = (paloverde/total)*100,
            creosote = (creosote/total)*100) %>%
  left_join(pasture_elev_SRER_df, by = "FID")  %>% select(-noclass,-total,-bareground,-grass)%>%
  mutate(grazing = ifelse(AUY_per_ha > 100, "1",'2')) #with or without grazing


mutate(grazing = ifelse(AUY_per_ha > 850, '3', ifelse(AUY_per_ha < 270, "1",'2'))) #low, med, high
mutate(grazing = ifelse(AUY_per_ha > 1, "1",'2')) #with or without grazing

#write.csv(freq_species_fin, file = 'species_freq.csv')




low_elev <- freq_species_fin %>% filter(layer == 1)
midlow_elev <- freq_species_fin %>% filter(layer == 2)
midhigh_elev <- freq_species_fin %>% filter(layer == 3)
high_elev <- freq_species_fin %>% filter(layer == 4)


low_elev_melt <- low_elev %>% melt(id.var = c('layer','FID_pastur','pasture','AUY_per_ha','FID_elevat','grazing',"FID"),
                                   variable.name = 'species')%>% select(layer,species,value,grazing) %>% filter(layer != 0)

midlow_elev_melt <- midlow_elev %>% melt(id.var = c('layer','FID_pastur','pasture','AUY_per_ha','FID_elevat','grazing',"FID"),
                                         variable.name = 'species')%>% select(layer,species,value,grazing) %>% filter(layer != 0)


midhigh_elev_melt <- midhigh_elev %>% melt(id.var = c('layer','FID_pastur','pasture','AUY_per_ha','FID_elevat','grazing',"FID"),
                                         variable.name = 'species')%>% select(layer,species,value,grazing) %>% filter(layer != 0)

high_elev_melt <- high_elev %>% melt(id.var = c('layer','FID_pastur','pasture','AUY_per_ha','FID_elevat','grazing',"FID"),
                                           variable.name = 'species')%>% select(layer,species,value,grazing) %>% filter(layer != 0)


windows()
ggplot(low_elev_melt, aes(x = grazing, y =value, fill = species)) + ylab('Percent % Cover ') + coord_cartesian(ylim = c(0, 16)) +
  stat_summary(fun = "mean", geom = "bar", position = "dodge")+
  scale_fill_manual(values=c('dark green', 'dark grey', 'dark blue','pink','brown'))+ scale_x_discrete(labels= c("AUY < 100", "AUY > 100"))

windows()
ggplot(midlow_elev_melt, aes(x = grazing, y =value, fill = species)) + ylab('Percent % Cover ') +coord_cartesian(ylim = c(0, 16)) +
  stat_summary(fun = "mean", geom = "bar", position = "dodge") +
  scale_fill_manual(values=c('dark green', 'dark grey', 'dark blue','pink','brown'))+ scale_x_discrete(labels= c("AUY < 100", "AUY > 100"))

windows()
ggplot(midhigh_elev_melt, aes(x = grazing, y =value, fill = species)) + ylab('Percent % Cover ') +coord_cartesian(ylim = c(0, 16)) +
  stat_summary(fun = "mean", geom = "bar", position = "dodge") +
  scale_fill_manual(values=c('dark green', 'dark grey', 'dark blue','pink','brown'))+ scale_x_discrete(labels= c("AUY < 100", "AUY > 100"))

windows()
ggplot(high_elev_melt, aes(x = grazing, y =value, fill = species)) + ylab('Percent % Cover ') +coord_cartesian(ylim = c(0, 16)) +
  stat_summary(fun = "mean", geom = "bar", position = "dodge") +
  scale_fill_manual(values=c('dark green', 'dark grey', 'dark blue','pink','brown'))+ scale_x_discrete(labels= c("AUY < 100", "AUY > 100"))





##CHM and species###
creosote <- tif
creosote[creosote != 7] = NA
creosote_project<- projectRaster(creosote,CHM)
creosote_chm <- mask(CHM, creosote_project)
plot(creosote_chm)
plot(creosote_project)

paloverde <- tif
paloverde[paloverde != 6] = NA
paloverde_project<- projectRaster(paloverde,CHM)
paloverde_chm <- mask(CHM, paloverde_project)


lotebush <- tif
lotebush[lotebush != 5] = NA
lotebush_project<- projectRaster(lotebush,CHM)
lotebush_chm <- mask(CHM, lotebush_project)

cactus <- tif
cactus[cactus != 4] = NA
cactus_project<- projectRaster(cactus,CHM)
cactus_chm <- mask(CHM, cactus_project)

mesquite <- tif
mesquite[mesquite != 3] = NA
mesquite_project<- projectRaster(mesquite,CHM)
mesquite_chm <- mask(CHM, mesquite_project)

# extract the data
chm_creosote_mean <- exact_extract(creosote_chm, pasture_elev_SRER, 'mean') %>% 
  as.data.frame() %>%  
  rownames_to_column() %>% 
  rename(FID = 1, creosote = 2) %>% 
  transform(FID = as.numeric(FID))
chm_paloverde_mean <- exact_extract(paloverde_chm, pasture_elev_SRER, 'mean') %>% 
  as.data.frame()  %>% 
  rownames_to_column() %>% 
  rename(FID = 1, paloverde = 2)%>% 
  transform(FID = as.numeric(FID))
chm_lotebush_mean <- exact_extract(lotebush_chm, pasture_elev_SRER, 'mean') %>% 
  as.data.frame()  %>% 
  rownames_to_column() %>% 
  rename(FID = 1, lotebush = 2)%>% 
  transform(FID = as.numeric(FID))
chm_cactus_mean <- exact_extract(cactus_chm, pasture_elev_SRER, 'mean') %>% 
  as.data.frame()  %>% 
  rownames_to_column() %>% 
  rename(FID = 1, cactus = 2)%>% 
  transform(FID = as.numeric(FID))
chm_mesquite_mean <- exact_extract(mesquite_chm, pasture_elev_SRER, 'mean') %>%
  as.data.frame()  %>% 
  rownames_to_column() %>% 
  rename(FID = 1, mesquite = 2)%>% 
  transform(FID = as.numeric(FID))


pasture_elev_SRER_df$FID <- as.numeric(pasture_elev_SRER_df$FID)
chm_species_mean_final <- chm_mesquite_mean %>% 
  left_join(chm_cactus_mean, by = "FID") %>%
  left_join(chm_lotebush_mean, by = "FID") %>%
  left_join(chm_paloverde_mean, by = "FID") %>%
  left_join(chm_creosote_mean, by = "FID") %>% 
  left_join(pasture_elev_SRER_df, by = "FID")
  










low_elev_chm <- chm_species_mean_final %>% filter(layer == 1)
midlow_elev_chm <- chm_species_mean_final %>% filter(layer == 2)
midhigh_elev_chm <- chm_species_mean_final %>% filter(layer == 3)
high_elev_chm <- chm_species_mean_final %>% filter(layer == 4)


low_elev_chm_melt <- low_elev_chm %>% 
  melt(id.var = c('layer','FID_pastur','pasture','AUY_per_ha','FID_elevat',"FID"),variable.name = 'species')%>% 
  select(layer,species,value,AUY_per_ha) %>% 
  filter(layer != 0)%>%
  mutate(grazing = ifelse(AUY_per_ha > 100, "1",'2')) #with or without grazing


midlow_elev_chm_melt <- midlow_elev_chm %>% 
  melt(id.var = c('layer','FID_pastur','pasture','AUY_per_ha','FID_elevat',"FID"),variable.name = 'species')%>% 
  select(layer,species,value,AUY_per_ha) %>% 
  filter(layer != 0)%>%
  mutate(grazing = ifelse(AUY_per_ha > 100, "1",'2')) #with or without grazing

midhigh_elev_chm_melt <- midhigh_elev_chm %>% 
  melt(id.var = c('layer','FID_pastur','pasture','AUY_per_ha','FID_elevat',"FID"),variable.name = 'species')%>% 
  select(layer,species,value,AUY_per_ha) %>% 
  filter(layer != 0)%>%
  mutate(grazing = ifelse(AUY_per_ha > 100, "1",'2')) #with or without grazing

high_elev_chm_melt <- high_elev_chm %>% 
  melt(id.var = c('layer','FID_pastur','pasture','AUY_per_ha','FID_elevat',"FID"),variable.name = 'species')%>% 
  select(layer,species,value,AUY_per_ha) %>% 
  filter(layer != 0)%>%
  mutate(grazing = ifelse(AUY_per_ha > 100, "1",'2')) #with or without grazing


windows()
ggplot(low_elev_chm_melt, aes(x = grazing, y =value, fill = species)) + ylab('Percent % Cover ') + coord_cartesian(ylim = c(0, 4)) +
  stat_summary(fun = "mean", geom = "bar", position = "dodge")+
  scale_fill_manual(values=c('dark green', 'dark grey', 'dark blue','pink','brown'))+ scale_x_discrete(labels= c("AUY < 100", "AUY > 100"))

windows()
ggplot(midlow_elev_chm_melt, aes(x = grazing, y =value, fill = species)) + ylab('Percent % Cover ') +coord_cartesian(ylim = c(0, 4)) +
  stat_summary(fun = "mean", geom = "bar", position = "dodge") +
  scale_fill_manual(values=c('dark green', 'dark grey', 'dark blue','pink','brown'))+ scale_x_discrete(labels= c("AUY < 100", "AUY > 100"))

windows()
ggplot(midhigh_elev_chm_melt, aes(x = grazing, y =value, fill = species)) + ylab('Percent % Cover ') +coord_cartesian(ylim = c(0, 4)) +
  stat_summary(fun = "mean", geom = "bar", position = "dodge") +
  scale_fill_manual(values=c('dark green', 'dark grey', 'dark blue','pink','brown'))+ scale_x_discrete(labels= c("AUY < 100", "AUY > 100"))

windows()
ggplot(high_elev_chm_melt, aes(x = grazing, y =value, fill = species)) + ylab('Percent % Cover ') +coord_cartesian(ylim = c(0, 4)) +
  stat_summary(fun = "mean", geom = "bar", position = "dodge") +
  scale_fill_manual(values=c('dark green', 'dark grey', 'dark blue','pink','brown'))+ scale_x_discrete(labels= c("AUY < 100", "AUY > 100"))








###crownarea
lin <- function(x){x * 0.05 + 0.6}

ttops <- vwf(CHM = mesquite_chm, winFun = lin, minHeight = 2)
plot(kootenayCHM, xlab = "", ylab = "", xaxt='n', yaxt = 'n')

plot(ttops, col = "blue", pch = 20, cex = 0.5, add = TRUE)

mean(ttops$height)
crowns <- mcws(treetops = ttops, CHM = kootenayCHM, minHeight = 1.5, verbose = FALSE)
plot(crowns, col = sample(rainbow(50), length(unique(crowns[])), replace = TRUE), legend = FALSE, xlab = "", ylab = "", xaxt='n', yaxt = 'n')
crownsPoly <- mcws(treetops = ttops, CHM = kootenayCHM, format = "polygons", minHeight = 1.5, verbose = FALSE)
plot(kootenayCHM, xlab = "", ylab = "", xaxt='n', yaxt = 'n')
plot(crownsPoly, border = "blue", lwd = 0.5, add = TRUE)
# Compute average crown diameter
crownsPoly[["crownDiameter"]] <- sqrt(crownsPoly[["crownArea"]]/ pi) * 2

# Mean crown diameter
mean(crownsPoly$crownDiameter)
sp_summarise(crownsPoly, variables = c("crownArea", "height"))










##UNION i nR
sptemp %>% 
  group_by(ID_2) %>%
  summarise(geometry = sf::st_union(geometry)) %>%
  ungroup()

require(sp)

zips <- readOGR("cb_2015_us_zcta510_500k.shp")
states <- readOGR("states.geojson", "OGRGeoJSON")

proj4string(states)
proj4string(zips)


MN <- states[states@data$name =='Minnesota', ]
plot(MN)


zips_subset <- zips[MN,]

plot(zips_subset)
plot(MN, border="green", lwd = 3, add = TRUE)
# Required packages
libs <- c("rgdal", "maptools", "gridExtra")
lapply(libs, require, character.only = TRUE)


# Import Oregon census data
oregon <- readOGR(dsn = "path/to/data", layer = "orcounty")
oregon.coords <- coordinates(oregon)


# Generate IDs for grouping
oregon.id <- cut(oregon.coords[,1], quantile(oregon.coords[,1]), include.lowest=TRUE)

# Merge polygons by ID
oregon.union <- unionSpatialPolygons(oregon, oregon.id)

# Plotting
plot(oregon)
plot(oregon.union, add = TRUE, border = "red", lwd = 2)


# Convert SpatialPolygons to data frame
oregon.df <- as(oregon, "data.frame")

# Aggregate and sum desired data attributes by ID list
oregon.df.agg <- aggregate(oregon.df[, 6:8], list(oregon.id), sum)
row.names(oregon.df.agg) <- as.character(oregon.df.agg$Group.1)


# Reconvert data frame to SpatialPolygons
oregon.shp.agg <- SpatialPolygonsDataFrame(oregon.union, oregon.df.agg)

# Plotting
grid.arrange(spplot(oregon, "AREA", main = "Oregon: original county area"), 
             spplot(oregon.shp.agg, "AREA", main = "Oregon: aggregated county area"), ncol = 1)
