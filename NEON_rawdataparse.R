library(readxl)
library(geosphere)
library(tidyverse)
library(plotrix)
library(sp)


##RAW GROUND DATA
coor <- c(51.079167, 10.453)
tree23 <- destPoint(coor,110, 76)
plotcenter <- destPoint(tree23,98, 28.1)
MOAB_25 <- c(-109.344632,38.26229)
ground_moab <- read.csv("D:/projects/RaBET/NEON_MOAB/ground_data.csv")
###FUNCTION 3 - ground data azimuth and distance to coordinates###
tls_ground_coor <- function(coor, dist_azim) {
  
  coor_list <- list() #creates empty list for new coordinates
  
  for (i in 1:nrow(dist_azim)) {
    stemAzimuth <- dist_azim$Azimuth[i]
    stemDistance <- dist_azim$Distance[i]
    
    new_coordinates <- destPoint(coor, stemAzimuth,  stemDistance) %>%
      as.data.frame() %>% 
      add_column(treeID = dist_azim$TreeID[i]) %>%
      add_column(stemAzimuth = stemAzimuth) %>%
      add_column(stemDistance = stemDistance) %>%
      add_column(species = dist_azim$Species[i]) %>%
      add_column(Height = dist_azim$Height[i])%>%
      add_column(DBH = dist_azim$DBH[i]) 
    
    
    coor_list[[i]] <- new_coordinates #put result of loop in list
    
  }
  
  coor_df<- coor_list %>% bind_rows() %>% na.omit() 
  
  
  return(coor_df)
}

Hainich_plot <- tls_ground_coor(coor, ground) 
moab_plot <- tls_ground_coor(MOAB_25,ground_moab)
write.csv(tree.points.ground, file = "ground_validation_moab_utm.csv")
setwd("D:/projects/RaBET/NEON_MOAB/")

###tree_ring_ground_spatialpoints 1
xy_ground <- data.frame(treeID = moab_plot$species, X = moab_plot$lon, Y = moab_plot$lat) ##CONVERTLATTOLONG
coordinates(xy_ground) <- ~ X + Y
proj4string(xy_ground) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")  ## for example
tree.points.ground <- spTransform(xy_ground, CRS("+proj=utm +zone=12 +ellps=WGS84")) 

writeOGR(obj=tree.points.ground, dsn="D:/projects/RaBET/NEON_MOAB", layer="ground_validation_moab_utm.csv", driver="ESRI Shapefile")
