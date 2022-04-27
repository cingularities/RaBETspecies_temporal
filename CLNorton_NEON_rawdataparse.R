library(readxl)
library(geosphere)
library(tidyverse)
library(plotrix)
library(sp)


setwd("D:/projects/RaBET/NEON_ONAQ/")

plot_coor_raw <- dir("D:/projects/RaBET/NEON_struct-plant/ONAQ/plot_coor/",full.names=T, pattern = "*.csv")
dist_azim_raw <- dir("D:/projects/RaBET/NEON_struct-plant/ONAQ/dist_azim/",full.names=T, pattern = "*.csv")


#ground validation
neon_ground_coor <- function(plot_coor_raw,dist_azim_raw) {
  #read plot coordinate
  plot_coor <- plot_coor_raw %>% map_dfr(~read_csv(.x) %>% select(c(plotID,decimalLatitude,decimalLongitude))) 
  #read distance, azimuth, and species
  dist_azim <- dist_azim_raw  %>% map_dfr(~read_csv(.x) %>% select(c(plotID,pointID,stemAzimuth,stemDistance,scientificName))) 
  
  #merge and clean data sets
  plot_coor_select <- plot_coor %>% select(c(plotID,decimalLatitude,decimalLongitude)) %>%
  left_join(dist_azim, by = c("plotID"))%>%
  select(c(plotID,pointID,decimalLatitude,decimalLongitude,stemAzimuth,stemDistance,scientificName))%>% 
  distinct()%>%
  na.omit()

  coor_list <- list() #creates empty list for new coordinates
  
  #create a loop to GET all populated coordinates based on azimuth and distance 
  for (i in 1:nrow(plot_coor_select)) {
    
    decLon <- plot_coor_select$decimalLongitude[i] 
    decLat <- plot_coor_select$decimalLatitude[i]
    stemAzimuth <- plot_coor_select$stemAzimuth[i]
    stemDistance <- plot_coor_select$stemDistance[i]
    
    coord <- c(decLon,decLat)
    new_coordinates <- destPoint(coord, stemAzimuth , stemDistance) %>% as.data.frame() %>%
      add_column(plotID = plot_coor_select$plotID[i]) %>% add_column(pointID =plot_coor_select$pointID[i]) %>%
      add_column(orig_decLat = decLat) %>% add_column(orig_decLon = decLon) %>% add_column(stemAzimuth = stemAzimuth) %>%
      add_column(stemDistance = stemDistance) %>% add_column(species = plot_coor_select$scientificName[i]) 
      

    coor_list[[i]] <- new_coordinates #put result of loop in the list created above one siteID at a time
    
  }
  
  coor_df<- coor_list %>% bind_rows() 

  return(coor_df)
}



ONAQ_val_point_coor <- neon_ground_coor(plot_coor_raw, dist_azim_raw)

write.csv(ONAQ_val_point_coor, file = "ONAQ_val_point_coor.csv")

##populate coordinates using world -> WGS84 in ARCmap













###PROJECTION AND SHAPEFILE CREATION WOKRING CODE 
### Get long and lat from your data.frame. Make sure that the order is in lon/lat.
xy <- CLBJ_val_point_coor[,c(1,2)]

spdf <- SpatialPointsDataFrame(coords = xy, data = CLBJ_val_point_coor,
                               proj4string = CRS("+proj=longlat +zone=14N +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))


##NOT PROJECTING IN SAME AREA AS RASTER, NEED EDITING
CLBJ<-raster("P:/RaBET/Results/NEON_2021_CLBJ_ndvi_ndwi_ci.tif")
crs(CLBJ)
projection(CLBJ)

CLBJ_val_point_coor_points <- SpatialPointsDataFrame(CLBJ_val_point_coor[,c("lat","lon")],CLBJ_val_point_coor,
    proj4string = CRS("+proj=utm +zone=14N+south=T ellps=WGS84 +datum=WGS84 +units=m +no_defs"))

CLBJ_val_point_coor_transform <- spTransform(CLBJ_val_point_coor_points, CRS("+proj=utm +zone=14 +datum=WGS84 +units=m +no_defs"))
CRS(CLBJ_val_point_coor_transform)
plot(CLBJ_val_point_coor_points)

writeOGR(obj=spdf, dsn="P:/RaBET/Texas", layer="CLBJ_val_point_coor_points_spdf", driver="ESRI Shapefile", overwrite_layer=TRUE) #also you were missing the driver argument
