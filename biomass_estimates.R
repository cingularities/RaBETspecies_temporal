#sCRIPT WRITTEN BY CYNTHIA NORTON



SRER_CHM_2021 <- raster("//snow/projects/RaBET/RaBET_species/NEON_FINAL/SRER/NEON_CHM_2021_mask.tif")



#######DRONE LiDAR#########
##############################XYZ DATA PREP######################
gc()
memory.limit(size=100000)
get_lidr_threads()
set_lidr_threads(15)
##Bring las point cloud into Rstudio
drone_points = readTLS(drone)
gc()

##Voxelize to reduce point cloud density to 1 pnt/3 cubic cm
#drone_points_voxel = voxelize_points(drone_points, 0.03)
#plot(points_voxel, color ='Z')
gc()
##Delete the points on periphery of the cloud that are outside of the 'plot'
##This clipping procedure will be based on user defined plot center coordinate (GPS) and radius of plot
#The plot center coordinates and radii are currently found in the file 'Site_overview_SantaRitaMts_October2021.xls
drone_center_coord_easting_x = mean(MBA_points@data$X)
drone_center_coord_northin_y = mean(MBA_points@data$Y)
drone_points_voxel_clip = clip_circle(drone_points, drone_center_coord_easting_x, drone_center_coord_northin_y, 50)
#plot(points_voxel_clip, color = 'Z')
gc()
##Separate trees from ground using lidr
drone_points_voxel_clip@header@VLR <- list()
drone_points_classify = classify_ground(drone_points, algorithm = csf(sloop_smooth = TRUE, class_threshold = 0.1, cloth_resolution =  0.5, rigidness = 2))
#writeLAS(drone_points_voxel_clip,file = "MBA_drone_points_classify.las")
#plot(points_classify, color = 'Classification')
##Make a point cloud file for just ground points
drone_ground_points = filter_poi(drone_points_classify, Classification == 2)
#plot(ground_points)
#Make a point cloud file for just non-ground (trees) points
drone_tree_points = filter_poi(drone_points_classify, Classification != 2)
#plot(tree_points)
##Create a grided digital terrain model from ground points
drone_DTM = grid_terrain(drone_ground_points, res = 0.5, algorithm = knnidw(k = 10, p = 2))
##Calculate vertical distance of each tree point above the DTM. This creates a vegetation height point cloud.
drone_normalized_height = normalize_height(las = drone_tree_points, algorithm = drone_DTM)
##Remove points that are below 0
drone_AGL_clean = filter_poi(drone_normalized_height, Z >= 0.1, Z <= 40)
plot(drone_AGL_clean)
#AGL_clean_understory = filter_poi(AGL_clean, Z >= 1, Z <= 15)
#AGL_clean_overstory = filter_poi(AGL_clean, Z >= 15, Z  <= 40)
#plot(AGL_clean_understory, color = 'Z')
#plot(AGL_clean_overstory, color = 'Z')
drone_AGL_clean@header@VLR <- list()
##Export normalized point cloud to .las
#writeLAS(drone_AGL_clean, file = "drone_AGL_clean_mtbA.las")##Create canopy height model raster. I don't think this is totally necessary. 
drone_CHM = rasterize_canopy(drone_normalized_height, res = 0.5)
windows()
plot(drone_CHM)
grid(NULL,NULL)
points(MBA_tree.points.ground_df$Y ~ MBA_tree.points.ground_df$X, col = "red", pch = 16)

#(drone_CHM, file = 'drone_CHM.tif')
points(MBA_myTreeGraph)
crs(drone_CHM) <- "+proj=utm +zone=12N +ellps=WGS84" 


#The eigen method seems to find more tree occurrences than the hough method
drone_trees = locate_trees(drone_AGL_clean, lmf(ws=2.5, shape = "circular")) %>% rename(TreeID = treeID)
drone_trees_df = data.frame(st_coordinates(drone_trees[,3]))
plot(drone_trees)
#drone_myTreeLocs = get_raster_eigen_treelocs(las = drone_AGL_clean)


drone_treeseg_dalponte= segment_trees(drone_AGL_clean, dalponte2016(chm = drone_CHM, treetops = drone_trees, th_tree = 1.5, th_seed = 0.20,
                                                                    th_cr = 0.20, max_cr = 35, ID = "TreeID"))
drone_clean_dat <- drone_treeseg_dalponte[drone_treeseg_dalponte$treeID %in% names(which(table(drone_treeseg_dalponte$treeID) > 5)),]
drone_metric = tree_metrics(drone_clean_dat, .stdtreemetrics)
drone_hulls4  = delineate_crowns(drone_clean_dat,attribute="treeID")
drone_hulls4@data = dplyr::left_join(drone_hulls4@data, drone_clean_dat@data)
windows()

MTA_trueCentroids = gCentroid(drone_hulls4,byid=TRUE)
plot(drone_hulls4)
points(coordinates(drone_hulls4),pch=1)
points(drone_hulls4,pch=2)

windows()
plot(drone_hulls4)
points(MBA_merged_trees_ITG_inv$Y ~ MBA_merged_trees_ITG_inv$X, col = "green", pch = 16)
points(drone_trees_df$Y ~ drone_trees_df$X, col = "blue", pch = 16)
points(MBA_tree.points.ground_df$Y ~ MBA_tree.points.ground_df$X, col = "red", pch = 16)


windows()
plot(drone_trees_df$Y ~ drone_trees_df$X, col = "blue", pch = 16)
points(MBA_tree.points.ground_df$Y ~ MBA_tree.points.ground_df$X, col = "red", pch = 16)
points(MBA_merged_trees_ITG_inv$Y ~ MBA_merged_trees_ITG_inv$X, col = "green", pch = 16)

windows()
plot(drone_hulls4)
points(MBA_tree.points.ground_df$Y ~ MBA_tree.points.ground_df$X, col = "red", pch = 16)



windows()
plot(drone_hulls4)
points(MBA_merged_trees_ITG_inv$Y ~ MBA_merged_trees_ITG_inv$X, col = "green", pch = 16)




##Plot AGL_clean with tree points and tree IDs
x = plot(drone_AGL_clean, color = "Z")
add_treeMap(x, MBA_myTreeGraph, color = 'yellow', size = 3)
add_TreeIDs(x, MBA_merged_trees, color='yellow', cex=2)





CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")  







##CHM for loop
NEONpoints2017 = list.files("//snow/projects/RaBET/RaBET_species/NEON_SRER/2017/lidar",pattern="*.laz$", full.names=TRUE)
NEONpoints2018 = list.files("//snow/projects/RaBET/RaBET_species/NEON_WGER/lidar/NEON_WGER_2018_ARS",pattern="*.laz$", full.names=TRUE)
NEONpoints2019 = list.files("//snow/projects/RaBET/RaBET_species/NEON_SRER/2019/lidar",pattern="*.laz$", full.names=TRUE)
NEONpoints2020 = list.files("//snow/projects/RaBET/RaBET_species/NEON_SRER/2020/lidar",pattern="*.laz$", full.names=TRUE)
NEONpoints2021 = list.files("//snow/projects/RaBET/RaBET_species/NEON_SRER/2021/lidar",pattern="*.laz$", full.names=TRUE)
SRER  = list.files('//snow/projects/RaBET/RaBET_species/NEON_SRER/lidar/NEON_SRER_2018_ARS',pattern="*.laz$", full.names=TRUE)
SRER_22  = list.files('//snow/projects/RaBET/RaBET_species/NEON_SRER/lidar/RaBET_SRER_2022_ARS',pattern="*.las$", full.names=TRUE)


for(fileName in NEONpoints2018){
  #Bring point cloud into Rstudio
  #Identify ground points using a cloth simulation filter algorithm
  
  NEONpoints <-readLAS(fileName)
  
  #Calculate vegetation height above the ground
  NEON_DSM = normalize_height(NEONpoints, algorithm = knnidw(k = 12, p = 3), na.rm = TRUE)
  
  #Remove points that are below 0
  NEON_DSM_clean = filter_poi(NEON_DSM, Z < 45)
  
  
  #plot(NEON_DSM_clean)
  NEON_DTM = grid_terrain(NEONpoints, res = 0.5, algorithm = knnidw(k = 12, p = 2))
  
  #Create canopy height model(raster) from the normalized canopy points
  #NEON_CHM = grid_canopy(NEON_DSM_clean, res = 0.5, p2r(subcircle = 0.3, na.fill = NULL))
  #windows()
  
  
  #plot(NEON_DSM_clean)
  
  #Create canopy height model(raster) from the normalized canopy points
  
  #Export the Canopy Height Model to a .tif
  writeRaster(NEON_DTM,   
              filename=paste("//snow/projects/RaBET/RaBET_species/NEON_WGER/lidar/NEON_WGER_2018_ARS/",substr(fileName, 90, 1000), "_2018_DTM"),
              format="GTiff",
              overwrite=TRUE)
}






CHM2018 <- list.files(path = "//snow/projects/RaBET/RaBET_species/NEON_SRER/lidar", pattern = ".tif$", full.names = TRUE)
CHM2018 <- lapply(CHM2018, raster)
CHM2018final <- do.call(merge, c(CHM2018, tolerance = 1))
writeRaster(CHM2018final,
            file= "CHM2018final_SRER",
            format="GTiff",
            overwrite=TRUE)