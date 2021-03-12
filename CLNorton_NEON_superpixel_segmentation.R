library(raster)
library(RColorBrewer)
library(OpenImageR)
library(SuperpixelImageSegmentation)


setwd("P:/RaBET/Results")
ndwi <- raster("P:/RaBET/Indices/ndwi_rast_neon_19.tif")
pri <- raster("P:/RaBET/Indices/pri_rast_neon_19.tif")
swiri <- raster("P:/RaBET/Indices/swiri_rast_neon_19.tif")
Brick.region <- brick(ndwi,pri,swiri)
print(nrows <- Brick.region@nrows)
print(ncols <- Brick.region@ncols)
#writeRaster(Brick.region, filename = "brickswiriprindwi.tif")
plotRGB(Brick.region, r = 3, g = 2, b = 1, stretch = "lin" )
jpeg("FalseColor.jpg", width = ncols, height = nrows)
dev.off()
getwd()
False.Color <- readImage("FalseColor.jpg")
Region.slic = superpixels(input_image = False.Color, method = "slic", 
                          superpixel = 500,compactness = 30, return_slic_data = TRUE, 
                          return_labels = TRUE, write_slic = "",verbose = FALSE)
#Warning message:
#  The input data has values between 0.000000 and 1.000000. The image-data will be multiplied by the value: 255!
imageShow(Region.slic$slic_data)


NDVI.region <- raster("P:/RaBET/Indices/ndvi_rast_neon_19.tif")
NDVI.region <- brick(NDVI.region)


plot(NDVI.region)
str(NDVI.region)
NDVI.region@ncols
NDVI.mat <- matrix(NDVI.region@data@values,nrow = NDVI.region@nrows,ncol = NDVI.region@nrows, byrow = TRUE)
NDVI.mat
m0 <- min(NDVI.mat)

m0
m1 <- max(NDVI.mat)
m1
NDVI.mat1 <- (NDVI.mat - m0)/(m1 - m0)
imageShow(NDVI.mat)

NDVI.data <- False.Color
NDVI.data[,,1] <- NDVI.mat1
NDVI.data[,,2] <- NDVI.mat1
NDVI.data[,,3] <- NDVI.mat1

NDVI.80 = superpixels(input_image = NDVI.data, method = "slic", superpixel = 15000,
                      compactness = 30, return_slic_data = TRUE,
                      return_labels = TRUE, write_slic = "",
                      verbose = FALSE)
imageShow(NDVI.80$slic_data)


str(NDVI.80)


R0 <- NDVI.80
for (i in 1:nrow(R0$label))
  for (j in 1:ncol(R0$label))
    if (R0$label[i,j] != 0)
      R0$slic_data[i,j,] <- c(255,255,255)
imageShow(R0$slic_data)



Bdry <- NDVI.80
for (i in 1:nrow(Bdry$label))
  for (j in 1:ncol(Bdry$label))
    if (!(Bdry$slic_data[i,j,1] == 0 &
          Bdry$slic_data[i,j,2] == 0 &
          Bdry$slic_data[i,j,3] == 0))
      Bdry$slic_data[i,j,] <- c(255,255,255)
dry.norm <- NormalizeObject(Bdry$slic_data)
imageShow(Bdry$slic_data)





NDVI.means <- make.segments(NDVI.80, mean)
imageShow(NDVI.means$slic_data)


set.seed(123)
NDVI.clus <-
  kmeans(as.vector(NDVI.means$slic_data[,,1]), 5)
vege.class <- matrix(NDVI.clus$cluster,
                     nrow = NDVI.region@nrows,
                     ncol = NDVI.region@ncols, byrow = FALSE)
class.ras <- raster(vege.class, xmn = W,
                    xmx = E, ymn = S, ymx = N, crs =
                      CRS("+proj=utm +zone=10 +ellps=WGS84"))


class.ras <- ratify(class.ras)
rat.class <- levels(class.ras)[[1]]
rat.class$landcover <- c("Water", "Open",
                         +  "Scrub", "Med. Crop", "Dense Crop")
levels(class.ras) <- rat.class
levelplot(class.ras, margin=FALSE,
          +   col.regions= c("blue", "tan",
                             +   "lightgreen", "green", "darkgreen"),
          +    main = "Land Cover Types")


NDVI.rasmns <- raster(NDVI.means$slic_data[,,1],
                      +   xmn = W, xmx = E, ymn = S, ymx = N,
                      +   crs = CRS("+proj=utm +zone=10 +ellps=WGS84"))
NDVI.polymns <- rasterToPolygons(NDVI.rasmns,
                                 +   dissolve = TRUE)
plot(class.ras, col = c("blue", "tan",
                        +   "lightgreen", "green", "darkgreen"),
     +   main = "Land Cover Types", legend = FALSE)
legend("bottom", legend = c("Water", "Open",
                            +   "Scrub", "Med. Crop", "Dense Crop"),
       +   fill = c("blue", "tan", "lightgreen", "green",
                    +   "darkgreen"))
plot(NDVI.polymns, add = TRUE)























NDVI.region <- raster("P:/RaBET/Indices/ndvi_rast_neon_19.tif")
plot(NDVI.region)
brickndvindwiswiri <- brick(NDVI.region, ndwi,swiri)
print(nrows <- brickndvindwiswiri@nrows)
print(ncols <- brickndvindwiswiri@ncols)
#writeRaster(NDVI.region, filename = "NEON.NDVI.jpg")
plot(brickndvindwiswiri)
plot(pri)
plotRGB(brickndvindwiswiri, r = 1, g = 2, b = 3, stretch = "lin" )
jpeg("brickndvindwiswiri.jpg", width = ncols, height = nrows)
brickndvindwiswiri <- readImage("brickndvindwiswiri.jpg")


init = Image_Segmentation$new()

spx = init$spixel_segmentation(input_image = False.Color, 
                               superpixel = 1000, 
                               AP_data = TRUE,
                               use_median = TRUE, 
                               sim_wL = 3, 
                               sim_wA = 10, 
                               sim_wB = 10,
                               sim_color_radius = 10, 
                               verbose = TRUE)


OpenImageR::imageShow(spx$AP_image_data)


