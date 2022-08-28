# install neonUtilities - can skip if already installed
install.packages("neonUtilities")
# load neonUtilities
library(neonUtilities)
library(doParallel)
library(raster)


setwd("P:/RaBET/Texas/2021/")

byFileAOP("DP3.30006.001", site="CLBJ", 
          year="2021", check.size=T)
#Script created by Cynthia L. Norton for 
# Load the necessary libraries
library(httr)
library(jsonlite)

# Request data using the GET function & the API call
req <- GET("http://data.neonscience.org/api/v0/products/DP1.10098.001")
req

## Response [https://data.neonscience.org/api/v0/products/DP1.10098.001]
##   Date: 2021-06-16 01:03
##   Status: 200
##   Content-Type: application/json;charset=UTF-8
##   Size: 70.1 kB

# Make the data readable by jsonlite
req.text <- content(req, as="text")

# Flatten json into a nested list
avail <- jsonlite::fromJSON(req.text, 
                            simplifyDataFrame=T, 
                            flatten=T)


# View description of data product
avail$data$productDescription

## [1] "Structure measurements, including height, crown diameter, and stem diameter, as well as mapped position of individual woody plants"

# View data product abstract
avail$data$productAbstract

## [1] "This data product contains the quality-controlled, native sampling resolution data from in-situ measurements of live and standing dead woody individuals and shrub groups, from all terrestrial NEON sites with qualifying woody vegetation. The exact measurements collected per individual depend on growth form, and these measurements are focused on enabling biomass and productivity estimation, estimation of shrub volume and biomass, and calibration / validation of multiple NEON airborne remote-sensing data products. In general, comparatively large individuals that are visible to remote-sensing instruments are mapped, tagged and measured, and other smaller individuals are tagged and measured but not mapped. Smaller individuals may be subsampled according to a nested subplot approach in order to standardize the per plot sampling effort. Structure and mapping data are reported per individual per plot; sampling metadata, such as per growth form sampling area, are reported per plot. For additional details, see the user guide, protocols, and science design listed in the Documentation section in this data product's details webpage.\n\nLatency:\nThe expected time from data and/or sample collection in the field to data publication is as follows, for each of the data tables (in days) in the downloaded data package. See the Data Product User Guide for more information.\n\nvst_apparentindividual:  90\n\nvst_mappingandtagging:  90\n\nvst_perplotperyear:  300\n\nvst_shrubgroup:  90"

