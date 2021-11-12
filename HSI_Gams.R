#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Script for computing a habitat suitability model (HSI) for chamois in Bavaria
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# Authors: Lisa Mandl, Rudi Reiner (NPV BGD)
# Purpose: Compute potential habitats for chamois (rupicapra rupicapra) in the 
# bavarian alps
# Created: 11.11.2021
# Copyright: (c) L. Mandl/ R. Reiner 2021
# License: CC 0
### Data sources:
# Bayerische Vermessungsverwaltung - www.geodaten.bayern.de" (counties)
# European Environment Agency (EEA) under the framework of the Copernicus programme (DEM)
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# Tasks:
# 1 Prepare data (import, clipping)
# 2 Compute parameters from DEM
# 3 Implement fuzzy logic
# Apply weighted overlay
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# load packages
install.packages('solaR')

library(gstat)
library(raster)
library(ggpubr)
library(maptools)
library(rgeos)
library(exactextract)
library(sf)
library(mapview)
library(data.table)
library(sp)
library(future)
library(rgdal)
library(tmap)
library(tmaptools)
library(RStoolbox)
library(terra)
library(solaR)
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# Assign your working directory
#-------------------------------------------------------------------------------
myDirectory <- "D:/Mandl_L_NP_BGD/HSI_Gams/Processed_data"

# set your working directory
setwd(myDirectory)

# assign an output directory
outdir <- myDirectory


#-------------------------------------------------------------------------------
# Data preparation
#-------------------------------------------------------------------------------

### Load data (DEM, counties)

# import DEM from Copernicus Land Monitoring Service
DEM <- raster("D:/Mandl_L_NP_BGD/HSI_Gams/DEM_Cop/eu_dem_E40N20/eu_dem_v11_E40N20.TIF")

# look at raster attributes (esp. to CRS)
DEM

plot(DEM)

# calculate and save the min and max values of the raster to the raster object
#DEM <- setMinMax(DEM)

# view raster attributes again to check for plausibility
#DEM

# import counties shape
counties <- readOGR("D:/Mandl_L_NP_BGD/HSI_Gams/counties/lkr_ex.shp")


counties <- spTransform(counties, CRS(proj4string(DEM_UTM)))

# check CRS of counties layer
st_crs(counties)

# reproject DEM
DEM_UTM <- projectRaster(DEM, crs = crs("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
DEM_UTM

#### clip DEM to according counties

### filter counties according to their "SCH" attribute
# Bad Tölz: 09173
# Berchtesgadener Land: 09172
# Garmisch-Partenkirchen: 09180
# Miesbach: 09182
# Oberallgäu: 09780
# Ostallgäu: 09777
# Rosenheim: 09187
# Traunstein: 09189
# Weilheim-Schongau: 09190

counties_sub <- counties[counties$SCH %in% c("09173", "09172", "09180", "09182", "09780", "09777", "09187", "09189", "09190"),]
counties_sub <- spTransform(counties_sub, CRS(proj4string(DEM_UTM)))

extent(counties_sub)

# does not work, error: do not overlap
DEM_clip_temp <- crop(DEM, extent(counties_sub))
DEM_clip <- mask(DEM_clip_temp, counties_sub)

#--------------------------------------------------------------------------------
# continue with clipped DEM in ArcGIS Pro...
#-------------------------------------------------------------------------------

### import clipped DEM
DEM_UTM_cl <- raster("D:/Mandl_L_NP_BGD/HSI_Gams/Processed_data/DEM_UTM_clip.tif")
plot(DEM_UTM_cl)


# remove NA from border (and yes, it is not the common way doing it like that...)
DEM_clip_temp <- crop(DEM_UTM_cl, extent(counties_sub))
DEM_clip <- mask(DEM_clip_temp, counties_sub)
plot(DEM_clip)


#-------------------------------------------------------------------------------
# compute parameters from DEM
#-------------------------------------------------------------------------------

# parameters: slope, aspect, roughness, TRI


slope<-terrain(DEM_clip, opt="slope", unit="degrees", neighbors=8)
plot(slope)

aspect<-terrain(DEM_clip, opt="aspect", unit="degrees", neighbors=8)
plot(aspect)

roughness<-terrain(DEM_clip, opt="roughness")
plot(roughness)

TRI<-terrain(DEM_clip, opt="TRI")
plot(TRI)

