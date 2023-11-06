library(readr)
library(foreign)
library(raster)
library(tidyverse)
library(sf)
grid <- read_delim("B:/ALEJANDRA/TABLAS/grid.txt", 
                           delim = ";", escape_double = FALSE, 
                   col_types = cols(CODE = col_skip()), locale = locale(decimal_mark = ","), 
                           trim_ws = TRUE)

grid <- sf::read_sf("B:/ALEJANDRA/grid10km/grid_10km_PORC_LAKES_WGS84_25.shp")


bio_1 <- read.dbf("B:/ALEJANDRA/TABLAS/SALIDA/bio_1.dbf")
colnames(bio_1) <-  c("FID",  "COUNT", "AREA",  "MEAN")


kk <- merge(grid,bio_1, by.x="CODE", by.y="FID")
pp <- select(kk, Lat, Long, MEAN)
rasterized <- rasterize(pp, terra::rast(resolution = 0.0083, crs = "+proj=longlat +datum=WGS84"), pp$MEAN)
writeRaster(rasterized, "B:/ALEJANDRA/TABLAS/RASTER/kk.tif" )
plot(rasterized)


---------------------------------------------------------------------
  ----------------------------------------------------------
  

kk <- left_join(grid,bio_1, by = c("CODE", "FID"))

kk <- left_join(grid, bio_1)
kk <- kk[,c(3,4,9)]


kk$Lat <- as.numeric(kk$Lat)
kk$Long <- as.numeric(kk$Long)
library(terra)
pp <- rast(kk, type="xyz")
pp <- rast(kk, 
     crs = "+proj=longlat +datum=WGS84") 

pp <- spdf(data, crs = "+proj=longlat +datum=WGS84", llcols = NULL, na.action = na.omit)

pp <- st_as_sf(data, coords = c("Long","Lat"))
