library(readr)
library(foreign)
library(raster)
library(tidyverse)
library(sf)
library(writexl)
library(terra)

grid <- sf::read_sf("B:/ALEJANDRA/grid10km/grid_10km_PORC_LAKES_WGS84_25.shp")

archivos <- list.files("B:/ALEJANDRA/TABLAS/SALIDA_2/", full.names = T, pattern = "\\.dbf$")
arc <- list.files("B:/ALEJANDRA/TABLAS/SALIDA_2/",  pattern = "\\.dbf$")
arc <- gsub("\\..*","",arc)

for (i in 1:length(archivos)){#length(archivos)
  kk <- read.dbf(archivos[i])
  colnames(kk) <-  c("FID",  "COUNT", "AREA",  "MEAN")
  kk <- select(kk, FID, MEAN)
  colnames(kk) <- c("FID",paste0(arc[i]))
  grid <- merge(grid, kk,all.x = TRUE, by.x="CODE", by.y="FID")
  
}
write_csv2(kk, "B:/ALEJANDRA/A_RESULTADOS/all_variables_grid_realm.csv" )

# Raster creation
for (i in 1:length(archivos)){
  kk <- read.dbf(archivos[i])
  colnames(kk) <-  c("FID",  "COUNT", "AREA",  "MEAN")
  kk <- merge(grid, SOIL_FAO_GRID, by.x="CODE", by.y="FID_", all.x = TRUE)
  pp <- select(kk, Lat, Long, DOMSOI)
  rasterized <- rasterize(pp, terra::rast(resolution = 0.0083, crs = "+proj=longlat +datum=WGS84"), pp$DOMSOI)
  writeRaster(rasterized, paste0("B:/ALEJANDRA/TABLAS/RASTER_2/", arc[i], ".tif"))
}

writeRaster(rasterized, "B:/ALEJANDRA/TABLAS/RASTER_2/REALM.tif")

sum(is.na(grid$chelsa_ai_1981_2010))
raster <- list.files("B:/ALEJANDRA/TABLAS/RASTER_2/", full.names = T)

a <- terra::rast(raster[63])
b <- terra::rast(raster[3])
c <- terra::rast(raster[15])
d <- c(a,b,c)
terra::plot(c)
plot(rasterized)

mm <- terra::focalPairs(d, w=5, "pearson", na.rm=TRUE)



variables <-  read_delim("B:/ALEJANDRA/A_RESULTADOS/all_variables_grid_realm.csv",
                         delim = ";",
                         escape_double = FALSE,
                         locale = locale(decimal_mark = ",",
                                         grouping_mark = ""),
                         trim_ws = TRUE)



no_data <- as.data.frame(rbind(colnames(variables), colSums(is.na(variables))))
no_data <- no_data[-1,]
no_data <- as.data.frame(t(no_data))
colnames(no_data) <- c("missing_values")
no_data <- no_data %>% mutate(Percentage = (as.numeric(no_data$missing_values)/1331804)*100)
no_data$Percentage <- round(no_data$Percentage, digits = 2)

writexl::write_xlsx(no_data, "B:/ALEJANDRA/A_RESULTADOS/data_missing.xlsx")
write.csv2(no_data, "B:/ALEJANDRA/A_RESULTADOS/data_missing.csv")

archivos <- list.files("B:/ALEJANDRA/TABLAS/", full.names = T, pattern = "\\.dbf$")
#REALM

a <- read.dbf("B:/ALEJANDRA/TABLAS/REALM_GRID_TABLE.dbf")
a <- a[,c(5,9)]
kk <- left_join(variables, a)

rm(a)



# SOil
#soil <- sf::read_sf("B:/ALEJANDRA/SOIL_CLASS/DSMW.shp")
#soil <- left_join(soil, cl, by = c("DOMSOI"="DOMSOI"))
#st_write(soil, "B:/ALEJANDRA/SOIL_CLASS/FAO_SOIL.shp")
#st_crs(soil)$proj4string



SOIL_FAO_GRID <- read.dbf("B:/ALEJANDRA/SOIL_CLASS/soil_FAO_grid.dbf")

cl <- read_csv("B:/ALEJANDRA/SOIL_CLASS/classes_soil.csv")
cl <- unique(classes_soil$DOMSOI)
cl <- as.data.frame(sort(cl, decreasing = F))
cl <- cbind(cl, "FAO_number"=1:nrow(cl))
colnames(cl) <- c("DOMSOI", "SOIL_CODE")
rm(classes_soil)

SOIL_FAO_GRID <- left_join(SOIL_FAO_GRID, cl, by = c("MAJORITY" = "SOIL_CODE"))
SOIL_FAO_GRID <- filter(SOIL_FAO_GRID, SOIL_FAO_GRID$MAJORITY>=1)
SOIL_FAO_GRID <- as.data.frame(SOIL_FAO_GRID[,c(1,5)])
colnames(SOIL_FAO_GRID) <- c("FID", "SOIL")

kk <- left_join(grid, SOIL_FAO_GRID, by = c("CODE" = "FID"))

kk <- merge(grid, SOIL_FAO_GRID, by.x="CODE", by.y="FID", all.x = TRUE)
pp <- select(kk, Lat, Long, SOIL)

#define non-uniform spatial extent
e.cont = terra::ext(pp[,2,1])

#create raster
r.cont = terra::rast(e.cont)

rasterized <- terra::rasterize(pp, r.cont, pp$SOIL)

rasterized <- rasterize(pp, terra::rast(resolution = 0.083, crs = "+proj=longlat +datum=WGS84"), pp$SOIL)
writeRaster(rasterized, paste0("B:/ALEJANDRA/TABLAS/RASTER_2/", arc[i], ".tif"))
mm <- rast(grid$geometry)
summary(mm)

variables <- left_join(variables, SOIL_FAO_GRID, by = c("CODE" = "FID_"))
write.csv2(variables, "B:/ALEJANDRA/A_RESULTADOS/all_variables_grid_realm.csv")
