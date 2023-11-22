library(readr)
library(foreign)
library(raster)
library(tidyverse)
library(sf)
#grid <- read_delim("B:/ALEJANDRA/TABLAS/grid.txt", 
#                           delim = ";", escape_double = FALSE, 
#                   col_types = cols(CODE = col_skip()), locale = locale(decimal_mark = ","), 
#                           trim_ws = TRUE)

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

for (i in 1:length(archivos)){
  kk <- read.dbf(archivos[i])
  colnames(kk) <-  c("FID",  "COUNT", "AREA",  "MEAN")
  kk <- merge(grid, a, by.x="CODE", by.y="CODE")
  pp <- select(kk, Lat, Long, REALM)
  rasterized <- rasterize(pp, terra::rast(resolution = 0.0083, crs = "+proj=longlat +datum=WGS84"), pp$REALM)
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



library(readr)
library(tidyverse)
library(writexl)
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

library(foreign)
a <- read.dbf("B:/ALEJANDRA/TABLAS/REALM_GRID_TABLE.dbf")
a <- a[,c(5,9)]
kk <- left_join(variables, a)

