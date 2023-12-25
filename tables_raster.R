library(readr)
library(foreign)
library(raster)
library(tidyverse)
library(sf)
library(writexl)
library(terra)
library(stars)

grid_test <- sf::read_sf("A:/sel.shp")
pp <- select(grid_test, Lat, Long, CODE)
rasterized <- st_rasterize(grid_test)
writeRaster(rasterized, paste0("A:/test1.tif"))
write_stars(rasterized, "A:/test1.tif")

grid <- sf::read_sf("B:/ALEJANDRA/SDM_PLANTS/grid10km/grid_10km_PORC_LAKES_WGS84_25.shp")

archivos <- list.files("B:/ALEJANDRA/SDM_PLANTS/TABLAS/SALIDA/", full.names = T, pattern = "\\.dbf$")
arc <- list.files("B:/ALEJANDRA/SDM_PLANTS/TABLAS/SALIDA/",  pattern = "\\.dbf$")
arc <- gsub("\\..*","",arc)

#Variables
for (i in 1:length(archivos)){#length(archivos)
  kk <- read.dbf(archivos[i])
  colnames(kk) <-  c("FID",  "COUNT", "AREA",  "MEAN")
  kk <- select(kk, FID, MEAN)
  colnames(kk) <- c("FID",paste0(arc[i]))
  grid <- merge(grid, kk,all.x = TRUE, by.x="CODE", by.y="FID")
}
kk <- as.data.frame(colnames(variables))

variables <-  read_delim("B:/ALEJANDRA/SDM_PLANTS/A_RESULTADOS/all_variables_grid_realm_soil.csv",
                         delim = ";",
                         escape_double = FALSE,
                         locale = locale(decimal_mark = ",",
                                         grouping_mark = ""),
                         trim_ws = TRUE)
kk <- head(variables, n = 50)
variables <- variables[, -82]
variables_1 <- variables[,1:81]
variables_2 <- variables[,82:83]

variables_1[is.na(variables_1)] <- -99999
variables_3 <- cbind(variables_1, variables_2)

write_csv2(variables_3, "B:/ALEJANDRA/SDM_PLANTS/RASTER_10KM/variables.csv")

puntos <- variables %>% 
  st_as_sf(coords = c('Long', 'Lat')) %>%
  st_set_crs(4326)

colnames(puntos[, 1:78]) <- seq(1, 79, 1)
st_write(puntos,"B:/ALEJANDRA/SDM_PLANTS/RASTER_10KM/puntos_kk.shp" )








# Raster creation
for (i in 1:length(archivos)){
  kk <- read.dbf(archivos[i])
  colnames(kk) <-  c("FID",  "COUNT", "AREA",  "MEAN")
  kk <- merge(grid, kk, by.x="CODE", by.y="FID", all.x = TRUE)
  pp <- select(kk, Lat, Long, MEAN)
  rasterize_kk <- st_rasterize(pp )
  write_stars(rasterized, paste0("B:/ALEJANDRA/SDM_PLANTS/RASTER_10KM/", arc[i], ".tif"))
}
rasterized_2 = as(rasterized, "Raster")
writeRaster(rasterized_2, "A:/test24.tif")

writeRaster(rasterized, "B:/ALEJANDRA/SDM_PLANTS/TABLAS/RASTER_2/REALM.tif")

sum(is.na(grid$chelsa_ai_1981_2010))
raster <- list.files("B:/ALEJANDRA/TABLAS/RASTER_2/", full.names = T)

a <- terra::rast(raster[63])
b <- terra::rast(raster[3])
c <- terra::rast(raster[15])
d <- c(a,b,c)
terra::plot(c)
plot(rasterized)

mm <- terra::focalPairs(d, w=5, "pearson", na.rm=TRUE)



variables <-  read_delim("B:/ALEJANDRA/SDM_PLANTS/A_RESULTADOS/all_variables_grid_realm_soil.csv",
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
cl <- unique(cl$DOMSOI)
cl <- as.data.frame(sort(cl, decreasing = F))
cl <- cbind(cl, "FAO_number"=1:nrow(cl))
colnames(cl) <- c("DOMSOI", "SOIL_CODE")


SOIL_FAO_GRID <- left_join(SOIL_FAO_GRID, cl, by = c("MAJORITY" = "SOIL_CODE"))
SOIL_FAO_GRID <- filter(SOIL_FAO_GRID, SOIL_FAO_GRID$MAJORITY>=1)
SOIL_FAO_GRID <- as.data.frame(SOIL_FAO_GRID[,c(1,4)])
colnames(SOIL_FAO_GRID) <- c("CODE", "SOIL")
SOIL_FAO_GRID$SOIL <- as.factor(SOIL_FAO_GRID$SOIL)

kk <- left_join(grid, SOIL_FAO_GRID, by = c("CODE" = "CODE"))

kk <- merge(grid, SOIL_FAO_GRID, by.x="CODE", by.y="CODE", all.x = TRUE)
pp <- select(kk, Lat, Long, SOIL)

#define non-uniform spatial extent
e.cont = terra::ext(pp[,2,1])

#create raster
r.cont = terra::rast(e.cont)

rasterized <- terra::rasterize(pp, r.cont, pp$SOIL)

rasterized <- rasterize(pp, terra::rast(resolution = 0.0083, crs = "+proj=longlat +datum=WGS84"), pp$SOIL)
writeRaster(rasterized, "B:/ALEJANDRA/TABLAS/RASTER_2/soil_FAO_2.tif")
mm <- rast(grid$geometry)
summary(mm)

variables <- left_join(variables, SOIL_FAO_GRID, by = c("CODE" = "FID_"))
write.csv2(cl, "B:/ALEJANDRA/A_RESULTADOS/cl.csv")

a <- rast("B:/ALEJANDRA/TABLAS/RASTER_2/soil_FAO_2.tif")
b <- rast("B:/ALEJANDRA/TABLAS/RASTER_2/chelsa_bio5_1981_2010.tif")
p <- c(a,b)
plot(p)




library(readr)
library(foreign)
library(raster)
library(tidyverse)
library(sf)
library(terra)
library(fasterize)

variables <- sf::read_sf("B:/ALEJANDRA/SDM_PLANTS/RASTER_10KM/variables_homo.shp")
names(variables) <-              c("...1",   "CODE",       "area",       "Lat" ,       "Long" ,      "PORCENTAJE" ,"AREA_2",    
                                   "ai_v3_yr"                        ,"chelsa_ai_1981_2010"              ,"chelsa_bio1_1981_2010"           ,"chelsa_bio10_1981_2010"          
                                   ,"chelsa_bio11_1981_2010"          ,"chelsa_bio12_1981_2010"           ,"chelsa_bio13_1981_2010"          ,"chelsa_bio14_1981_2010"          
                                   ,"chelsa_bio15_1981_2010"          ,"chelsa_bio16_1981_2010"           ,"chelsa_bio17_1981_2010"          ,"chelsa_bio18_1981_2010"          
                                   ,"chelsa_bio19_1981_2010"          ,"chelsa_bio2_1981_2010"            ,"chelsa_bio3_1981_2010"           ,"chelsa_bio4_1981_2010"           
                                   ,"chelsa_bio5_1981_2010"           ,"chelsa_bio6_1981_2010"            ,"chelsa_bio7_1981_2010"           ,"chelsa_bio8_1981_2010"           
                                   ,"chelsa_bio9_1981_2010"           ,"chelsa_clt_mean_1981_2010"        ,"chelsa_cmi_mean_1981_2010"       ,"chelsa_fcf_1981_2010"            
                                   ,"chelsa_fgd_1981_2010"            ,"chelsa_gdd0_1981_2010"            ,"chelsa_gdd10_1981_2010"          ,"chelsa_gdd5_1981_2010"           
                                   ,"chelsa_gddlgd0_1981_2010"        ,"chelsa_gddlgd10_1981_2010"        ,"chelsa_gddlgd5_1981_2010"        ,"chelsa_gdgfgd0_1981_2010"        
                                   ,"chelsa_gdgfgd10_1981_2010"       ,"chelsa_gdgfgd5_1981_2010"         ,"chelsa_gsp_1981_2010"            ,"chelsa_gst_1981_2010"            
                                   ,"chelsa_hurs_1981_2010"           ,"chelsa_kg0_1981_2010"             ,"chelsa_kg1_1981_2010"            ,"chelsa_kg2_1981_2010"            
                                   ,"chelsa_kg3_1981_2010"            ,"chelsa_kg4_1981_2010"             ,"chelsa_kg5_1981_2010"            ,"chelsa_lgd_1981_2010"            
                                   ,"chelsa_ngd0_1981_2010"           ,"chelsa_ngd10_1981_2010"           ,"chelsa_ngd5_1981_2010"           ,"chelsa_npp_1981_2010"            
                                   ,"chelsa_pet_penman_mean_1981_2010","chelsa_rsds_1981_2010"            ,"chelsa_scd_1981_2010"            ,"chelsa_sfcwind_mean_1981_2010"   
                                   ,"chelsa_swb_1981_2010"            ,"chelsa_swe_1981_2010"             ,"chelsa_vpd_mean_1981_2010"       ,"dem_aspect"                      
                                   ,"dem_elevation"                   ,"dem_slope"                        ,"et0_v3_yr"                       ,"isric_wv0010_0_15cm_1000"        
                                   ,"isric_wv0033_0_15cm_1000"        ,"isric_wv1500_0_15cm_1000"         ,"soilgrid_bdod"                   ,"soilgrid_cec"                    
                                   ,"soilgrid_cfvo"                   ,"soilgrid_clay"                    ,"soilgrid_nitrogen"               ,"soilgrid_ocd"                    
                                   ,"soilgrid_phh2o"                  ,"soilgrid_sand"                    ,"soilgrid_silt"                   ,"soilgrid_soc"                    
                                   ,"solar_max"                       ,"solar_min", "realm", "fao_soil", "geometry")  

template <- rast("B:/ALEJANDRA/SDM_PLANTS/RASTER_10KM/template.tif")

archivos <- list.files("B:/ALEJANDRA/SDM_PLANTS/TABLAS/SALIDA/", full.names = T, pattern = "\\.dbf$")
arc <- list.files("B:/ALEJANDRA/SDM_PLANTS/TABLAS/SALIDA/",  pattern = "\\.dbf$")
arc <- gsub("\\..*","",arc)

for(i in 1:81){
  a <- terra::rasterize(variables, template, field = variables[82])
  a <- classify(a, cbind(-99999, NA))
  writeRaster(a, paste0("B:/ALEJANDRA/SDM_PLANTS/RASTER_10KM/", variables[82], "2.tif"))
}

variables$chelsa_fcf
i=1
st_crs(variables)
juntos <- c(rast("B:/ALEJANDRA/SDM_PLANTS/RASTER_10KM/RASTER/chelsa_bio3_1981_2010.tif"),
            rast("B:/ALEJANDRA/SDM_PLANTS/RASTER_10KM/RASTER/chelsa_gdgfgd10_1981_2010.tif"))
terra::plot(juntos)

chelsa_gdgfgd10_1981_2010.tif
library(terra)
a <- rast("A:/ai_v3_yr.tif")
b <- project(a, "+proj=longlat +datum=WGS84")
plot(a)
plot(b)



archivos <- list.files("B:/ALEJANDRA/SDM_PLANTS/RASTER_10KM/RASTER", full.names = T, pattern = "\\.tif$")
arc <- list.files("B:/ALEJANDRA/SDM_PLANTS/RASTER_10KM/RASTER", pattern = "\\.tif$")
arc <- gsub("\\..*","",arc)

for (i in 1:length(archivos)){
  a <- rast(archivos[i])
  a <- project(a, "+proj=longlat +datum=WGS84")
  writeRaster(a, paste0("B:/ALEJANDRA/SDM_PLANTS/RASTER_10KM_WGS84/", arc[i], "_WGS84.tif"))
}
            
