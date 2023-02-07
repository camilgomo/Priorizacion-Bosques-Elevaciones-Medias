library(raster)
library(rworldmap)

# Set working directory

setwd("C:/Users/ProCAT Costa Rica/Downloads")

# llamar los diferentes tiles de los raster altura de dosel

hc1 = raster("ETH_GlobalCanopyHeight_10m_2020_N09W084_Map.tif")

hc2 = raster("ETH_GlobalCanopyHeight_10m_2020_N06W084_Map.tif")

hc3 = raster("ETH_GlobalCanopyHeight_10m_2020_N09W087_Map.tif")

hc4 = raster("ETH_GlobalCanopyHeight_10m_2020_N09W081_Map.tif")

hc5 = raster("ETH_GlobalCanopyHeight_10m_2020_N06W081_Map.tif")

hc6 = raster("ETH_GlobalCanopyHeight_10m_2020_N06W078_Map.tif")

# llamar una mascara con las dimensiones delarea de estudio

mask = raster("Sce_migracionCA_Norm_CR_Pan.tif")

# cambiar la resolucion con la mascara, para probar los procedimientos 

hc1_res <- resample(hc1, mask, method='bilinear')
plot(hc1_res)

hc2_res <- resample(hc2, mask, method='bilinear')
plot(hc2_res)

hc3_res <- resample(hc3, mask, method='bilinear')
plot(hc3_res)

hc4_res <- resample(hc4, mask, method='bilinear')

hc5_res <- resample(hc5, mask, method='bilinear')

hc6_res <- resample(hc6, mask, method='bilinear')

# generar una lista de los tiles para luego organizarlos en un solo raster

tiles_hc <- list(hc1_res, hc2_res, hc3_res, hc4_res, hc5_res, hc6_res)
names(tiles_hc)[1:2] <- c('x', 'y')
tiles_hc$fun <- mean
tiles_hc$na.rm <- TRUE

merge_hc <- do.call(mosaic, tiles_hc)

plot(merge_hc)

# se realiza el mismo procedimiento con la cobertura

cover1 = raster("treecover2010_10N_090W.tif")

cover2 = raster("Hansen_GFC-2019-v1.7_treecover2000_20N_090W.tif")

cover3 = raster("Hansen_GFC-2019-v1.7_treecover2000_10N_080W.tif")


cover1_res <- resample(cover1, mask, method='bilinear')
plot(cover1_res)

cover2_res <- resample(cover2, mask, method='bilinear')
plot(cover2_res)

cover3_res <- resample(cover3, mask, method='bilinear')
plot(cover3_res)

tiles <- list(cover1_res, cover2_res, cover3_res)
names(tiles)[1:2] <- c('x', 'y')
tiles$fun <- mean
tiles$na.rm <- TRUE

merge_cover <- do.call(mosaic, tiles)

plot(merge_cover)

# probamos con un stack si se encuentran en la misma resolucion y proyeccion

hc_cover = stack(merge_hc, merge_cover)
plot(hc_cover) 

# reclasificamos la capa de altura con los valores mayores a 15 m de altura de
# arboles

hc_15 = reclassify(merge_hc, c(-Inf, 15, 0, 15, Inf, 1))
plot(hc_15)

# reclasificamos la capa de cobertura con los valores mayores a 70%
# de cobertura de dosel

cover_70 = reclassify(merge_cover, c(-Inf, 70, 0, 70, Inf, 1))

hc_cover_15_70 = stack(hc_15, cover_70)
plot(hc_cover_15_70)

# sumamos tanto cobertura como altura para determinar los sitios con arboles

SUMhc15_cover70 = hc_15 + cover_70

plot(SUMhc15_cover70)

hc15_cover70 <-calc(SUMhc15_cover70, fun=function(x){ x[x < 1.9] <- 0; return(x)} )
hc15_cover70<-calc(hc15_cover70, fun=function(x){ x[x > 1.9] <- 1; return(x)} )
plot(hc15_cover70)

worldMap <- getMap()

Elevation <- raster("AltWorld.tif")
Ele.crop=crop(Elevation, mask)
plot(Ele.crop)

Ele.re = reclassify(Ele.crop, c(-Inf, 750, 0, 750, 2000, 1, 2000, Inf, 0))
plot(Ele.re)

Ele2 <- resample(Ele.re, hc15_cover70)
plot(Ele2)

b=stack(hc15_cover70, Ele2)
plot(b)

hc15_cover70_Ele = hc15_cover70 + Ele2
plot(hc15_cover70_Ele)

Prior_hc15_cover70 <-calc(hc15_cover70_Ele, fun=function(x){ x[x < 1.9] <- 0; return(x)} )
Prior_hc15_cover70 <-calc(Prior_hc15_cover70, fun=function(x){ x[x > 1.9] <- 1; return(x)} )
plot(Prior_hc15_cover70)

plot(Prior_hc15_cover70)

writeRaster(Prior_hc15_cover70, "Prior_hc15_cover70.tif", 
            format = "GTiff", overwrite=TRUE)

forest_birds = stack(Prior_hc15_cover70, mask)

plot(forest_birds)

prueba <- resample(Prior_hc15_cover70, mask)
plot(prueba)

SUMforest_birds = Prior_hc15_cover70 + mask
plot(SUMforest_birds)

forest_birds <-calc(SUMforest_birds, fun=function(x){ x[x < 1.9] <- 0; return(x)} )
forest_birds <-calc(forest_birds, fun=function(x){ x[x > 1.9] <- 1; return(x)} )
plot(forest_birds)

writeRaster(forest_birds, "Bosques15.70_aves.tif", 
            format = "GTiff")

plot(mask)

land_panama = raster("CoberturaBocosaUsoSuelo_2021_25k.tif")

plot(land_panama)

LandPan_WGS <- projectRaster(land_panama, 
                               crs = as.character(Elevation@crs))
plot(LandPan_WGS)

writeRaster(LandPan_WGS, "LandCoverPanama_WGS84.tif", 
            format = "GTiff")
