library(raster)
library(rgdal)

# Set working directory

setwd("C:/Users/ProCAT Costa Rica/Downloads")



# llamar los diferentes tiles de los raster altura de dosel

hc1 = raster("ETH_GlobalCanopyHeight_10m_2020_N09W084_Map.tif")

hc1.15 = reclassify(hc1, c(-Inf, 15, 0, 15, Inf, 1))

writeRaster(hc1.15, "hc1.15final.tif", 
            format = "GTiff", overwrite=TRUE)

plot(hc1.15)

# llamamos el raster

hc1.15=raster("hc1.15final.tif")

# cortamos en el rango altitudinal de 700 a 2000 msnm

worldMap <- getMap()

Elevation <- raster("AltWorld.tif")
Ele.crop=crop(Elevation, hc1.15)
plot(Ele.crop)

# reclasificamos para tener la elevacion de interes (750 y 2000 m)

Ele.re = reclassify(Ele.crop, c(-Inf, 750, 0, 750, 2000, 1, 2000, Inf, 0))
plot(Ele.re)

# remuestreamos la capa de elevacion

Ele2 <- resample(Ele.re, hc1.15)
plot(Ele2)

hc2 = raster("ETH_GlobalCanopyHeight_10m_2020_N06W084_Map.tif")

hc2.15 = reclassify(hc2, c(-Inf, 15, 0, 15, Inf, 1))

writeRaster(hc2.15, "hc2.15final.tif", 
            format = "GTiff", overwrite=TRUE)

plot(hc2.15)

hc3 = raster("ETH_GlobalCanopyHeight_10m_2020_N09W087_Map.tif")

hc3.15 = reclassify(hc3, c(-Inf, 15, 0, 15, Inf, 1))

writeRaster(hc3.15, "hc3.15final.tif", 
            format = "GTiff", overwrite=TRUE)

plot(hc3.15)

hc4 = raster("ETH_GlobalCanopyHeight_10m_2020_N09W081_Map.tif")

hc4.15 = reclassify(hc4, c(-Inf, 15, 0, 15, Inf, 1))

writeRaster(hc4.15, "hc4.15final.tif", 
            format = "GTiff", overwrite=TRUE)

plot(hc4.15)

hc5 = raster("ETH_GlobalCanopyHeight_10m_2020_N06W081_Map.tif")

hc5.15 = reclassify(hc5, c(-Inf, 15, 0, 15, Inf, 1))

writeRaster(hc5.15, "hc5.15final.tif", 
            format = "GTiff", overwrite=TRUE)


hc6 = raster("ETH_GlobalCanopyHeight_10m_2020_N06W078_Map.tif")

hc6.15 = reclassify(hc6, c(-Inf, 15, 0, 15, Inf, 1))

writeRaster(hc6.15, "hc6.15final.tif", 
            format = "GTiff", overwrite=TRUE)


# se realiza el mismo procedimiento con la cobertura

cover1 = raster("treecover2010_10N_090W.tif")

cover1.70 = reclassify(cover1, c(-Inf, 70, 0, 70, Inf, 1))

writeRaster(cover1.70, "cover1.70final.tif", 
            format = "GTiff", overwrite=TRUE)

cover2 = raster("Hansen_GFC-2019-v1.7_treecover2000_20N_090W.tif")

cover2.70 = reclassify(cover2, c(-Inf, 70, 0, 70, Inf, 1))

writeRaster(cover2.70, "cover2.70final.tif", 
            format = "GTiff", overwrite=TRUE)

cover3 = raster("Hansen_GFC-2019-v1.7_treecover2000_10N_080W.tif")

cover3.70 = reclassify(cover3, c(-Inf, 70, 0, 70, Inf, 1))

writeRaster(cover3.70, "cover3.70final.tif", 
            format = "GTiff", overwrite=TRUE)


# Llamamos todos los raster

hc1.15= raster("hc1.15final.tif")

hc2.15= raster("hc2.15final.tif")

hc3.15= raster("hc3.15final.tif")

hc4.15= raster("hc4.15final.tif")

hc5.15= raster("hc5.15final.tif")

hc6.15= raster("hc6.15final.tif")

cover1.70 = raster("cover1.70final.tif")

cover2.70 = raster("cover2.70final.tif")

cover3.70 = raster("cover3.70final.tif")

# Definimos un rea de estudio
AreaEstudio = readOGR(dsn = ".", layer = "AreaEstudio_CR_Panama")

# Cortamos los tiles al area

cover1.70_crop = crop(cover1.70,AreaEstudio)

plot(cover1.70_crop)

cover2.70_crop = crop(cover2.70,AreaEstudio)

plot(cover2.70_crop)

cover3.70_crop = crop(cover3.70,AreaEstudio)

hc1.15_crop = crop(hc1.15,AreaEstudio)

hc2.15_crop = crop(hc2.15,AreaEstudio)

hc3.15_crop = crop(hc3.15,AreaEstudio)

hc4.15_crop = crop(hc4.15,AreaEstudio)

hc5.15_crop = crop(hc5.15,AreaEstudio)

hc6.15_crop = crop(hc6.15,AreaEstudio)


# juntamos tiles de altura de dosel

tiles.h <- list(hc1.15_crop, hc2.15_crop, hc3.15_crop, hc4.15_crop,
                hc5.15_crop, hc6.15_crop)
names(tiles.h)[1:2] <- c('x', 'y')
tiles.h$fun <- mean
tiles.h$na.rm <- TRUE

hc.merge <- do.call(mosaic, tiles.h)

plot(hc.merge)

# juntamos tiles de cobertura

tiles.cov <- list(cover1.70_crop, cover2.70_crop, cover3.70_crop)
names(tiles.cov)[1:2] <- c('x', 'y')
tiles.cov$fun <- mean
tiles.cov$na.rm <- TRUE

cover.merge <- do.call(mosaic, tiles.cov)

plot(hc.merge)


# hacemos que tengan la misma extensión los raster de dosel y cobertura

resample.cover = resample(hc.merge, cover.merge, method='bilinear')

prueba = stack(resample.cover, hc.merge)

plot(prueba)

# Sumamos los raster de cobertura de más del 70% y altura de dosel 
# de más de 15 m

setwd("E:/selva")

cover70 = raster("cover70CRPan.tif")

hc15 = raster("hc15CRPan.tif")

cover70hc15 = stack(cover70, hc15)

sum.cover70hc15 = cover70+hc15

# Las areas con esta cobertura y altura se les asigna el valor de 1

hc15_cover70 <-calc(sum.cover70hc15, fun=function(x){ x[x < 1.9] <- 0; return(x)} )
hc15_cover70<-calc(hc15_cover70, fun=function(x){ x[x > 1.9] <- 1; return(x)} )
plot(hc15_cover70)

writeRaster(hc15_cover70, "bosques70cobert15alt.tif", 
            format = "GTiff", overwrite=TRUE)

bosques = raster("bosques70cobert15alt.tif")
plot(bosques)

# Llamamos la capa de elevacion 

elevacion = raster("Elevacion_CR_Pan.tif")

ele.re = reclassify(elevacion, c(-Inf, 750, 0, 750, 2000, 1, 2000, Inf, 0))

plot(ele.re)

# Remuestreamos la capa de elevacion con la de cobertura

resample.ele = resample(ele.re, bosques, method='bilinear')

writeRaster(hc15_cover70, "bosques70cobert15alt.tif", 
            format = "GTiff", overwrite=TRUE)

writeRaster(resample.ele, "ele_CRPanresamp.tif", 
            format = "GTiff", overwrite=TRUE)

# Si estamos trabajando en un computador de escritorio usual, debemos liberar
# espacio en el disco antes de continuar, por lo tanto guardamos
# cada capa generada e iniciamos luego de liberar espacio.
# en windows para liberar espacio presionamos Windows+R y en la ventana
# escribimos %TMP% buscamos la carpeta raster y eliminamos los 
# archivos temporales

# Seleccionamos los bosques a la elevacion de interes

setwd("E:/selva")

bosques = raster("bosques70cobert15alt.tif")

elev.cent = raster("ele_CRPanresamp.tif")

sum.ele.bosq = elev.cent + bosques

ele.bos <-calc(sum.ele.bosq, fun=function(x){ x[x < 1.9] <- 0; return(x)} )
ele.bos<-calc(ele.bos, fun=function(x){ x[x > 1.9] <- 1; return(x)} )
plot(ele.bos)

writeRaster(ele.bos, "bosque750_2000.tif", 
            format = "GTiff", overwrite=TRUE)


landcover_pan = raster("CoberturaBocosaUsoSuelo_2021_25k.tif")

plot()




Bosques700.2000 = raster("bosque750_2000.tif")

lc_panama = raster("LandCoverPanama_WGS84.tif")

plot(lc_panama)

resample.lcPan = resample(lc_panama, Bosques700.2000, method='bilinear')




prueba = stack(Bosques700.2000, lc_panama)

plot(Bosque700.2000_UTM17N)

Bosque700.2000_UTM17N <- projectRaster(Bosques700.2000, 
                               crs = as.character(landcover_pan@crs))

writeRaster(Bosque700.2000_UTM17N, "bosque750_2000UTM17N.tif", 
            format = "GTiff", overwrite=TRUE)



plot(Bosques700.2000)


lc_pan.poly = readOGR(dsn = ".", layer = "CoberturaBoscosaUsoSuelo_2021_25k")

v <- extract(aves_UTM17N, lc_pan.poly) 

poly = st_read("E:/selva/CoberturaBoscosaUsoSuelo_2021_25k.shp")

library(maptools)
library(sf)
install.packages("stars")
library(stars)

for (i in 1:length(grids)){
  ex <- extract(s, poly, fun=sum, na.rm=TRUE, df=TRUE)
}

aves = raster("Focal_Areas 2 sp75_CANorm_CR_Pan (1).tif")

aves_UTM17N <- projectRaster(aves, crs = as.character(landcover_pan@crs))

writeRaster(aves_UTM17N, "aves_UTM17N.tif", 
            format = "GTiff", overwrite=TRUE)


aves.st=read_stars("aves_UTM17N.tif")

intersect.bosque_lc <- st_intersection(poly, aves.st)


setwd("C:/Users/ProCAT Costa Rica/Downloads")

setwd("E:/selva")

lc_panama = raster("LandCoverPanama_WGS84.tif")

shplcPan = rasterToPolygons(lc_panama)

plot(lc_panama)




lcCR = raster("MC19.tif")

plot(lcCR)

lcCR_WGS <- projectRaster(lcCR, 
                             crs = as.character(lc_panama@crs))

#seleccionar solo areas de bosque
lcCR_reclas = reclassify(lcCR_WGS, c(-Inf, 67, 1, 67, Inf, 0))

plot(lcCR_reclas)

resample.lcCR = resample(lcCR_reclas, Bosques700.2000, method='bilinear')

plot(resample.lcCR)

elev.cent = raster("ele_CRPanresamp.tif")

sum.ele.bosq = elev.cent + resample.lcCR

ele.bosCR <-calc(sum.ele.bosq, fun=function(x){ x[x < 1.9] <- 0; return(x)} )
ele.bosCR<-calc(ele.bosCR, fun=function(x){ x[x > 1.9] <- 1; return(x)} )
plot(ele.bosCR)

writeRaster(ele.bosCR, "lcCR_700_2000.tif", 
            format = "GTiff", overwrite=TRUE)

#

lcPan_reclas = reclassify(lc_panama, c(0, 8, 1, 8, Inf, 0))
plot(lcPan_reclas)

resample.lcPan = resample(lcPan_reclas, Bosques700.2000, method='bilinear')

elev.cent = raster("ele_CRPanresamp.tif")

sum.ele.bosqPan = elev.cent + resample.lcPan

ele.bosqPan <-calc(sum.ele.bosqPan, fun=function(x){ x[x < 1.9] <- 0; return(x)} )
ele.bosPan<-calc(ele.bosPan, fun=function(x){ x[x > 1.9] <- 1; return(x)} )
plot(ele.bosPan)

writeRaster(ele.bosCR, "lcPan_700_2000.tif", 
            format = "GTiff", overwrite=TRUE)


tiles_lc_CRPan <- list(resample.lcCR, resample.lcPan)
names(tiles_lc_CRPan)[1:2] <- c('x', 'y')
tiles_lc_CRPan$fun <- mean
tiles_lc_CRPan$na.rm <- TRUE

merge_lc_CRPan <- do.call(mosaic, tiles_lc_CRPan)

plot(merge_lc_CRPan)





Ele3 <- resample(Ele.re, cov70_h15)

hc15_cover70.ele = cov70_h15 + Ele3
plot(hc15_cover70.ele)

hccov.ele <-calc(hc15_cover70.ele, fun=function(x){ x[x < 1.9] <- 0; return(x)} )
hccov.ele <-calc(hccov.ele, fun=function(x){ x[x > 1.9] <- 1; return(x)} )
plot(hccov.ele)


cov70_h15 = raster("Prior_hc15_cover70.tif")

plot(cov70_h15)

bosq_clPanama = stack(cov70_h15, merge_lc_CRPan)
plot(bosq_clPanama)

SUMbosq_clPanama = cov70_h15 + merge_lc_CRPan

plot(SUMbosq_clPanama)

# Sobreponer las capas raster sobre los poligonos de la clasificacion 
# estadistica zonal, cell stats
