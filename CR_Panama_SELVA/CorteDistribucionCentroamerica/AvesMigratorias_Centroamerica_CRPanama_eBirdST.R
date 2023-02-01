####### eBird status and trends - Plan de conservacion 

####### Activar la clave para aceder a las capas de eBird 
##expires KEY 18 July 2022 - f0spt9d4kstc

set_ebirdst_access_key("do4n5k0bqnio", overwrite = TRUE)

##Instalar los paquetes necesarios
install.packages(c("tidyverse", "raster", "sf", "ebirdst", "rnaturalearth"))
install.packages("ebirdst")
install.packages("rworldmap")
install.packages("terra")

## Cargar los paquetes
library(ebirdst)
library(raster)
library(dplyr)
library(tidyverse)
library(sf)
library(sp)
library(rgdal)
library(rworldmap)
library(terra)
worldMap <- getMap()

## Seteamos la carpeta donde se alojan los archivos, ubicando la ruta de acceso

setwd("C:/Users/ProCAT Costa Rica/Downloads")

## Llamar la capa de elevacion que vamos a usar como base y 
## cortarlo a nuestra region de interes.


Ele <- raster("AltWorld.tif")
template <- Ele
extent(template)<- c(-95, -64, -12, 24)
Elevation <- crop(Ele,template)
plot(Elevation)

#####  Vermivora chrysoptera - Estacion ################################
##### Reinita Alidorada/Golden-winged Warbler ##########################

## descargar de Status & Trends las capas de abundancia modelada
dl_path1 <- ebirdst_download(species = "Vermivora chrysoptera")

## Llamar al dl_path y grabar el resultado para ir directo a las capas 
## la proxima vez
dl_path1

#### Cargar los rasters que tienen la superficie de abundancia para cada 
## semana del año
Verm_chry <- load_raster(product = "abundance", path = dl_path1)

## Definir las semanas que queremos para el periodo de INVIERNO (estacionario)

Verm_chry1 <- Verm_chry[[1]]
Verm_chry2 <- Verm_chry[[2]]
Verm_chry3 <- Verm_chry[[3]]
Verm_chry4 <- Verm_chry[[4]]
Verm_chry5 <- Verm_chry[[5]]
Verm_chry6 <- Verm_chry[[6]]
Verm_chry7 <- Verm_chry[[7]]
Verm_chry8 <- Verm_chry[[8]]
Verm_chry49 <- Verm_chry[[49]]
Verm_chry50 <- Verm_chry[[50]]
Verm_chry51 <- Verm_chry[[51]]
Verm_chry52 <- Verm_chry[[52]]

## Combinar las semanas para generar una sola capa para el periodo de 
## invierno
Verm_chry_Winter <- Verm_chry1+Verm_chry2+Verm_chry3+Verm_chry4+
  Verm_chry5+Verm_chry6+Verm_chry7+Verm_chry8+Verm_chry49+Verm_chry50+
  Verm_chry51+Verm_chry52

## Guardamos el raster generado y volvemos a abrirlo con la funcion raster
## para que pase de clase spatraster a rasterlayer
writeRaster(Verm_chry_Winter, "Verm_chry_Winter.tif", overwrite =T)
Verm_chry_raster=raster("Verm_chry_Winter.tif")
class(Verm_chry_raster)

## Cortar a la region de interes, se puede ajustar para captar solo uno o dos 
## paises

template <- Verm_chry_raster
extent(template)<- c(-10500000, -6000000, -2000000, 3500000)
Verm_chry.corte <- crop(Verm_chry_raster,template)
plot(Verm_chry.corte)

## Reproyectamos el rastercon crs WGS84 basandonos en la capa de elevacion

Verm_chry_WGS <- projectRaster(Verm_chry.corte, 
                               crs = as.character(Elevation@crs))

plot(Verm_chry_WGS)

## Para cortar a nuestra área de interés utilizamos un poligono que incluya 
## los limites

AreaEstudio = readOGR(dsn = ".", layer = "AreaEstudio_CentroamericaChiapas")

Verm_chry_AbundCA = crop(Verm_chry_WGS,AreaEstudio)

plot(Verm_chry_AbundCA)

## Guardar el raster para no tener que hacer los pasos arriba cada vez
writeRaster(Verm_chry_AbundCA, "Verm_chry_AbundCA.tif", 
            format = "GTiff")

## Cortar la capa de la cerulea a las mismas dimensiones a lo de elevaci?n
Verm_chry_AbundCA.ele <- crop(Verm_chry_AbundCA,Elevation)

plot(Verm_chry_AbundCA.ele)

## Eliminamos los ceros 0, transformandolos en NAs

Verm_chry_AbundCA.ele[Verm_chry_AbundCA.ele==0] <- NA

plot(Verm_chry_AbundCA.ele)

## transformamos los valores de abundancia en un rango de 0 a 1 normalizando

min_val <- min(Verm_chry_AbundCA.ele[], na.rm = T)
max_val <- max(Verm_chry_AbundCA.ele[], na.rm = T)

Vchry = (Verm_chry_AbundCA.ele - min_val)/(max_val - min_val)
plot(Vchry)
hist(Vchry, ylim=c(0,15000))

# Cortamos en los limites de cada pais

AreaEstudio2 = readOGR(dsn = ".", layer = "AreaEstudio_CR_Panama")

Verm_chryNormCR_Pan = crop(Vchry,AreaEstudio2)

plot(Verm_chryNormCR_Pan)


## calculamos los cuantiles para determinar un umbral de abundancia para 
##priorizar la presencia y abundancia de la especie
quantile(Verm_chryNormCR_Pan, c(.50, .75))

# 50%       75% 
# 0.1840535 0.3273431

Vchry50 <- reclassify(Verm_chryNormCR_Pan, c(-Inf, 0.1840535, 0, 0.1840535, Inf, 1))
plot(Vchry50)

Vchry75 <- reclassify(Verm_chryNormCR_Pan, c(-Inf, 0.3273431, 0, 0.3273431, Inf, 1))
plot(Vchry75) 

## Guardar para usos futuros, capa transformada
writeRaster(Verm_chryNormCR_Pan, "Vchry_inviernoCA_Norm_CR_Pan.tif", format = "GTiff",
            overwrite=TRUE)

#########

#####  Cardellina canadensis - Migracion ##############################
#####  Reinita de Canadá/Canada Warbler  ##############################

dl_path2 <- ebirdst_download(species = "Cardellina canadensis")

## Llamar al dl_path y grabar el resultado para ir directo a las capas 
##la proxima vez
dl_path2

#### Cargar los rasters que tienen la superficie de abundancia para cada semana del a?o
Car_ca <- load_raster(product = "abundance", path = dl_path2)

## Definir las semanas que queremos para el periodo de MIGRACION
Car_ca15 <- Car_ca[[15]]
Car_ca16 <- Car_ca[[16]]
Car_ca17 <- Car_ca[[17]]
Car_ca18 <- Car_ca[[18]]
Car_ca19 <- Car_ca[[19]]
Car_ca36 <- Car_ca[[36]]
Car_ca37 <- Car_ca[[37]]
Car_ca38 <- Car_ca[[38]]
Car_ca39 <- Car_ca[[39]]
Car_ca40 <- Car_ca[[40]]
Car_ca41 <- Car_ca[[41]]
Car_ca42 <- Car_ca[[42]]

## Combinar las semanas para generar una sola capa para el periodo de 
## migracion
Car_ca_mig <- Car_ca15+Car_ca16+Car_ca17+Car_ca18+Car_ca19+Car_ca36+
  Car_ca37+Car_ca38+Car_ca39+Car_ca40+Car_ca41+Car_ca42

writeRaster(Car_ca_mig, "Car_ca_mig.tif", overwrite =T)
Car_ca_raster=raster("Car_ca_mig.tif")
class(Car_ca_raster)


## Cortar a la region de interes, se puede ajustar para captar solo uno o dos 
## paises

template <- Car_ca_raster
extent(template)<- c(-10500000, -6000000, -2000000, 3500000)
Car_ca.corte <- crop(Car_ca_raster,template)
plot(Car_ca.corte)

Car_ca_WGS <- projectRaster(Car_ca.corte, 
                               crs = as.character(Elevation@crs))

plot(Car_ca_WGS)

## cortamos con el poligono del area de estudio

Car_ca_AbundCA = crop(Car_ca_WGS,AreaEstudio)

plot(Car_ca_AbundCA)

## Guardar el raster para no tener que hacer los pasos arriba cada vez
writeRaster(Car_ca_AbundCA, "Car_ca_AbundCA.tif", 
            format = "GTiff")

## Cortar la capa a las mismas dimensiones a lo de elevacion
Car_ca_AbundCA.ele <- crop(Car_ca_AbundCA,Elevation)

plot(Car_ca_AbundCA.ele)

## Eliminamos los ceros 0, transformandolos en NAs

Car_ca_AbundCA.ele[Car_ca_AbundCA.ele==0] <- NA

plot(Car_ca_AbundCA.ele)

## transformamos los valores de abundancia en un rango de 0 a 1 normalizando

min_val <- min(Car_ca_AbundCA.ele[], na.rm = T)
max_val <- max(Car_ca_AbundCA.ele[], na.rm = T)

Cca = (Car_ca_AbundCA.ele - min_val)/(max_val - min_val)
plot(Cca)
hist(Cca, ylim=c(0,15000))

# Cortamos en los limites de cada pais

Car_caNormCR_Pan = crop(Cca,AreaEstudio2)

plot(Car_caNormCR_Pan)

quantile(Car_caNormCR_Pan, c(.50, .75))

# 50%       75% 
# 0.1641240 0.2669736

Cca50 <- reclassify(Car_caNormCR_Pan, c(-Inf, 0.1641240, 0, 0.1641240, Inf, 1))
plot(Cca50)

Cca75 <- reclassify(Car_caNormCR_Pan, c(-Inf, 0.2669736, 0, 0.2669736, Inf, 1))
plot(Cca75)

## Guardar para usos futuros, capa transformada
writeRaster(Car_caNormCR_Pan, "Cca_migracionCA_Norm_CR_Pan.tif", 
            format = "GTiff", overwrite=TRUE)

Cca_migracion=raster("Cca_migracionCA_Norm_CR_Pan.tif")

#########

#####  Setophaga cerulea - Migracion ################################ 
#####  Reinita Cerúlea/Cerulean Warbler #############################

dl_path3 <- ebirdst_download(species = "Setophaga cerulea")

## Llamar al dl_path y grabar el resultado para ir directo a las capas 
##la proxima vez
dl_path3

## Cargar los rasters que tienen la superficie de abundancia para cada 
## semana del anho

Set_ce <- load_raster(product = "abundance", path = dl_path3)

## Definir las semanas que queremos para el periodo de MIGRACION
Set_ce13 <- Set_ce[[13]]
Set_ce14 <- Set_ce[[14]]
Set_ce15 <- Set_ce[[15]]
Set_ce16 <- Set_ce[[16]]
Set_ce17 <- Set_ce[[17]]
Set_ce32 <- Set_ce[[32]]
Set_ce33 <- Set_ce[[33]]
Set_ce34 <- Set_ce[[34]]
Set_ce35 <- Set_ce[[35]]
Set_ce36 <- Set_ce[[36]]
Set_ce37 <- Set_ce[[37]]
Set_ce38 <- Set_ce[[38]]

## Combinar las semanas para generar una sola capa para el periodo de 
## migracion
Set_ce_mig <- Set_ce13+Set_ce14+Set_ce15+Set_ce16+Set_ce17+
  Set_ce32+Set_ce33+Set_ce34+Set_ce35+Set_ce36+Set_ce37+Set_ce38

writeRaster(Set_ce_mig, "Set_ce_mig.tif", overwrite =T)
Set_ce_raster=raster("Set_ce_mig.tif")
class(Set_ce_raster)
plot(Set_ce_raster)

## Cortar a la region de interes, se puede ajustar para captar solo uno o dos 
## paises

template <- Set_ce_raster
extent(template)<- c(-10500000, -6000000, -2000000, 3500000)
Set_ce.corte <- crop(Set_ce_raster,template)
plot(Set_ce.corte)

Set_ce_WGS <- projectRaster(Set_ce.corte, 
                            crs = as.character(Elevation@crs))

plot(Set_ce_WGS)

Set_ce_AbundCA = crop(Set_ce_WGS,AreaEstudio)

plot(Set_ce_AbundCA)

## Guardar el raster para no tener que hacer los pasos arriba cada vez
writeRaster(Set_ce_AbundCA, "Set_ce_AbundCA.tif", 
            format = "GTiff", overwrite=TRUE)

## Cortar la capa de la cerulea a las mismas dimensiones a lo de elevaci?n
Set_ce_AbundCA.ele <- crop(Set_ce_AbundCA,Elevation)

plot(Set_ce_AbundCA.ele)

## Eliminamos los ceros 0, transformandolos en NAs

Set_ce_AbundCA.ele[Set_ce_AbundCA.ele==0] <- NA

plot(Set_ce_AbundCA.ele)

## transformamos los valores de abundancia en un rango de 0 a 1 normalizando

min_val <- min(Set_ce_AbundCA.ele[], na.rm = T)
max_val <- max(Set_ce_AbundCA.ele[], na.rm = T)

## Estandarizamos de 0 a 1 normalizando

Sce = (Set_ce_AbundCA.ele - min_val)/(max_val - min_val)
plot(Sce)
hist(Sce, ylim=c(0,15000))

# Cortamos en los limites de cada pais

Set_ceNormCR_Pan = crop(Sce,AreaEstudio2)

plot(Set_ceNormCR_Pan)

quantile(Set_ceNormCR_Pan, c(.50, .75))


## 50%        75% 
## 0.04221954 0.09520192 

Sce50 <- reclassify(Set_ceNormCR_Pan, c(-Inf, 0.04221954, 0, 0.04221954, Inf, 1))
plot(Sce50)

Sce75 <- reclassify(Set_ceNormCR_Pan, c(-Inf, 0.09520192, 0, 0.09520192, Inf, 1))
plot(Sce75) 

## Guardar para usos futuros, capa transformada
writeRaster(Set_ceNormCR_Pan, "Sce_migracionCA_Norm_CR_Pan.tif", 
            format = "GTiff", overwrite=TRUE)

Sce_migracion=raster("Sce_migracionCA_Norm_CR_Pan.tif")

#####

### Sumar las presencias

## cuantil 50%

a50=stack(Vchry50, Cca50, Sce50)
plot(a50)

SUM_AVES50 <- Vchry50 + Cca50 + Sce50
plot(SUM_AVES50)

## Seleccionar solo los pixeles con dos 2 o mas especies
FOCAL_M1.50 <-calc(SUM_AVES50, fun=function(x){ x[x < 1.9] <- 0; return(x)} )
FOCAL_M1.50 <-calc(FOCAL_M1.50, fun=function(x){ x[x > 1.9] <- 1; return(x)} )
plot(FOCAL_M1.50)

## Cortar a la elevacion del plan >750 m
Ele.crop=crop(Elevation, AreaEstudio2)
plot(Ele.crop)

Ele.re = reclassify(Ele.crop, c(-Inf, 750, 0, 750, 2000, 1, 2000, Inf, 0))
plot(Ele.re)

Ele2 <- resample(Ele.re, FOCAL_M1.50)
plot(Ele2)

b=stack(FOCAL_M1.50, Ele2)
plot(b)

Aves_Elev50 = FOCAL_M1.50 + Ele2
plot(Aves_Elev50)

FOCAL_Aves_Elev50 <-calc(Aves_Elev50, fun=function(x){ x[x < 1.9] <- 0; return(x)} )
FOCAL_Aves_Elev50 <-calc(FOCAL_Aves_Elev50, fun=function(x){ x[x > 1.9] <- 1; return(x)} )
plot(FOCAL_Aves_Elev50)

FOCAL_Aves_Elev50[FOCAL_Aves_Elev50==0] <- NA

plot(FOCAL_Aves_Elev50)

writeRaster(FOCAL_Aves_Elev50, "Focal_Areas 2 sp50_CANorm_CR_Pan.tif", format = "GTiff", 
            overwrite=TRUE)

shpFOCAL50 = rasterToPolygons(FOCAL_Aves_Elev50)
plot(shpFOCAL50)

raster::shapefile(shpFOCAL50, "shpFOCAL50.shp", overwrite=TRUE)


## cuantil 75%
a75=stack(Vchry75, Cca75, Sce75)
plot(a75)

SUM_AVES75 <- Vchry75 + Cca75 + Sce75
plot(SUM_AVES75)

## Seleccionar solo los pixeles con dos 2 o m?s especies
FOCAL_M1.75 <-calc(SUM_AVES75, fun=function(x){ x[x < 1.9] <- 0; return(x)} )
FOCAL_M1.75 <-calc(FOCAL_M1.75, fun=function(x){ x[x > 1.9] <- 1; return(x)} )
plot(FOCAL_M1.75)

Ele2 <- resample(Ele.re, FOCAL_M1.75)
plot(Ele2)

b=stack(FOCAL_M1.75, Ele2)
plot(b)

Aves_Elev75 = FOCAL_M1.75 + Ele2
plot(Aves_Elev75)

FOCAL_Aves_Elev75 <-calc(Aves_Elev75, fun=function(x){ x[x < 1.9] <- 0; return(x)} )
FOCAL_Aves_Elev75 <-calc(FOCAL_Aves_Elev75, fun=function(x){ x[x > 1.9] <- 1; return(x)} )
plot(FOCAL_Aves_Elev75)

FOCAL_Aves_Elev75[FOCAL_Aves_Elev75==0] <- NA

plot(FOCAL_Aves_Elev75)

writeRaster(FOCAL_Aves_Elev75, "Focal_Areas 2 sp75_CANorm_CR_Pan.tif", 
            format = "GTiff", overwrite=TRUE)

shpFOCAL75 = rasterToPolygons(FOCAL_Aves_Elev75)
plot(shpFOCAL75)

raster::shapefile(shpFOCAL75, "shpFOCAL75.shp")
