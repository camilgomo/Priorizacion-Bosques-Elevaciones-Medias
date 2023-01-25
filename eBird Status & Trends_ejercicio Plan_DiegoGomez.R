####### eBird status and trends - Plan de conservacion 

##Instalar los paquetes necesarios
install.packages(c("tidyverse", "raster", "sf", "ebirdst", "rnaturalearth"))
remotes::install_github("CornellLabofOrnithology/ebirdst")
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

####### Activar la clave para aceder a las capas de eBird 
##expires KEY 18 July 2022 - f0spt9d4kstc
set_ebirdst_access_key("do4n5k0bqnio", overwrite = TRUE)

## Generar unos paletas de color que vamos a usar m?s tarde
FineGray <- c("gray80", "gray72", "gray64", "gray56", "gray48", "gray42", "gray34", "gray26", "gray18", "gray10", "gray2", "black")
Ceruleanblue4 <- c("#40E0D04B", "cyan")
Forest <- c("#FFFFFF0A", "lightgreen")
Map <- c("#FFFFFF0A", "orange", "lightgreen", "darkgreen")
Baseline <- c("#FFFFFF0A", "#40E0D06E" )

## Llamar la capa de elevacion que vamos a usar como base y 
## cortarlo a nuestra region de interes.
Ele <- raster("C:/Users/ProCAT Costa Rica/Downloads/AltWorld.tif")
template <- Ele
extent(template)<- c(-95, -64, -12, 24)
Elevation <- crop(Ele,template)
plot(Elevation)

#####  Vermivora chrysoptera ################################ 

## descargar de Statu & Trends las capas de abundancia modelada
dl_path <- ebirdst_download(species = "Vermivora chrysoptera")

## Llamar al dl_path y grabar el resultado para ir directo a las capas 
## la proxima vez
dl_path

#### Cargar los rasters que tienen la superficie de abundancia para cada 
## semana del año
Verm_chry <- load_raster(product = "abundance", path = dl_path)

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

class(Verm_chry_Winter)

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

AreaEstudio = readOGR(dsn = ".", layer = "AreaEstudio_CR_Panama")

Verm_chry_AbundCRPan = crop(Verm_chry_WGS,AreaEstudio)

plot(Verm_chry_AbundCRPan)

## Guardar el raster para no tener que hacer los pasos arriba cada vez
writeRaster(Verm_chry_AbundCRPan, "Verm_chry_AbundCRPan.tif", 
            format = "GTiff")

## Cortar la capa de la cerulea a las mismas dimensiones a lo de elevaci?n
Verm_chry_AbundCRPan.ele <- crop(Verm_chry_AbundCRPan,Elevation)

plot(Verm_chry_AbundCRPan)

## Exploracion de los datos para estandarizar (este paso podr?a cambiar)
Verm_chry_AbundCRPan.ele
hist(Verm_chry_AbundCRPan.ele, ylim=c(0,10000))
quantile(Verm_chry_AbundCRPan.ele, c(.50, .60, .75, .80, .90)) 
## 99 quantile es 4.767025 y vamos a usar esto para estandarizar la capa en valores entre 0 y 1 

## Volver todos los valores de la capa entre 0 y 1. Los 10% de los valores m?s bajos > NA.
Vchry <- Verm_chry_AbundCRPan.ele/1.4
Vchry2 <- calc(Vchry, fun=function(x){ x[x > 1] <- 1; return(x)} )
Vchry2 <- calc(Vchry2, fun=function(x){ x[x < 0.01] <- NA; return(x)} )
plot(Vchry2)

## transformamos los valores de abundancia en un rango de 0 a 1 normalizando

Vchry3 = (Verm_chry_AbundCRPan.ele - 0)/(2.9894 - 0)
plot(Vchry3)
hist(Vchry3, ylim=c(0,10000))
plot(Verm_chry_AbundCRPan.ele)

## calculamos los cuantiles para determinar un umbral de abundancia para 
##priorizar la presencia y abundancia de la especie
quantile(Vchry3, c(.50, .60, .75, .80, .90))

Vchry50 <- reclassify(Vchry3, c(-Inf, 0, 0, 0, Inf, 1))
plot(Vchry50)

Vchry60 <- reclassify(Vchry3, c(-Inf, 0.0081, 0, 0.0081, Inf, 1))
plot(Vchry60)

Vchry75 <- reclassify(Vchry3, c(-Inf, 0.195, 0, 0.195, Inf, 1))
plot(Vchry75) 

Vchry80 <- reclassify(Vchry3, c(-Inf, 0.2934, 0, 0.2934, Inf, 1))
plot(Vchry80)

Vchry90 <- reclassify(Vchry3, c(-Inf, 0.4707, 0, 0.4707, Inf, 1))
plot(Vchry90)

## Guardar para usos futuros, capa transformada
writeRaster(Vchry3, "Vchry_invierno.tif", format = "GTiff")

## Crear Mapa
tiff(file="Vchry_invierno.tiff",width=1800,height=1950, res=150)
plot(Elevation, col=FineGray,cex.axis=1.5, legend=FALSE)
plot(Vchry3, col = Ceruleanblue4, add=TRUE, legend=TRUE)
plot(worldMap, add=TRUE, border = "white", cex = 2, lwd = 1.5,  legend=FALSE)
scalebar(500, xy=c(6, 24), below = "Kms", type='bar', divs=2)
dev.off()

#########

#####  Cardellina canadensis ################################     
dl_path <- ebirdst_download(species = "Cardellina canadensis")

## Llamar al dl_path y grabar el resultado para ir directo a las capas 
##la proxima vez
dl_path

#### Cargar los rasters que tienen la superficie de abundancia para cada semana del a?o
Car_ca <- load_raster(product = "abundance", path = dl_path)

## Definir las semanas que queremos para el periodo de MIGRACION
Car_ca14 <- Car_ca[[14]]
Car_ca15 <- Car_ca[[15]]
Car_ca16 <- Car_ca[[16]]
Car_ca17 <- Car_ca[[17]]
Car_ca33 <- Car_ca[[33]]
Car_ca34 <- Car_ca[[34]]
Car_ca35 <- Car_ca[[35]]
Car_ca36 <- Car_ca[[36]]
Car_ca37 <- Car_ca[[37]]
Car_ca38 <- Car_ca[[38]]

## Combinar las semanas para generar una sola capa para el periodo de 
## migracion
Car_ca_mig <- Car_ca14+Car_ca15+Car_ca16+Car_ca17+Car_ca33+
  Car_ca34+Car_ca35+Car_ca36+Car_ca37+Car_ca38

writeRaster(Car_ca_mig, "Car_ca_mig.tif", overwrite =T)
Car_ca_raster=raster("Car_ca_mig.tif")
class(Car_ca_raster)


## Cortar a la region de interes, se puede ajustar para captar solo uno o dos paises

template <- Car_ca_raster
extent(template)<- c(-10500000, -6000000, -2000000, 3500000)
Car_ca.corte <- crop(Car_ca_raster,template)
plot(Car_ca.corte)

Car_ca_WGS <- projectRaster(Car_ca.corte, 
                               crs = as.character(Elevation@crs))

plot(Car_ca_WGS)

AreaEstudio = readOGR(dsn = ".", layer = "AreaEstudio_CR_Panama")

Car_ca_AbundCRPan = crop(Car_ca_WGS,AreaEstudio)

plot(Car_ca_AbundCRPan)

## Guardar el raster para no tener que hacer los pasos arriba cada vez
writeRaster(Car_ca_AbundCRPan, "Car_ca_AbundCRPan.tif", 
            format = "GTiff")
## Car_ca_WGS <- raster("C:\\Users\\fuzzy\\Documents\\SELVA\\PIF\\Highlands Plan Workshops\\Highlands materials\\MAPS2\\CERW_winter.tif")

## Cortar la capa de la cerulea a las mismas dimensiones a lo de elevaci?n
Car_ca_AbundCRPan.ele <- crop(Car_ca_AbundCRPan,Elevation)

plot(Car_ca_AbundCRPan.ele)

## Exploracion de los datos para estandarizar (este paso podr?a cambiar)
Car_ca_AbundCRPan.ele
hist(Car_ca_AbundCRPan.ele, ylim=c(0,10000))
quantile(Car_ca_AbundCRPan.ele, c(.50, .60, .75, .80, .90)) 
## 99 quantile es 4.767025 y vamos a usar esto para estandarizar la capa en valores entre 0 y 1 

## Volver todos los valores de la capa entre 0 y 1. Los 10% de los valores m?s bajos > NA.
Cca <- Car_ca_AbundCRPan.ele/0.28
Cca2 <- calc(Cca, fun=function(x){ x[x > 1] <- 1; return(x)} )
Cca2 <- calc(Cca2, fun=function(x){ x[x < 0.01] <- NA; return(x)} )
plot(Cca2)

Cca3 = (Car_ca_AbundCRPan.ele - 0)/(0.7938 - 0)
plot(Cca3)
hist(Cca3, ylim=c(0,10000))
plot(Car_ca_AbundCRPan.ele)

quantile(Cca3, c(.50, .60, .75, .80, .90))

Cca50 <- reclassify(Cca3, c(-Inf, 0.04838, 0, 0.04838, Inf, 1))
plot(Cca50)

Cca60 <- reclassify(Cca3, c(-Inf, 0.1167, 0, 0.1167, Inf, 1))
plot(Cca60)

Cca75 <- reclassify(Cca3, c(-Inf, 0.2849, 0, 0.2849, Inf, 1))
plot(Cca75) 

Cca80 <- reclassify(Cca3, c(-Inf, 0.3543, 0, 0.3543, Inf, 1))
plot(Cca80)

Cca90 <- reclassify(Cca3, c(-Inf, 0.5031, 0, 0.5031, Inf, 1))
plot(Cca90)

## Guardar para usos futuros, capa transformada
writeRaster(Cca3, "Cca_migracion.tif", format = "GTiff")

Cca_migracion=raster("Cca_migracion.tif")

## Crear Mapa
tiff(file="Cca_migracion.tif",width=1800,height=1950, res=150)
plot(Elevation, col=FineGray,cex.axis=1.5, legend=FALSE)
plot(Vchry3, col = Ceruleanblue4, add=TRUE, legend=TRUE)
plot(worldMap, add=TRUE, border = "white", cex = 2, lwd = 1.5,  legend=FALSE)
scalebar(500, xy=c(6, 24), below = "Kms", type='bar', divs=2)
dev.off()


#########

#####  Setophaga cerulea ################################     
dl_path <- ebirdst_download(species = "Setophaga cerulea")

## Llamar al dl_path y grabar el resultado para ir directo a las capas 
##la proxima vez
dl_path

#### Cargar los rasters que tienen la superficie de abundancia para cada semana del a?o
Set_ce <- load_raster(product = "abundance", path = dl_path)

## Definir las semanas que queremos para el periodo de MIGRACION
Set_ce11 <- Set_ce[[11]]
Set_ce12 <- Set_ce[[12]]
Set_ce13 <- Set_ce[[13]]
Set_ce14 <- Set_ce[[14]]
Set_ce15 <- Set_ce[[15]]
Set_ce16 <- Set_ce[[16]]
Set_ce31 <- Set_ce[[31]]
Set_ce32 <- Set_ce[[32]]
Set_ce33 <- Set_ce[[33]]
Set_ce34 <- Set_ce[[34]]
Set_ce35 <- Set_ce[[35]]
Set_ce36 <- Set_ce[[36]]
## Combinar las semanas para generar una sola capa para el periodo de 
## migracion
Set_ce_mig <- Set_ce11+Set_ce12+Set_ce13+Set_ce14+Set_ce15+
  Set_ce16+Set_ce31+Set_ce32+Set_ce33+Set_ce34+Set_ce35+Set_ce36

writeRaster(Set_ce_mig, "Set_ce_mig.tif", overwrite =T)
Set_ce_raster=raster("Set_ce_mig.tif")
class(Set_ce_raster)
plot(Set_ce_raster)

## Cortar a la region de interes, se puede ajustar para captar solo uno o dos paises

template <- Set_ce_raster
extent(template)<- c(-10500000, -6000000, -2000000, 3500000)
Set_ce.corte <- crop(Set_ce_raster,template)
plot(Set_ce.corte)

Set_ce_WGS <- projectRaster(Set_ce.corte, 
                            crs = as.character(Elevation@crs))

plot(Set_ce_WGS)

AreaEstudio = readOGR(dsn = ".", layer = "AreaEstudio_CR_Panama")

Set_ce_AbundCRPan = crop(Set_ce_WGS,AreaEstudio)

plot(Set_ce_AbundCRPan)

## Guardar el raster para no tener que hacer los pasos arriba cada vez
writeRaster(Set_ce_AbundCRPan, "Set_ce_AbundCRPan.tif", 
            format = "GTiff")
## Car_ca_WGS <- raster("C:\\Users\\fuzzy\\Documents\\SELVA\\PIF\\Highlands Plan Workshops\\Highlands materials\\MAPS2\\CERW_winter.tif")

## Cortar la capa de la cerulea a las mismas dimensiones a lo de elevaci?n
Set_ce_AbundCRPan.ele <- crop(Set_ce_AbundCRPan,Elevation)

plot(Set_ce_AbundCRPan.ele)

## Exploracion de los datos para estandarizar (este paso podr?a cambiar)
Set_ce_AbundCRPan.ele
hist(Set_ce_AbundCRPan.ele, ylim=c(0,10000))
quantile(Set_ce_AbundCRPan.ele, c(.50, .60, .75, .80, .90)) 
## 99 quantile es 4.767025 y vamos a usar esto para estandarizar la capa en valores entre 0 y 1 

## Volver todos los valores de la capa entre 0 y 1. Los 10% de los valores m?s bajos > NA.
Sce <- Set_ce_AbundCRPan.ele/0.053
Sce2 <- calc(Sce, fun=function(x){ x[x > 1] <- 1; return(x)} )
Sce2 <- calc(Sce2, fun=function(x){ x[x < 0.01] <- NA; return(x)} )
plot(Sce2)

Sce3 = (Set_ce_AbundCRPan.ele - 0)/(0.6604 - 0)
plot(Sce3)
hist(Sce3, ylim=c(0,10000))
plot(Set_ce_AbundCRPan.ele)

quantile(Sce3, c(.50, .60, .75, .80, .90))

Sce50 <- reclassify(Sce3, c(-Inf, 0.0035, 0, 0.0035, Inf, 1))
plot(Sce50)

Sce60 <- reclassify(Sce3, c(-Inf, 0.0193, 0, 0.0193, Inf, 1))
plot(Sce60)

Sce75 <- reclassify(Sce3, c(-Inf, 0.0644, 0, 0.0644, Inf, 1))
plot(Sce75) 

Sce80 <- reclassify(Sce3, c(-Inf, 0.0803, 0, 0.0803, Inf, 1))
plot(Sce80)

Sce90 <- reclassify(Sce3, c(-Inf, 0.1268, 0, 0.1268, Inf, 1))
plot(Sce90)

## Guardar para usos futuros, capa transformada
writeRaster(Sce3, "Sce_migracion.tif", format = "GTiff")
plot(Sce3)
Cca_migracion=raster("Cca_migracion.tif")

## Crear Mapa
tiff(file="Cca_migracion.tif",width=1800,height=1950, res=150)
plot(Elevation, col=FineGray,cex.axis=1.5, legend=FALSE)
plot(Vchry3, col = Ceruleanblue4, add=TRUE, legend=TRUE)
plot(worldMap, add=TRUE, border = "white", cex = 2, lwd = 1.5,  legend=FALSE)
scalebar(500, xy=c(6, 24), below = "Kms", type='bar', divs=2)
dev.off()




#####

### Sumar las presencias

## cuantil 50%

a50=stack(Vchry50, Cca50, Sce50)
plot(a50)

SUM_AVES50 <- Vchry50 + Cca50 + Sce50
plot(SUM_AVES50)

## Seleccionar solo los pixeles con dos 2 o m?s especies
FOCAL_M1.50 <-calc(SUM_AVES50, fun=function(x){ x[x < 1.9] <- 0; return(x)} )
FOCAL_M1.50 <-calc(FOCAL_M1.50, fun=function(x){ x[x > 1.9] <- 1; return(x)} )
plot(FOCAL_M1.50)

## Cortar a la elevaci?n del plan >750 m
Ele.crop=crop(Elevation, AreaEstudio)
plot(Ele.crop)

Ele.re = reclassify(Ele.crop, c(-Inf, 750, 0, 750, 2000, 1, 2000, Inf, 0))
plot(Ele.re)

Ele2 <- resample(Ele.re, FOCAL_M1.50)
plot(Ele2)

b=stack(FOCAL_M1.50, Ele2)
plot(b)

Aves_Elev50 = FOCAL_M1.50 + Ele2
plot(Aves_Elev50)

Prior_2spp.50 = reclassify(Aves_Elev50, c(-Inf, 1.9, 0, 2, Inf, 1))

plot(Prior_2spp.50)

writeRaster(Prior_2spp.50, "Focal_Areas 2 sp50.tif", format = "GTiff")


## cuantil 60%

a60=stack(Vchry60, Cca60, Sce60)
plot(a60)

SUM_AVES60 <- Vchry60 + Cca60 + Sce60
plot(SUM_AVES60)

## Seleccionar solo los pixeles con dos 2 o m?s especies
FOCAL_M1.60 <-calc(SUM_AVES60, fun=function(x){ x[x < 1.9] <- 0; return(x)} )
FOCAL_M1.60 <-calc(FOCAL_M1.60, fun=function(x){ x[x > 1.9] <- 1; return(x)} )
plot(FOCAL_M1.60)

Ele2 <- resample(Ele.re, FOCAL_M1.60)
plot(Ele2)

b=stack(FOCAL_M1.60, Ele2)
plot(b)

Aves_Elev60 = FOCAL_M1.60 + Ele2
plot(Aves_Elev60)

Prior_2spp.60 = reclassify(Aves_Elev60, c(-Inf, 1.9, 0, 2, Inf, 1))

plot(Prior_2spp.60)

writeRaster(Prior_2spp.60, "Focal_Areas 2 sp60.tif", format = "GTiff")

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

Prior_2spp.75 = reclassify(Aves_Elev75, c(-Inf, 1.9, 0, 2, Inf, 1))

plot(Prior_2spp.75)

writeRaster(Prior_2spp.75, "Focal_Areas 2 sp75.tif", format = "GTiff")


## cuantil 80%

a80=stack(Vchry80, Cca80, Sce80)
plot(a80)

SUM_AVES80 <- Vchry80 + Cca80 + Sce80
plot(SUM_AVES80)

## Seleccionar solo los pixeles con dos 2 o m?s especies
FOCAL_M1.80 <-calc(SUM_AVES80, fun=function(x){ x[x < 1.9] <- 0; return(x)} )
FOCAL_M1.80 <-calc(FOCAL_M1.80, fun=function(x){ x[x > 1.9] <- 1; return(x)} )
plot(FOCAL_M1.80)

Ele2 <- resample(Ele.re, FOCAL_M1.80)
plot(Ele2)

b=stack(FOCAL_M1.80, Ele2)
plot(b)

Aves_Elev80 = FOCAL_M1.80 + Ele2
plot(Aves_Elev80)

Prior_2spp.80 = reclassify(Aves_Elev80, c(-Inf, 1.9, 0, 2, Inf, 1))

plot(Prior_2spp.80)

writeRaster(Prior_2spp.80, "Focal_Areas 2 sp80.tif", format = "GTiff")


## cuantil 90%

a90=stack(Vchry90, Cca90, Sce90)
plot(a90)

SUM_AVES90 <- Vchry90 + Cca90 + Sce90
plot(SUM_AVES90)

## Seleccionar solo los pixeles con dos 2 o m?s especies
FOCAL_M1.90 <-calc(SUM_AVES90, fun=function(x){ x[x < 1.9] <- 0; return(x)} )
FOCAL_M1.90 <-calc(FOCAL_M1.90, fun=function(x){ x[x > 1.9] <- 1; return(x)} )
plot(FOCAL_M1.90)

Ele2 <- resample(Ele.re, FOCAL_M1.90)
plot(Ele2)

b=stack(FOCAL_M1.90, Ele2)
plot(b)

Aves_Elev90 = FOCAL_M1.90 + Ele2
plot(Aves_Elev90)

Prior_2spp.90 = reclassify(Aves_Elev90, c(-Inf, 1.9, 0, 2, Inf, 1))

plot(Prior_2spp.90)

writeRaster(Prior_2spp.90, "Focal_Areas 2 sp90.tif", format = "GTiff")

prior_cuantiles=stack(Prior_2spp.50, Prior_2spp.60, Prior_2spp.75,
                      Prior_2spp.80,  Prior_2spp.90)


plot(prior_cuantiles)

sum_prior_cuantiles = (Prior_2spp.50 + Prior_2spp.60 + Prior_2spp.75 +
                       Prior_2spp.80 +  Prior_2spp.90)/5

plot(sum_prior_cuantiles)


########## CERW Migracion ################################################## 
dl_path <-  "C:\\Users\\fuzzy\\AppData\\Local\\ebirdst\\ebirdst\\cerwar-ERD2019-STATUS-20201002-a1fafd19"

CERW <- load_raster(product = "abundance", path = dl_path)

CERW11 <- CERW[[11]]
CERW12 <- CERW[[12]]
CERW13 <- CERW[[13]]
CERW14 <- CERW[[14]]
CERW15 <- CERW[[15]]
CERW16 <- CERW[[16]]
CERW31 <- CERW[[31]]
CERW32 <- CERW[[32]]
CERW33 <- CERW[[33]]
CERW34 <- CERW[[34]]
CERW35 <- CERW[[35]]
CERW36 <- CERW[[36]]

CERW_M <- CERW11+CERW12+CERW13+CERW14+CERW15+CERW16+CERW31+CERW32+CERW33+CERW34+CERW35+CERW36

## Cortar a la region de interes
template <- CERW_M
extent(template)<- c(-10500000, -6000000, -2000000, 3500000)
CW2 <- crop(CERW_M,template)

## Reprojectar
CERW_WGS <- projectRaster(CW2, crs = as.character(Elevation@crs))

writeRaster(CERW_WGS, "CERW_migration.tif", format = "GTiff")
##CERW_WGS <- raster("C:\\Users\\fuzzy\\Documents\\SELVA\\PIF\\Highlands Plan Workshops\\Highlands materials\\MAPS2\\CERW_migration.tif")

## Cortar para modelar migraci?n en Centroamerica
template <- CERW_WGS
extent(template)<- c(-95, -77, 6, 24)
CERW <- crop(CERW_WGS,template)

## Explorar datos
CERW
hist(CERW, ylim=c(0,10000))
quantile(CERW, c(.95, .975, .99)) 
#99 quantile is 0.9043266

## Adjustar para generar valores entre 0 y 1
CERW2 <- CERW/0.9043266
CERW3 <- calc(CERW2, fun=function(x){ x[x > 1] <- 1; return(x)} )
CERW3 <- calc(CERW3, fun=function(x){ x[x < 0.01] <- NA; return(x)} )

## Volver el raster al mismo extensi?n con que empezamos
CERW_M <- extend(CERW3, Elevation, value=NA)
writeRaster(CERW_M, "CERW_MigT.tif", format = "GTiff")
##CERW_MigT <- raster("C:\\Users\\fuzzy\\Documents\\SELVA\\PIF\\Highlands Plan Workshops\\Highlands materials\\MAPS2\\CERW_MigT.tif")


## Generar mapa
tiff(file="CERW_migration.tiff",width=1800,height=1950, res=150)
plot(Elevation, col=FineGray,cex.axis=1.5, legend=FALSE)
plot(CERW_M, col = Ceruleanblue4, add=TRUE, legend=TRUE)
plot(worldMap, add=TRUE, border = "white", cex = 2, lwd = 1.5,  legend=FALSE)
scalebar(500, xy=c(-94, -11), below = "Kms", type='bar', divs=2)
dev.off()

#####  CANADA WARBLER ############################################################################# 
##dl_path <- ebirdst_download(species = "Canada Warbler")
dl_path
dl_path <- "C:\\Users\\fuzzy\\AppData\\Local\\ebirdst\\ebirdst\\canwar-ERD2019-STATUS-20201005-922894ac"

CERW <- load_raster(product = "abundance", path = dl_path)
CERW1 <- CERW[[1]]
CERW2 <- CERW[[2]]
CERW3 <- CERW[[3]]
CERW4 <- CERW[[4]]
CERW5 <- CERW[[5]]
CERW6 <- CERW[[6]]
CERW7 <- CERW[[7]]
CERW8 <- CERW[[8]]
CERW49 <- CERW[[49]]
CERW50 <- CERW[[50]]
CERW51 <- CERW[[51]]
CERW52 <- CERW[[52]]

CAWA_Winter <- CERW1+CERW2+CERW3+CERW4+CERW5+CERW6+CERW7+CERW8+CERW49+CERW50+CERW51+CERW52

## Crop to region of interest
template <- CAWA_Winter
extent(template)<- c(-10500000, -6000000, -2000000, 3500000)
CAWA2 <- crop(CAWA_Winter,template)

## Reproject using Elevation
CAWA_WGS <- projectRaster(CAWA2, crs = as.character(Elevation@crs))

writeRaster(CAWA_WGS, "CAWA_winter.tif", format = "GTiff")
CAWA_WGS <- raster("C:\\Users\\fuzzy\\Documents\\SELVA\\PIF\\Highlands Plan Workshops\\Highlands materials\\MAPS2\\CAWA_winter.tif")

CAWA <- crop(CAWA_WGS,Elevation)

CAWA
hist(CAWA, ylim=c(0,10000))
quantile(CAWA, c(.95, .975, .99)) 
#99 quantile is 9.249645

CAWA2 <- CAWA/9.249645
CAWA3 <- calc(CAWA2, fun=function(x){ x[x > 1] <- 1; return(x)} )
CAWA3 <- calc(CAWA3, fun=function(x){ x[x < 0.01] <- NA; return(x)} )

writeRaster(CAWA3, "CAWA_winterT.tif", format = "GTiff")

## Create Map
tiff(file="CAWA_winter.tiff",width=1800,height=1950, res=150)
plot(Elevation, col=FineGray,cex.axis=1.5, legend=FALSE)
plot(CAWA3, col = Ceruleanblue4, add=TRUE, legend=TRUE)
plot(worldMap, add=TRUE, border = "white", cex = 2, lwd = 1.5,  legend=FALSE)
scalebar(500, xy=c(-94, -11), below = "Kms", type='bar', divs=2)
dev.off()

########## CAWA Migration ###################################################################### 
dl_path <- "C:\\Users\\fuzzy\\AppData\\Local\\ebirdst\\ebirdst\\canwar-ERD2019-STATUS-20201005-922894ac"

CERW <- load_raster(product = "abundance", path = dl_path)

CERW14 <- CERW[[14]]
CERW15 <- CERW[[15]]
CERW16 <- CERW[[16]]
CERW17 <- CERW[[17]]
CERW33 <- CERW[[33]]
CERW34 <- CERW[[34]]
CERW35 <- CERW[[35]]
CERW36 <- CERW[[36]]
CERW37 <- CERW[[37]]
CERW38 <- CERW[[38]]

CERW_M <- CERW14+CERW15+CERW16+CERW17+CERW33+CERW34+CERW35+CERW36+CERW37+CERW38

## Crop to region of interest
template <- CERW_M
extent(template)<- c(-10500000, -6000000, -2000000, 3500000)
CW2 <- crop(CERW_M,template)

## Reproject using Elevation
CAWA_WGS <- projectRaster(CW2, crs = as.character(Elevation@crs))

writeRaster(CAWA_WGS, "CAWA_migration.tif", format = "GTiff")
##CAWA_WGS <- raster("C:\\Users\\fuzzy\\Documents\\SELVA\\PIF\\Highlands Plan Workshops\\Highlands materials\\MAPS2\\CAWA_migration.tif")

template <- CAWA_WGS
extent(template)<- c(-95, -77, 6, 24)
CAWA <- crop(CAWA_WGS,template)

CAWA 
hist(CAWA, ylim=c(0,10000))
quantile(CAWA, c(.95, .975, .99)) 
#99 quantile is 2.137404

CAWA2 <- CAWA/2.137404
CAWA3 <- calc(CAWA2, fun=function(x){ x[x > 1] <- 1; return(x)} )
CAWA3 <- calc(CAWA3, fun=function(x){ x[x < 0.01] <- NA; return(x)} )

CAWA_M <- extend(CAWA3, Elevation, value=NA)
writeRaster(CAWA_M, "CAWA_MigT.tif", format = "GTiff")
##CAWA_MigT <- raster("C:\\Users\\fuzzy\\Documents\\SELVA\\PIF\\Highlands Plan Workshops\\Highlands materials\\MAPS2\\CAWA_MigT.tif")

## Create Map
tiff(file="CAWA_migration.tiff",width=1800,height=1950, res=150)
plot(Elevation, col=FineGray,cex.axis=1.5, legend=FALSE)
plot(CAWA_M, col = Ceruleanblue4, add=TRUE, legend=TRUE)
plot(worldMap, add=TRUE, border = "white", cex = 2, lwd = 1.5,  legend=FALSE)
scalebar(500, xy=c(-94, -11), below = "Kms", type='bar', divs=2)
dev.off()

###################   Golden-winged Warbler #############################################################   
##dl_path <- ebirdst_download(species = "Golden-winged Warbler")
dl_path

dl_path <- "C:\\Users\\fuzzy\\AppData\\Local\\ebirdst\\ebirdst\\gowwar-ERD2019-STATUS-20201002-bd794e90"

CERW <- load_raster(product = "abundance", path = dl_path)
CERW1 <- CERW[[1]]
CERW2 <- CERW[[2]]
CERW3 <- CERW[[3]]
CERW4 <- CERW[[4]]
CERW5 <- CERW[[5]]
CERW6 <- CERW[[6]]
CERW7 <- CERW[[7]]
CERW8 <- CERW[[8]]
CERW49 <- CERW[[49]]
CERW50 <- CERW[[50]]
CERW51 <- CERW[[51]]
CERW52 <- CERW[[52]]

GWWA_Winter <- CERW1+CERW2+CERW3+CERW4+CERW5+CERW6+CERW7+CERW8+CERW49+CERW50+CERW51+CERW52

## Crop to region of interest
template <- GWWA_Winter
extent(template)<- c(-10500000, -6000000, -2000000, 3500000)
CAWA2 <- crop(GWWA_Winter,template)

## Reproject using Elevation
GWWA_WGS <- projectRaster(CAWA2, crs = as.character(Elevation@crs))

writeRaster(GWWA_WGS, "GWWA_winter.tif", format = "GTiff")

##GWWA_WGS <- raster("C:\\Users\\fuzzy\\Documents\\SELVA\\PIF\\Highlands Plan Workshops\\Highlands materials\\MAPS2\\GWWA_winter.tif")

GWWA <- crop(GWWA_WGS,Elevation)

GWWA
hist(GWWA, ylim=c(0,10000))
quantile(GWWA, c(.95, .975, .99)) 
#99 quantile is 4.6411595

GWWA2 <- GWWA/4.6411595
GWWA3 <- calc(GWWA2, fun=function(x){ x[x > 1] <- 1; return(x)} )
GWWA3 <- calc(GWWA3, fun=function(x){ x[x < 0.01] <- NA; return(x)} )

writeRaster(GWWA3, "GWWA_winterT.tif", format = "GTiff")

## Create Map
tiff(file="GWWA_winter.tiff",width=1800,height=1950, res=150)
plot(Elevation, col=FineGray,cex.axis=1.5, legend=FALSE)
plot(GWWA3, col = Ceruleanblue4, add=TRUE, legend=TRUE)
plot(worldMap, add=TRUE, border = "white", cex = 2, lwd = 1.5,  legend=FALSE)
scalebar(500, xy=c(-94, -11), below = "Kms", type='bar', divs=2)
dev.off()


######## Combinando las 3 especies  #############################################################   

### WINTER & MIGRATION
CAWAw <- raster("C:\\Users\\fuzzy\\Documents\\SELVA\\PIF\\Highlands Plan Workshops\\Highlands materials\\MAPS2\\CAWA_winterT.tif")
CAWAm <- raster("C:\\Users\\fuzzy\\Documents\\SELVA\\PIF\\Highlands Plan Workshops\\Highlands materials\\MAPS2\\CAWA_MigT.tif")
CERWw <- raster("C:\\Users\\fuzzy\\Documents\\SELVA\\PIF\\Highlands Plan Workshops\\Highlands materials\\MAPS2\\CERW_winterT.tif")
CERWm <- raster("C:\\Users\\fuzzy\\Documents\\SELVA\\PIF\\Highlands Plan Workshops\\Highlands materials\\MAPS2\\CERW_MigT.tif")
GWWAw <- raster("C:\\Users\\fuzzy\\Documents\\SELVA\\PIF\\Highlands Plan Workshops\\Highlands materials\\MAPS2\\GWWA_winterT.tif")


### CERW invierno, crear una raster binario, top 25% de los pixeles en cuanto a abundancia
hist(CERWw, ylim=c(0,10000))
quantile(CERWw, c(.25, .50, .75)) 
#75% quantile is 0.316
CERWw2 <- calc(CERWw, fun=function(x){ x[x > 0.315] <- 1; return(x)} )
CERWw3 <- calc(CERWw2, fun=function(x){ x[x < 1] <- NA; return(x)} )

### CERW migracion binary top 25%
hist(CERWm, ylim=c(0,10000))
quantile(CERWm, c(.25, .50, .75)) 
#75% quantile is 0.271
CERWm2 <- calc(CERWm, fun=function(x){ x[x > 0.270] <- 1; return(x)} )
CERWm3 <- calc(CERWm2, fun=function(x){ x[x < 1] <- NA; return(x)} )

### CAWA winter binary top 25%
hist(CAWAw, ylim=c(0,10000))
quantile(CAWAw, c(.25, .50, .75)) 
#75% quantile is 0.712
CAWAw2 <- calc(CAWAw, fun=function(x){ x[x > 0.711] <- 1; return(x)} )
CAWAw3 <- calc(CAWAw2, fun=function(x){ x[x < 1] <- NA; return(x)} )

### CAWA migracion binary top 25%
hist(CAWAm, ylim=c(0,10000))
quantile(CAWAm, c(.25, .50, .75)) 
#75% quantile is 0.510
CAWAm2 <- calc(CAWAm, fun=function(x){ x[x > 0.509] <- 1; return(x)} )
CAWAm3 <- calc(CAWAm2, fun=function(x){ x[x < 1] <- NA; return(x)} )

### GWWA winter binary top 25%
hist(GWWAw, ylim=c(0,10000))
quantile(GWWAw, c(.25, .50, .75)) 
#75% quantile is 0.580
GWWAw2 <- calc(GWWAw, fun=function(x){ x[x > 0.579] <- 1; return(x)} )
GWWAw3 <- calc(GWWAw2, fun=function(x){ x[x < 1] <- NA; return(x)} )

####################### Cominbando las 3 especies
### Convertir NAs a 0 para poder combinar los rasters
CAWAw3[is.na(CAWAw3[])] <- 0
CAWAm3[is.na(CAWAm3[])] <- 0
CERWw3[is.na(CERWw3[])] <- 0
CERWm3[is.na(CERWm3[])] <- 0
GWWAw3[is.na(GWWAw3[])] <- 0

##Centroamerica
COMB_CA <- CAWAm3 + CERWm3 + GWWAw3

##Suramerica
COMB_SA <- CAWAw3 + CERWw3 + GWWAw3


## Seleccionar solo los pixeles con dos 2 o m?s especies
FOCAL_M1 <-calc(COMB, fun=function(x){ x[x < 1.9] <- 0; return(x)} )
FOCAL_M1 <-calc(FOCAL_M1, fun=function(x){ x[x > 1.9] <- 1; return(x)} )

## Cortar a la elevaci?n del plan >750 m
Ele1 <-calc(Elevation, fun=function(x){ x[x < 750] <- 1; return(x)} )
Ele1 <-calc(Ele1, fun=function(x){ x[x > 749] <- 0; return(x)} )

# ajustar resoluci?n para que sean iguales entre capas
Ele2 <- resample(Ele1, FOCAL_M1)
Ele3 <-calc(Ele2, fun=function(x){ x[x < 1] <- 0; return(x)} )

FOCAL_M2 <- FOCAL_M1 - Ele3

## limpiar
FOCAL_M2 <-calc(FOCAL_M2, fun=function(x){ x[x < 1] <- 0; return(x)} )

## Guardar areas focal base (shape??)
writeRaster(FOCAL_M2, "Focal_Areas 2 sp.tif", format = "GTiff")
FOCAL_M2 <- raster("C:\\Users\\fuzzy\\Documents\\SELVA\\PIF\\Highlands Plan Workshops\\Highlands materials\\MAPS2\\Focal_Areas 2 sp.tif")



