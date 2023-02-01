####### eBird status and trends - Plan de conservacion 

####### Activar la llave para aceder a las capas de eBird 
##expires KEY 18 July 2022 
#set_ebirdst_access_key("7s5n13g2ukna", overwrite = TRUE)


##############################################################################
#Cargar librerias
library(ebirdst)
library(raster)
library(dplyr)
library(tidyverse) # entra en conflicto con otras funciones raster::extract() / raster::select()
library(sf) 
library(sp)
library(terra)
library(rgdal)
library(rworldmap)
worldMap <- getMap()

## Generar unos paletas de color que vamos a usar mas tarde
FineGray <- c("gray80", "gray72", "gray64", "gray56", "gray48", "gray42", "gray34", "gray26", "gray18", "gray10", "gray2", "black")
Ceruleanblue4 <- c("#40E0D04B", "cyan")
Forest <- c("#FFFFFF0A", "lightgreen")
Map <- c("#FFFFFF0A", "orange", "lightgreen", "darkgreen")
Baseline <- c("#FFFFFF0A", "#40E0D06E" )

## Llamar la capa de elevacion que vamos a usar como base y cortarlo a nuestra region de interes.
Ele <- raster("C:\\Users\\BYU Rental\\Desktop\\Taller\\AltWorld.tif") # WGS84
Elevation <- crop(Ele,Area)

#####  Cerulean Warbler ################################################################################         
dl_path <- ebirdst_download(species = "Cerulean Warbler") #Linea para LLAMAR los datos de la ESPECIE


## Llamar al dl_patch y grabar el resultado para ir directo a las capas la proxima vez
dl_path

###dl_path <-  "C:\\Users\\BYU Rental\\AppData\\Roaming\\R\\data\\R\\ebirdst\\2021\\cerwar"

#### Cargar los rasters que tienen la superficie de abundancia para cada semana del a#o
ceru <- load_raster(product = "abundance", path = dl_path)

weeks <- parse_raster_dates(ceru)

## Definir las semanas que queremos para el periodo de MIGRACION
ceru_mig <- ceru[[c(13, 14, 15, 16, 17, 32, 33, 34, 35, 36, 37, 38)]]


## Guardamos el raster generado y volvemos a abrirlo con la funcion raster
## para que pase de clase spatraster a rasterlayer
writeRaster(ceru_mig, "ceru_mig.tif", overwrite =T)
ceru_mig_raster=raster("ceru_mig.tif")
class(ceru_mig_raster)


## Cortar a la region de interes
template <- ceru_mig_raster
extent(template)<- c(-10500000, -6000000, -2000000, 3500000)
ceru_crop <- crop(ceru_mig_raster,template)

plot(ceru_crop)

## Reprojectar usando la capa de elevacion para convertirlo a WGS84
cer_WGS  <- projectRaster(ceru_crop, 
                          crs = as.character(Elevation@crs))


## Para cortar a nuestra area de interes 

#cargar los shapes para Honduras y Nicaragua
HN = readOGR(dsn = "C:/Users/BYU Rental/Desktop/R/hnd")
NI = readOGR(dsn = "C:/Users/BYU Rental/Desktop/R/nica")

Area <- bind(HN, NI) #Unir shapes de ambos paises

#Resto de Centroamerica
GT = readOGR(dsn = "C:/Users/BYU Rental/Desktop/R/gua")
CR= readOGR(dsn = "C:/Users/BYU Rental/Desktop/R/cr")
PA = readOGR(dsn = "C:/Users/BYU Rental/Desktop/R/pa")
CHI = readOGR(dsn = "C:/Users/BYU Rental/Desktop/R/chia")

ca <- bind(Area, GT, CR, PA, CHI)
plot(ca)


#cortar a centroamerica
ceru_cam = crop(cer_WGS,ca)
plot(ceru_cam)


## Cortar la capa de ceru_cam a las mismas dimensiones de elevacion
ceru_ele <- crop(ceru_cam,Elevation)
cer_ele <- writeRaster(ceru_ele, "ceru_ele.tif", overwrite =T)

## Exploracion de los datos para estandarizar (este paso podria cambiar)
ceru_ele
hist(ceru_ele, ylim=c(0,10000))

ceru.sin <- ceru_ele
ceru.sin[ceru.sin==0] <- NA  #transformando cero en NA

#Normalizar Datos
min_val <- min(ceru.sin[], na.rm = T)
max_val <- max(ceru.sin[], na.rm = T)

norm_cer <- (ceru.sin - min_val)/(max_val - min_val)
norm_cer


## Guardar para usos futuros
writeRaster(norm_cer, "norm_cer.tif", overwrite =T)


#####  Canada Warbler ############################################################################         
dl_path2 <- ebirdst_download(species = "Canada Warbler") #Linea para LLAMAR los datos de la ESPECIE


## Llamar al dl_patch y grabar el resultado para ir directo a las capas la proxima vez
dl_path2

###dl_path2 <-  "C:\\Users\\BYU Rental\\AppData\\Roaming\\R\\data\\R\\ebirdst\\2021\\canwar"

#### Cargar los rasters que tienen la superficie de abundancia para cada semana del a#o
cana <- load_raster(product = "abundance", path = dl_path2)

weeks <- parse_raster_dates(cana)

## Definir las semanas que queremos para el periodo de MIGRACION
cana_mig <- cana[[c(15, 16, 17, 18, 19, 36, 37, 38, 39, 40, 41, 42)]]


## Guardamos el raster generado y volvemos a abrirlo con la funcion raster para que pase de clase spatraster a rasterlayer
writeRaster(cana_mig, "cana_mig.tif", overwrite =T)
cana_mig_raster=raster("cana_mig.tif")
class(cana_mig_raster)


## Cortar a la region de interes, se puede ajustar para captar solo uno o dos paises
template <- cana_mig_raster
extent(template)<- c(-10500000, -6000000, -2000000, 3500000)
cana_crop <- crop(cana_mig_raster,template)

plot(cana_crop)

## Reprojectar usando la capa de elevacion para convertirlo a WGS84
cana_WGS  <- projectRaster(cana_crop, 
                          crs = as.character(Elevation@crs))

plot(cana_WGS)


## Para cortar a Centroamerica
cana_cam = crop(cana_WGS,ca)
plot(cana_cam)

## Guardar el raster 
writeRaster(cana_cam, "canaM_cam.tif", overwrite =T)


## Cortar la capa a las mismas dimensiones de la elevacion
cana_ele <- crop(cana_cam,Elevation)

## Exploracion de los datos para estandarizar (este paso podria cambiar)
cana_ele
hist(cana_ele, ylim=c(0,10000))

cana.sin <- cana_ele
cana.sin[cana.sin==0] <- NA  #transformando cero en NA


#Normalizar Datos
min_val <- min(cana.sin[], na.rm = T)
max_val <- max(cana.sin[], na.rm = T)

norm_can <- (cana.sin - min_val)/(max_val - min_val)
norm_can

## Guardar para usos futuros, capa transformada
writeRaster(norm_can, "norm_can.tif", overwrite =T)


#################  Vermivora chrysoptera ####################################################### 

## descargar de Statu & Trends las capas de abundancia modelada
dl_path3 <- ebirdst_download(species = "Vermivora chrysoptera")

## Llamar al dl_path y grabar el resultado para ir directo a las capas 
## la proxima vez
dl_path3

#### Cargar los rasters que tienen la superficie de abundancia para cada 
## semana del año
gol <- load_raster(product = "abundance", path = dl_path3)

## Definir las semanas que queremos para el periodo de INVIERNO (estacionario)

gol_mig <- gol[[c(1, 2, 3, 4, 5, 6, 7, 8, 49, 50, 51, 52)]]


## Guardamos el raster generado y volvemos a abrirlo con la funcion raster
## para que pase de clase spatraster a rasterlayer
writeRaster(gol_mig, "gol_mig.tif", overwrite =T)
gol_raster=raster("gol_mig.tif")
class(gol_raster)


## Cortar a la region de interes, se puede ajustar para captar solo uno o dos 
## paises
template <- gol_raster
extent(template)<- c(-10500000, -6000000, -2000000, 3500000)
gol_crop <- crop(gol_raster,template)
plot(gol_crop)

## Reproyectamos el raster con crs WGS84 basandonos en la capa de elevacion
gol_WGS <- projectRaster(gol_crop, 
                               crs = as.character(Elevation@crs))

plot(gol_WGS)

## Para cortar a Centroamerica
gol_cam = crop(gol_WGS,ca)
plot(gol_cam)

## Guardar el raster
writeRaster(gol_cam, "golM_ca.tif", overwrite =T) 
      

## Cortar la capa a las mismas dimensiones a lo de elevacion
gol.ele <- crop(gol_cam,Elevation)

plot(gol.ele)

## Exploracion de los datos para estandarizar (este paso podria cambiar)
gol.ele
hist(gol.ele, ylim=c(0,15000))

gol.sin <- gol.ele
gol.sin[gol.sin==0] <- NA  #transformando cero en NA

#Normalizar Datos
min_val <- min(gol.sin[], na.rm = T)
max_val <- max(gol.sin[], na.rm = T)

norm_gol <- (gol.sin - min_val)/(max_val - min_val)
hist(norm_gol)


## Guardar para usos futuro
writeRaster(norm_gol, "norm_gol.tif", overwrite =T)


######## COMBINANDO LAS 3 ESPECIES  ############################################   

### WINTER & MIGRATION
ceruM <- raster("C:/Users/BYU Rental/Desktop/R/norm_cer.tif")
canaM <- raster("C:/Users/BYU Rental/Desktop/R/norm_can.tif")
golW <- raster("C:/Users/BYU Rental/Desktop/R/norm_gol.tif")

#Cortar al area de estudio
ceruM <- crop(ceruM, Area) 
canaM <- crop(canaM, Area)
golW <- crop(golW, Area)


########## RASTER .50 de los pixeles en cuanto a abundancia 

###   Normalizando Datos
# CERWAR
min_val <- min(ceruM[], na.rm = T)
max_val <- max(ceruM[], na.rm = T)

norm_ceruM <- (ceruM - min_val)/(max_val - min_val)

#CANWAR
min_val <- min(canaM[], na.rm = T)
max_val <- max(canaM[], na.rm = T)

norm_canaM <- (canaM - min_val)/(max_val - min_val)

#GWWAR
min_val <- min(golW[], na.rm = T)
max_val <- max(golW[], na.rm = T)

norm_golW <- (golW - min_val)/(max_val - min_val)

########## RASTER .50 de los pixeles en cuanto a abundancia (Normalizados)
# CERWAR
quantile(norm_ceruM, c(.50)) 

#50% quantile is 0.1177528  
ceruM.5 <- calc(norm_ceruM, fun=function(x){ x[x > 0.1177528   ] <- 1; return(x)} )
ceruM3.5 <- calc(ceruM.5, fun=function(x){ x[x < 1] <- NA; return(x)} )

####
### CANWAR 
quantile(norm_canaM, c(.50)) 

#50% quantile is 0.1797606   
canaM.5 <- calc(norm_canaM, fun=function(x){ x[x > 0.1797606  ] <- 1; return(x)} )
canaM3.5 <- calc(canaM.5, fun=function(x){ x[x < 1] <- NA; return(x)} )

######
### GWWAR
quantile(norm_golW, c(.50)) 

#50% quantile is 0.1856677 
golW.5 <- calc(norm_golW, fun=function(x){ x[x > 0.1856677  ] <- 1; return(x)} )
golW3.5 <- calc(golW.5, fun=function(x){ x[x < 1] <- NA; return(x)} )


####################### Combinando las 3 especies para 50%

### Convertir NAs a 0 para poder combinar los rasters
ceruM3.5[is.na(ceruM3.5[])] <- 0
canaM3.5[is.na(canaM3.5[])] <- 0
golW3.5[is.na(golW3.5[])] <- 0

COMB_CA.5 <- ceruM3.5 + canaM3.5 + golW3.5

## Seleccionar solo los pixeles con dos 2 o mas especies
FOCAL_M1.5 <-calc(COMB_CA.5, fun=function(x){ x[x < 1.9] <- 0; return(x)} )
FOCAL_M1.5 <-calc(FOCAL_M1.5, fun=function(x){ x[x > 1.9] <- 1; return(x)} )


## Cortar a la elevacion del plan 750 - 2000 m
Ele.re = reclassify(Elevation2, c(-Inf, 750, 0, 750, 2000, 1, 2000, Inf, 0))

# ajustar resolucion para que sean iguales entre capas
Ele2. <- resample(Ele.re, FOCAL_M1.5)

FOCAL_M1.5 <- FOCAL_M1.5 + Ele2.

## limpiar
FOCAL_M2.5 <- reclassify(FOCAL_M1.5 , c(-Inf, 1.9, 0, 2, Inf, 1))

#Reemplazando ceros
focal.5 <- FOCAL_M2.5
focal.5[focal.5==0] <- NA  #transformando cero en NA 

## Guardar area focal
writeRaster(FOCAL_M2.5, "Focal_50.tif", overwrite =T)

#Guardar como shp
focal_50 = rasterToPolygons(focal.5)
raster::shapefile(focal_50, "Focal_50.shp", overwrite=TRUE)


############################ RASTER .75 de los pixeles en cuanto a abundancia

######   Valores normalizados
######  CERWAR
quantile(norm_ceruM, c(.75)) 

#75% quantile 0.3104568  
ceruM.7 <- calc(norm_ceruM, fun=function(x){ x[x > 0.3104568  ] <- 1; return(x)} )
ceruM3.7 <- calc(ceruM.7, fun=function(x){ x[x < 1] <- NA; return(x)} )


####### CANWAR
quantile(norm_canaM, c(.75)) 

#75% quantile is 0.3363886 
canaM.7 <- calc(norm_canaM, fun=function(x){ x[x > 0.3363886 ] <- 1; return(x)} )
canaM3.7 <- calc(canaM.7, fun=function(x){ x[x < 1] <- NA; return(x)} )


######  GWWAR
quantile(norm_golW, c(.75)) 

#75% quantile is 0.2498879   
golW.7 <- calc(norm_golW, fun=function(x){ x[x > 0.2498879  ] <- 1; return(x)} )
golW3.7 <- calc(golW.7, fun=function(x){ x[x < 1] <- NA; return(x)} )

####################### Combinando las 3 especies para 70%

### Convertir NAs a 0 para poder combinar los rasters
ceruM3.7[is.na(ceruM3.7[])] <- 0
canaM3.7[is.na(canaM3.7[])] <- 0
golW3.7[is.na(golW3.7[])] <- 0

COMB_CA.7 <- ceruM3.7 + canaM3.7 + golW3.7

## Seleccionar solo los pixeles con dos 2 o mas especies
FOCAL_M1.7 <-calc(COMB_CA.7, fun=function(x){ x[x < 1.9] <- 0; return(x)} )
FOCAL_M1.7 <-calc(FOCAL_M1.7, fun=function(x){ x[x > 1.9] <- 1; return(x)} )


# ajustar resolucion para que sean iguales entre capas
Ele2.7 <- resample(Ele.re, FOCAL_M1.7)

FOCAL_M1.7 <- FOCAL_M1.7 + Ele2.7

## limpiar
FOCAL_M2.7 <- reclassify(FOCAL_M1.7, c(-Inf, 1.9, 0, 2, Inf, 1))


focal.7 <- FOCAL_M2.7
focal.7[focal.7==0] <- NA  #transformando cero en NA


## Guardar raster
writeRaster(FOCAL_M2.7, "Focal_75.tif", overwrite =T)

#Guardar como shp
focal_75 = rasterToPolygons(focal.7)
raster::shapefile(focal_75, "Focal_75.shp", overwrite= T)
