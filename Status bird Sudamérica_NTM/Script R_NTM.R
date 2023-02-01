####### eBird status and trends - Plan de conservación 
# Generar directorio 
getwd()
setwd("C:\\Users\\Nataly\\Documents\\Selva\\Script R\\Status bird")
####### Activar la llave para aceder a las capas de eBird 
##expires KEY 18 July 2022 
set_ebirdst_access_key("7s5n13g2ukna", overwrite = TRUE)

##Instalar los paquetes necesarios
install.packages(c("tidyverse", "raster", "sf", "ebirdst", "rnaturalearth"))
remotes::install_github("ebird/ebirdst")
install.packages("remotes")
install.packages("rworldmap")
install.packages("terra")
install.packages("rgdal")

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
Ele <- raster("C:\\Users\\Nataly\\Documents\\NTM\\SIB BASICO\\Selva\\AltWorld.tif") # WGS84
template <- Ele
extent(template)<- c(-81, -64, -12, 34)
Elevation <- crop(Ele,template)


#####  Cerulean Warbler ################################################################################         
dl_path <- ebirdst_download(species = "Cerulean Warbler") #Linea para LLAMAR los datos de la ESPECIE


## Llamar al dl_patch y grabar el resultado para ir directo a las capas la proxima vez
dl_path

###dl_path <-  "C:\\Users\\Nataly\\AppData\\Roaming\\R\\data\\R\\ebirdst\\2021\\cerwar"

#### Cargar los rasters que tienen la superficie de abundancia para cada semana del a#o
ceru <- load_raster(product = "abundance", path = dl_path)

weeks <- parse_raster_dates(ceru)

## Definir las semanas que queremos para el periodo de invierno
ceru_inv <- ceru[[c(1, 2, 3, 4, 5, 6, 7, 8, 50, 51, 52)]]


## Guardamos el raster generado y volvemos a abrirlo con la funcion raster
## para que pase de clase spatraster a rasterlayer
writeRaster(ceru_inv, "ceru_inv.tif", overwrite =T)
ceru_inv_raster=raster("ceru_inv.tif")
class("ceru_inv_raster")


## Cortar a la region de interes, se puede ajustar para captar solo uno o dos paises
template <- ceru_inv_raster
extent(template)<- c(-9100000, -7500000, -3700000, 1500000)
ceru_crop <- crop(ceru_inv_raster,template)

plot(ceru_crop)

## Reprojectar usando la capa de elevacion para convertirlo a WGS84
cer_WGS  <- projectRaster(ceru_crop, 
                          crs = as.character(Elevation@crs))

plot(cer_WGS)


## Para cortar a nuestra área de interés utilizamos un poligono que incluya 
## los limites

#Sudamerica Cargar shape

Area<- readOGR("C:\\Users\\Nataly\\Documents\\NTM\\SIB BASICO\\Selva\\Sudamerica\\Sudamérica.shp",verbose=TRUE)                       

#Metodo 2
Area1<- st_read("C:\\Users\\Nataly\\Documents\\NTM\\SIB BASICO\\Selva\\Sin islas\\Sudamerica.shp")
plot(st_geometry(Area1),axes=TRUE)

CERW_AbundCRPan = crop(cer_WGS,Area1)
plot(CERW_AbundCRPan)

## Guardar el raster para no tener que hacer los pasos arriba cada vez
writeRaster(CERW_AbundCRPan, "CERW_AbundCRPan.tif", overwrite =T)

#CERW_WGS <- raster("C://Users//Nataly//Documents//Selva//Script R//Status bird//CERW_winter.tif")


## Cortar la capa de S cerulea a las mismas dimensiones a lo de elevacion
ceru_ele <- crop(CERW_AbundCRPan,Elevation)

## Exploracion de los datos para estandarizar (este paso podria cambiar)
ceru_ele
hist(ceru_ele, ylim=c(0,10000))

ceru.sin <- ceru_ele
ceru.sin[ceru.sin==0] <- NA  #transformando cero en NA

ceruM.sin <- writeRaster(ceru.sin, "ceruM_sin.tif", overwrite =T)

#Normalizar Datos
min_val <- min(ceru.sin[], na.rm = T)
max_val <- max(ceru.sin[], na.rm = T)

norm_cer <- (ceru.sin - min_val)/(max_val - min_val)
norm_cer

#quantile(ceru_ele, c(.50, .60, .75, .80, .90, .95)) 
quantile(norm_cer, c(.50, .60, .75, .80, .90, .95)) 

#       50%        60%        75%        80%        90%        95% 
# 0.06299090 0.08602067 0.13110484 0.14881779 0.19687149 0.23468444 
CERWA50 <- reclassify(norm_cer, c(-Inf, 0.06299090, 0, 0.06299090, Inf, 1))
plot(CERWA50)

CERWA75 <- reclassify(norm_cer, c(-Inf,0.13110484, 0, 0.13110484, Inf, 1))
plot(CERWA75) 

## Guardar para usos futuros, capa transformada
writeRaster(norm_cer, "norm_cer.tif", overwrite =T)


## Crear Mapa
tiff(file="ceruW_sin.tif",width=1800,height=1950, res=150)
plot(Elevation, col=FineGray,cex.axis=1.5, legend=FALSE)
plot(norm_cer, col = Ceruleanblue4, add=TRUE, legend=TRUE)
plot(worldMap, add=TRUE, border = "white", cex = 2, lwd = 1.5,  legend=FALSE)
scalebar(500, xy=c(-85, -15), below = "Kms", type='bar', divs=2)
dev.off()

#####  Canada Warbler ############################################################################         
dl_path2 <- ebirdst_download(species = "Canada Warbler") #Linea para LLAMAR los datos de la ESPECIE


## Llamar al dl_patch y grabar el resultado para ir directo a las capas la proxima vez
dl_path2

###dl_path <-  "C:\\Users\\BYU Rental\\AppData\\Roaming\\R\\data\\R\\ebirdst\\2021\\cerwar"

#### Cargar los rasters que tienen la superficie de abundancia para cada semana del a#o
cana <- load_raster(product = "abundance", path = dl_path2)

weeks <- parse_raster_dates(cana)

## Definir las semanas que queremos para el periodo de MIGRACION
cana_inv <- cana[[c(1, 2, 3, 4, 5, 6, 7, 8,50,51,52)]]


## Guardamos el raster generado y volvemos a abrirlo con la funcion raster
## para que pase de clase spatraster a rasterlayer
writeRaster(cana_inv, "cana_inv.tif", overwrite =T)
cana_inv_raster=raster("cana_inv.tif")
class(cana_inv_raster)


## Cortar a la region de interes, se puede ajustar para captar solo uno o dos paises
template <- cana_inv_raster
extent(template)<- c(-9100000, -7500000, -3700000, 1500000)
cana_crop <- crop(cana_inv_raster,template)

plot(cana_crop)

## Reprojectar usando la capa de elevacion para convertirlo a WGS84
cana_WGS  <- projectRaster(cana_crop, 
                           crs = as.character(Elevation@crs))

plot(cana_WGS)


## Para cortar a nuestra área de interés utilizamos un poligono que incluya 
## los limites

cana_HNI = crop(cana_WGS,Area1)
plot(cana_HNI)


## Guardar el raster para no tener que hacer los pasos arriba cada vez
writeRaster(cana_HNI, "cana_HNI.tif", overwrite =T)


## Cortar la capa de S cerulea a las mismas dimensiones a lo de elevacion
cana_ele <- crop(cana_HNI,Elevation)

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


#quantile(ceru_ele, c(.50, .60, .75, .80, .90, .95)) 
quantile(norm_can, c(.50, .60, .75, .80, .90, .95)) 
# 50%       60%       75%       80%       90%       95% 
#0.1662405 0.1907389 0.2308992 0.2485215 0.2978886 0.3489176

# observar quantile
norm50 <- reclassify(norm_can, c(-Inf, 0.1662405 , 0, 0.1662405 , Inf, 1))
plot(norm50)

norm75 <- reclassify(norm_can, c(-Inf,0.2308992, 0, 0.2308992, Inf, 1))
plot(norm75) 

## Guardar para usos futuros, capa transformada
writeRaster(norm_can, "norm_can.tif", overwrite =T)


## Crear Mapa
tiff(file="norm_canW.tif",width=1800,height=1950, res=150)
plot(Elevation, col=FineGray,cex.axis=1.5, legend=FALSE)
plot(norm_can, col = Ceruleanblue4, add=TRUE, legend=TRUE)
plot(worldMap, add=TRUE, border = "white", cex = 2, lwd = 1.5,  legend=FALSE)
scalebar(500, xy=c(-94, -11), below = "Kms", type='bar', divs=2)
dev.off()

#################  Vermivora chrysoptera ####################################################### 

## descargar de Statu & Trends las capas de abundancia modelada
dl_path3 <- ebirdst_download(species = "Vermivora chrysoptera")

## Llamar al dl_path y grabar el resultado para ir directo a las capas 
## la proxima vez
dl_path3

#### Cargar los rasters que tienen la superficie de abundancia para cada 
## semana del año
Vercr <- load_raster(product = "abundance", path = dl_path3)

## Definir las semanas que queremos para el periodo de INVIERNO (estacionario)

Vercr_inv <- Vercr[[c(1, 2, 3, 4, 5, 6, 7, 8, 50, 51, 52)]]


## Guardamos el raster generado y volvemos a abrirlo con la funcion raster
## para que pase de clase spatraster a rasterlayer
writeRaster(Vercr_inv, "Vercr_inv.tif", overwrite =T)
Vercr_raster=raster("Vercr_inv.tif")
class(Vercr_raster)


## Cortar a la region de interes, se puede ajustar para captar solo uno o dos 
## paises

template <- Vercr_raster
extent(template)<- c(-9100000, -7500000, -3700000, 1500000)
Vercr_crop <- crop(Vercr_raster,template)
plot(Vercr_crop)

## Reproyectamos el rastercon crs WGS84 basandonos en la capa de elevacion

Vercr_WGS <- projectRaster(Vercr_crop, 
                         crs = as.character(Elevation@crs))

plot(Vercr_WGS)

## Para cortar a nuestra área de interés utilizamos un poligono que incluya 
## los limites

Vercr_HNI = crop(Vercr_WGS,Area1)
plot(Vercr_HNI)

## Guardar el raster para no tener que hacer los pasos arriba cada vez
writeRaster(Vercr_HNI, "Vercr_HNI.tif", overwrite =T) 


## Cortar la capa de la cerulea a las mismas dimensiones a lo de elevaci?n
Vercr.ele <- crop(Vercr_HNI,Elevation)

plot(Vercr.ele)

## Exploracion de los datos para estandarizar (este paso podria cambiar)
Vercr.ele
hist(Vercr.ele, ylim=c(0,15000))

Vercr.sin <- Vercr.ele
Vercr.sin[Vercr.sin==0] <- NA  #transformando cero en NA

VercrW <- writeRaster(Vercr.sin, "Vercr_sin.tif", overwrite =T)

#Normalizar Datos
min_val <- min(Vercr.sin[], na.rm = T)
max_val <- max(Vercr.sin[], na.rm = T)

norm_Vercr <- (Vercr.sin - min_val)/(max_val - min_val)
hist(norm_Vercr)


#quantile(Vercr_ele, c(.50, .60, .75, .80, .90, .95)) 
quantile(norm_Vercr, c(.50, .60, .75, .80, .90, .95)) 
# observar quantile
#50%        60%        75%        80%        90%        95% 
#0.05671795 0.07071982 0.09714399 0.10885106 0.14031849 0.17597169 

# observar quantile
Vercr50 <- reclassify(norm_Vercr, c(-Inf, 0.05671795 , 0, 0.05671795 , Inf, 1))
plot(Vercr50)

Vercr75 <- reclassify(norm_Vercr, c(-Inf, 0.09714399 , 0, 0.09714399 , Inf, 1))
plot(Vercr75)

Vercr3 <- calc(norm_Vercr, fun=function(x){ x[x < 0.01] <- NA; return(x)} )
writeRaster(Vercr3, "Vercr3.tif", overwrite =T)


## Guardar para usos futuro
writeRaster(norm_Vercr, "norm_Vercr.tif", overwrite =T)


## Crear Mapa
tiff(file="norm_VercrW.tif",width=1800,height=1950, res=150)
plot(Elevation, col=FineGray,cex.axis=1.5, legend=FALSE)
plot(norm_Vercr, col = Ceruleanblue4, add=TRUE, legend=TRUE)
plot(worldMap, add=TRUE, border = "white", cex = 2, lwd = 1.5,  legend=FALSE)
scalebar(500, xy=c(6, 24), below = "Kms", type='bar', divs=2)
dev.off()

######## COMBINANDO LAS 3 ESPECIES  ############################################   
library(raster)  
### WINTER 
ceruM <- raster("C:/Users/Nataly/Documents/Selva/Script R/Status bird/norm_cer.tif")
canaM <- raster("C:/Users/Nataly/Documents/Selva/Script R/Status bird/norm_can.tif")
VercrW <- raster("C:/Users/Nataly/Documents/Selva/Script R/Status bird/norm_Vercr.tif")


#################################################################################
### CERW invierno, crear una raster binario, top 10% de los pixeles en cuanto a abundancia
hist(ceruM, ylim=c(0,4000))
quantile(ceruM, c(.50, .60, .75, .80, .90, .95)) 
#       50%        60%        75%        80%        90%        95% 
#0.06299090 0.08602067 0.13110484 0.14881778 0.19687149 0.23468444 

#75% quantile is 0.13110484
ceruM2 <- calc(ceruM, fun=function(x){ x[x > 0.13110484] <- 1; return(x)} )
ceruM3 <- calc(ceruM2, fun=function(x){ x[x < 1] <- NA; return(x)} )

################################################################################
### CAN invierno, crear una raster binario, top 10% de los pixeles en cuanto a abundancia
hist(canaM, ylim=c(0,80000))
quantile(canaM, c(.50, .60, .75, .80, .90, .95)) 
#50%       60%       75%       80%       90%       95% 
#0.1662405 0.1907389 0.2308992 0.2485215 0.2978886 0.3489176   
#75% quantile is 0.2308992
canaM2 <- calc(canaM, fun=function(x){ x[x > 0.2308992] <- 1; return(x)} )
canaM3 <- calc(canaM2, fun=function(x){ x[x < 1] <- NA; return(x)} )
################################################################################
### Vermivora chrysoptera  invierno, crear una raster binario, top 5% de los pixeles en cuanto a abundancia
hist(VercrW, ylim=c(0,5000))
quantile(VercrW, c(.50, .60, .75, .80, .90, .95)) 
#50%        60%        75%        80%        90%        95% 
#0.05671795 0.07071981 0.09714399 0.10885106 0.14031848 0.17597169 

#75% quantile is 0.09714399 
VercrW2 <- calc(VercrW, fun=function(x){ x[x > 0.09714399 ] <- 1; return(x)} )
VercrW3 <- calc(VercrW2, fun=function(x){ x[x < 1] <- NA; return(x)} )

####################### Cominbando las 3 especies
### Convertir NAs a 0 para poder combinar los rasters
ceruM3[is.na(ceruM3[])] <- 0
canaM3[is.na(canaM3[])] <- 0
VercrW3[is.na(VercrW3[])] <- 0

COMB_CA.6 <- ceruM3 + canaM3 + VercrW3

## Seleccionar solo los pixeles con dos 2 o más especies
FOCAL_M1.6 <-calc(COMB_CA.6, fun=function(x){ x[x < 1.9] <- 0; return(x)} )
FOCAL_M1.6 <-calc(FOCAL_M1.6, fun=function(x){ x[x > 1.9] <- 1; return(x)} )

## Cortar a la elevacion del plan >750 m
Ele1 <-calc(Elevation, fun=function(x){ x[x < 750] <- 1; return(x)} )
Ele1 <-calc(Ele1, fun=function(x){ x[x > 749] <- 0; return(x)} )

## Cortar a la elevación del plan >1000 m
Ele1.6 <-calc(Elevation, fun=function(x){ x[x < 1000] <- 1; return(x)} )
Ele1.6 <-calc(Ele1.6, fun=function(x){ x[x > 999] <- 0; return(x)} )
# ajustar resoluci?n para que sean iguales entre capas
Ele2.6 <- resample(Ele1, FOCAL_M1.6)
Ele3.6 <-calc(Ele, fun=function(x){ x[x < 1] <- 0; return(x)} )

FOCAL_M2.6 <- FOCAL_M1.6 - Ele3.6

## limpiar
FOCAL_M2.6 <-calc(FOCAL_M2.6, fun=function(x){ x[x < 1] <- 0; return(x)} )

# Metodo 2 
Ele1 = reclassify(Elevation, c(-Inf, 1000, 0, 1000, 2500, 1, 2500, Inf, 0))
plot(Ele1)
Ele2.6 <- resample(Ele1, FOCAL_M1.6)
Ele3.6 <- crop(Ele2.6, Area1)

FOCAL_M1.6 <- FOCAL_M1.6 + Ele3.6

## limpiar
FOCAL_M2.6 <- reclassify(FOCAL_M1.6 , c(-Inf, 1.9, 0, 2, Inf, 1))

## Guardar areas focal base (shape??)
writeRaster(FOCAL_M2.6, "Focal_Areas2sp_75.tif", overwrite =T)
FOCAL_2sp.6 <- raster("C:/Users/Nataly/Documents/Selva/Script R/Status bird/Focal_Areas2sp_75.tif")

plot(FOCAL_2sp.6)

#################################################################################
### CERW invierno, crear una raster binario, .50 de los pixeles en cuanto a abundancia
hist(ceruM, ylim=c(0,4000))
quantile(ceruM, c(.50, .60, .75, .80, .90, .95)) 
#        50%        60%        75%        80%        90%        95% 
#0.06299090 0.08602067 0.13110484 0.14881778 0.19687149 0.23468444 


#50% quantile 0.06299090
ceruM.7 <- calc(ceruM, fun=function(x){ x[x > 0.06299090] <- 1; return(x)} )
ceruM3.7 <- calc(ceruM.7, fun=function(x){ x[x < 1] <- NA; return(x)} )

################################################################################
### CAN invierno, crear una raster binario, top 50% de los pixeles en cuanto a abundancia
hist(canaM, ylim=c(0,4000))
quantile(canaM, c(.50, .60, .75, .80, .90, .95)) 
#      50%       60%       75%       80%       90%       95% 
#0.1662405 0.1907389 0.2308992 0.2485215 0.2978886 0.3489176   

#50% quantile is 0.1662405
canaM.7 <- calc(canaM, fun=function(x){ x[x > 0.1662405] <- 1; return(x)} )
canaM3.7 <- calc(canaM.7, fun=function(x){ x[x < 1] <- NA; return(x)} )

################################################################################
### Golden invierno, crear una raster binario, top 50% de los pixeles en cuanto a abundancia
hist(VercrW, ylim=c(0,5000))
quantile(VercrW, c(.50, .60, .75, .80, .90, .95)) 
#      50%       60%       75%       80%       90%       95% 
#0.05671795 0.07071981 0.09714399 0.10885106 0.14031848 0.17597169  

#50% quantile is 0.05671795
VercrW.7 <- calc(VercrW, fun=function(x){ x[x > 0.05671795] <- 1; return(x)} )
VercrW3.7 <- calc(VercrW.7, fun=function(x){ x[x < 1] <- NA; return(x)} )

####################### Cominbando las 3 especies
### Convertir NAs a 0 para poder combinar los rasters
ceruM3.7[is.na(ceruM3.7[])] <- 0
canaM3.7[is.na(canaM3.7[])] <- 0
VercrW3.7[is.na(VercrW3.7[])] <- 0

COMB_CA.7 <- ceruM3.7 + canaM3.7 + VercrW3.7

## Seleccionar solo los pixeles con dos 2 o mas especies
FOCAL_M1.7 <-calc(COMB_CA.7, fun=function(x){ x[x < 1.9] <- 0; return(x)} )
FOCAL_M1.7 <-calc(FOCAL_M1.7, fun=function(x){ x[x > 1.9] <- 1; return(x)} )

# Metodo 2 Cortar por elevación
Ele1 = reclassify(Elevation, c(-Inf, 1000, 0, 1000, 2500, 1, 2500, Inf, 0))
plot(Ele1)
Ele2.7 <- resample(Ele1, FOCAL_M1.7)
Ele3.7 <- crop(Ele2.7, Area1)

FOCAL_M1.7 <- FOCAL_M1.7 + Ele3.7

## limpiar
FOCAL_M2.7 <- reclassify(FOCAL_M1.7 , c(-Inf, 1.9, 0, 2, Inf, 1))

## Guardar areas focal base (shape??)
writeRaster(FOCAL_M2.7, "Focal_Areas2sp_50.tif", overwrite =T)
FOCAL_2sp.7 <- raster("C:/Users/Nataly/Documents/Selva/Script R/Status bird/Focal_Areas2sp_50.tif")

plot(FOCAL_2sp.7)

