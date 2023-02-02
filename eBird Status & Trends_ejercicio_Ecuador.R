####### eBird status and trends - Plan de conservación 

####### Activar la clave para aceder a las capas de eBird 
##expires KEY 18 July 2022 - f0spt9d4kstc
set_ebirdst_access_key("f0spt9d4kstc", overwrite = TRUE)

## Instalar los paquetes necesarios
install.packages(c("tidyverse", "raster", "sf", "ebirdst", "rnaturalearth"))
remotes::install_github("CornellLabofOrnithology/ebirdst")
install.packages("ebirdst")
install.packages("rworldmap")
install.packages("terra")

## Liberar espacio en la memoria 

rm(list = ls())
.rs.restartR()

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

# Establecer el directorio de trabajo

setwd("D:\\selva")

## Generar unos paletas de color que vamos a usar m?s tarde
FineGray <- c("gray80", "gray72", "gray64", "gray56", "gray48", "gray42", "gray34", "gray26", "gray18", "gray10", "gray2", "black")
Ceruleanblue4 <- c("#40E0D04B", "cyan")
Forest <- c("#FFFFFF0A", "lightgreen")
Map <- c("#FFFFFF0A", "orange", "lightgreen", "darkgreen")
Baseline <- c("#FFFFFF0A", "#40E0D06E" )

## Llamar la capa de elevación que vamos a usar como base y 
## cortarlo a nuestra región de interés.

Ele <- raster("D:\\selva\\AltWorld.tif")
template <- Ele
extent(template)<- c(-81, -64, -12, 34)
Elevation <- crop(Ele,template)


#####  Cargar datos de abundancia de las 3 especies #################        
dl_path <- ebirdst_download(species = "Cerulean Warbler")
dv_path <- ebirdst_download(species = "Golden-winged Warbler")
dc_path <- ebirdst_download(species = "Canada Warbler")
## Llamar al dl_patch y grabar el resultado para ir directo a las capas la proxima vez
dl_path
dv_path
dc_path

#######################################################################
############ Ejercicio CERULEAN WARBLER (Invierno SA) #################

### Cargar los rasters que tienen la superficie de abundancia,
### para cada semana del año

CERW <- load_raster(product = "abundance", path = dl_path)

## Definir las semanas que queremos para el periodo de INVIERNO (estacionario)
CERW_Winter <- CERW[[c(1, 2, 3, 4, 5, 6, 7, 8, 50, 51, 52)]]

class(CERW_Winter)

## Guardamos el raster generado y volvemos a abrirlo con la funcion raster
## para que pase de clase spatraster a rasterlayer
writeRaster(CERW_Winter, "CERW_Winter.tif", overwrite =T)
CERW_Winter_raster=raster("CERW_Winter.tif")
class(CERW_Winter_raster)

## Cortar a la region de interés, se puede ajustar para captar solo uno o dos paises

template <- CERW_Winter_raster
extent(template)<- c(-9100000, -7500000, -3700000, 1500000)
CERW.corte <- crop(CERW_Winter_raster,template)
plot(CERW.corte)

## Reproyectar usando la capa de elevación para convertirlo a WGS84

CERW_WGS <- projectRaster(CERW.corte, crs = as.character(Elevation@crs))  

plot(CERW_WGS)

## Cargar shp de sudamerica sin islas

SA = st_read("D:/selva/Sudamerica.shp")
plot(st_geometry(SA),axes=TRUE)

## Cortar el raster proyectado con la extensión de Sudamérica 
 
CERW_SA = crop(CERW_WGS,SA)
plot(CERW_SA)

## Guardar el raster para no tener que hacer los pasos arriba cada vez
writeRaster(CERW_SA, "CERW_SA.tif", overwrite=TRUE)
CERW_SAR=raster("CERW_SA.tif")

## Cortar la capa de la cerulea a las mismas dimensiones a lo de elevación
CERW_ele <- crop(CERW_SA,Elevation)

## Exploración de los datos para estandarizar
CERW_ele
hist(CERW_ele, ylim=c(0,10000))

# Transformar 0s en NAs
CERW.sin <- CERW_ele
CERW.sin[CERW.sin==0] <- NA 

## Guardar distribución de invierno, cortada y sin 0s
CERWI.sin <- writeRaster(CERW.sin, "CERWI_sin.tif", overwrite =T) 

# Normalizar datos de abundancia para que tomen valores entre 0-1

min_val <- min(CERWI.sin[], na.rm = T)
max_val <- max(CERWI.sin[], na.rm = T)

norm_CERW <- (CERWI.sin - min_val)/(max_val - min_val)
norm_CERW

## Guardar para usos futuros, capa normalizada

writeRaster(norm_CERW, "norm_CERW.tif", overwrite =T) # raster cortado y normalizado

## calculamos los cuantiles para determinar umbrale de abundancia para 
## priorizar la presencia y abundancia de la especie

quantile(norm_CERW, c(.50, .60, .75, .80, .90, .95))
#     50%        60%        75%        80%        90%        95% 
#  0.06299090 0.08602068 0.13110484 0.14881779 0.19687148 0.23468444

hist(norm_CERW)

## Usar valores de cuantiles 50% y 75% para determinar umbrales de  abundancia
norm_CERW50 <- reclassify(norm_CERW, c(-Inf, 0.06299090, 0, 0.06299090, Inf, 1))
plot(norm_CERW50)

norm_CERW75 <- reclassify(norm_CERW, c(-Inf, 0.13110484, 0, 0.13110484, Inf, 1))
plot(norm_CERW75)

## Guardar los rasters generados

writeRaster(norm_CERW50, "norm_CERW50.tif", overwrite =T)
writeRaster(norm_CERW75, "norm_CERW75.tif", overwrite =T)

## Crear Mapa

tiff(file="norm_CeruleanWarbler75.tif",width=1800,height=1950, res=150)
plot(Elevation, col=FineGray,cex.axis=1.5, legend=FALSE)
plot(norm_CERW75, col = Ceruleanblue4, add=TRUE, legend=TRUE)
plot(worldMap, add=TRUE, border = "white", cex = 2, lwd = 1.5,  legend=FALSE)
scalebar(500, xy=c(-94, -11), below = "Kms", type='bar', divs=2)
dev.off()

#####################################################################
########## Ejercicio CANADA WARBLER (Invierno) ###################### 

dc_path

#### Cargar los rasters que tienen la superficie de abundancia para cada semana del año
CANW <- load_raster(product = "abundance", path = dc_path)

## Definir las semanas que queremos para el periodo de INVIERNO (estacionario)
CANW_Winter <- CANW[[c(1, 2, 3, 4, 5, 6, 7, 8, 50, 51, 52)]]

class(CANW_Winter)

## Guardamos el raster generado y volvemos a abrirlo con la funcion raster
## para que pase de clase spatraster a rasterlayer
writeRaster(CANW_Winter, "CANW_Winter.tif", overwrite =T)
CANW_Winter_raster=raster("CANW_Winter.tif")
class(CANW_Winter_raster)

## Cortar a la region de interés, se puede ajustar para captar solo uno o dos paises

template <- CANW_Winter_raster
extent(template)<- c(-9100000, -7500000, -3700000, 1500000)
CANW.corte <- crop(CANW_Winter_raster,template)
plot(CANW.corte)


## Reproyectar usando la capa de elevación para convertirlo a WGS84

CANW_WGS <- projectRaster(CANW.corte, crs = as.character(Elevation@crs))  

plot(CANW_WGS)

## Cortar el raster proyectado con la extensión de Sudamérica 
CANW_SA = crop(CANW_WGS,SA)
plot(CANW_SA)

## Guardar el raster para no tener que hacer los pasos arriba cada vez
writeRaster(CANW_SA, "CANW_SA.tif", overwrite=TRUE)
#CANW_SAR=raster("CANW_SA.tif")

## Cortar la capa de la Reinita de Canadá a las mismas dimensiones a lo de elevación
CANW_ele <- crop(CANW_SA,Elevation)

## Exploración de los datos para estandarizar 
CANW_ele
hist(CANW, ylim=c(0,10000))

# Transformar 0s en NAs
CANW.sin <- CANW_ele
CANW.sin[CANW.sin==0] <- NA

## Guardar distribución de invierno, cortada y sin 0s
CANWI.sin <- writeRaster(CANW.sin, "CANWI_sin.tif", overwrite =T) 

# Normalizar datos de abundancia para que tomen valores entre 0-1

min_val <- min(CANWI.sin[], na.rm = T)
max_val <- max(CANWI.sin[], na.rm = T)

norm_CANW <- (CANWI.sin - min_val)/(max_val - min_val)
norm_CANW

## Guardar para usos futuros, capa transformada
writeRaster(norm_CANW, "norm_CANW.tif", overwrite =T) # raster cortado y normalizado

## calculamos los cuantiles para determinar un umbral de abundancia para 
## priorizar la presencia y abundancia de la especie

quantile(norm_CANW, c(.50, .60, .75, .80, .90, .95))
#     50%       60%       75%       80%       90%       95% 
#  0.1662405 0.1907389 0.2308992 0.2485215 0.2978886 0.3489176 
hist(norm_CANW)

## Usar valores de cuantiles 50% y 75% para determinar umbrales de  abundancia
norm_CANW50 <- reclassify(norm_CANW, c(-Inf, 0.1662405, 0, 0.1662405, Inf, 1))
plot(norm_CANW50)

norm_CANW75 <- reclassify(norm_CANW, c(-Inf, 0.2308992, 0, 0.2308992, Inf, 1))
plot(norm_CANW75)

## Guardar los rasters generados
writeRaster(norm_CANW50, "norm_CANW50.tif", overwrite =T)
writeRaster(norm_CANW75, "norm_CANW75.tif", overwrite =T)


## Crear Mapa
tiff(file="norm_CanadaWarbler75.tif",width=1800,height=1950, res=150)
plot(Elevation, col=FineGray,cex.axis=1.5, legend=FALSE)
plot(norm_CANW75, col = Ceruleanblue4, add=TRUE, legend=TRUE)
plot(worldMap, add=TRUE, border = "white", cex = 2, lwd = 1.5,  legend=FALSE)
scalebar(500, xy=c(-94, -11), below = "Kms", type='bar', divs=2)
dev.off()


#############################################################################
########## Ejercicio Golden-Winged Warbler Invierno #########################

dv_path <- ebirdst_download(species = "Golden-winged Warbler")

## Llamar al dv_path y grabar el resultado para ir directo a las capas la proxima vez

dv_path

#### Cargar los rasters que tienen la superficie de abundancia para cada semana del año
GOLW <- load_raster(product = "abundance", path = dv_path)

## Definir las semanas que queremos para el periodo de INVIERNO (estacionario)

GOLW_Winter <- GOLW[[c(1, 2, 3, 4, 5, 6, 7, 8, 50, 51, 52)]]

class(GOLW_Winter)

## Guardamos el raster generado y volvemos a abrirlo con la funcion raster
## para que pase de clase spatraster a rasterlayer
writeRaster(GOLW_Winter, "GOLW_Winter.tif", overwrite =T)
GOLW_Winter_raster=raster("GOLW_Winter.tif")
class(GOLW_Winter_raster)

## Cortar a la region de interés, se puede ajustar para captar solo uno o dos paises

template <- GOLW_Winter_raster
extent(template)<- c(-9100000, -7500000, -3700000, 1500000)
GOLW.corte <- crop(GOLW_Winter_raster,template)
plot(GOLW.corte)

## Reproyectar usando la capa de elevación para convertirlo a WGS84

GOLW_WGS <- projectRaster(GOLW.corte, crs = as.character(Elevation@crs))  

plot(GOLW_WGS)

## Cargar capa de Sudamérica sin islas 
SA = st_read("D:/selva/Sudamerica.shp")
plot(st_geometry(SA),axes=TRUE)

## Cortar el raster proyectado con la extensión de Sudamérica 
GOLW_SA = crop(GOLW_WGS,SA)
plot(GOLW_SA)

## Guardar el raster para no tener que hacer los pasos arriba cada vez
writeRaster(GOLW_SA, "GOLW_SA.tif", overwrite=TRUE)
#GOLW_SAR=raster("GOLW_SA.tif")

## Cortar la capa de la cerulea a las mismas dimensiones a lo de elevación
GOLW_ele <- crop(GOLW_SA,Elevation)

## Exploración de los datos para estandarizar (este paso podría cambiar)
GOLW_ele
hist(GOLW_ele, ylim=c(0,15000))

# Transformar 0s en NAs
GOLW.sin <- GOLW_ele
GOLW.sin[GOLW.sin==0] <- NA 

GOLWI.sin <- writeRaster(GOLW.sin, "GOLWI_sin.tif", overwrite =T) ## Distribucion de invierno, cortada y sin NAs

# Normalizar datos de abundancia para que tomen valores entre 0-1

min_val <- min(GOLWI.sin[], na.rm = T)
max_val <- max(GOLWI.sin[], na.rm = T)

norm_GOLW <- (GOLWI.sin - min_val)/(max_val - min_val)
norm_GOLW

## Guardar para usos futuros, capa transformada y normalizada
writeRaster(norm_GOLW, "norm_GOLW.tif", overwrite =T) # raster cortado y normalizado

## calculamos los cuantiles para determinar un umbral de abundancia para 
## priorizar la presencia y abundancia de la especie

quantile(norm_GOLW, c(.50, .60, .75, .80, .90, .95))
#      50%        60%        75%        80%        90%        95% 
#  0.05671795 0.07071981 0.09714399 0.10885106 0.14031849 0.17597169 

hist(norm_GOLW)

## Usar valores de cuantiles 50% y 75% para determinar umbrales de  abundancia
norm_GOLW50 <- reclassify(norm_GOLW, c(-Inf, 0.05671795, 0, 0.05671795, Inf, 1))
plot(norm_GOLW50)

norm_GOLW75 <- reclassify(norm_GOLW, c(-Inf, 0.09714399, 0, 0.09714399, Inf, 1))
plot(norm_GOLW75)

## Guardar los rasters generados 
writeRaster(norm_GOLW50, "norm_GOLW50.tif", overwrite =T)
writeRaster(norm_GOLW75, "norm_GOLW75.tif", overwrite =T)

## Crear Mapa
tiff(file="norm_GoldenWingedWarbler75.tif",width=1800,height=1950, res=150)
plot(Elevation, col=FineGray,cex.axis=1.5, legend=FALSE)
plot(norm_GOLW75, col = Ceruleanblue4, add=TRUE, legend=TRUE)
plot(worldMap, add=TRUE, border = "white", cex = 2, lwd = 1.5,  legend=FALSE)
scalebar(500, xy=c(-94, -11), below = "Kms", type='bar', divs=2)
dev.off()

########################################################################
############ COMBINANDO LAS 3 ESPECIES  ################################   

ceruM <- raster("D:/selva/norm_CERW.tif")
canaM <- raster("D:/selva/norm_CANW.tif")
VercrW <- raster("D:/selva/norm_GOLW.tif")


#################################################################################
### CERW invierno, crear una raster binario, cuantil 0.75
hist(ceruM, ylim=c(0,4000))
quantile(ceruM, c(.50, .60, .75, .80, .90, .95)) 
#       50%        60%        75%        80%        90%        95% 
#   0.06299090 0.08602067 0.13110484 0.14881778 0.19687149 0.23468444 

# 75% quantile is 0.13110484
ceruM2 <- calc(ceruM, fun=function(x){ x[x > 0.13110484] <- 1; return(x)} )
ceruM3 <- calc(ceruM2, fun=function(x){ x[x < 1] <- NA; return(x)} )

################################################################################
### CANW invierno, crear una raster binario, cuantil 0.75
hist(canaM, ylim=c(0,80000))
quantile(canaM, c(.50, .60, .75, .80, .90, .95)) 

#      50%       60%       75%       80%       90%       95% 
#  0.1662405 0.1907389 0.2308992 0.2485215 0.2978886 0.3489176   

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

####################### Cominbando las 3 especies #########################
## Convertir NAs a 0 para poder combinar los rasters

ceruM3[is.na(ceruM3[])] <- 0
canaM3[is.na(canaM3[])] <- 0
VercrW3[is.na(VercrW3[])] <- 0

COMB_CA.6 <- ceruM3 + canaM3 + VercrW3

## Seleccionar solo los pixeles con dos 2 o m?s especies
FOCAL_M1.6 <-calc(COMB_CA.6, fun=function(x){ x[x < 1.9] <- 0; return(x)} )
FOCAL_M1.6 <-calc(FOCAL_M1.6, fun=function(x){ x[x > 1.9] <- 1; return(x)} )

# Reclasificar la elevación de acuerdo al rango definido en el plan (SA 1000-2500) 
Ele1 = reclassify(Elevation, c(-Inf, 1000, 0, 1000, 2500, 1, 2500, Inf, 0))
plot(Ele1)
Ele2.6 <- resample(Ele1, FOCAL_M1.6)
Ele3.6 <- crop(Ele2.6, SA)

FOCAL_M1.6 <- FOCAL_M1.6 + Ele3.6

## limpiar
FOCAL_M2.6 <- reclassify(FOCAL_M1.6 , c(-Inf, 1.9, 0, 2, Inf, 1))

## Guardar areas focal base (shape??)
writeRaster(FOCAL_M2.6, "Focal_Areas2sp_75.tif", overwrite =T)
FOCAL_2sp.6 <- raster("Focal_Areas2sp_75.tif")

plot(FOCAL_2sp.6)

#################################################################################
### CERW invierno, crear una raster binario, .50 de los pixeles en cuanto a abundancia

hist(ceruM, ylim=c(0,4000))
quantile(ceruM, c(.50, .60, .75, .80, .90, .95)) 

#        50%        60%        75%        80%        90%        95% 
#    0.06299090 0.08602067 0.13110484 0.14881778 0.19687149 0.23468444 


# 50% quantile 0.06299090
ceruM.7 <- calc(ceruM, fun=function(x){ x[x > 0.06299090] <- 1; return(x)} )
ceruM3.7 <- calc(ceruM.7, fun=function(x){ x[x < 1] <- NA; return(x)} )

################################################################################
### CAN invierno, crear una raster binario, cuantil 0.5

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

# Metodo 2 Cortar por elevaci?n
Ele1 = reclassify(Elevation, c(-Inf, 1000, 0, 1000, 2500, 1, 2500, Inf, 0))
plot(Ele1)
Ele2.7 <- resample(Ele1, FOCAL_M1.7)
Ele3.7 <- crop(Ele2.7, SA)

FOCAL_M1.7 <- FOCAL_M1.7 + Ele3.7

## limpiar
FOCAL_M2.7 <- reclassify(FOCAL_M1.7 , c(-Inf, 1.9, 0, 2, Inf, 1))

## Guardar areas focal base (shape??)
writeRaster(FOCAL_M2.7, "Focal_Areas2sp_50.tif", overwrite =T)
FOCAL_2sp.7 <- raster("Focal_Areas2sp_50.tif")

plot(FOCAL_2sp.7)

tiff(file="Mapa_Focal_2sp75.tif",width=1800,height=1950, res=150)
plot(Elevation, col=FineGray,cex.axis=1.5, legend=FALSE)
plot(FOCAL_2sp.6, col = Ceruleanblue4, add=TRUE, legend=TRUE)
plot(worldMap, add=TRUE, border = "white", cex = 2, lwd = 1.5,  legend=FALSE)
scalebar(500, xy=c(-94, -11), below = "Kms", type='bar', divs=2)
dev.off()

