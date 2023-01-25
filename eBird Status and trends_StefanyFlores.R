####### eBird status and trends - Plan de conservaci?n 

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
Ele <- raster("C:\\Users\\BYU Rental\\Desktop\\Taller\\AltWorld.tif") # WGS84
template <- Ele
extent(template)<- c(-95, -64, -12, 24)
Elevation <- crop(Ele,template)
Elevation2 <- crop(Ele,Area)

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


## Cortar a la region de interes, se puede ajustar para captar solo uno o dos paises
template <- ceru_mig_raster
extent(template)<- c(-10500000, -6000000, -2000000, 3500000)
ceru_crop <- crop(ceru_mig_raster,template)

plot(ceru_crop)

## Reprojectar usando la capa de elevacion para convertirlo a WGS84
cer_WGS  <- projectRaster(ceru_crop, 
                          crs = as.character(Elevation@crs))

plot(cer_WGS)


## Para cortar a nuestra área de interés utilizamos un poligono que incluya 
## los limites

#cargar los shapes
HN = readOGR(dsn = "C:/Users/BYU Rental/Desktop/R/hnd")
NI = readOGR(dsn = "C:/Users/BYU Rental/Desktop/R/nica")

Area <- bind(HN, NI) #Unir shapes de ambos paises
plot(Area)


ceru_HNI = crop(cer_WGS,Area)

plot(ceru_HNI)


## Guardar el raster para no tener que hacer los pasos arriba cada vez
writeRaster(ceru_HNI, "ceru_HNI.tif", overwrite =T)

#CERW_WGS <- raster("C:\\Users\\BYU Rental\\Desktop\\R\\CERW_winter.tif")


## Cortar la capa de S cerulea a las mismas dimensiones a lo de elevacion
ceru_ele <- crop(ceru_HNI,Elevation)

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

#        50%       60%       75%       80%       90%       95% 
#  0.1177528 0.1816610 0.3104568 0.3653414 0.4853728 0.5676747 


## Guardar para usos futuros, capa transformada
writeRaster(norm_cer, "norm_cer.tif", overwrite =T)


## Crear Mapa
tiff(file="ceruM_sin.tif",width=1800,height=1950, res=150)
plot(Elevation2, col=FineGray,cex.axis=1.5, legend=FALSE)
plot(ceruM.sin, col = Ceruleanblue4, add=TRUE, legend=TRUE)
plot(worldMap, add=TRUE, border = "white", cex = 2, lwd = 1.5,  legend=FALSE)
scalebar(500, xy=c(-94, -11), below = "Kms", type='bar', divs=2)
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
cana_mig <- cana[[c(15, 16, 17, 18, 19, 36, 37, 38, 39, 40, 41, 42)]]


## Guardamos el raster generado y volvemos a abrirlo con la funcion raster
## para que pase de clase spatraster a rasterlayer
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


## Para cortar a nuestra área de interés utilizamos un poligono que incluya 
## los limites

cana_HNI = crop(cana_WGS,Area)
plot(cana_HNI)


## Guardar el raster para no tener que hacer los pasos arriba cada vez
writeRaster(cana_HNI, "cana_HNI.tif", overwrite =T)

#CERW_WGS <- raster("C:\\Users\\BYU Rental\\Desktop\\R\\CERW_winter.tif")


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


## Guardar para usos futuros, capa transformada
writeRaster(norm_can, "norm_can.tif", overwrite =T)


## Crear Mapa
tiff(file="norm_can.tif",width=1800,height=1950, res=150)
plot(Elevation2, col=FineGray,cex.axis=1.5, legend=FALSE)
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

## Reproyectamos el rastercon crs WGS84 basandonos en la capa de elevacion

gol_WGS <- projectRaster(gol_crop, 
                               crs = as.character(Elevation@crs))

plot(gol_WGS)

## Para cortar a nuestra área de interés utilizamos un poligono que incluya 
## los limites

gol_HNI = crop(gol_WGS,Area)
plot(gol_HNI)

## Guardar el raster para no tener que hacer los pasos arriba cada vez
writeRaster(gol_HNI, "gol_HNI.tif", overwrite =T) 
      

## Cortar la capa de la cerulea a las mismas dimensiones a lo de elevaci?n
gol.ele <- crop(gol_HNI,Elevation)

plot(gol.ele)

## Exploracion de los datos para estandarizar (este paso podria cambiar)
gol.ele
hist(gol.ele, ylim=c(0,15000))

gol.sin <- gol.ele
gol.sin[gol.sin==0] <- NA  #transformando cero en NA

golW <- writeRaster(gol.sin, "gol_sin.tif", overwrite =T)

#Normalizar Datos
min_val <- min(gol.sin[], na.rm = T)
max_val <- max(gol.sin[], na.rm = T)

norm_gol <- (gol.sin - min_val)/(max_val - min_val)
hist(norm_gol)


#quantile(ceru_ele, c(.50, .60, .75, .80, .90, .95)) 
quantile(norm_gol, c(.50, .60, .75, .80, .90, .95)) 

GWWA3 <- calc(norm_gol, fun=function(x){ x[x < 0.01] <- NA; return(x)} )
writeRaster(GWWA3, "GWWA3.tif", overwrite =T)


## Guardar para usos futuro
writeRaster(norm_gol, "norm_gol.tif", overwrite =T)


## Crear Mapa
tiff(file="norm_gol.tif",width=1800,height=1950, res=150)
plot(Elevation2, col=FineGray,cex.axis=1.5, legend=FALSE)
plot(norm_gol, col = Ceruleanblue4, add=TRUE, legend=TRUE)
plot(worldMap, add=TRUE, border = "white", cex = 2, lwd = 1.5,  legend=FALSE)
scalebar(500, xy=c(6, 24), below = "Kms", type='bar', divs=2)
dev.off()

######## COMBINANDO LAS 3 ESPECIES  ############################################   

### WINTER & MIGRATION
ceruM <- raster("C:/Users/BYU Rental/Desktop/R/norm_cer.tif")
canaM <- raster("C:/Users/BYU Rental/Desktop/R/norm_can.tif")
golW <- raster("C:/Users/BYU Rental/Desktop/R/norm_gol.tif")


#################################################################################
### CERW migracion, crear una raster binario, top 10% de los pixeles en cuanto a abundancia
hist(ceruM, ylim=c(0,4000))
quantile(ceruM, c(.50, .60, .75, .80, .90, .95)) 
#        50%       60%       75%       80%       90%       95% 
#  0.1177528 0.1816610 0.3104568 0.3653414 0.4853728 0.5676747 

#90% quantile is 0.4853728
ceruM2 <- calc(ceruM, fun=function(x){ x[x > 0.4853728] <- 1; return(x)} )
ceruM3 <- calc(ceruM2, fun=function(x){ x[x < 1] <- NA; return(x)} )

################################################################################
### CAN migracion, crear una raster binario, top 10% de los pixeles en cuanto a abundancia
hist(canaM, ylim=c(0,4000))
quantile(canaM, c(.50, .60, .75, .80, .90, .95)) 
#      50%       60%       75%       80%       90%       95% 
#0.1797606 0.2419024 0.3363886 0.3723322 0.4606149 0.5302712 

#90% quantile is 0.4606149
canaM2 <- calc(canaM, fun=function(x){ x[x > 0.4606149] <- 1; return(x)} )
canaM3 <- calc(canaM2, fun=function(x){ x[x < 1] <- NA; return(x)} )

################################################################################
### Golden invierno, crear una raster binario, top 5% de los pixeles en cuanto a abundancia
hist(golW, ylim=c(0,5000))
quantile(golW, c(.50, .60, .75, .80, .90, .95)) 
#      50%       60%       75%       80%       90%       95% 
#0.1856677 0.2094833 0.2498879 0.2656566 0.3280640 0.3935074 

#95% quantile is 0.3935074 
golW2 <- calc(golW, fun=function(x){ x[x > 0.3935074 ] <- 1; return(x)} )
golW3 <- calc(golW2, fun=function(x){ x[x < 1] <- NA; return(x)} )

####################### Cominbando las 3 especies
### Convertir NAs a 0 para poder combinar los rasters
ceruM3[is.na(ceruM3[])] <- 0
canaM3[is.na(canaM3[])] <- 0
golW3[is.na(golW3[])] <- 0

COMB_CA <- ceruM3 + canaM3 + golW3

## Seleccionar solo los pixeles con dos 2 o m?s especies
FOCAL_M1 <-calc(COMB_CA, fun=function(x){ x[x < 1.9] <- 0; return(x)} )
FOCAL_M1 <-calc(FOCAL_M1, fun=function(x){ x[x > 1.9] <- 1; return(x)} )

## Cortar a la elevacion del plan >750 m
Ele1 <-calc(Elevation, fun=function(x){ x[x < 750] <- 1; return(x)} )
Ele1 <-calc(Ele1, fun=function(x){ x[x > 749] <- 0; return(x)} )

# ajustar resolucion para que sean iguales entre capas
Ele2 <- resample(Ele1, FOCAL_M1)
Ele3 <-calc(Ele2, fun=function(x){ x[x < 1] <- 0; return(x)} )

FOCAL_M2 <- FOCAL_M1 - Ele3

## limpiar
FOCAL_M2 <-calc(FOCAL_M2, fun=function(x){ x[x < 1] <- 0; return(x)} )

## Guardar areas focal base (shape??)
writeRaster(FOCAL_M2, "Focal_Areas2sp.tif", overwrite =T)
FOCAL_2sp <- raster("C:/Users/BYU Rental/Desktop/R/Focal_Areas2sp.tif")

plot(FOCAL_2sp)


#################################################################################
### CERW migracion, crear una raster binario, .50 de los pixeles en cuanto a abundancia
hist(ceruM, ylim=c(0,4000))
quantile(ceruM, c(.50, .60, .75, .80, .90, .95)) 
#        50%       60%       75%       80%       90%       95% 
#  0.1177528 0.1816610 0.3104568 0.3653414 0.4853728 0.5676747 

#90% quantile is 0.1177528
ceruM.5 <- calc(ceruM, fun=function(x){ x[x > 0.1177528] <- 1; return(x)} )
ceruM3.5 <- calc(ceruM.5, fun=function(x){ x[x < 1] <- NA; return(x)} )

################################################################################
### CAN migracion, crear una raster binario, top 50% de los pixeles en cuanto a abundancia
hist(canaM, ylim=c(0,4000))
quantile(canaM, c(.50, .60, .75, .80, .90, .95)) 
#      50%       60%       75%       80%       90%       95% 
#0.1797606 0.2419024 0.3363886 0.3723322 0.4606149 0.5302712 

#90% quantile is 0.1797606
canaM.5 <- calc(canaM, fun=function(x){ x[x > 0.1797606] <- 1; return(x)} )
canaM3.5 <- calc(canaM.5, fun=function(x){ x[x < 1] <- NA; return(x)} )

################################################################################
### Golden invierno, crear una raster binario, top 50% de los pixeles en cuanto a abundancia
hist(golW, ylim=c(0,5000))
quantile(golW, c(.50, .60, .75, .80, .90, .95)) 
#      50%       60%       75%       80%       90%       95% 
#0.1856677 0.2094833 0.2498879 0.2656566 0.3280640 0.3935074 

#95% quantile is 0.1856677 
golW.5 <- calc(golW, fun=function(x){ x[x > 0.1856677 ] <- 1; return(x)} )
golW3.5 <- calc(golW.5, fun=function(x){ x[x < 1] <- NA; return(x)} )

####################### Cominbando las 3 especies
### Convertir NAs a 0 para poder combinar los rasters
ceruM3.5[is.na(ceruM3.5[])] <- 0
canaM3.5[is.na(canaM3.5[])] <- 0
golW3.5[is.na(golW3.5[])] <- 0

COMB_CA.5 <- ceruM3.5 + canaM3.5 + golW3.5

## Seleccionar solo los pixeles con dos 2 o m?s especies
FOCAL_M1.5 <-calc(COMB_CA.5, fun=function(x){ x[x < 1.9] <- 0; return(x)} )
FOCAL_M1.5 <-calc(FOCAL_M1.5, fun=function(x){ x[x > 1.9] <- 1; return(x)} )

## Cortar a la elevaci?n del plan >750 m
Ele1.5 <-calc(Elevation, fun=function(x){ x[x < 750] <- 1; return(x)} )
Ele1.5 <-calc(Ele1.5, fun=function(x){ x[x > 749] <- 0; return(x)} )

# ajustar resoluci?n para que sean iguales entre capas
Ele2.5 <- resample(Ele1.5, FOCAL_M1.5)
Ele3.5 <-calc(Ele2.5, fun=function(x){ x[x < 1] <- 0; return(x)} )

FOCAL_M2.5 <- FOCAL_M1.5 - Ele3.5

## limpiar
FOCAL_M2.5 <-calc(FOCAL_M2.5, fun=function(x){ x[x < 1] <- 0; return(x)} )

## Guardar areas focal base (shape??)
writeRaster(FOCAL_M2.5, "Focal_Areas2sp_50.tif", overwrite =T)
FOCAL_2sp.5 <- raster("C:/Users/BYU Rental/Desktop/R/Focal_Areas2sp_50.tif")

plot(FOCAL_2sp.5)

#################################################################################
### CERW migracion, crear una raster binario, .60 de los pixeles en cuanto a abundancia
hist(ceruM, ylim=c(0,4000))
quantile(ceruM, c(.50, .60, .75, .80, .90, .95)) 
#        50%       60%       75%       80%       90%       95% 
#  0.1177528 0.1816610 0.3104568 0.3653414 0.4853728 0.5676747 

#60% quantile 0.1816610
ceruM.6 <- calc(ceruM, fun=function(x){ x[x > 0.1816610] <- 1; return(x)} )
ceruM3.6 <- calc(ceruM.6, fun=function(x){ x[x < 1] <- NA; return(x)} )

################################################################################
### CAN migracion, crear una raster binario, top 40% de los pixeles en cuanto a abundancia
hist(canaM, ylim=c(0,4000))
quantile(canaM, c(.50, .60, .75, .80, .90, .95)) 
#      50%       60%       75%       80%       90%       95% 
#0.1797606 0.2419024 0.3363886 0.3723322 0.4606149 0.5302712 

#60% quantile is 0.2419024
canaM.6 <- calc(canaM, fun=function(x){ x[x > 0.2419024] <- 1; return(x)} )
canaM3.6 <- calc(canaM.6, fun=function(x){ x[x < 1] <- NA; return(x)} )

################################################################################
### Golden invierno, crear una raster binario, top 40% de los pixeles en cuanto a abundancia
hist(golW, ylim=c(0,5000))
quantile(golW, c(.50, .60, .75, .80, .90, .95)) 
#      50%       60%       75%       80%       90%       95% 
#0.1856677 0.2094833 0.2498879 0.2656566 0.3280640 0.3935074 

#95% quantile is 0.2094833 
golW.6 <- calc(golW, fun=function(x){ x[x > 0.2094833] <- 1; return(x)} )
golW3.6 <- calc(golW.6, fun=function(x){ x[x < 1] <- NA; return(x)} )

####################### Cominbando las 3 especies
### Convertir NAs a 0 para poder combinar los rasters
ceruM3.6[is.na(ceruM3.6[])] <- 0
canaM3.6[is.na(canaM3.6[])] <- 0
golW3.6[is.na(golW3.6[])] <- 0

COMB_CA.6 <- ceruM3.6 + canaM3.6 + golW3.6

## Seleccionar solo los pixeles con dos 2 o m?s especies
FOCAL_M1.6 <-calc(COMB_CA.6, fun=function(x){ x[x < 1.9] <- 0; return(x)} )
FOCAL_M1.6 <-calc(FOCAL_M1.6, fun=function(x){ x[x > 1.9] <- 1; return(x)} )

## Cortar a la elevaci?n del plan >750 m
Ele1.6 <-calc(Elevation, fun=function(x){ x[x < 750] <- 1; return(x)} )
Ele1.6 <-calc(Ele1.6, fun=function(x){ x[x > 749] <- 0; return(x)} )

# ajustar resoluci?n para que sean iguales entre capas
Ele2.6 <- resample(Ele1.6, FOCAL_M1.6)
Ele3.6 <-calc(Ele2.6, fun=function(x){ x[x < 1] <- 0; return(x)} )

FOCAL_M2.6 <- FOCAL_M1.6 - Ele3.6

## limpiar
FOCAL_M2.6 <-calc(FOCAL_M2.6, fun=function(x){ x[x < 1] <- 0; return(x)} )

## Guardar areas focal base (shape??)
writeRaster(FOCAL_M2.6, "Focal_Areas2sp_60.tif", overwrite =T)
FOCAL_2sp.6 <- raster("C:/Users/BYU Rental/Desktop/R/Focal_Areas2sp_60.tif")

plot(FOCAL_2sp.6)

#################################################################################
### CERW migracion, crear una raster binario, .75 de los pixeles en cuanto a abundancia
hist(ceruM, ylim=c(0,4000))
quantile(ceruM, c(.50, .60, .75, .80, .90, .95)) 
#        50%       60%       75%       80%       90%       95% 
#  0.1177528 0.1816610 0.3104568 0.3653414 0.4853728 0.5676747 

#75% quantile 0.3104568
ceruM.7 <- calc(ceruM, fun=function(x){ x[x > 0.3104568] <- 1; return(x)} )
ceruM3.7 <- calc(ceruM.7, fun=function(x){ x[x < 1] <- NA; return(x)} )

################################################################################
### CAN migracion, crear una raster binario, top 25% de los pixeles en cuanto a abundancia
hist(canaM, ylim=c(0,4000))
quantile(canaM, c(.50, .60, .75, .80, .90, .95)) 
#      50%       60%       75%       80%       90%       95% 
#0.1797606 0.2419024 0.3363886 0.3723322 0.4606149 0.5302712 

#60% quantile is 0.3363886
canaM.7 <- calc(canaM, fun=function(x){ x[x > 0.3363886] <- 1; return(x)} )
canaM3.7 <- calc(canaM.7, fun=function(x){ x[x < 1] <- NA; return(x)} )

################################################################################
### Golden invierno, crear una raster binario, top 25% de los pixeles en cuanto a abundancia
hist(golW, ylim=c(0,5000))
quantile(golW, c(.50, .60, .75, .80, .90, .95)) 
#      50%       60%       75%       80%       90%       95% 
#0.1856677 0.2094833 0.2498879 0.2656566 0.3280640 0.3935074 

#95% quantile is 0.2498879 
golW.7 <- calc(golW, fun=function(x){ x[x > 0.2498879] <- 1; return(x)} )
golW3.7 <- calc(golW.7, fun=function(x){ x[x < 1] <- NA; return(x)} )

####################### Cominbando las 3 especies
### Convertir NAs a 0 para poder combinar los rasters
ceruM3.7[is.na(ceruM3.7[])] <- 0
canaM3.7[is.na(canaM3.7[])] <- 0
golW3.7[is.na(golW3.7[])] <- 0

COMB_CA.7 <- ceruM3.7 + canaM3.7 + golW3.7

## Seleccionar solo los pixeles con dos 2 o m?s especies
FOCAL_M1.7 <-calc(COMB_CA.7, fun=function(x){ x[x < 1.9] <- 0; return(x)} )
FOCAL_M1.7 <-calc(FOCAL_M1.7, fun=function(x){ x[x > 1.9] <- 1; return(x)} )

## Cortar a la elevaci?n del plan >750 m
Ele1.7 <-calc(Elevation, fun=function(x){ x[x < 750] <- 1; return(x)} )
Ele1.7 <-calc(Ele1.7, fun=function(x){ x[x > 749] <- 0; return(x)} )

# ajustar resoluci?n para que sean iguales entre capas
Ele2.7 <- resample(Ele1.7, FOCAL_M1.7)
Ele3.7 <-calc(Ele2.7, fun=function(x){ x[x < 1] <- 0; return(x)} )

FOCAL_M2.7 <- FOCAL_M1.7 - Ele3.7

## limpiar
FOCAL_M2.7 <-calc(FOCAL_M2.7, fun=function(x){ x[x < 1] <- 0; return(x)} )

## Guardar areas focal base (shape??)
writeRaster(FOCAL_M2.7, "Focal_Areas2sp_75.tif", overwrite =T)
FOCAL_2sp.7 <- raster("C:/Users/BYU Rental/Desktop/R/Focal_Areas2sp_75.tif")

plot(FOCAL_2sp.7)


#################################################################################
### CERW migracion, crear una raster binario, .80 de los pixeles en cuanto a abundancia
hist(ceruM, ylim=c(0,4000))
quantile(ceruM, c(.50, .60, .75, .80, .90, .95)) 
#        50%       60%       75%       80%       90%       95% 
#  0.1177528 0.1816610 0.3104568 0.3653414 0.4853728 0.5676747 

#80% quantile 0.3653414
ceruM.8 <- calc(ceruM, fun=function(x){ x[x > 0.3653414] <- 1; return(x)} )
ceruM3.8 <- calc(ceruM.8, fun=function(x){ x[x < 1] <- NA; return(x)} )

################################################################################
### CAN migracion, crear una raster binario, top 20% de los pixeles en cuanto a abundancia
hist(canaM, ylim=c(0,4000))
quantile(canaM, c(.50, .60, .75, .80, .90, .95)) 
#      50%       60%       75%       80%       90%       95% 
#0.1797606 0.2419024 0.3363886 0.3723322 0.4606149 0.5302712 

#80% quantile is 0.3723322
canaM.8 <- calc(canaM, fun=function(x){ x[x > 0.3723322] <- 1; return(x)} )
canaM3.8 <- calc(canaM.8, fun=function(x){ x[x < 1] <- NA; return(x)} )

################################################################################
### Golden invierno, crear una raster binario, top 20% de los pixeles en cuanto a abundancia
hist(golW, ylim=c(0,5000))
quantile(golW, c(.50, .60, .75, .80, .90, .95)) 
#      50%       60%       75%       80%       90%       95% 
#0.1856677 0.2094833 0.2498879 0.2656566 0.3280640 0.3935074 

#80% quantile is 0.2656566
golW.8 <- calc(golW, fun=function(x){ x[x > 0.2656566] <- 1; return(x)} )
golW3.8 <- calc(golW.7, fun=function(x){ x[x < 1] <- NA; return(x)} )

####################### Cominbando las 3 especies
### Convertir NAs a 0 para poder combinar los rasters
ceruM3.8[is.na(ceruM3.8[])] <- 0
canaM3.8[is.na(canaM3.8[])] <- 0
golW3.8[is.na(golW3.8[])] <- 0

COMB_CA.8 <- ceruM3.8 + canaM3.8 + golW3.8

## Seleccionar solo los pixeles con dos 2 o m?s especies
FOCAL_M1.8 <-calc(COMB_CA.8, fun=function(x){ x[x < 1.9] <- 0; return(x)} )
FOCAL_M1.8 <-calc(FOCAL_M1.8, fun=function(x){ x[x > 1.9] <- 1; return(x)} )

## Cortar a la elevaci?n del plan >750 m
Ele1.8 <-calc(Elevation, fun=function(x){ x[x < 750] <- 1; return(x)} )
Ele1.8 <-calc(Ele1.8, fun=function(x){ x[x > 749] <- 0; return(x)} )

# ajustar resoluci?n para que sean iguales entre capas
Ele2.8 <- resample(Ele1.8, FOCAL_M1.8)
Ele3.8 <-calc(Ele2.8, fun=function(x){ x[x < 1] <- 0; return(x)} )

FOCAL_M2.8 <- FOCAL_M1.8 - Ele3.8

## limpiar
FOCAL_M2.8 <-calc(FOCAL_M2.8, fun=function(x){ x[x < 1] <- 0; return(x)} )

## Guardar areas focal base (shape??)
writeRaster(FOCAL_M2.8, "Focal_Areas2sp_80.tif", overwrite =T)
FOCAL_2sp.8 <- raster("C:/Users/BYU Rental/Desktop/R/Focal_Areas2sp_80.tif")

plot(FOCAL_2sp.8)
