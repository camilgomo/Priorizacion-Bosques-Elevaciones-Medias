# Explorando capas de vegetacion

#Cargar librerias
library(raster)


## Llamar la capa de elevacion
Eleva <- raster("C:\\Users\\BYU Rental\\Desktop\\Taller\\altitudeCUT.tif") 

#Reclasificar segun las elevaciones del plan 750 - 2000 m
Ele.re = reclassify(Eleva, c(-Inf, 750, 0, 750, 2000, 1, 2000, Inf, 0))

####### Cobertura arborea #################################################

# Resolucion 30m


tree <- raster("C:\\Users\\BYU Rental\\Desktop\\Taller\\Capas\\treecover.tif") 
loss <- raster("C:\\Users\\BYU Rental\\Desktop\\Taller\\Capas\\Loss.tif")

treeco <- tree - loss

treco.crop <- crop(treeco, Area)


####   Cortar a la cobertura del plan >70%  ######################

tree.re = reclassify(treco.crop, c(-Inf, 70, 0, 70, Inf, 1))

#Ajustar a las mismas dimensiones
treeco.res <- resample(Ele.re, tree.re)

tree.70 <- tree.re + treeco.res

#Limpiar
tree.ele = reclassify(tree.70, c(-Inf, 1.9, 0, 2, Inf, 1))

## Guardar raster
writeRaster(tree.ele, "tree_70.tif", overwrite =T)


####### Altura de dosel ###################################################

# La union de rasters de altura de dosel (2020) con raster de Loss (Hansen 2021) 
#  fue realizado por separado (ArcGIS Pro).
# El raster se comprimio con metodo Lossless

# Resolucion 30m


dosel <- raster("C:\\Users\\BYU Rental\\Desktop\\Taller\\dosel\\dosel15.tif")


####  Reclasificar a la altura del plan > 15 m  #####################
dosel.re = reclassify(dosel, c(-Inf, 15, 0, 15, Inf, 1))

#Reajustar a las mismas dimensiones
El.resam <- resample(Ele.re, dosel.re)


dosel.uni <- dosel.re + El.resam


#Limpiar
dos.ele = reclassify(dosel.uni, c(-Inf, 1.9, 0, 2, Inf, 1))

## Guardar raster
writeRaster(dos.ele, "dosel_15.tif", overwrite =T)

######################################################################
##                                                                 ##  
########### Cobertura y dosel JUNTOS #################################

tree_70 <- raster("C:\\Users\\BYU Rental\\Desktop\\R\\tree_70.tif")
dos_15 <- raster("C:\\Users\\BYU Rental\\Desktop\\R\\dosel_15.tif")

dosel15 <- resample (dos_15, tree_70)

codo <- tree_70 + dosel15

## Seleccionar solo los pixeles con las dos condiciones 
codo_ca <-calc(codo, fun=function(x){ x[x < 3.9] <- 0; return(x)} )
codo_ca <-calc(codo_ca, fun=function(x){ x[x > 3.9] <- 1; return(x)} )

#Limpiar
vegetacion = reclassify(co70_do15, c(-Inf, 0.9, 0, 0.9, Inf, 1))

## Guardar raster
writeRaster(codo_ca, "codo_7015(2).tif", overwrite =T)

co <- raster("C:\\Users\\BYU Rental\\Desktop\\R\\codo_7015(2).tif")
