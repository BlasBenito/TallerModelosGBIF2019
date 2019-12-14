#INSTALANDO PAQUETES
####################
    # install.packages(c("car","plotmo","raster","corrplot", "dismo", "foreach", "doParallel", "ggplot2", "viridis", "cowplot", "tidyr", "rgdal"), dep=TRUE)
#NOTA IMPORTANTE: LAS LIBRERÍAS SE INSTALAN UNA SOLA VEZ, DESPUÉS DE ESTO, YA ESTÁN EN NUESTRO ORDENADOR, Y SOLO QUEDA CARGARLAS CUANDO LAS NECESITEMOS. ALGUNAS TARDAN UN RATO EN INSTALARSE, PACIENCIA!

#AJUSTA LA CARPETA DE TRABAJO A LA LOCALIZACIÓN DE ESTE SCRIPT
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#CARGANDO PAQUETES
#LAS LIBRERÍAS SOLO SE CARGAN UNA VEZ POR SESIÓN
# library(car)
# library(plotmo)
# library(raster)
# library(corrgram)
# library(dismo)
library(ggplot2)
library(raster)
library(rgdal)

###############
#BUSCANDO AYUDA
###############
#página web de ayuda
help.start() 

#busca funciones relacionadas
help.search("regression") 

#busca en la lista de distribución de R y en los manuales
RSiteSearch("regression") 

#muestra la ayuda de una función específica
#ES LA QUE MÁS SUELO USAR
help(lm) 

#muestra la ayuda de una libería
help(package = "stats") 

#muestra los nombres de todas las funciones relacionados
apropos("lm")

#nos muestra el ejemplo de una función concreta
example(lm) 

#demostraciones
demo(persp)
demo(graphics)


###############################
#DATOS RASTER (MAPAS DIGITALES)
###############################


#IMPORTANDO MAPAS
#################
#lista de variables
clima.lista <-  list.files(path="./datos/mapas", pattern='bio', full.names=TRUE)
clima.lista
#stack de variables (el stack lee las variables desde el disco duro, no las carga en el espacio de trabajo)
clima.stack <-  raster::stack(clima.lista)

#brick de variables (el brick tiene las variables en la memoria RAM del ordenador, es más rápido, pero lo usaremos solo si tenemos RAM suficiente)
clima.brick <-  raster::brick(clima.stack)
#fíjate que ahora, en la pestaña Environment, variables.brick aparece con su tamaño (3.5MB)


#PLOTEANDO MAPAS
#################
#dibujamos los mapas
raster::plot(clima.brick, col = viridis::viridis(30))

#vemos los nombres de las variables
names(clima.brick)

#dibujamos un mapa concreto
raster::plot(clima.brick, "bio5", col = viridis::viridis(30))
raster::plot(clima.brick[["bio5"]], col = viridis::viridis(30))


#VIENDO CARACTERÍSTICAS DE LOS MAPAS
#####################################
#RESUMEN DEL OBJETO
clima.brick

#RESOLUCIÓN
raster::xres(clima.brick)
raster::yres(clima.brick)
#¿cuanto es eso en km?
raster::xres(clima.brick)*111.19

#EXTENSION
raster::extent(clima.brick)


#CAMBIANDO CARACTERÍSTICAS DE LOS MAPAS
######################################

#CAMBIANDO RESOLUCION
clima.brick.coarse <- raster::aggregate(
  x = clima.brick, 
  fact = 10, 
  fun = mean
  )

raster::res(clima.brick.coarse)
raster::plot(clima.brick.coarse, col = viridis::viridis(30))


#CAMBIANDO LA PROYECCIÓN
#fíjate que en "crs" la definición del sistema de coordenadas, viene como NA, le ponemos el que le corresponde
#NOTA: los sistemas de referencia se escriben en formato Proj4.
#Puedes consultar el Proj4 de cada sistema de referencia aquí: http://www.spatialreference.org/
raster::projection(clima.brick.coarse) <- sp::CRS("+init=epsg:4326")
clima.brick.coarse

#a UTM (zona 30N)
clima.brick.coarse.utm <- raster::projectRaster(
  from = clima.brick.coarse, 
  crs = crs("+init=epsg:25830")
  )

raster::plot(clima.brick.coarse.utm)


#OPERACIONES VARIADAS CON MAPAS RASTER
#########################################

#álgebra de mapas
rangot <-  clima.brick[["bio5"]] - clima.brick[["bio6"]]
names(rangot) <-  "rangot"
raster::plot(rangot)

#álgebra de mapas con calc
mediat <- raster::calc(
  x = clima.brick[[c("bio5", "bio6")]], 
  fun = mean
  )
raster::plot(mediat)

#añadimos la nueva variable al brick
clima.brick <-  raster::addLayer(x = rangot, clima.brick)

#recortando un área concreta
#la extensión nueva va en orden: xmin, xmax, ymin, ymax
extension.nueva <- c(-10, 5, 35, 45)
clima.peninsula <- raster::crop(
  x = clima.brick, 
  y = extension.nueva
  )
raster::plot(clima.peninsula)

#transformar los mapas en un data frame
variables.df <- raster::as.data.frame(
  x = clima.brick, 
  xy = TRUE
  )
str(variables.df)
#muchos valores nulos, los quitamos
variables.df <- na.omit(variables.df)
str(variables.df)


#guarda el raster para verlo en un GIS
writeRaster(
  clima.peninsula[[1]], 
  filename="mapa.asc", 
  format="ascii",
  overwrite=TRUE
  )


#IGUALANDO LA EXTENSIÓN Y RESOLUCIÓN DE MAPAS RASTER
###################################################
elev <- raster("datos/igualar_variables/elevacion.asc")
hfp <- raster("datos/igualar_variables/hfp.asc")
ndvi <- raster("datos/igualar_variables/ndvi.asc")

#plot
x11()
par(mfrow=c(1,3))
raster::plot(elev)
raster::plot(hfp)
raster::plot(ndvi)

#comprobamos extensión
extent(elev)
extent(hfp)
extent(ndvi)

#comprobamos resolucion
xres(elev)
xres(hfp)
xres(ndvi)

#1 - IGUALAMOS EXTENSIÓN Y RESOLUCIÓN
#primero con elev
elev2 <- resample(
  x=elev, 
  y=ndvi, 
  method="bilinear" #equivalente a promedio
  )

#comparamos extensión
extent(ndvi)
extent(elev2)

#comparamos resolución
xres(ndvi)
xres(elev2)

#plot
x11()
par(mfrow=c(1,2))
raster::plot(elev)
raster::plot(elev2)

#lo hacemos con el siguiente mapa, ya no hacen falta las comprobaciones
hfp2 <- resample(
  x=hfp, 
  y=ndvi, 
  method="bilinear"
  )

#comprobamos que ha ido bien
x11()
par(mfrow=c(1, 3))
raster::plot(ndvi)
raster::plot(elev2)
raster::plot(hfp2)

#2 - PREPARAMOS UN MAPA DE CELDAS NULAS (MÁSCARA) COMUNES A TODOS LOS MAPAS (USAMOS MULTIPLICACIÓN PARA PROPAGAR)
valores.nulos <- ndvi * elev2 * hfp2 
#la multiplicación de todos los mapas propaga los valores nulos
x11()
raster::plot(valores.nulos)


#3 - APLICAMOS EL MAPA DE CELDAS NULAS A TODOS LOS MAPAS
#ponemos todos los mapas juntos en un brick
variables.brick <- brick(
  ndvi, 
  elev2, 
  hfp2
  )
names(variables.brick)<-c("ndvi", "elev", "hfp")

#aplicamos la máscara de valores nulos a variables.brick
variables.brick <- mask(
  x = variables.brick, 
  mask = valores.nulos
  )
x11()
raster::plot(variables.brick) #so far so good

#4 - RECORTE FINAL
#finalmente recortamos el brick con la extensión del mapa con menor extensión, porque nuestro mapa de referencia era el de mayor extensión, pero las áreas sobrantes han quedado ocultas por las celdas nulas
extent(elev)
extent(hfp) #parece que tiene una extensión menor
variables.brick <- crop(
  x = variables.brick, 
  y = extent(hfp)
  )
raster::plot(variables.brick)

#5 - EXPORTAMOS LOS MAPAS
#guardamos las variables preparadas al disco duro
raster::writeRaster(
  variables.brick[["elev"]], 
  filename="./igualar_variables/elev_final.asc", 
  format="ascii", 
  overwrite=TRUE
  )

raster::writeRaster(
  variables.brick[["hfp"]], 
  filename="./igualar_variables/hfp_final.asc", 
  format="ascii", 
  overwrite=TRUE
  )

raster::writeRaster(
  variables.brick[["ndvi"]], 
  filename="./igualar_variables/ndvi_final.asc", 
  format="ascii", 
  overwrite=TRUE
  )


#######################
#ESTRUCTURAS DE CONTROL
#######################

#BUCLES FOR
###########

#BUCLE FOR SOBRE SECUENCIA NUMÉRICA
for (year in 1980:2000){
 print(year)
}

rm(year)

#vectorizado
sapply(X = 1980:2000, FUN = function(x) print(x))


#BUCLE FOR SOBRE VECTOR DE CARACTERES
generos <- c("Abies","Fagus","Pinus","Quercus")
for (genero in generos){
 print(genero)
}

#vectorizado
sapply(X = generos, FUN = function(x) print(x))

rm(genero)


#BUCLE PARA DIBUJAR LOS MAPAS DE VARIABLES EN UN PDF MULTIPÁGINA
pdf("variables.pdf", width=20, height=15, pointsize=20)
for (variable in 1:length(names(clima.brick))){
 plot(clima.brick, variable)
}
dev.off()

rm(variable)


#¿COMO DETENEMOS LA EJECUCIÓN DE UN BUCLE?
for (genero in generos){
 print(genero)
 if(genero=="Pinus"){break} #condición para parar el bucle
}

rm(genero)


#ANÁLISIS Y MODELIZACIÓN
########################

#LA TABLA CON LA QUE VAMOS A HACER LOS ANÁLISIS ES MUY GRANDE, LA REMUESTREAMOS
n.filas <- nrow(variables.df)
n.filas

#MUESTREAMOS 1000 filas
variables.df.small <- variables.df[sample(n.filas, 1000), ]
nrow(variables.df.small)

#GUARDAMOS LA NUEVA TABLA
write.table(variables.df.small, "Tabla.csv", sep=",", row.names=FALSE, col.names=TRUE, quote=FALSE)

#CORRELOGRAMA
corrplot::corrplot(
  cor(variables.df.small)
  )


#CÁLCULO DE UN MODELO LINEAL
#¿SE PUEDE PREDECIR LA TEMPERATURA MEDIA MÁXIMA EN FUNCIÓN DE LA TEMPERATURA MEDIA MÍNIMA Y LA PRECIPITACIÓN?
#ajustamos el modelo
modelo <- lm(
  formula = bio5 ~ bio6 + bio12, 
  data = variables.df.small
  )
#resumen del modelo
summary(modelo)

#curvas de respuseta
plotmo::plotmo(
  modelo, 
  level = 0.68, 
  all2 = TRUE
  )

#predicción
prediccion <- raster::predict(
  object = clima.peninsula, 
  model = modelo
  )
raster::plot(prediccion)

#comparamos predicción con realidad
cor(
  na.omit(as.vector(prediccion)), 
  na.omit(as.vector(clima.peninsula[["bio5"]]))
  )


##################
#CREANDO FUNCIONES
##################
#imagina que tenemos dos vectores de números representando probabilidad de presencia y probabilidad de ausencia
prob.pres <- rnorm(n=100, mean=0.7, sd=0.1)
prob.aus <- rnorm(n=100, mean=0.3, sd=0.1)

#nuestro objetivo es hacer un plot como este para cualquier par arbitrario de vectores numéricos
plot(
  density(prob.pres), 
  xlim = c(0, 1), 
  col = "blue4", 
  lwd = 1.5, 
  xlab = "Probability"
  )
lines(
  density(prob.aus), 
  col = "red4", 
  lwd = 1.5
  )

#el esqueleto de una función siempre es
plot.dist <- function(x, y){
  plot(
    density(x), 
    xlim = c(0, 1), 
    col = "blue4", 
    lwd = 1.5, 
    xlab = "Probability"
  )
  lines(
    density(y), 
    col = "red4", 
    lwd = 1.5
  )
}

#usándola
plot.dist(
  x = rnorm(n=100, mean=0.7, sd=0.1),
  y = rnorm(n=100, mean=0.3, sd=0.1)
  )


##################################
#CONTROLANDO EL ESPACIO DE TRABAJO
##################################
#ver los objetos que he creado durante la sesión
objects()
#están listados en la pestaña Environment

#guarda el espacio de trabajo en la carpeta de trabajo (setwd())
save(list=ls(all=TRUE), file="mi_espacio_de_trabajo.Rdata")
#para que el fichero "mi_espacio_de_trabajo.Rdata" aparezca en la pestaña Files,
#tienes que darle al botón de refrescar

#borra todo (OJO!)
rm(list=ls())

#carga el espacio de trabajo
load("mi_espacio_de_trabajo.Rdata")

#ver las librerías y data.frames en el espacio de trabajo
search()

