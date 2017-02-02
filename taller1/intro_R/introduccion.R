####################################
#DEFINIENDO EL DIRECTORIO DE TRABAJO
####################################

#ESTABLECE EL DIRECTORIO DE TRABAJO
setwd("c:/taller1/intro_R/datos")
setwd("/home/blas/Dropbox/DOCENCIA/CURSOS_MDE_GBIF/2015/sesiones/taller1/intro_R")


#INSTALANDO PAQUETES
####################
install.packages(c("car","plotmo","raster","corrgram", "dismo"), dep=TRUE)
#NOTA IMPORTANTE: LAS LIBRERÍAS SE INSTALAN UNA SOLA VEZ, DESPUÉS DE ESTO, YA ESTÁN EN NUESTRO ORDENADOR, Y SOLO QUEDA CARGARLAS CUANDO LAS NECESITEMOS. ALGUNAS TARDAN UN RATO EN INSTALARSE, PACIENCIA!

#CARGANDO PAQUETES
#LAS LIBRERÍAS SOLO SE CARGAN UNA VEZ POR SESIÓN
library(car)
library(plotmo)
library(raster)
library(corrgram)
library(dismo)


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



###############
#TIPOS DE DATOS
###############
#NUMERIC
x<-10.5
x
is.numeric(x)
is.character(x)
#borramos el objeto "x", no lo vamos a necesitar más

rm(x) #comprueba que ha desaparecido del espacio de trabajo


#CHARACTER (SIEMPRE entre comillas)
y<-"perro"
y
is.numeric(y)
is.character(y)

rm(y)


#VALORES PERDIDOS O NO NUMÉRICOS
#Ojo, algunas funciones no funcionarán con valores NA o NaN
x<-c(1, 2, 3, 4, 5, 6, 7, NA) #NA -> Not Available
is.na(x)
mean(x)
help(mean)
mean(x, na.rm=T)

rm(x)


#REDONDEANDO NÚMEROS
round(5.9, digits=0)

#valor absoluto
abs(-5.9)


#####################
#ESTRUCTURAS DE DATOS
#####################

#VECTOR
#######
z<-c(1, 2, 3, 4, 5, 6)
z
length(z)
is.vector(z)
z<-1:6
z
z<-seq(1, 6, 1)
z

#ALGUNAS FUNCIONES APLICABLES A VECTORES
max(z)
min(z)
sum(z)
mean(z)
median(z)
range(z)
sort(z)

rm(z)


#DATA FRAME
###########
#cargamos una tabla de ejemplo que ya está en R
data(cars)
cars
is.data.frame(cars)

#para ver los datos, pincha sobre la entrada "cars" en la pestaña Environment de Rstudio

#vemos la estructura interna del dataframe
str(cars) 
#fíjate en los elementos que empiezan con $, son columnas. En R, cada columna de un data.frame es un vector

#ÍNDICES PARA ACCEDER A VALORES DE UN DATA FRAME
#accedemos a las columnas por separado
cars$speed
cars$dist
#otro modo de hacerlo
cars[, "speed"]
cars[, "dist"]

#accedemos a la fila 10
cars[10,]

#accedemos a varias filas
cars[5:15,]

#accedemos a varias filas y una de las columnas
cars[5:15, "speed"]

#seleccionamos un subset con una condición sobre una variable
#ojo a la coma después del paréntesis, el espacio vacío quiere decir "todas las columnas"
cars2<-cars[cars$dist < 60, ]
cars2

rm(cars)
rm(cars2)

#CREAR UN DATA FRAME
#definimos las columnas
x<-c(10, 20, 40, 80)
y<-c("s","s","n","n")

#las unimos
xy<-data.frame(x, y)

#le ponemos nombre
names(xy)<-c("valor","factor")

#crear un dataframe en blanco (es útil ocasionalmente, lo dejo aquí como referencia)
df.blanco<-dataframe(especie=as.character(), latitud=numeric(), longitud=numeric(), stringsAsFactors=FALSE)

rm(x, y, xy, d1)


#importa un dataframe del disco duro
d1<-read.table("./datos/clima.csv", header=TRUE, sep=",")
str(d1)

rm(d1)


###############################
#DATOS RASTER (MAPAS DIGITALES)
###############################
#IMPORTA LOS MAPAS
#lista de variables
lista.variables <- list.files(path="./datos/mapas", pattern='bio', full.names=TRUE)
lista.variables
#stack de variables (el stack lee las variables desde el disco duro, no las carga en el espacio de trabajo)
variables.clima.stack <- stack(lista.variables)

#dibujamos los mapas
plot(variables.clima.stack)

#vemos los nombres de las variables
names(variables.clima.stack)

#dibujamos un mapa concreto
plot(variables.clima.stack, "bio5")
plot(variables.clima.stack[["bio5"]])

#brick de variables (el brick tiene las variables en la memoria RAM del ordenador, es más rápido, pero lo usaremos solo si tenemos RAM suficiente)
variables.clima.brick <- brick(variables.clima.stack)
#fíjate que ahora, en la pestaña Environment, variables.brick aparece con su tamaño (3.5MB)

#se plotea igual que un stack
plot(variables.clima.brick)

#vemos las características
variables.clima.brick

#vemos lo que hay dentro
slotNames(variables.clima.brick)

#vemos lo que hay en el slot extent
slot(variables.clima.brick, "extent")

#otra forma de acceder al slot
variables.clima.brick@extent
#otra forma de verla
extension<-extent(variables.clima.brick)
extension

#fíjate que coord. ref. viene como NA, le ponemos el que le corresponde
#NOTA: los sistemas de referencia se escriben en formato Proj4.
#Puedes consultar el Proj4 de cada sistema de referencia aquí: http://www.spatialreference.org/
sistema.referencia<-CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
projection(variables.clima.brick)<-sistema.referencia
variables.clima.brick

#vemos la resolución espacial
xres(variables.clima.brick)
yres(variables.clima.brick)
#¿cuanto es eso en km?
xres(variables.clima.brick)*111.19

#operaciones con raster: rango de temperatura
rangot<-variables.clima.brick[["bio5"]] - variables.clima.brick[["bio6"]]
names(rangot)<-"rangot"

#añadimos la nueva variable al brick
variables.clima.brick<-addLayer(rangot, variables.clima.brick)
plot(variables.clima.brick)

#convertimos una variable en un mapa binario a partir de un determinado valor
plot(variables.clima.brick[["rangot"]] > 250)

#transformar los mapas en un data frame
variables.df<-as.data.frame(variables.clima.brick)
str(variables.df)
#muchos valores nulos, los quitamos
variables.df<-na.omit(variables.df)
str(variables.df)
#NO BORRAMOS variables.df, VAMOS A USARLO LUEGO

#sacamos un raster del brick
bio5<-variables.clima.brick[["bio5"]]
plot(bio5)

#le cambiamos la resolución
bio5.lowres<-aggregate(bio5, fact=5, fun=mean)
plot(bio5.lowres)

#lo recortamos
#la extensión nueva va en orden: xmin, xmax, ymin, ymax
extension.nueva<-c(-10, 5, 35, 45)
bio5.lowres.peninsula<-crop(bio5.lowres, extension.nueva)
plot(bio5.lowres.peninsula)

#lo proyectamos a ETRS89 / UTM zone 30N a una resolución de 50KM
bio5.lowres.peninsula.utm<-projectRaster(bio5.lowres.peninsula, crs="+proj=utm +zone=30 +ellps=GRS80 +units=m +no_defs ", res=c(50000, 50000))

#vemos la resolución
yres(bio5.lowres.peninsula.utm)
xres(bio5.lowres.peninsula.utm)

#ploteamos los dos
par(mfrow=c(1,2))
plot(bio5.lowres.peninsula)
plot(bio5.lowres.peninsula.utm)

#guarda el raster para verlo en un GIS
writeRaster(bio5.lowres.peninsula.utm, filename="bio5_peninsula_utm.asc", format="ascii", overwrite=TRUE)

#BORRAMOS OBJETOS QUE NO NECESITAMOS
rm(bio5, bio5.lowres, bio5.lowres.peninsula.utm, list.variables, extension, rangot, bio5.lowres.peninsula, extension.nueva, sistema.referencia, variables.clima.stack)






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

#BUCLE FOR SOBRE VECTOR DE CARACTERES
generos<-c("Abies","Fagus","Pinus","Quercus")
for (genero in generos){
  print(genero)
}

rm(genero)

#BUCLE PARA DIBUJAR LOS MAPAS DE VARIABLES EN UN PDF MULTIPÁGINA
pdf("variables.pdf", width=20, height=15, pointsize=20)
for (variable in 1:length(names(variables.brick))){
  plot(variables.brick, variable)
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
n.filas<-nrow(variables.df)
n.filas

#MUESTREAMOS 1000 filas
muestra.1000<-sample(n.filas, 1000) #devuelve índices de casos seleccionados al azar
variables.df.small<-variables.df[muestra.1000, ]
nrow(variables.df.small)

#GUARDAMOS LA NUEVA TABLA
write.table(variables.df.small, "Tabla.csv", sep=",", row.names=FALSE, col.names=TRUE, quote=FALSE)

#CORRELOGRAMA
corrgram(variables.df.small, order=TRUE, lower.panel=panel.ellipse, upper.panel=panel.pts, text.panel=panel.txt)

#ANÁLISIS DE CORRELACIÓN
variables.cor<-cor(variables.df.small)
variables.cor


#CÁLCULO DE UN MODELO LINEAL
#¿SE PUEDE PREDECIR LA TEMPERATURA MEDIA MÁXIMA EN FUNCIÓN DE LA TEMPERATURA MEDIA MÍNIMA Y LA PRECIPITACIÓN?
#ajustamos el modelo
modelo<-lm(bio5 ~ bio6 + bio12, data=variables.df.small)
#resumen del modelo
summary(modelo)
#curvas de respuseta
plotmo(modelo, level=0.68, all2=TRUE)
#predicción sobre las variables (requiere tener cargado 'raster')
help(predict) #hay tres versiones distintas
#para usar una concreta, usamos la sintaxis nombre_libreria::nombre_funcion
prediccion<-raster::predict(object=variables.clima.brick, model=modelo, progress="text")
plot(prediccion)

##################
#CREANDO FUNCIONES
##################
#creamos una serie de 1000 números que siguen una distribución normal, con una media al azar entre 50 y 100, y una desviación al azar entre 1 y 10, es decir, media y desviación desconocidas
x<-rnorm(n=1000, mean=20, sd=5)

#dibujamos el histograma
hist(x)

#el gráfico de densidad
plot(density(x))

#vamos a sumar los valores de la serie con un bucle
suma<-sum(x)
suma

#vamos a calcular el número de casos (aunque ya lo sabemos!)
n<-length(x)
n

#calculamos la media
media<-suma/n
media

#¿TENGO QUE ESCRIBIR TODO ESTE CÓDIGO CADA VEZ QUE QUIERO CALCULAR UNA MEDIA?
#NO, PARA ESO PODEMOS ESCRIBIR UNA FUNCIÓN
media.aritmetica<-function(x){
  sum(x)/length(x)
}

#ahora aplicamos la función a la serie de números
media.aritmetica(x)

#NOTA: esta función ya existe en R:
mean(x)

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

