###################################
#ESTABLECE EL DIRECTORIO DE TRABAJO
###################################
#DIRECTORIO DE TRABAJO
setwd("C:/taller2")
setwd("/home/blas/Dropbox/DOCENCIA/CURSOS_MDE_GBIF/2014/sesiones/sesion_2/taller2")

#CREA EL DIRECTORIO PARA GUARDAR LOS RESULTADOS
dir.create("resultados")

################################
#INSTALACION Y CARGA DE PAQUETES
################################
#NOTA: SOLO ES NECESARIO INSTALARLOS UNA VEZ. DESACTIVA ESTAS LÍNEAS PARA LA PRÓXIMA SESIÓN
#INSTALA PAQUETE DISMO Y TODAS SUS DEPENDENCIAS (EJECUTAR UNA SOLA VEZ)
# install.packages("dismo", dep=TRUE)
# install.packages("plotmo", dep=TRUE)
install.packages("rgeos", dep=TRUE)
install.packages("HH", dep=TRUE)

#CARGA LAS LIBRERIAS NECESARIAS (EJECUTAR SIEMPRE QUE TRABAJES CON EL SCRIPT)
library(raster) #TRABAJO CON DATOS RASTER
library(HH) #VARIANCE INFLATION FACTOR
library(rgeos) #OPERACIONES GEOMÉTRICAS CON INFO GEOGRÁFICA
library(dismo) #LIBRERIA PARA MODELOS DE DISTRIBUCION


################################################################
################################################################
#ANÁLISIS DE CORRELACIÓN DE LAS VARIABLES PREDICTIVAS
################################################################
################################################################


#IMPORTA LAS VARIABLES PREDICTORAS A UN BRICK
#############################################
#DESCOMPRIME LAS VARIABLES
unzip("./data/1_variables.zip", exdir="./resultados/variables", junkpaths=TRUE)
#ahora todas las variables están en el directorio variables

#LISTADO DE VARIABLES
lista.variables <- list.files(path="./resultados/variables",pattern='*.asc', full.names=TRUE)
lista.variables

#stack Y brick PREPARAN LAS VARIABLES EN UN UNICO OBJETO ESPACIAL
help(stack) #mapas en disco, más lento (pero usa menos RAM)
variables <- brick(stack(lista.variables))

#RESOLUCION DE LAS VARIABLES
xres(variables)
yres(variables)
res.grados<-xres(variables)
#en km
res.grados*111.19

#DIBUJA LAS VARIABLES PREDICTORAS
plot(variables, maxnl=38)

#TRANSFORMA LOS MAPAS EN UNA TABLA
variables.tabla<-as.data.frame(variables)

#ELIMINA LOS VALORES NULOS
variables.tabla<-na.omit(variables.tabla)

#MATRIZ DE CORRELACIÓN
help(cor)
variables.correlacion<-cor(variables.tabla)

#MATRIZ DE DISTANCIAS ('ABS' = VALOR ABSOLUTO, PARA ELIMINAR CORRELACIONES NEGATIVAS)
help(as.dist)
help(abs)
variables.dist<-abs(as.dist(variables.correlacion))

#CLUSTER DE VARIABLES SEGÚN LA DISTANCIA (MENOR DISTANCIA = MAYOR CORRELACIÓN)
help(hclust)
variables.cluster<-hclust(1-variables.dist)

#GRAFICO DEL CLUSTER DE CORRELACIONES 
plot(variables.cluster)

#GRÁFICO DEL CLUSTER DE CORRELACIONES EXPORTADO A PDF
pdf("./resultados/correlacion.pdf", width=12, height=12, pointsize=20)
plot(variables.cluster)
dev.off()

#SELECCIONAMOS LAS VARIABLES CON LAS QUE QUEREMOS TRABAJAR
variables.seleccionadas<-c("landcover_veg_bare", "bio5", "bio15", "bio14", "ndvi_average", "landcover_veg_herb", "landcover_veg_tree", "bio8", "diversidad_topo", "bio12", "bio6", "human_footprint", "bio7", "ndvi_range")

#HACEMOS UNA NUEVA TABLA SOLO CON ESAS VARIABLES
variables.tabla2<-variables.tabla[ , variables.seleccionadas]

#PERO PUEDE HABER VARIABLES QUE SON COMBINACIÓN LINEAL DE OTRAS VARIABLES...
#CALCULAMOS EL VARIANCE INFLATION FACTOR (explicación en la diapositiva)
resultado.vif<-vif(variables.tabla2)
resultado.vif

#EL MÁXIMO VALOR PERMITIDO ES 5
#QUITAMOS bio7
variables.tabla2$bio7<-NULL
resultado.vif<-vif(variables.tabla2)
resultado.vif

#QUITAMOS landcover_veg_bare
variables.tabla2$landcover_veg_bare<-NULL
resultado.vif<-vif(variables.tabla2)
resultado.vif

#QUITAMOS bio8
variables.tabla2$bio8<-NULL
resultado.vif<-vif(variables.tabla2)
resultado.vif

#QUITAMOS bio15
variables.tabla2$bio15<-NULL
resultado.vif<-vif(variables.tabla2)
resultado.vif

#QUITAMOS landcover_veg_herb
variables.tabla2$landcover_veg_herb<-NULL
resultado.vif<-vif(variables.tabla2)
resultado.vif

#QUITAMOS bio12
variables.tabla2$bio12<-NULL
resultado.vif<-vif(variables.tabla2)
resultado.vif

#sacamos los nombres del resultado para usarlos como lista de variables seleccionadas
variables.seleccionadas<-names(resultado.vif)

#nos quedamos solo con las variables del brick que nos interesan
variables<-variables[[variables.seleccionadas]]

#ploteamos las variables
png("./resultados/variables.png", width=2200, height=1300, pointsize=30)
par(oma=c(3,3,3,3))
plot(variables)
dev.off()

#podemos borrar variables.tabla, no la vamos a usar más
rm(variables.correlacion, variables.tabla, variables.tabla2, lista.variables, res.grados, resultado.vif, variables.cluster, variables.dist, variables.seleccionadas)

#ganamos un poco de memoria RAM
gc()


################################################################
################################################################
#PREPARACION DE LAS TABLAS DE DATOS PARA HACER LOS MODELOS
################################################################
################################################################

#DESCOMPRIME LOS REGISTROS DE PRESENCIA
unzip("./data/2_presencia_Ursus_arctos.zip", exdir="./resultados/presencia")

#VAMOS A DARLE UN VISTAZO CON UNA HOJA DE CÁLCULO AL FICHERO /presencia/occurrence.txt. EL SEPARADOR ES TABULADOR (\t).

#ALGUNAS COLUMNAS DAN PROBLEMAS AL IMPORTARLAS A R, PORQUE TIENEN CARACTERES RAROS. VAMOS A ELIMINARLAS:

#CON EXCEL LE QUITARÍAMOS VARIAS COLUMNAS (YA ESTÁ HECHO) PARA QUEDARNOS CON: basis_of_record specific_epithet species_id  country_code	latitude	longitude	year	coordinate_precision


#IMPORTA REGISTROS DE PRESENCIA
#------------------------------
#importa la tabla
presencia.completa<-read.table("./resultados/presencia/occurrence.txt",header=T, sep='\t', fill=TRUE, check.names=TRUE, stringsAsFactors=FALSE)
#stringsAsFactors=FALSE sirve para procesar mejor la columna coordinate_precision, que lleva mezclados números y caracteres

#borramos los ficheros del disco duro, ya no los necesitamos
unlink("./resultados/presencia", recursive=TRUE)

#hacemos una copia sobre la que haremos los cambios
presencia<-presencia.completa

#si tienes problemas con la memoria, importa este
#presencia<-read.table("presencia/ocurrencia_limpia",header=T, sep=',', fill=TRUE)

#vemos la estructura
str(presencia)


#LIMPIEZA DE LA TABLA
#####################

#QUITAMOS TODOS LOS REGISTROS QUE QUEDAN FUERA DEL ÁREA DE TRABAJO O TIENEN VALORES NULOS PARA LAS VARIABLES
#----------------------------------------------------------------------
presencias.sobre.variables<-extract(x=variables, y=presencia[ , c("longitude","latitude")])
str(presencias.sobre.variables)
#no es un data.frame, lo convertimos
presencias.sobre.variables<-data.frame(presencias.sobre.variables)
#comprobamos como quedó
str(presencias.sobre.variables)
#lo unimos a las presencias
presencia<-cbind(presencia, presencias.sobre.variables)
#quitamos los registros que tienen nulos en los valores de las variables
presencia<-presencia[!(is.na(presencia$bio5)), ]
#ploteamos
plot(variables[[1]])
points(presencia$longitude,presencia$latitude)


#QUITAMOS OTRAS ESPECIES (SI LAS HAY)
#------------------------------------
unique(presencia$specific_epithet)
unique(presencia$species_id)
#hay 2 nombres pero un solo código de especie, todo bien
#si hubiera más, ejecutamos la siguiente línea para quedarnos solo con lo que queremos
presencia<-presencia[presencia$species_id==2433433, ]
#ponemos un único nombre en la columna specific_epithet
presencia$specific_epithet<-"Ursus arctos"


#QUITAMOS LOS REGISTROS FÓSILES (SI LOS HAY)
#-------------------------------------------
barplot(table(presencia$basis_of_record))
#vemos la distribución de los especímenes fósiles y preservados
plot(variables[[1]])
points(presencia[presencia$basis_of_record == "FOSSIL_SPECIMEN", ]$longitude,presencia[presencia$basis_of_record == "FOSSIL_SPECIMEN", ]$latitude, col="red", cex=1.2)
#nos quedamos con todos los registros que no son FOSSIL_SPECIMEN
presencia<-presencia[presencia$basis_of_record != "FOSSIL_SPECIMEN", ]
barplot(table(presencia$basis_of_record))

#QUITAMOS REGISTROS ANTIGUOS
#---------------------------
#distribución de la variable 'year'.
hist(presencia$year)
#¿cuantas celdas con datos vacíos hay para el campo 'year'?
sum(is.na(presencia$year))
#¿donde están esos datos vacíos?
plot(variables[[1]])
points(presencia$longitude,presencia$latitude)
points(presencia[is.na(presencia$year), ]$longitude,presencia[is.na(presencia$year), ]$latitude, col="red", cex=1.2)
#En general, los datos con fecha incierta tienen datos actuales cerca, decido obviarlos. Quitamos registros más antiguos que 1970, para ajustar los datos a la ventana temporal de las variables climáticas
presencia<-presencia[!is.na(presencia$year), ]
#comprobamos que los hemos quitado
sum(is.na(presencia$year))
#nos quedamos con los registros con year mayor o igual a 1970
presencia<-presencia[presencia$year >= 1970, ] #esto se lleva por delante los vacíos
#guardando a png
png("./resultados/presencias_inicial.png", width=1200, height=900, pointsize=30)
plot(variables[[1]], main="Presencias inicial")
points(presencia$longitude,presencia$latitude, cex=0.2)
dev.off()


#DAMOS UN VISTAZO A LA RESOLUCIÓN ESPACIAL DE LOS REGISTROS
#----------------------------------------------------------
#vemos el tipo de los datos
str(presencia$coordinate_precision)
#ojo, son caracteres, no números, los pasamos a numéricos
presencia$coordinate_precision<-as.numeric(presencia$coordinate_precision)
#vemos los valores de los datos
unique(presencia$coordinate_precision)
#vemos su distribución
barplot(table(presencia$coordinate_precision))
#¿cuantas celdas con datos vacíos hay para el campo 'year'?
sum(is.na(presencia$coordinate_precision))
#¿donde están esos datos vacíos?
plot(variables[[1]])
points(presencia$longitude,presencia$latitude)
points(presencia[is.na(presencia$coordinate_precision), ]$longitude, presencia[is.na(presencia$coordinate_precision), ]$latitude, col="red", cex=1.2)
#quitamos los casos (EN ESTA OCASIÓN LO DEJAMOS COMENTADO, NO LO NECESITAMOS)
#presencia<-presencia[presencia$coordinate_precision <= 20000, ]

#SEPARACIÓN DE LAS PRESENCIAS POR UNA DISTANCIA MÍNIMA
#-----------------------------------------------------
#Este tratamiento se hace para reducir la autocorrelación espacial de la muestra, que infla los valores de AUC al evaluar el modelo

#veamos la resolución de las variables
xres(variables)
yres(variables)
res.grados<-xres(variables)

#número de celdas vacías que vamos a dejar entre un punto y el siguiente
celdas.vacias<-1
#distancia mínima entre puntos consecutivos
distancia.minima<-res.grados*celdas.vacias

#¿cuanto es eso en km?
distancia.minima*111.19

#COMENZAMOS A ELIMINAR REGISTROS
#contador, empieza en la primera fila
fila<-1

#repite la operación hasta que se cumple la condición de salida
repeat{
  
  #contenido de la fila (para no tirar de toda la tabla en todas las operaciones)
  f<-presencia[fila, ]
  
  #genera los límites de la cuadrícula de búsqueda
  ymax<-f$latitude + distancia.minima
  ymin<-f$latitude - distancia.minima
  xmax<-f$longitude + distancia.minima
  xmin<-f$longitude - distancia.minima
  
  #selecciona de la tabla los datos con coordenadas dentro del rectángulo que no tienen las mismas coordenadas que la fila con la que estamos trabajando, y las elimina de la tabla
  presencia<-presencia[!((presencia$latitude <= ymax) & (presencia$latitude >= ymin) & (presencia$longitude <= xmax) & (presencia$longitude >= xmin) & (presencia$latitude != f$latitude | presencia$longitude != f$longitude)), ]
  
  #estima de filas por procesar
  print(paste("Processed rows: ", fila, " out of ", nrow(presencia), sep=""))
  
  #suma 1 al contador de la fila
  fila<-fila+1
  
  #condición de salida cuando llega a la última fila
  if(fila>=nrow(presencia))break
}

#LIMPIEZA DE DUPLICADOS EN LAS COORDENADAS
#-----------------------------------------
#buscamos registros duplicados en las coordenadas
duplicados<-duplicated(presencia[ , c("latitude", "longitude")])
#¿cuantos duplicados hay?
length(duplicados[duplicados==TRUE])
#selecciona los no duplicados (el símbolo ! significa "todo menos los duplicados")
presencia<-presencia[!duplicados, ]
#comprobamos que no han quedado duplicados (solo para cultivar vuestra fé)
duplicados<-duplicated(presencia[ , c("latitude", "longitude")])
length(duplicados[duplicados==TRUE])

#guardando a png
png("./resultados/presencias_final.png", width=1200, height=900, pointsize=30)
plot(variables[[1]], main="Presencias final")
points(presencia$longitude,presencia$latitude, cex=0.2)
dev.off()

#cuantas presencias tenemos
nrow(presencia)

#LLEGADO ESTE PUNTO, YA TENEMOS LAS PRESENCIAS PARA TRABAJAR CON MAXENT, LAS GUARDAMOS

#crea presencia maxent
presencia.maxent<-data.frame(presencia$specific_epithet, presencia$longitude, presencia$latitude)

#le pone nombres correctos de las columnas
names(presencia.maxent)<-c("Species","Lon","Lat")

#escribe presencia maxent
write.table(presencia.maxent, file="./resultados/presencia_maxent.csv", sep=",", row.names=FALSE, quote=FALSE)

#TERMINAMOS DE PREPARAR LA TABLA DE PRESENCIA, ELIMINANDO ALGUNAS COLUMNAS QUE NO SIRVEN, Y AÑADIMOS LA COLUMNA DE PRESENCIA CON UNOS.
#ESTO LO HACEMOS PARA QUE LA ESTRUCTURA DE LA TABLA DE PRESENCIA ENCAJE CON LA ESTRUCTURA DE LAS TABLAS DE BACKGROND, PSEUDOAUSENCIAS Y AUSENCIAS
#cambiamos latitude y longitude por y y x (más corto)
colnames(presencia)[5]<-"y"
colnames(presencia)[6]<-"x"
#eliminamos columnas que no necesitamos
presencia$specific_epithet<-NULL
presencia$basis_of_record<-NULL
presencia$species_id<-NULL
presencia$country_code<-NULL
presencia$year<-NULL
presencia$coordinate_precision<-NULL
presencia$presencia<-1


#guardamos también la tabla original de presencia
write.table(presencia, file="./resultados/presencia_limpia.csv", sep=",", row.names=FALSE, quote=FALSE)

#recuperamos algo de memoria RAM borrando objetos que no vamos a usar mas
rm(f, presencias.sobre.variables, celdas.vacias, distancia.minima, duplicados, fila, res.grados, xmax, xmin, ymax, ymin)
gc()




########################
########################
#PREPARACIÓN DE AUSENCIA
########################
########################

#####################################################
#GENERA LAS BACKGROUND, PSEUDO-AUSENCIA Y "AUSENCIAS"
#####################################################
help(randomPoints)

#GENERAMOS LOS PUNTOS DE BACKGROUND
###################################
background <- randomPoints(mask=variables, n=20000)
str(background)
#no es un data.frame, lo transformamos en data.frame
background<-data.frame(background)
#extraemos los valores de las variables sobre los puntos, convirtiendo a data.frame
background.variables<-data.frame(extract(variables, background))
#unimos las coordenadas con los valores de las variables
background<-cbind(background, background.variables)
#le añadimos la columna de presencia
background$presencia<-0
#unimos las presencias y el background en una única tabla
presencia.background<-rbind(presencia, background)
#guardamos la tabla
write.table(presencia.background, "./resultados/presencia_background.csv", row.names=FALSE, col.names=TRUE, quote=FALSE, sep=",")
#borramos objetos que no necesitamos
rm(background, background.variables)
gc()
#RECUERDA EL NOMBRE DE LA TABLA: presencia.background!!!

#guardando a png
png("./resultados/presencias_background.png", width=1200, height=900, pointsize=30)
plot(variables[[1]], main="Background")
points(presencia.background[presencia.background$presencia==1, ]$x, presencia.background[presencia.background$presencia==1, ]$y, cex=0.3)
points(presencia.background[presencia.background$presencia==0, ]$x, presencia.background[presencia.background$presencia==0, ]$y, pch=20, cex=0.01, col="gray40")
dev.off()


#GENERAMOS LAS PSEUDOAUSENCIAS
##############################
#usamos presencia.completa en lugar de presencia porque presencia.completa tiene todas las presencias posibles.
#sin embargo, queremos tantas pseudo-ausencias como presencias hay en presencia, de ahí (n=nrow(presencia))
pseudoausencia <- randomPoints(mask=variables, n=nrow(presencia), p=presencia.completa[ , c("latitude","longitude")], excludep=TRUE)
str(pseudoausencia)
#no es un data.frame, lo transformamos en data.frame
pseudoausencia<-data.frame(pseudoausencia)
#extraemos los valores de las variables sobre los puntos, convirtiendo a data.frame
pseudoausencia.variables<-data.frame(extract(variables, pseudoausencia))
#unimos las coordenadas con los valores de las variables
pseudoausencia<-cbind(pseudoausencia, pseudoausencia.variables)
#le añadimos la columna de presencia
pseudoausencia$presencia<-0
#unimos las presencias y el background en una única tabla
presencia.pseudoausencia<-rbind(presencia, pseudoausencia)
#guardamos la tabla
write.table(presencia.pseudoausencia, "./resultados/presencia_pseudoausencia.csv", row.names=FALSE, col.names=TRUE, quote=FALSE, sep=",")
#borramos objetos que no necesitamos
rm(pseudoausencia, pseudoausencia.variables)
gc()
#RECUERDA EL NOMBRE DE LA TABLA: presencia.background!!!

#guardando a png
png("./resultados/presencias_pseudoausencia.png", width=1200, height=900, pointsize=30)
plot(variables[[1]], main="Pseudoausencia")
points(presencia.pseudoausencia[presencia.pseudoausencia$presencia==1, ]$x, presencia.pseudoausencia[presencia.pseudoausencia$presencia==1, ]$y, cex=0.3)
points(presencia.pseudoausencia[presencia.pseudoausencia$presencia==0, ]$x, presencia.pseudoausencia[presencia.pseudoausencia$presencia==0, ]$y, pch=20, cex=0.3, col="gray40")
dev.off()



#GENERAMOS LAS AUSENCIAS
########################
#NO TENEMOS AUSENCIAS REALES (EN GBIF NO SUELE HABERLAS), PERO VAMOS A SIMULAR UN CONJUNTO DE AUSENCIAS, COMO SI HUBIÉRAMOS REALIZADO MUESTREOS ALREDEDOR DE LAS ZONAS DE PRESENCIA

#DEFINIMOS UN RADIO MÁXIMO ALREDEDOR DE LAS PRESENCIAS CONOCIDAS (TOMANDO presencia.completa COMO REFERENCIA)
radio=100000 #en metros, 100km

#convertimos las coordenadas de presencia a un objeto espacial (tipo sp)
presencia.sp<-presencia[ , c("x", "y")]
coordinates(presencia.sp)<-c("x", "y")

#creamos buffer alrededor de los puntos a 50 km
help(circles)
buffer<-circles(presencia.sp, d=radio, lonlat=TRUE)
plot(buffer)

#hacemos un dissolve sobre los polígonos para unir los que intersectan
buffer.dissolve<-gUnaryUnion(buffer@polygons)
plot(buffer.dissolve)

#extraemos los identificadores de las celdas de 'variables' que están dentro de los círculos
celdas<-unlist(cellFromPolygon(variables, p=buffer.dissolve))

#con estas 'celdas' creamos una máscara para restringir el área sobre la que vamos a crear presencias
#creamos la máscara 1 como un mapa en blanco
mascara.temp<-raster(variables)

#creamos una lista de tantos valores nulos (NaN) como celdas tiene la máscara
valores<-rep(NaN, ncell(mascara.temp))

#le damos valor uno a los valores que tienen el mismo índice que las celdas que están dentro del buffer
valores[celdas]<-1

#le damos a la máscara con los valores 1 y NaN donde corresponde
help(setValues)
mascara.temp<-setValues(mascara.temp, values=valores)
plot(mascara.temp)

#multiplicamos mascara1 por la variable para intersectarlas, pero transformando los valores de la variable en 1 con (variable > 0) NADA INTUITIVO, LO SÉ...
mascara<-mascara.temp*(variables[[1]] > 0)
plot(mascara)

#generamos las ausencias (con n=nrow(presencia) generamos tantas ausencias como presencias hay)
ausencia <- randomPoints(mask=mascara, n=nrow(presencia), p=presencia.completa[ , c("latitude","longitude")], excludep=TRUE)
str(ausencia)
#no es un data.frame, lo transformamos en data.frame
ausencia<-data.frame(ausencia)
#extraemos los valores de las variables sobre los puntos, convirtiendo a data.frame
ausencia.variables<-data.frame(extract(variables, ausencia))
#unimos las coordenadas con los valores de las variables
ausencia<-cbind(ausencia, ausencia.variables)
#le añadimos la columna de presencia
ausencia$presencia<-0
#unimos las presencias y el background en una única tabla
presencia.ausencia<-rbind(presencia, ausencia)
#guardamos el fichero completo de presencia ausencia
write.table(presencia.ausencia, "./resultados/presencia_ausencia.csv", row.names=FALSE, col.names=TRUE, quote=FALSE, sep=",")

#borramos objetos que no necesitamos
rm(ausencia, ausencia.variables, buffer, buffer.dissolve, celdas, mascara, mascara.temp, radio, valores, presencia.sp)
gc()


#SELECCIONAMOS PUNTOS DE EVALUACIÓN
#PARA TENER UN MARCO COMPARATIVO COMÚN, SIEMPRE VAMOS A EVALUAR LOS MODELOS CON LAS MISMAS AUSENCIAS
#VAMOS A SELECCIONAR n AUSENCIAS Y n PRESENCIAS DE LA TABLA presencia.ausencia

#PORCENTAJE DE PRESENCIAS A UTILIZAR
porcentaje<-40
#calcula numero de datos y porcentaje de particion
numero.presencias.total<-nrow(presencia)
numero.ausencias.total<-numero.presencias.total
numero.presencias.evaluacion<-round((porcentaje*numero.presencias.total)/100)
numero.ausencias.evaluacion<-numero.presencias.evaluacion

#SELECCIONAMOS AL AZAR LOS ÍNDICES PARA LAS PRESENCIAS Y LAS AUSENCIAS
help(sample)
muestra.presencia<-sample(numero.presencias.total, numero.presencias.evaluacion)
muestra.presencia #son índices de casos de presencia
#a los índices de las ausencias le sumamos el número total de presencias (para que no se solapen)
muestra.ausencia<-sample(numero.ausencias.total, numero.ausencias.evaluacion)+numero.presencias.total
muestra.ausencia

#GENERAMOS FICHERO DE EVALUACIÓN, solo necesita x, y, y presencia
presencia.ausencia.evaluacion<-presencia.ausencia[c(muestra.presencia, muestra.ausencia), c("x","y","presencia")]

#guardamos las presencias y ausencias de evaluación
write.table(presencia.ausencia.evaluacion, "./resultados/presencia_ausencia_evaluacion.csv", row.names=FALSE, col.names=TRUE, quote=FALSE, sep=",")

#lo guardamos también para maxent (solo las presencias con las columnas Species, Long, Lat)
presencia.ausencia.evaluacion.maxent<-presencia.ausencia.evaluacion[presencia.ausencia.evaluacion$presencia==1, ]
#creamos la columna con el nombre de la especie repitiendo el nombre de la especie tantas veces como filas hay en la tabla que queremos guardar
columna.especie<-rep(unique(presencia.completa$specific_epithet)[1], nrow(presencia.ausencia.evaluacion.maxent))
#unimos las columnas
presencia.ausencia.evaluacion.maxent<-cbind(columna.especie, presencia.ausencia.evaluacion.maxent[ , c("x", "y")])
#le pone nombres correctos de las columnas
names(presencia.ausencia.evaluacion.maxent)<-c("Species","Lon","Lat")
#guardamos la tabla
write.table(presencia.ausencia.evaluacion.maxent, "./resultados/presencia_maxent_test.csv", row.names=FALSE, col.names=TRUE, quote=FALSE, sep=",")

#guardando a png
png("./resultados/presencias_evaluacion.png", width=1200, height=900, pointsize=30)
plot(variables[[1]], main="Presencias y ausencias de evaluación")
points(presencia.ausencia.evaluacion[presencia.ausencia.evaluacion$presencia==1, ]$x, presencia.ausencia.evaluacion[presencia.ausencia.evaluacion$presencia==1, ]$y, cex=0.3)
points(presencia.ausencia.evaluacion[presencia.ausencia.evaluacion$presencia==0, ]$x, presencia.ausencia.evaluacion[presencia.ausencia.evaluacion$presencia==0, ]$y, pch=20, cex=0.3, col="gray40")
dev.off()

#NOTA IMPORTANTE: LAS PRESENCIAS QUE HEMOS SELECCIONADO PARA EVALUACION, TENEMOS QUE QUITARLA TAMBIÉN DE LAS TABLAS presencia.background, presencia.pseudoausencia y presencia.ausencia. AL QUITARLAS, GENERAREMOS NUEVAS TABLAS QUE SE LLAMAŔAN presencia.background.entrenamiento, presencia.pseudoausencia.entrenamiento y presencia.ausencia.entrenamiento. SOLO USAREMOS ESTAS TABLAS CUANDO NECESITEMOS EVALUAR LOS MODELOS. LOS MODELOS FINALES SIEMPRE DEBEN HACERSE CON TODAS LAS PRESENCIAS!!

#generamos las tablas nuevas sin esas presencias
#OJO, ABSURDECES DE R AQUÍ: anteriormente para eliminar filas hemos usado !, pero esta vez usamos -. Porque resulta que ! es para índices lógicos (como en los duplicados, o en las selecciones con condiciones) y - es para índices numéricos (como lo que haremos ahora con muestra.presencia). Explicación, aquí: http://rwiki.sciviews.org/doku.php?id=tips:data-frames:remove_rows_data_frame
presencia.maxent.entrenamiento<-presencia.maxent[-muestra.presencia, ]
presencia.entrenamiento<-presencia[-muestra.presencia, ]
presencia.background.entrenamiento<-presencia.background[-muestra.presencia, ]
presencia.pseudoausencia.entrenamiento<-presencia.pseudoausencia[-muestra.presencia, ]
presencia.ausencia.entrenamiento<-presencia.ausencia[-c(muestra.presencia, muestra.ausencia), ]

#guardamos las tablas
write.table(presencia.maxent.entrenamiento, "./resultados/presencia_maxent_entrenamiento.csv", row.names=FALSE, col.names=TRUE, quote=FALSE, sep=",")
write.table(presencia.entrenamiento, "./resultados/presencia_entrenamiento.csv", row.names=FALSE, col.names=TRUE, quote=FALSE, sep=",")
write.table(presencia.background.entrenamiento, "./resultados/presencia_background_entrenamiento.csv", row.names=FALSE, col.names=TRUE, quote=FALSE, sep=",")
write.table(presencia.pseudoausencia.entrenamiento, "./resultados/presencia_pseudoausencia_entrenamiento.csv", row.names=FALSE, col.names=TRUE, quote=FALSE, sep=",")
write.table(presencia.ausencia.entrenamiento, "./resultados/presencia_ausencia_entrenamiento.csv", row.names=FALSE, col.names=TRUE, quote=FALSE, sep=",")

#BORRAMOS TODO LO QUE NO NECESITAMOS
rm(muestra.ausencia, muestra.presencia, numero.ausencias.evaluacion, numero.ausencias.total, numero.presencias.evaluacion, numero.presencias.total, porcentaje)

#GUARDAMOS TODOS LOS OBJETOS IMPORTANTES EN UN FICHERO DE R PARA CARGARLO DESDE AHÍ A LA SIGUIENTE SESIÓN
save(presencia, presencia.entrenamiento, presencia.ausencia, presencia.ausencia.entrenamiento, presencia.ausencia.evaluacion, presencia.background, presencia.background.entrenamiento, presencia.pseudoausencia, presencia.pseudoausencia.entrenamiento, variables, file="./resultados/1_prepara_presencia.Rdata")
