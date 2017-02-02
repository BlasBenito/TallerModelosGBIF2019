#http://cran.r-project.org/src/contrib/Archive/colorout/colorout_0.9-9.tar.gz
#install.packages("colorout_0.9-9.tar.gz", repos=NULL, type="source")
#library(colorout)

#DIRECTORIO DE TRABAJO
setwd("/home/blas/Dropbox/DOCENCIA/CURSOS_MDE_GBIF/2014/sesiones/sesion_2/taller2")

#CREA EL DIRECTORIO PARA GUARDAR LOS RESULTADOS DE LAS PROYECCIONES
dir.create("./resultados/proyecciones")


#CARGA LAS LIBRERIAS NECESARIAS (EJECUTAR SIEMPRE QUE TRABAJES CON EL SCRIPT)
library(dismo) #LIBRERIA PARA MODELOS DE DISTRIBUCION
library(mgcv) #OTRA LIBRERIA PARA GAM
library(kernlab) #SUPPORT VECTOR MACHINES
library(randomForest) #RANDOM FOREST

#CARGA LAS PRESENCIAS Y LAS VARIABLES
load("./resultados/1_prepara_presencia.Rdata")

#VARIABLES
variables.eu<-variables

#borramos tablas que no vamos a utilizar
rm(presencia.ausencia.entrenamiento,presencia.ausencia.evaluacion,  presencia.entrenamiento, presencia.pseudoausencia.entrenamiento, presencia.background.entrenamiento, variables)
gc()


######################################################
#IMPORTAMOS LAS VARIABLES DE PROYECCIÒN A NORTEAMÉRICA
######################################################
#descomprimimos las variables
unzip("./data/3_variables_norteamerica.zip", exdir="./resultados/variables_norteamerica", junkpaths=TRUE)

#LISTADO DE VARIABLES
lista.variables <- list.files(path="./resultados/variables_norteamerica",pattern='*.asc', full.names=TRUE)
#brick
variables.na <- brick(stack(lista.variables))
#comprobamos los nombres de las variables
names(variables.na)
#son muchas variables, nos quedamos con las las mismas que hay en variables.eu
variables.na<-variables.na[[names(variables.eu)]]
#plot
png("./resultados/proyecciones/variables_na.png", width=3000, height=2000, pointsize=50)
par(oma=c(3,3,4,3))
plot(variables.na)
dev.off()



#VAMOS A EXAMINAR LA SIMILITUD EN EL ESPACIO ECOLÓGICO ENTRE LAS LOCALIDADES DE PRESENCIA EN eu Y LAS CONDICIONES DE NORTEAMÉRICA
#veamos las diferencias con boxplot
variables.eu.df<-na.omit(as.data.frame(variables.eu))
variables.na.df<-na.omit(as.data.frame(variables.na))
#boxplot
par(mfrow=c(2,4))
boxplot(variables.eu.df$bio5,variables.na.df$bio5, names=c("eu", "na"), notch=TRUE, main="bio5")
boxplot(variables.eu.df$bio14,variables.na.df$bio14, names=c("eu", "na"), notch=TRUE, main="bio14")
boxplot(variables.eu.df$human_footprint,variables.na.df$human_footprint, names=c("eu", "na"), notch=TRUE, main="human_footprint")
boxplot(variables.eu.df$landcover_veg_tree,variables.na.df$landcover_veg_tree, names=c("eu", "na"), notch=TRUE, main="landcover_veg_tree")
boxplot(variables.eu.df$ndvi_average,variables.na.df$ndvi_average, names=c("eu", "na"), notch=TRUE, main="ndvi_average")
boxplot(variables.eu.df$ndvi_range,variables.na.df$ndvi_range, names=c("eu", "na"), notch=TRUE, main="ndvi_range")
boxplot(variables.eu.df$diversidad_topo,variables.na.df$diversidad_topo, names=c("eu", "na"), notch=TRUE, main="diversidad_topo")


#ANÁLISIS MESS
#necesita registros de presencia con los valores de las variables, y un brick con las variables de la región de destino
mess.na<-mess(x=variables.na, v=presencia[, names(variables.eu)], full=TRUE)
dev.off()
plot(mess.na)
#no vienen los nombres en el gráfico, pero son estos
names(mess.na)<-c(names(variables.na), "mess")
#plot
png("./resultados/proyecciones/mess.png", width=3000, height=2000, pointsize=50)
par(oma=c(3,3,4,3))
plot(mess.na)
dev.off()


#¿DONDE ESTÁ EL DESVÍO MÁXIMO DE CADA VARIABLE?
dev.off()
mess.na.max<-which.max(mess.na) #no funciona según la versión de R y librería Raster
plot(mess.na.max)
#le ponemos una paleta de color más intuitiva
library(RColorBrewer)
#creamos una paleta con 8 colores
paleta.color<-brewer.pal(n=8, name="Set1")
#plot con leyenda
png("./resultados/proyecciones/mess_limitante.png", width=1800, height=1000, pointsize=20)
plot(mess.na.max, col=paleta.color, legend=FALSE)
legend("bottomleft", c(names(variables.na)), col=paleta.color, lwd=10, cex=1.6)
dev.off()


#borra objetos que no vamos a necesitar
rm(mess.na, mess.na.max, paleta.color, variables.eu.df, variables.na.df)
gc()

#AHORA QUE SABEMOS UN POCO COMO ESTÁN DISTRIBUÍDAS LAS VARIABLES EN USA, HACEMOS LOS MODELOS


###################
#CALIBRAMOS MODELOS
###################
#PARA VER QUÉ MODELOS FUNCIONAN MEJOR PROYECTANDO, VOLVEMOS A HACER MODELOS CON AUSENCIAS, PSEUDOAUSENCIAS Y BACKGROUND. PRIMERO LOS CALIBRAMOS CON LAS VARIABLES Y PRESENCIAS EN EUROPA
#USAREMOS PARA CALIBRAR LAS TABLAS QUE CONTIENEN TODOS LOS PUNTOS, ES DECIR, SON MODELOS CALIBRADOS CON TODOS LOS DATOS DISPONIBLES

#PREPARAMOS LAS FÓRMULAS
#----------------------
formula.regresion<-as.formula(paste("presencia ~ ", paste(names(variables.eu), collapse="+"), collapse=""))

formula.regresion.poly<-as.formula(paste("presencia ~ poly(", paste(names(variables.eu), collapse=", 2) + poly("), ", 2)", collapse=""))

formula.gam<-as.formula(paste("presencia ~ s(", paste(names(variables.eu), collapse=") + s("), ")", collapse=""))


#CALCULAMOS PESOS PARA LOS BACKGROUND PONDERADOS DE GLM Y GAM
#------------------------------------------------------------
n.presencias<-nrow(presencia.background[presencia.background$presencia==1, ])
n.background<-nrow(presencia.background[presencia.background$presencia==0, ])
pesos<-c(rep(1/n.presencias, n.presencias), rep(1/n.background, n.background))


#CALIBRAMOS MODELOS
#-------------------
#GLM
m.glm.backgroundw<-glm(formula.regresion.poly, family=quasibinomial(link=logit), data=presencia.background, weights=pesos)
#GAM
m.gam.backgroundw<-bam(formula.gam, family=quasibinomial(link=logit), data=presencia.background, weights=pesos)
#RANDOM FOREST
m.rf.pseudoausencia<-randomForest(formula.regresion, data=presencia.pseudoausencia, ntrees=5000, mtry=3, nodesize=10)
#SVM
m.svm.pseudoausencia<-ksvm(formula.regresion, data=presencia.pseudoausencia, kernel="laplacedot", kpar=list(sigma=1))

#############################################
#PROYECTAMOS SOBRE LAS VARIABLES DE na
#############################################
m.glm.backgroundw.map.na<-predict(variables.na, m.glm.backgroundw, type="response")
m.gam.backgroundw.map.na<-predict(variables.na, m.gam.backgroundw, type="response")
m.rf.pseudoausencia.map.na<-predict(variables.na, m.rf.pseudoausencia, type="response")
m.svm.pseudoausencia.map.na<-predict(variables.na, m.svm.pseudoausencia)
#ensamblamos con un promedio
m.ensamblado.map.na<-(m.glm.backgroundw.map.na+m.gam.backgroundw.map.na+m.rf.pseudoausencia.map.na+m.svm.pseudoausencia.map.na)/4

#los metemos en un brick
modelos.na<-brick(m.glm.backgroundw.map.na, m.gam.backgroundw.map.na, m.rf.pseudoausencia.map.na, m.svm.pseudoausencia.map.na, m.ensamblado.map.na)

#le ponemos los nombres
names(modelos.na)<-c("glm background ponderado", "gam background ponderado", "rf pseudoausencia", "svm pseudoausencia", "ensamblado")
#plot
plot(modelos.na)

#borramos los mapas para hacer espacio
rm(m.glm.backgroundw.map.na, m.gam.backgroundw.map.na, m.rf.pseudoausencia.map.na, m.svm.pseudoausencia.map.na, m.ensamblado.map.na, m.glm.backgroundw, m.gam.backgroundw, m.rf.pseudoausencia, m.svm.pseudoausencia)
gc()


##############################
#EVALUACIÓN DE LA PROYECCIÓN
##############################

#IMPORTAMOS LOS PUNTOS DE PRESENCIA PARA VERLOS SOBRE EL MODELO (TENEMOS LOS PUNTOS DE NORTEAMÉRICAEN EL FICHERO DE PRESENCIAS ORIGINAL)
#lo descomprimimos
unzip("./data/2_presencia_Ursus_arctos.zip", exdir="./resultados/presencia")
#lo importamos
presencia.na<-read.table("./resultados/presencia/occurrence.txt",header=T, sep='\t', fill=TRUE, check.names=TRUE, stringsAsFactors=FALSE)
str(presencia.na)

#LIMPIAMOS LA TABLA (MISMOS PASOS QUE EN 1_prepara_presencias.R)
#QUITAMOS TODOS LOS REGISTROS FUERA DEL ÁREA DE TRABAJO
presencias.sobre.variables<-data.frame(extract(x=variables.na, y=presencia.na[ , c("longitude","latitude")]))
presencia.na<-cbind(presencia.na, presencias.sobre.variables)
presencia.na<-presencia.na[!(is.na(presencia.na[ , names(variables.na)])), ]

#QUITAMOS LOS REGISTROS FÓSILES
presencia.na<-presencia.na[presencia.na$basis_of_record != "FOSSIL_SPECIMEN", ]

#QUITAMOS REGISTROS ANTIGUOS
presencia.na<-presencia.na[!is.na(presencia.na$year), ]
presencia.na<-presencia.na[presencia.na$year >= 1970, ]

#ELIMINAMOS REGISTROS DE BAJA RESOLUCIÓN (PUES NO, CASI NINGUNO TIENE LA RESOLUCIÓN DE LAS COORDENADAS)
#presencia.na$coordinate_precision<-as.numeric(presencia.na$coordinate_precision)
#presencia.na<-presencia.na[presencia.na$coordinate_precision <= 20000, ]

#LIMPIEZA DE DUPLICADOS EN LAS COORDENADAS
duplicados<-duplicated(presencia.na[ , c("latitude", "longitude")])
length(duplicados[duplicados==TRUE])
presencia.na<-presencia.na[!duplicados, ]

#NOS QUEDAMOS SOLO CON LAS COORDENADAS
presencia.na<-presencia.na[ , c("longitude","latitude")]
names(presencia.na)<-c("x", "y")

#PLOTEAMOS LAS PRESENCIAS SOBRE LOS MAPAS
#función para meter en el plot(brick)
#http://rstudio-pubs-static.s3.amazonaws.com/5476_5fa4cefefbaa476c98db31ea7f7089b8.html
plotea.puntos <- function() {
  points(presencia.na, cex=0.1)
}
#plot
plot(modelos.na, addfun=plotea.puntos, maxnl=18)


#EXTRAEMOS LOS VALORES DE LAS PRESENCIAS DE LOS MODELOS
#-------------------------------------------------------
presencia.na.valores<-data.frame(extract(modelos.na, presencia.na))


#GENERAMOS AUSENCIAS Y EXTRAEMOS SUS VALORES SOBRE LOS MODELOS
#-------------------------------------------------------------
#generamos la máscara (código comentado en 1_prepara_presencia.R)
library(rgeos)
radio=100000
presencia.sp<-presencia.na[ , c("x", "y")]
coordinates(presencia.sp)<-c("x", "y")
buffer<-circles(presencia.sp, d=radio, lonlat=TRUE)
buffer.dissolve<-gUnaryUnion(buffer@polygons)
celdas<-unlist(cellFromPolygon(variables.na, p=buffer.dissolve))
mascara.temp<-raster(variables.na)
valores<-rep(NaN, ncell(mascara.temp))
valores[celdas]<-1
mascara.temp<-setValues(mascara.temp, values=valores)
mascara<-mascara.temp*(variables.na[[1]] > 0)
#generamos las ausencias (con n=nrow(presencia) generamos tantas ausencias como presencias hay)
ausencia.na <- data.frame(randomPoints(mask=mascara, n=nrow(presencia.na), p=presencia.na, excludep=TRUE))
#extraemos los valores de los modelos
ausencia.na.valores<-data.frame(extract(modelos.na, ausencia.na))


#EVALUAMOS LOS MODELOS
#---------------------
#creamos una tabla para guardar los resultados
resultados.evaluacion.proyecciones<-data.frame(modelo=character(), auc=numeric(), cor=numeric(), stringsAsFactors=FALSE)

#contador de filas
fila=0

#EMPIEZA A EJECUTAR EL LOOP AQUÍ
#abrimos un pdf para guardar los gráficos
pdf("./resultados/proyecciones/evaluacion_modelos.pdf", width=15, height=8, pointsize=30)

#iteramos por cada uno de los modelos
for (modelo.na in names(modelos.na)){
  
  #evaluamos el modelo
  evaluacion<-evaluate(p=presencia.na.valores[, modelo.na], a=ausencia.na.valores[, modelo.na])
  
  #suma 1 a la fila de la tabla de resultados
  fila=fila+1
  
  #llenamos la tabla
  resultados.evaluacion.proyecciones[fila, "modelo"]<-modelo.na
  resultados.evaluacion.proyecciones[fila, "auc"]<-evaluacion@auc
  resultados.evaluacion.proyecciones[fila, "cor"]<-evaluacion@cor
  
  #dibujamos el gráfico
  #plots
  par(mfrow=c(1,3), mar=c(2,2,4,2), oma=c(3,3,5,3))
  density(evaluacion)
  boxplot(evaluacion, col=c("blue", "red"))
  plot(evaluacion, "ROC")
  mtext(modelo.na, outer=TRUE, cex=1.3)
  
}

dev.off()
#TERMINA DE EJECUTAR EL LOOP AQUÍ

#ORDENAMOS LA TABLA RESULTANTE
resultados.evaluacion.proyecciones<-resultados.evaluacion.proyecciones[order(resultados.evaluacion.proyecciones$auc, decreasing=TRUE), ]
resultados.evaluacion.proyecciones #mejor lo vemos en el panel

#LA GUARDAMOS
write.table(resultados.evaluacion.proyecciones, "./resultados/proyecciones/evaluacion_proyecciones.csv", row.names=FALSE, col.names=TRUE, quote=FALSE, sep=",")

#PLOTEAMOS EL MEJOR MODELO
dev.off()
plot(modelos.na[["glm.background.ponderado"]], main="GLM weighted background")
points(presencia.na, cex=0.05)
