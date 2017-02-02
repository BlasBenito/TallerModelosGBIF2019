###################################
#ESTABLECE EL DIRECTORIO DE TRABAJO
###################################
#DIRECTORIO DE TRABAJO
setwd("C:/taller2")
setwd("/home/blas/Dropbox/DOCENCIA/CURSOS_MDE_GBIF/2015/sesiones/taller2")

#################################################################
#PARA GUARDAR O CARGAR EL ESPACIO DE TRABAJO CUANDO SEA NECESARIO
#################################################################

#CARGA LAS PRESENCIAS Y LAS VARIABLES
#load("./resultados/2_modelos.Rdata")
#save("./resultados/2_modelos.Rdata")


################################
#INSTALACION Y CARGA DE PAQUETES
################################
#NOTA: SOLO ES NECESARIO INSTALARLOS UNA VEZ. DESACTIVA ESTAS LÍNEAS PARA LA PRÓXIMA SESIÓN
#INSTALA PAQUETE DISMO Y TODAS SUS DEPENDENCIAS (EJECUTAR UNA SOLA VEZ)

install.packages("tree", dep=TRUE)
install.packages("
", dep=TRUE)
install.packages("party", dep=TRUE)
install.packages("mgcv", dep=TRUE)


#CARGA LAS LIBRERIAS NECESARIAS (EJECUTAR SIEMPRE QUE TRABAJES CON EL SCRIPT)
library(dismo) #LIBRERIA PARA MODELOS DE DISTRIBUCION
library(plotmo) #LIBRERIA PARA VISUALIZACION DE CURVAS DE RESPUESTA
library(mgcv) #OTRA LIBRERIA PARA GAM
library(randomForest) #RANDOM FOREST

#CARGA FUNCIONES
source("funcionesSDM_taller2.R")

#CARGA LAS PRESENCIAS Y LAS VARIABLES
unzip("./data/variables_y_presencia.zip", exdir="./data/")
load("./data/variables_y_presencia/variables_y_presencia.Rdata")

#CREA CARPETA PARA GUARDAR LOS MODELOS
dir.create("./resultados")
dir.create("./resultados/modelos")
dir.create("./resultados/proyecciones")

#CREAMOS CARPETAS PARA TRABAJAR LUEGO CON MAXENT
dir.create("./resultados/maxent")
dir.create("./resultados/maxent/ejecucion1")
dir.create("./resultados/maxent/ejecucion2")
dir.create("./resultados/maxent/ejecucion3")
dir.create("./resultados/maxent/ejecucion4")
dir.create("./resultados/maxent/evaluacion1")
dir.create("./resultados/maxent/evaluacion2")
dir.create("./resultados/maxent/evaluacion3")
dir.create("./resultados/maxent/evaluacion4")
dir.create("./resultados/maxent/threshold1")
dir.create("./resultados/maxent/proyeccion_espacio")
dir.create("./resultados/maxent/proyeccion_tiempo")

################################################
#EXPLORANDO LOS DATOS PARA UNA VARIABLE AMBIENTAL
################################################

#guardamos todo en un solo pdf
pdf("./resultados/analisis_exploratorio1.pdf", width=15, height=6, pointsize=30)
#itera por las variables
for (variable in names(variables)){

par(mfrow=c(1,3))

#límites de la variable en los gráficos
variable.min<-min(presencia.background[, variable])
variable.max<-max(presencia.background[, variable])

#density plot
#------------
#presencias vs. background
d0<-density(presencia.background[presencia.background$presencia==0, variable], from=variable.min, to=variable.max)
d1<-density(presencia.background[presencia.background$presencia==1, variable], from=variable.min, to=variable.max)
plot(d0, col="red", xlim=c(variable.min,variable.max), lwd=3, main=paste(variable, " vs. background", sep=""), xlab=variable, ylim=c(0, max(d0$y, d1$y)))
lines(d1, col="forestgreen", lwd=3)

#presencias vs ausencias
d0<-density(presencia.ausencia[presencia.ausencia$presencia==0, variable], from=variable.min, to=variable.max)
d1<-density(presencia.ausencia[presencia.ausencia$presencia==1, variable], from=variable.min, to=variable.max)
plot(d0, col="red", xlim=c(variable.min,variable.max), lwd=3, main=paste(variable, " vs. ausencias", sep=""), xlab=variable, ylim=c(0, max(d0$y, d1$y)))
lines(d1, col="forestgreen", lwd=3)

#presencias vs pseudoausencias
d0<-density(presencia.pseudoausencia[presencia.pseudoausencia$presencia==0, variable], from=variable.min, to=variable.max)
d1<-density(presencia.pseudoausencia[presencia.pseudoausencia$presencia==1, variable], from=variable.min, to=variable.max)
plot(d0, col="red", xlim=c(variable.min,variable.max), lwd=3, main=paste(variable, " vs. pseudoausencias", sep=""), xlab=variable, ylim=c(0, max(d0$y, d1$y)))
lines(d1, col="forestgreen", lwd=3)
}
dev.off()


#################################################################
#################################################################
#CALIBRANDO MODELOS
#################################################################
#################################################################

#NOTA IMPORTANTE, VAMOS A CALIBRAR TODOS LOS MODELOS CON LAS TABLAS presencia.*.entrenamiento, PORQUE LOS EVALUAREMOS TODOS JUNTOS AL FINAL PARA COMPARARLOS
 
################################
#GENERALIZED LINEAR MODELS (GLM)
################################
#AYUDA GLM
help(glm)

#EMPEZAMOS POCO A POCO, SOLO CON DOS VARIABLES
#-----------------------
m.glm.temp<-glm(presencia ~ bio5 + ndvi_range, family=binomial(link=logit), data=presencia.pseudoausencia.entrenamiento)

#vemos el resumen del resultado
summary(m.glm.temp)

#devianza explicada
Dsquared(m.glm.temp)

#curva de respuesta
plotmo(m.glm.temp, level=0.68, all2=TRUE)

#predecimos a un mapa
m.glm.temp.mapa<-predict(variables, m.glm.temp, type="response")

#plot
plot(m.glm.temp.mapa)
points(presencia[, c("x","y")], cex=0.1)


#AHORA CON DOS VARIABLES QUE INTERACCIONAN
#-----------------------------------------
m.glm.temp<-glm(presencia ~ bio5 * ndvi_range, family=binomial(link=logit), data=presencia.pseudoausencia.entrenamiento)

#vemos el resumen del resultado
summary(m.glm.temp)

#devianza explicada
Dsquared(m.glm.temp)

#curva de respuesta
plotmo(m.glm.temp, level=0.68, all2=TRUE)

#predecimos a un mapa
m.glm.temp.mapa<-predict(variables, m.glm.temp, type="response")

#plot
plot(m.glm.temp.mapa)
points(presencia[, c("x","y")], cex=0.1)


#AHORA CON DOS VARIABLES CON TRANSFORMACIÓN POLINOMIAL QUE INTERACCIONAN (grado 2)
#----------------------------------------------------
m.glm.temp<-glm(presencia ~ poly(bio5, 2) * poly(ndvi_range, 2), family=binomial(link=logit), data=presencia.pseudoausencia.entrenamiento)

#vemos el resumen del resultado
summary(m.glm.temp)

#devianza explicada
Dsquared(m.glm.temp)

#curva de respuesta
plotmo(m.glm.temp, level=0.68, all2=TRUE)

#predecimos a un mapa
m.glm.temp.mapa<-predict(variables, m.glm.temp, type="response")

#plot
plot(m.glm.temp.mapa)
points(presencia[, c("x","y")], cex=0.1)


#AHORA CON DOS VARIABLES CON TRANSFORMACIÓN POLINOMIAL (grado 4)
#----------------------------------------------------
m.glm.temp<-glm(presencia ~ poly(bio5, 4) * poly(ndvi_range, 4), family=binomial(link=logit), data=presencia.pseudoausencia.entrenamiento)

#vemos el resumen del resultado
summary(m.glm.temp)

#devianza explicada
Dsquared(m.glm.temp)

#curva de respuesta
plotmo(m.glm.temp, level=0.68)

#predecimos a un mapa
m.glm.temp.mapa<-predict(variables, m.glm.temp, type="response")

#plot
plot(m.glm.temp.mapa)
points(presencia[, c("x","y")], cex=0.1)



#VAMOS A TRABAJAR CON TODAS LAS VARIABLES
#----------------------------------------
#GENERA FÓRMULAS PARA LOS MODELOS DE REGRESIÓN
#formula de regresión polinomial de grado 2 sin interacciones
formula.regresion.poly<-as.formula(paste("presencia ~ poly(", paste(names(variables), collapse=", 2) + poly("), ", 2)", collapse=""))
formula.regresion.poly

#NOTA IMPORTANTE: VAMOS A CALIBRAR CADA MODELO CON AUSENCIAS, PSEUDO-AUSENCIAS Y BACKGROUND, PARA APRENDER COMO FUNCIONA CADA ALGORITMO CON ELLAS

#BACKGROUND
m.glm.background<-glm(formula.regresion.poly, family=binomial(link=logit), data=presencia.background.entrenamiento)

#BACKGROUND PONDERADO
pesos<-WeightPresenceBackground(presencia.background.entrenamiento[ , "presencia"])
#la función WeightPresenceBackground cuenta el número de presencias, el número de puntos de background, divide 1 por cada uno de ellos, y genera un vector con los valores resultantes
#para casos ponderados ponemos family=quasibinomial
m.glm.backgroundw<-glm(formula.regresion.poly, family=quasibinomial(link=logit), data=presencia.background.entrenamiento, weights=pesos)

#AUSENCIA
m.glm.ausencia<-glm(formula.regresion.poly, family=binomial(link=logit), data=presencia.ausencia.entrenamiento)

#PSEUDOAUSENCIA
m.glm.pseudoausencia<-glm(formula.regresion.poly, family=binomial(link=logit), data=presencia.pseudoausencia.entrenamiento)


#DIBUJAMOS LAS CURVAS DE RESPUESTA
help(plotmo)
#fíjate en las diferencias entre las curvas de respuesta de los modelos de background
plotmo(m.glm.background, level=0.68)
plotmo(m.glm.backgroundw, level=0.68)
plotmo(m.glm.ausencia, level=0.68)
plotmo(m.glm.pseudoausencia, level=0.68)

#resumen de los modelos
summary(m.glm.background)
summary(m.glm.backgroundw)
summary(m.glm.ausencia)
summary(m.glm.pseudoausencia)

#devianza explicada
Dsquared(m.glm.background)
Dsquared(m.glm.backgroundw)
Dsquared(m.glm.ausencia)
Dsquared(m.glm.pseudoausencia)

#PREDICCION GEOGRAFICA
m.glm.background.mapa<-predict(variables, m.glm.background, type="response")
m.glm.backgroundw.mapa<-predict(variables, m.glm.backgroundw, type="response")
m.glm.ausencia.mapa<-predict(variables, m.glm.ausencia, type="response")
m.glm.pseudoausencia.mapa<-predict(variables, m.glm.pseudoausencia, type="response")

#MAPAS
par(mfrow=c(2,2))
plot(m.glm.background.mapa, main="background")
plot(m.glm.backgroundw.mapa, main="background ponderado")
plot(m.glm.ausencia.mapa, main="absence")
plot(m.glm.pseudoausencia.mapa, main="pseudo-absence")

#guarda los modelos
writeRaster(m.glm.backgroundw.mapa, filename="./resultados/modelos/glm_background.asc", format="ascii", overwrite=TRUE)
writeRaster(m.glm.ausencia.mapa, filename="./resultados/modelos/glm_ausencia.asc", format="ascii", overwrite=TRUE)
writeRaster(m.glm.pseudoausencia.mapa, filename="./resultados/modelos/glm_pseudoausencia.asc", format="ascii", overwrite=TRUE)



#borra el mapa y el modelo de la memoria
rm(formula.regresion.poly, m.glm.ausencia, m.glm.ausencia.mapa, m.glm.background, m.glm.background.mapa, m.glm.backgroundw, m.glm.backgroundw.mapa, m.glm.interaccion, m.glm.pseudoausencia, m.glm.pseudoausencia.mapa, m.glm.seleccion, n.background, n.presencias, m.glm.temp.mapa, m.glm.temp, pesos)
gc()


#####################################
#MODELOS ADITIVOS GENERALIZADOS (GAM)
#####################################

#GENERA LA FÓRMULA PARA GAM
formula.gam<-as.formula(paste("presencia ~ s(", paste(names(variables), collapse=") + s("), ")", collapse=""))
formula.gam

#BACKGROUND
m.gam.background<-gam(formula.gam, family=binomial(link=logit), data=presencia.background.entrenamiento)

#BACKGROUND PONDERADO ojo a weights=pesos
pesos<-WeightPresenceBackground(presencia.background.entrenamiento[ , "presencia"])
m.gam.backgroundw<-gam(formula.gam, family=quasibinomial(link=logit), data=presencia.background.entrenamiento, weights=pesos)

#AUSENCIA
m.gam.ausencia<-gam(formula.gam, family=binomial(link=logit), data=presencia.ausencia.entrenamiento)

#PSEUDO AUSENCIA
m.gam.pseudoausencia<-gam(formula.gam, family=binomial(link=logit), data=presencia.pseudoausencia.entrenamiento)

#DIBUJAMOS LAS CURVAS DE RESPUESTA
plotmo(m.gam.background, level=0.68, type="response")
plotmo(m.gam.backgroundw, level=0.68, type="response")
plotmo(m.gam.ausencia, level=0.68, type="response")
plotmo(m.gam.pseudoausencia, level=0.68, type="response")
#¿CREES QUE TIENEN MÁS SENTIDO QUE LAS ANTERIORES?

#resumen de los modelos
summary(m.gam.background)
summary(m.gam.backgroundw)
summary(m.gam.ausencia)
summary(m.gam.pseudoausencia)

#devianza explicada
Dsquared(m.gam.background)
Dsquared(m.gam.backgroundw)
Dsquared(m.gam.ausencia)
Dsquared(m.gam.pseudoausencia)

#PREDICCION GEOGRAFICA
m.gam.background.mapa<-predict(variables, m.gam.background, type="response")
m.gam.backgroundw.mapa<-predict(variables, m.gam.backgroundw, type="response")
m.gam.ausencia.mapa<-predict(variables, m.gam.ausencia, type="response")
m.gam.pseudoausencia.mapa<-predict(variables, m.gam.pseudoausencia, type="response")

#MAPAS
par(mfrow=c(2,2))
plot(m.gam.background.mapa, main="background")
plot(m.gam.backgroundw.mapa, main="background ponderado")
plot(m.gam.ausencia.mapa, main="absence")
plot(m.gam.pseudoausencia.mapa, main="pseudo-absence")

#guarda los modelos
writeRaster(m.gam.backgroundw.mapa, filename="./resultados/modelos/gam_background.asc", format="ascii", overwrite=TRUE)
writeRaster(m.gam.ausencia.mapa, filename="./resultados/modelos/gam_ausencia.asc", format="ascii", overwrite=TRUE)
writeRaster(m.gam.pseudoausencia.mapa, filename="./resultados/modelos/gam_pseudoausencia.asc", format="ascii", overwrite=TRUE)

#borra el mapa y el modelo de la memoria
rm(m.gam.ausencia, m.gam.ausencia.mapa, m.gam.background, m.gam.background.mapa, m.gam.backgroundw, m.gam.backgroundw.mapa, m.gam.pseudoausencia, m.gam.pseudoausencia.mapa, pesos, formula.gam)
gc()



##############
#RANDOM FOREST
##############
#AYUDA DE randomForest
help(randomForest)

#PREPARAMOS LA FÓRMULA
formula.regresion<-as.formula(paste("presencia ~ ", paste(names(variables), collapse="+"), collapse=""))
formula.regresion

#BACKGROUND
m.rf.background<-randomForest(formula.regresion, data=presencia.background.entrenamiento, importance=TRUE, ntree=500, mtry=3, nodesize=10)

#AUSENCIA
m.rf.ausencia<-randomForest(formula.regresion, data=presencia.ausencia.entrenamiento, importance=TRUE, ntree=500, mtry=3, nodesize=10)

#PSEUDO-AUSENCIA
m.rf.pseudoausencia<-randomForest(formula.regresion, data=presencia.pseudoausencia.entrenamiento, importance=TRUE, ntree=500, mtry=3, nodesize=10)

#ERROR PLOT
par(mfrow=c(3, 1))
plot(m.rf.background)
plot(m.rf.ausencia)
plot(m.rf.pseudoausencia)

#DEVIANZA EXPLICADA
print(m.rf.background)
print(m.rf.ausencia)
print(m.rf.pseudoausencia)

#CURVAS DE RESPUESTA
plotmo(m.rf.background)
plotmo(m.rf.ausencia)
plotmo(m.rf.pseudoausencia)

#IMPORTANCIA DE LAS VARIABLES
varImpPlot(m.rf.background)
varImpPlot(m.rf.ausencia)
varImpPlot(m.rf.pseudoausencia)

#PREDICCIÓN GEOGRÁFICA
m.rf.background.mapa<-predict(variables, m.rf.background, type="response")
m.rf.ausencia.mapa<-predict(variables, m.rf.ausencia, type="response")
m.rf.pseudoausencia.mapa<-predict(variables, m.rf.pseudoausencia, type="response")

#MAPA
par(mfrow=c(1,3))
plot(m.rf.background.mapa, main="background")
plot(m.rf.ausencia.mapa, main="ausencia")
plot(m.rf.pseudoausencia.mapa, main="pseudoausencia")

#GUARDA EL MODELO
writeRaster(m.rf.background.mapa, filename="./resultados/modelos/rf_background.asc", format="ascii", overwrite=TRUE)
writeRaster(m.rf.ausencia.mapa, filename="./resultados/modelos/rf_ausencia.asc", format="ascii", overwrite=TRUE)
writeRaster(m.rf.pseudoausencia.mapa, filename="./resultados/modelos/rf_pseudoausencia.asc", format="ascii", overwrite=TRUE)

#borra objetos que no nos sirven
rm(m.rf.ausencia, m.rf.ausencia.mapa, m.rf.background, m.rf.background.mapa, m.rf.pseudoausencia, m.rf.pseudoausencia.mapa)
gc()



#######################################################################################
#######################################################################################
#ENSAMBLADO DE MODELOS
#######################################################################################
#######################################################################################


#ANÁLISIS DE SIMILITUD ENTRE LOS MODELOS
########################################
#importa los modelos
lista.modelos <- list.files(path="./resultados/modelos",pattern='.asc', full.names=TRUE)
modelos <- brick(stack(lista.modelos))

#transforma los modelos en dataframe eliminando nulos
modelos.df<-na.omit(as.data.frame(modelos))

#calcula matriz de correlaciones
modelos.cor<-cor(modelos.df)

#transforma matriz de correlación en distancias
modelos.dis<-abs(as.dist(modelos.cor))

#dibuja el árbol de correlación
dev.off()
pdf("./resultados/similitud_modelos.pdf", width=25, height=25, pointsize=30)
plot(hclust(1-modelos.dis))
dev.off()

#borra objetos que no vamos a usar más
rm(modelos, modelos.df, modelos.cor, modelos.dis)
gc()

#ENSAMBLADO
###########

#IMPORTA LOS MODELOS A STACKS (SOLO UN TIPO DE MODELOS COMO EJEMPLO)
lista.modelos.pseudoausencia <- list.files(path="./resultados/modelos",pattern='_pseudoausencia', full.names=TRUE)
modelos.pseudoausencia <- brick(stack(lista.modelos.pseudoausencia))
plot(modelos.pseudoausencia)

#ENSAMBLADO MEDIANTE PROMEDIO
ensamblado.pseudoausencia<-calc(modelos.pseudoausencia, mean, filename="./resultados/modelos/ensamblado_pseudoausencia.asc", overwrite=TRUE)

#DESVIACIÓN ESTÁNDAR
ensamblado.pseudoausencia.desviacion<-calc(modelos.pseudoausencia, sd)

#PLOT
dev.off()
#png("./resultados/ensamblados.png", width=4000, height=2200, pointsize=40)
par(mfrow=c(1,2), mar=c(4,4,4,4), oma=c(2,2,2,2))
plot(ensamblado.pseudoausencia, main="Media pseudoausencia")
plot(ensamblado.pseudoausencia.desviacion, main="Desviación pseudoausencia", col=rev(heat.colors(100)))
#dev.off()

#BORRAMOS OBJETOS QUE YA NO VAMOS A USAR
rm(modelos.pseudoausencia, ensamblado.pseudoausencia, ensamblado.pseudoausencia.desviacion, lista.modelos, lista.modelos.pseudoausencia)
gc()

#VOLVEMOS A LAS DIAPOSITIVAS
############################


##################################
##################################
#EVALUACION DE MODELOS
##################################
##################################

#IMPORTA TODOS LOS MODELOS, INCLUÍDOS EL ENSAMBLADO
#importa los modelos
lista.modelos <- list.files(path="./resultados/modelos",pattern='.asc', full.names=TRUE)
modelos <- brick(stack(lista.modelos))

#recuerda que tenemos presencias y ausencias de evaluación
str(presencia.ausencia.evaluacion)

#VAMOS A EMPEZAR EVALUANDO UN ÚNICO MODELO, PARA ENTENDER BIEN EL PROCESO
#########################################################################
#vemos los nombres de los modelos
names(modelos)

#nos quedamos con un modelo
modelo<-modelos[["gam_background"]]

#extraemos los valores de los puntos de evaluación en el modelo
presencia.ausencia.evaluacion$valores<-extract(modelo, presencia.ausencia.evaluacion[, c("x","y")])

#separamos los valores de las presencias y las ausencias
valores.presencias<-presencia.ausencia.evaluacion[presencia.ausencia.evaluacion$presencia==1, "valores"]
valores.ausencias<-presencia.ausencia.evaluacion[presencia.ausencia.evaluacion$presencia==0, "valores"]

#aplicamos la función evaluate de la librería dismo
evaluacion<-evaluate(p=valores.presencias, a=valores.ausencias)
evaluacion

#plots
par(mfrow=c(1,3), mar=c(2,2,4,2), oma=c(3,3,5,3))
density(evaluacion)
boxplot(evaluacion, col=c("blue", "red"))
plot(evaluacion, "ROC")

#vemos la estructura del objeto
str(evaluacion)

#sacamos el valor de auc (fíjate que es una @ en lugar de $ para mirar dentro de los slots)
auc<-evaluacion@auc
auc

#borramos objetos que no necesitamos
rm(modelo, valores.presencias, valores.ausencias, evaluacion, auc, lista.modelos)
gc()


#AHORA VAMOS A EVALUAR TODOS LOS MODELOS
########################################

#extraemos los valores de los puntos de evaluación en el modelo
valores.puntos.evaluacion<-extract(modelos, presencia.ausencia.evaluacion[, c("x","y")])
str(valores.puntos.evaluacion)
#es una lista, pasamos a data.frame
valores.puntos.evaluacion<-data.frame(valores.puntos.evaluacion)
str(valores.puntos.evaluacion)

#lo unimos con la columna presencia de presencias.test
valores.puntos.evaluacion$presencia<-presencia.ausencia.evaluacion$presencia
str(valores.puntos.evaluacion)

#creamos una tabla en blanco para guardar los resultados
resultados.evaluacion<-data.frame(modelo=character(), auc=numeric(), cor=numeric(), stringsAsFactors=FALSE)

#contador de filas
fila=0

#EMPIEZA A EJECUTAR EL LOOP AQUÍ
#abrimos un pdf para guardar los gráficos
pdf("./resultados/evaluacion_modelos.pdf", width=15, height=8, pointsize=30)

#iteramos por cada uno de los modelos
for (modelo in names(modelos)){
  
  #separamos los valores de las presencias y las ausencias
  #fíjate como en cada iteración tomamos la columna 'modelo'
  valores.presencias<-valores.puntos.evaluacion[valores.puntos.evaluacion$presencia==1, modelo]
  valores.ausencias<-valores.puntos.evaluacion[valores.puntos.evaluacion$presencia==0, modelo]
  
  #evaluamos el modelo
  evaluacion<-evaluate(p=valores.presencias, a=valores.ausencias)
  
  #suma 1 a la fila de la tabla de resultados
  fila=fila+1
  
  #llenamos la tabla
  resultados.evaluacion[fila, "modelo"]<-modelo
  resultados.evaluacion[fila, "auc"]<-evaluacion@auc
  resultados.evaluacion[fila, "cor"]<-evaluacion@cor
  
  #dibujamos el gráfico
  #plots
  par(mfrow=c(1,3), mar=c(2,2,4,2), oma=c(3,3,5,3))
  density(evaluacion)
  boxplot(evaluacion, col=c("blue", "red"))
  plot(evaluacion, "ROC")
  mtext(modelo, outer=TRUE, cex=1.3)
  
}

dev.off()
#TERMINA DE EJECUTAR EL LOOP AQUÍ

#ORDENAMOS LA TABLA RESULTANTE
resultados.evaluacion<-resultados.evaluacion[order(resultados.evaluacion$auc, decreasing=TRUE), ]
resultados.evaluacion #mejor lo vemos en el panel

#LA GUARDAMOS
write.table(resultados.evaluacion, "./resultados/evaluacion_modelos.csv", row.names=FALSE, col.names=TRUE, quote=FALSE, sep=",")

####################################################################################
####################################################################################
#APLICANDO THRESHOLDS
####################################################################################
####################################################################################

#############################################
#SELECCIÓN DE THRESHOLDS MEDIANTE PERCENTILES
#(PORCENTAJES DE OMISIÓN)
#############################################
#PARA CALCULAR THRESHOLDS NECESITAMOS LOS VALORES DE LOS PUNTOS DE PRESENCIA Y AUSENCIA DE EVALUACIÓN SOBRE LOS MODELOS, YA LOS TENEMOS EN valores.puntos.evaluacion
str(valores.puntos.evaluacion)

#VAMOS A TRABAJAR CON SOLO UNO DE LOS MODELOS (rf_pseudoausencia)
valores.presencias.rf.pseudoausencia<-valores.puntos.evaluacion[ , c("rf_pseudoausencia","presencia")]
valores.presencias.rf.pseudoausencia

#ELIGE UN PERCENTIL (si eliges 0.1, seleccionarás un threshold que contenga el 90% de las presencias)
percentil=0.1

#CALCULAMOS EL THRESHOLD
umbral1<-quantile(valores.presencias.rf.pseudoausencia[valores.presencias.rf.pseudoausencia$presencia==1, "rf_pseudoausencia"], probs=percentil, na.rm=TRUE)
umbral1

#DIBUJAMOS EL MAPA (el modelo original está en el brick 'modelos')
modelo.umbral.1<-modelos[["rf_pseudoausencia"]] > umbral1
dev.off()
plot(modelo.umbral.1)

#NÚMERO DE CELDAS DENTRO DEL UMBRAL (COMPLETAMENTE CONTRAINTUITIVO, SEGURO QUE HAY ALGÚN MÉTODO MÁS SENCILLO...)
suma.umbral.1<-length(Which(modelo.umbral.1, cells=TRUE))
suma.umbral.1


##########################################################
#SELECCIÓN DE THRESHOLD MEDIANTE TÉCNICAS DE OPTIMIZACIÓN
##########################################################

#TÉCNICAS DE OPTIMIZACIÓN QUE IMPLICAN A LA FUNCIÓN 'evaluate'
#############################################################

#separamos las presencias y las ausencias de valores.presencias.rf.pseudoausencia
presencia.evaluacion<-valores.presencias.rf.pseudoausencia[valores.presencias.rf.pseudoausencia$presencia == 1 , "rf_pseudoausencia"]
ausencia.evaluacion<-valores.presencias.rf.pseudoausencia[valores.presencias.rf.pseudoausencia$presencia == 0 , "rf_pseudoausencia"]

#LA FUNCIÓN EVALUATE PERMITE CALCULAR THRESHOLDS
#evaluamos
evaluacion.rf.pseudoausencia<-evaluate(p=presencia.evaluacion, a=ausencia.evaluacion)
evaluacion.rf.pseudoausencia
#fíjate en max TPR+TNR at : umbral
#ese valor es un threshold que maximiza la suma de clasificaciones correctas de presencias y ausencias
#TPR: TRUE POSITIVE RATE
#TNR: TRUE NEGATIVE RATE

#¿COMO SE CALCULA A PARTIR DE LO QUE HAY EN EL OBJETO evaluacion.rf.pseudoausencia?
umbral2<-evaluacion.rf.pseudoausencia@t[which.max(evaluacion.rf.pseudoausencia@TPR + evaluacion.rf.pseudoausencia@TNR)]
umbral2


#TAMBIÉN PERMITE CALCULAR THRESHOLDS MAXIMIZANDO OTRAS MEDIDAS DE EVALUACIÓN
#THRESHOLD MAXIMIZANDO KAPPA
umbral3<-evaluacion.rf.pseudoausencia@t[which.max(evaluacion.rf.pseudoausencia@kappa)]
umbral3

#THRESHOLD MAXIMIZANDO CORRECT CLASSIFICATION RATE
umbral4<-evaluacion.rf.pseudoausencia@t[which.max(evaluacion.rf.pseudoausencia@CCR)]
umbral4

#EN TODOS ESTOS CASOS DA EL MISMO RESULTADO, PERO NO SIEMPRE ES ASÍ

#PLOT
par(mfrow=c(2,2))
plot(modelos[["rf_pseudoausencia"]] > umbral1, main="Solo presencia")
plot(modelos[["rf_pseudoausencia"]] > umbral2, main="Corte presencias-ausencias")
plot(modelos[["rf_pseudoausencia"]] > umbral3, main="max TPR+TNR")
plot(modelos[["rf_pseudoausencia"]] > umbral4, main="max CCR")

#borramos tablas que no vamos a utilizar
rm(presencia.ausencia.entrenamiento,presencia.ausencia.evaluacion,  presencia.entrenamiento, presencia.pseudoausencia.entrenamiento, presencia.background.entrenamiento)
gc()



######################################################################################
######################################################################################
#PROYECCIÓN EN EL ESPACIO
######################################################################################
######################################################################################

#CAMBIAMOS EL NOMBRE A LAS VARIABLES
variables.eu<-variables

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

#VAMOS A EXAMINAR LA SIMILITUD EN EL ESPACIO ECOLÓGICO ENTRE AMBOS SUB-CONTINENTES
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
legend("bottomleft", c(names(variables.na)), pch=15, col=paleta.color, cex=1.6)
dev.off()


#borra objetos que no vamos a necesitar
rm(mess.na, mess.na.max, paleta.color, variables.eu.df, variables.na.df)
gc()

#AHORA QUE SABEMOS UN POCO COMO ESTÁN DISTRIBUÍDAS LAS VARIABLES EN USA, HACEMOS LOS MODELOS


###################
#CALIBRAMOS MODELOS
###################
#PRIMERO LOS CALIBRAMOS CON LAS VARIABLES Y PRESENCIAS EN EUROPA
#USAREMOS PARA CALIBRAR LAS TABLAS QUE CONTIENEN TODOS LOS PUNTOS, ES DECIR, SON MODELOS CALIBRADOS CON TODOS LOS DATOS DISPONIBLES

#PREPARAMOS LAS FÓRMULAS
#----------------------
formula.regresion.poly<-as.formula(paste("presencia ~ poly(", paste(names(variables.eu), collapse=", 2) + poly("), ", 2)", collapse=""))
formula.regresion.poly


#CALCULAMOS PESOS PARA LOS BACKGROUND PONDERADOS DE GLM Y GAM
#------------------------------------------------------------
pesos<-WeightPresenceBackground(presencia.background[ , "presencia"])

#CALIBRAMOS MODELO
#-----------------
#GLM
m.glm.backgroundw<-glm(formula.regresion.poly, family=quasibinomial(link=logit), data=presencia.background, weights=pesos)


################################
#PROYECTAMOS SOBRE LAS VARIABLES
################################
m.glm.backgroundw.map.eu<-predict(variables.eu, m.glm.backgroundw, type="response")
m.glm.backgroundw.map.na<-predict(variables.na, m.glm.backgroundw, type="response")

#plot
par(mfrow=c(1,2))
plot(m.glm.backgroundw.map.eu)
plot(m.glm.backgroundw.map.na)


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

#PLOTEAMOS LAS PRESENCIAS SOBRE EL MAPA
plot(m.glm.backgroundw.map.na)
points(presencia.na, cex=0.1)



###################################################################################
###################################################################################
#PROYECCIÓN EN EL TIEMPO
###################################################################################
###################################################################################

#NOS QUEDAMOS SOLO CON LOS NOMBRES DE LAS VARIABLES CLIMÁTICAS QUE HEMOS UTILIZADO
#vemos los nombres
lista.variables<-names(variables)
#tomamos solo los que necesitamos
lista.variables<-c(lista.variables[[1]], lista.variables[[2]], lista.variables[[6]])
#vemos que todo salió bien
lista.variables
#creamos un brick nuevo
variables.presente<-variables[[lista.variables]]


################################################
#IMPORTAMOS LAS VARIABLES DE PROYECCIÒN TEMPORAL
################################################
#descomprimimos las variables
unzip("./data/4_proyeccion_temporal.zip", exdir="./resultados/variables_proyeccion_tiempo")


#VARIABLES PALEO 21 Kyr
########################
lista.variables.paleo.21k <- list.files(path="./resultados/variables_proyeccion_tiempo/variables_paleo_21k",pattern='*.asc', full.names=TRUE)
#brick
variables.paleo.21k <- brick(stack(lista.variables.paleo.21k))
#nos quedamos solo con las necesarias
variables.paleo.21k<-variables.paleo.21k[[lista.variables]]


#VARIABLES FUTURO
#################
lista.variables.futuro <- list.files(path="./resultados/variables_proyeccion_tiempo/variables_futuro_ccsm_A1B",pattern='*.asc', full.names=TRUE)
#brick
variables.futuro <- brick(stack(lista.variables.futuro))
#nos quedamos solo con las necesarias
variables.futuro<-variables.futuro[[lista.variables]]

#ENTRENAMIENTO DEL MODELO
#############################

#PREPARAMOS LAS FÓRMULAS (SOLO LAS TRES VARIABLES CLIMÁTICAS)
formula.regresion.poly<-as.formula(paste("presencia ~ poly(", paste(names(variables.presente), collapse=", 2) + poly("), ", 2)", collapse=""))
formula.regresion.poly

#CALCULAMOS PESOS PARA LOS BACKGROUND PONDERADOS DE GLM Y GAM
pesos<-WeightPresenceBackground(presence.column=presencia.background[ , "presencia"])

#CALIBRAMOS EL MODELO
m.glm<-glm(formula.regresion.poly, family=quasibinomial(link=logit), data=presencia.background, weights=pesos)

#VEMOS CARACTERÍSTICAS DEL MODELO
summary(m.glm)
Dsquared(m.glm)
plotmo(m.glm, all2=TRUE, level=0.68)

#PROYECTAMOS SOBRE LAS VARIABLES DEL PRESENTE
m.glm.map.presente<-predict(variables.presente, m.glm, type="response")

#PROYECTAMOS SOBRE LAS VARIABLES DEL 21k
m.glm.map.paleo.21k<-predict(variables.paleo.21k, m.glm, type="response")

#PROYECTAMOS SOBRE LAS VARIABLES DEL FUTURO CSIRO
m.glm.map.futuro<-predict(variables.futuro, m.glm, type="response")


#PLOTEAMOS LOS MODELOS
######################
par(mfrow=c(1,3), mar=c(1,1,1,1), oma=c(3,3,4,3))
#glm
plot(m.glm.map.paleo.21k, main="GLM - 21 kyr BP")
plot(m.glm.map.presente, main="GLM - Present")
plot(m.glm.map.futuro, main="GLM - 2060")

