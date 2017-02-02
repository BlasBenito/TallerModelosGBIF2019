###################################
#ESTABLECE EL DIRECTORIO DE TRABAJO
###################################
#DIRECTORIO DE TRABAJO
#setwd("C:/taller2")
setwd("/home/blas/Dropbox/DOCENCIA/CURSOS_MDE_GBIF/2014/sesiones/sesion_2/taller2")

#CARGA LAS LIBRERIAS NECESARIAS (EJECUTAR SIEMPRE QUE TRABAJES CON EL SCRIPT)
library(dismo) #LIBRERIA PARA MODELOS DE DISTRIBUCION

##################################
##################################
#ENSAMBLADO DE MODELOS
##################################
##################################

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
pdf("./resultados/similitud_modelos.pdf", width=25, height=25, pointsize=30)
plot(hclust(1-modelos.dis))
dev.off()

#borra objetos que no vamos a usar más
rm(modelos, modelos.df, modelos.cor, modelos.dis)
gc()

#ENSAMBLADO
###########

#IMPORTA LOS MODELOS A STACKS
#ausencia
lista.modelos.ausencia <- list.files(path="./resultados/modelos",pattern='_ausencia', full.names=TRUE)
modelos.ausencia <- brick(stack(lista.modelos.ausencia))
plot(modelos.ausencia)

#pseudoausencia
lista.modelos.pseudoausencia <- list.files(path="./resultados/modelos",pattern='_pseudoausencia', full.names=TRUE)
modelos.pseudoausencia <- brick(stack(lista.modelos.pseudoausencia))
plot(modelos.pseudoausencia)

#background
lista.modelos.background <- list.files(path="./resultados/modelos",pattern='_background.asc', full.names=TRUE)
modelos.background <- brick(stack(lista.modelos.background))
plot(modelos.background)


#ENSAMBLADO MEDIANTE PROMEDIO
ensamblado.ausencia<-calc(modelos.ausencia, mean, filename="./resultados/modelos/ensamblado_ausencia.asc", overwrite=TRUE)

ensamblado.pseudoausencia<-calc(modelos.pseudoausencia, mean, filename="./resultados/modelos/ensamblado_pseudoausencia.asc", overwrite=TRUE)

ensamblado.background<-calc(modelos.pseudoausencia, mean, filename="./resultados/modelos/ensamblado_background.asc", overwrite=TRUE)

#DESVIACIÓN ESTÁNDAR
ensamblado.ausencia.desviacion<-calc(modelos.ausencia, sd)
ensamblado.pseudoausencia.desviacion<-calc(modelos.pseudoausencia, sd)
ensamblado.background.desviacion<-calc(modelos.background, sd)

#PLOT
dev.off()
#png("./resultados/ensamblados.png", width=4000, height=2200, pointsize=40)
par(mfrow=c(2,3), mar=c(4,4,4,4), oma=c(2,2,2,2))
plot(ensamblado.ausencia, main="Media ausencia")
plot(ensamblado.pseudoausencia, main="Media pseudoausencia")
plot(ensamblado.background, main="Media background")
plot(ensamblado.ausencia.desviacion, main="Desviación ausencia", col=rev(heat.colors(100)))
plot(ensamblado.pseudoausencia.desviacion, main="Desviación ausencia", col=rev(heat.colors(100)))
plot(ensamblado.background.desviacion, main="Desviación ausencia", col=rev(heat.colors(100)))
#dev.off()

#BORRAMOS OBJETOS QUE YA NO VAMOS A USAR
rm(modelos.ausencia, modelos.background, modelos.pseudoausencia, ensamblado.ausencia, ensamblado.background, ensamblado.background.desviacion, ensamblado.ausencia.desviacion, ensamblado.pseudoausencia, ensamblado.pseudoausencia.desviacion, lista.modelos, lista.modelos.pseudoausencia, lista.modelos.ausencia, lista.modelos.background)
gc()

#VOLVEMOS A LAS DIAPOSITIVAS
############################


##################################
##################################
#EVALUACION DE MODELOS
##################################
##################################

#IMPORTA TODOS LOS MODELOS, INCLUÍDOS LOS ENSAMBLADOS
#importa los modelos
lista.modelos <- list.files(path="./resultados/modelos",pattern='.asc', full.names=TRUE)
modelos <- brick(stack(lista.modelos))

#IMPORTA LAS PRESENCIAS DE EVALUACIÓN
#RECUERDA QUE LAS PRESENCIAS Y AUSENCIAS DE ESTE FICHERO NO SE HAN USADO EN NINGUNO DE LOS MODELOS
presencias.test<-read.table("./resultados/presencia_ausencia_evaluacion.csv", header=TRUE, sep=",")

#VAMOS A EMPEZAR EVALUANDO UN ÚNICO MODELO, PARA ENTENDER BIEN EL PROCESO
#########################################################################
#vemos los nombres de los modelos
names(modelos)

#nos quedamos con un modelo
modelo<-modelos[["maxent2"]]

#extraemos los valores de los puntos de evaluación en el modelo
presencias.test$valores<-extract(modelo, presencias.test[, c("x","y")])

#separamos los valores de las presencias y las ausencias
valores.presencias<-presencias.test[presencias.test$presencia==1, "valores"]
valores.ausencias<-presencias.test[presencias.test$presencia==0, "valores"]

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

#sacamos el valor de correlación
cor<-evaluacion@cor
cor

#borramos objetos que no necesitamos
rm(modelo, valores.presencias, valores.ausencias, evaluacion, auc, cor, lista.modelos)
gc()


#AHORA VAMOS A EVALUAR TODOS LOS MODELOS
########################################

#extraemos los valores de los puntos de evaluación en el modelo
valores.puntos.evaluacion<-extract(modelos, presencias.test[, c("x","y")])
str(valores.puntos.evaluacion)
#es una lista, pasamos a data.frame
valores.puntos.evaluacion<-data.frame(valores.puntos.evaluacion)
str(valores.puntos.evaluacion)

#lo unimos con la columna presencia de presencias.test
valores.puntos.evaluacion$presencia<-presencias.test$presencia
str(valores.puntos.evaluacion)

#creamos una tabla para guardar los resultados
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


#EVALUACIÓN KFOLD
#################
#importamos la tabla de presencia-ausencia completa y las variables (entree otras cosas)
load("./resultados/1_prepara_presencia.Rdata")

#comprobamos que está lo que necesitamos
str(presencia.ausencia)
names(variables)

#fórmula para el modelo
formula.regresion<-as.formula(paste("presencia ~ ", paste(names(variables), collapse="+"), collapse=""))

#separamos los datos en grupos
n.grupos<-20
grupos<-kfold(presencia.ausencia, n.grupos)
grupos

#objeto en el que guardamos los resultados de la evaluación
valores.auc.kfold<-vector()

#iteramos por los grupos
for (grupo in 1:n.grupos){
  
  #seleccionamos los registros que NO son del grupo como entrenamiento
  presencia.entrenamiento<-presencia.ausencia[grupos != grupo, ]
  
  #seleccionamos los registros del grupo como evaluacion, solo las coordenadas
  presencia.evaluacion<-presencia.ausencia[(grupos == grupo & presencia.ausencia$presencia == 1), c("x", "y")]
  ausencia.evaluacion<-presencia.ausencia[(grupos == grupo & presencia.ausencia$presencia == 0), c("x", "y")]
  
  #modelo (AQUÍ PODRÍA IR CUALQUIER MODELO!!!) Ojo a data=presencia.entrenamiento
  modelo<-glm(formula.regresion, family=binomial(link=logit), data=presencia.entrenamiento)
  
  #evalúa el modelo
  evaluacion<-evaluate(p=presencia.evaluacion, a=ausencia.evaluacion, model=modelo, x=variables)
  
  #guarda el resultado
  valores.auc.kfold[[grupo]]<-evaluacion@auc
  
}

#lista de valores
valores.auc.kfold
#valor medio de auc
mean(valores.auc.kfold)
#desviacion estándar
sd(valores.auc.kfold)
#error estándar
sd(valores.auc.kfold)/(sqrt(length(valores.auc.kfold)))
#distribución de los valores
plot(density(valores.auc.kfold))



#EVALUACIÓN BOOTSTRAP
#####################
#calculamos el porcentaje de datos de calibrado/evaluacion
porcentaje<-40
n.registros.evaluacion<-(porcentaje*nrow(presencia.ausencia))/100

#objeto en el que guardamos los resultados de la evaluación
valores.auc.bootstrap<-vector()

#iteramos por los grupos
for (iteracion in 1:20){
  
  print(iteracion)
  
  #muestreo aleatorio
  muestreo<-sample(nrow(presencia.ausencia), size=n.registros.evaluacion, replace=TRUE)
  
  #seleccionamos los registros que NO son del grupo como entrenamiento
  presencia.entrenamiento<-presencia.ausencia[-muestreo, ]
  
  #seleccionamos los registros del grupo como evaluacion, solo las coordenadas
  presencia.evaluacion<-presencia.ausencia[(muestreo & presencia.ausencia$presencia == 1), c("x", "y")]
  ausencia.evaluacion<-presencia.ausencia[(muestreo & presencia.ausencia$presencia == 0), c("x", "y")]
  
  #modelo (AQUÍ PODRÍA IR CUALQUIER MODELO!!!) Ojo a data=presencia.entrenamiento
  modelo<-glm(formula.regresion, family=binomial(link=logit), data=presencia.entrenamiento)
  
  #evalúa el modelo
  evaluacion<-evaluate(p=presencia.evaluacion, a=ausencia.evaluacion, model=modelo, x=variables)
  
  #guarda el resultado
  valores.auc.bootstrap[[iteracion]]<-evaluacion@auc
  
}

#lista de valores
valores.auc.bootstrap
#valor medio de auc
mean(valores.auc.bootstrap)
#desviacion estándar
sd(valores.auc.bootstrap)
#error estándar
sd(valores.auc.bootstrap)/(sqrt(length(valores.auc.bootstrap)))
#distribución de los valores
plot(density(valores.auc.bootstrap))

#comparamos los valores obtenidos por kfold y bootstrap para un mismo número de iteraciones
boxplot(valores.auc.kfold, valores.auc.bootstrap, names=c("kfold", "bootstrap"), notch=TRUE)
#el AUC medio es indistinguible, pero el bootstrap es más estable porque el tamaño de muestra (40%) usado en cada iteración es mayor que en el kfold (5% para 20 grupos)

#borramos objetos que no vamos a necesitar
rm(variables, valores.auc.kfold, valores.auc.bootstrap, porcentaje, n.registros.evaluacion, n.grupos, muestreo, modelo, iteracion, grupos, grupo, formula.regresion, fila, evaluacion, resultados.evaluacion, ausencia.evaluacion, presencia, presencia.ausencia, presencia.ausencia.entrenamiento, presencia.background, presencia.entrenamiento, presencia.pseudoausencia, presencia.ausencia.evaluacion, presencia.background.entrenamiento, presencia.pseudoausencia.entrenamiento, presencia.evaluacion, presencias.test)

#VAMOS A APRENDER A EVALUAR MODELOS CON MAXENT, VOLVEMOS A LAS DIAPOS
#####################################################################




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


##########################################################
#SELECCIÓN DE THRESHOLD MEDIANTE TÉCNICAS DE OPTIMIZACIÓN
##########################################################
#VAMOS A BUSCAR EL PUNTO EN EL QUE EL MODELO COMIENZA A PREDECIR PRESENCIA CON MAYOR PROBABILIDAD QUE AUSENCIAS
#separamos las presencias y las ausencias de valores.presencias.rf.pseudoausencia
presencia.evaluacion<-valores.presencias.rf.pseudoausencia[valores.presencias.rf.pseudoausencia$presencia == 1 , "rf_pseudoausencia"]
ausencia.evaluacion<-valores.presencias.rf.pseudoausencia[valores.presencias.rf.pseudoausencia$presencia == 0 , "rf_pseudoausencia"]

#vamos a plotearlas
d.presencia<-density(presencia.evaluacion, from=0, to=1)
d.ausencia<-density(ausencia.evaluacion, from=0, to=1)
plot(d.presencia, col="forestgreen", lwd=4, xlab="Presence probability")
lines(d.ausencia, col="red4", lwd=4)

#¿DONDE SE CRUZAN?
#d.presencia y d.ausencia tienen valores y
str(d.presencia)
str(d.ausencia)

#vamos a meterlos en un dataframe
temporal<-data.frame(d.presencia$x, d.presencia$y, d.ausencia$y)
names(temporal)<-c("umbral", "d.presencia","d.ausencia")
str(temporal)

#las presencias y las ausencias se cruzan donde d.presencia$y - d.ausencia$y ~ 0
temporal$diferencia<-abs(temporal$d.presencia - temporal$d.ausencia)

#lo comprobamos
lines(temporal$umbral, temporal$diferencia, col="blue", lwd=3)

#¿cual es el umbral en el que la diferencia se acerca a cero?
umbral2<-temporal[temporal$diferencia==min(temporal$diferencia), "umbral"]
umbral2

#DIBUJAMOS EL MAPA (el modelo original está en el brick 'modelos')
modelo.umbral.2<-modelos[["rf_pseudoausencia"]] > umbral2
dev.off()
plot(modelo.umbral.2)

#NÚMERO DE CELDAS DENTRO DEL UMBRAL (COMPLETAMENTE CONTRAINTUITIVO, SEGURO QUE HAY ALGÚN MÉTODO MÁS SENCILLO...)
suma.umbral.2<-length(Which(modelo.umbral.2, cells=TRUE))



#TÉCNICAS DE OPTIMIZACIÓN QUE IMPLICAN A LA FUNCIÓN 'evaluate'
#############################################################
#LA FUNCIÓN EVALUATE PERMITE CALCULAR THRESHOLDS
#evaluamos
evaluacion.rf.pseudoausencia<-evaluate(p=presencia.evaluacion, a=ausencia.evaluacion)
evaluacion.rf.pseudoausencia
#fíjate en max TPR+TNR at : umbral
#ese valor es un threshold que maximiza la suma de clasificaciones correctas de presencias y ausencias
#TPR: TRUE POSITIVE RATE
#TNR: TRUE NEGATIVE RATE

#¿COMO SE CALCULA A PARTIR DE LO QUE HAY EN EL OBJETO evaluacion.rf.pseudoausencia?
umbral3<-evaluacion.rf.pseudoausencia@t[which.max(evaluacion.rf.pseudoausencia@TPR + evaluacion.rf.pseudoausencia@TNR)]
umbral3


#TAMBIÉN PERMITE CALCULAR THRESHOLDS MAXIMIZANDO OTRAS MEDIDAS DE EVALUACIÓN
#THRESHOLD MAXIMIZANDO KAPPA
umbral4<-evaluacion.rf.pseudoausencia@t[which.max(evaluacion.rf.pseudoausencia@kappa)]
umbral4

#THRESHOLD MAXIMIZANDO CORRECT CLASSIFICATION RATE
umbral5<-evaluacion.rf.pseudoausencia@t[which.max(evaluacion.rf.pseudoausencia@CCR)]
umbral5

#EN TODOS ESTOS CASOS DA EL MISMO RESULTADO, PERO NO SIEMPRE ES ASÍ

#PLOT
par(mfrow=c(1,3))
plot(modelos[["rf_pseudoausencia"]] > umbral1, main="Solo presencia")
plot(modelos[["rf_pseudoausencia"]] > umbral2, main="Corte presencias-ausencias")
plot(modelos[["rf_pseudoausencia"]] > umbral3, main="max TPR+TNR")
