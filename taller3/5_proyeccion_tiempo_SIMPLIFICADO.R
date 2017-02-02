#DIRECTORIO DE TRABAJO
setwd("/home/blas/Dropbox/DOCENCIA/CURSOS_MDE_GBIF/2014/sesiones/sesion_2/taller2")

#CREA EL DIRECTORIO PARA GUARDAR LOS RESULTADOS DE LAS PROYECCIONES
dir.create("./resultados/proyecciones")


#CARGA LAS LIBRERIAS NECESARIAS (EJECUTAR SIEMPRE QUE TRABAJES CON EL SCRIPT)
library(dismo) #LIBRERIA PARA MODELOS DE DISTRIBUCION
library(mgcv) #OTRA LIBRERIA PARA GAM
library(kernlab) #SUPPORT VECTOR MACHINES
library(randomForest) #RANDOM FOREST
library(HH)

#CARGA LAS PRESENCIAS (Y LAS VARIABLES, que no las vamos a usar)
load("./resultados/1_prepara_presencia.Rdata")

#NOS QUEDAMOS SOLO CON LOS NOMBRES DE LAS VARIABLES CLIMÁTICAS QUE HEMOS UTILIZADO
#vemos los nombres
lista.variables<-names(variables)
#tomamos solo los que necesitamos
lista.variables<-c(lista.variables[[1]], lista.variables[[2]], lista.variables[[6]])
#vemos que todo salió bien
lista.variables

#borramos tablas que no vamos a utilizar
rm(presencia.ausencia.entrenamiento,presencia.ausencia.evaluacion, presencia.entrenamiento, presencia.pseudoausencia.entrenamiento, presencia.background.entrenamiento, variables, presencia.ausencia, presencia.pseudoausencia)
gc()

################################################
#IMPORTAMOS LAS VARIABLES DE PROYECCIÒN TEMPORAL
################################################
#descomprimimos las variables
unzip("./data/4_proyeccion_temporal.zip", exdir="./resultados/variables_proyeccion_tiempo")

#VARIABLES DEL PRESENTE
lista.variables.presente <- list.files(path="./resultados/variables_proyeccion_tiempo/variables_presente",pattern='*.asc', full.names=TRUE)
#brick
variables.presente <- brick(stack(lista.variables.presente))
#hay muchas más de las que necesitamos
names(variables.presente)
#nos quedamos solo con las necesarias
variables.presente<-variables.presente[[lista.variables]]
#a tabla
variables.presente.tabla<-na.omit(as.data.frame(variables.presente))

#VARIABLES PALEO 130 Kyr
########################
lista.variables.paleo.130k <- list.files(path="./resultados/variables_proyeccion_tiempo/variables_paleo_130k",pattern='*.asc', full.names=TRUE)
#brick
variables.paleo.130k <- brick(stack(lista.variables.paleo.130k))
#nos quedamos solo con las necesarias
variables.paleo.130k<-variables.paleo.130k[[lista.variables]]
#a tabla
variables.paleo.130k.tabla<-na.omit(as.data.frame(variables.paleo.130k))

#VARIABLES PALEO 21 Kyr
########################
lista.variables.paleo.21k <- list.files(path="./resultados/variables_proyeccion_tiempo/variables_paleo_21k",pattern='*.asc', full.names=TRUE)
#brick
variables.paleo.21k <- brick(stack(lista.variables.paleo.21k))
#nos quedamos solo con las necesarias
variables.paleo.21k<-variables.paleo.21k[[lista.variables]]
#a tabla
variables.paleo.21k.tabla<-na.omit(as.data.frame(variables.paleo.21k))

#VARIABLES FUTURO ccsm A1B
##########################
lista.variables.futuro.ccsm <- list.files(path="./resultados/variables_proyeccion_tiempo/variables_futuro_ccsm_A1B",pattern='*.asc', full.names=TRUE)
#brick
variables.futuro.ccsm <- brick(stack(lista.variables.futuro.ccsm))
#nos quedamos solo con las necesarias
variables.futuro.ccsm<-variables.futuro.ccsm[[lista.variables]]
#a tabla
variables.futuro.ccsm.tabla<-na.omit(as.data.frame(variables.futuro.ccsm))

#VARIABLES FUTURO ccsm A1B
##########################
lista.variables.futuro.csiro <- list.files(path="./resultados/variables_proyeccion_tiempo/variables_futuro_csiro_A1B",pattern='*.asc', full.names=TRUE)
#brick
variables.futuro.csiro <- brick(stack(lista.variables.futuro.csiro))
#nos quedamos solo con las necesarias
variables.futuro.csiro<-variables.futuro.csiro[[lista.variables]]
#a tabla
variables.futuro.csiro.tabla<-na.omit(as.data.frame(variables.futuro.csiro))

#borramos variables que no necesitamos
rm(lista.variables.paleo.130k, lista.variables.paleo.21k, lista.variables.presente, lista.variables.futuro.csiro, lista.variables.futuro.ccsm)
gc()


#######################################
#######################################
#ANÁLISIS EXPLORATORIO DE LAS VARIABLES
#######################################
#######################################

#EXPLORAMOS UN POCO LOS VALORES DE LAS VARIABLES
################################################
par(mfrow=c(3, 1))
#bio5
boxplot(variables.paleo.130k.tabla$bio5, variables.paleo.21k.tabla$bio5, variables.presente.tabla$bio5, variables.futuro.csiro.tabla$bio5, variables.futuro.ccsm.tabla$bio5, notch=TRUE, names=c("130kyrBP", "21kyrBP", "present", "2060 CSIRO", "2060 CCSM"), main="bio5", outline=FALSE)
#bio14
boxplot(variables.paleo.130k.tabla$bio14, variables.paleo.21k.tabla$bio14, variables.presente.tabla$bio14, variables.futuro.csiro.tabla$bio14, variables.futuro.ccsm.tabla$bio14, notch=TRUE, names=c("130kyrBP", "21kyrBP", "present", "2060 CSIRO", "2060 CCSM"), main="bio14", outline=FALSE)
#bio6
boxplot(variables.paleo.130k.tabla$bio6, variables.paleo.21k.tabla$bio6, variables.presente.tabla$bio6, variables.futuro.csiro.tabla$bio6, variables.futuro.ccsm.tabla$bio6, notch=TRUE, names=c("130kyrBP", "21kyrBP", "present", "2060 CSIRO", "2060 CCSM"), main="bio6", outline=FALSE)

dev.off()

#borramos las tablas, ya no las necesitamos
rm(variables.paleo.130k.tabla, variables.paleo.21k.tabla, variables.presente.tabla, variables.futuro.csiro.tabla, variables.futuro.ccsm.tabla)
gc()


#ANÁLISIS MESS 130 KYR BP
#########################
#necesita registros de presencia con los valores de las variables, y un brick con las variables de la región de destino
mess.paleo.130k<-mess(x=variables.paleo.130k, v=presencia[, lista.variables], full=TRUE)
#no vienen los nombres en el gráfico, pero son estos
names(mess.paleo.130k)<-c(lista.variables, "mess")
#plot
plot(mess.paleo.130k)


#ANÁLISIS MESS 21 KYR BP
#########################
mess.paleo.21k<-mess(x=variables.paleo.21k, v=presencia[, lista.variables], full=TRUE)
names(mess.paleo.21k)<-c(lista.variables, "mess")
plot(mess.paleo.21k)


#ANÁLISIS MESS FUTURO CSIRO
#########################
mess.futuro.csiro<-mess(x=variables.futuro.csiro, v=presencia[, lista.variables], full=TRUE)
names(mess.futuro.csiro)<-c(lista.variables, "mess")
plot(mess.futuro.csiro)


#ANÁLISIS MESS FUTURO CCSM
#########################
mess.futuro.ccsm<-mess(x=variables.futuro.ccsm, v=presencia[, lista.variables], full=TRUE)
names(mess.futuro.ccsm)<-c(lista.variables, "mess")
plot(mess.futuro.ccsm)

#borramos los mapas mess, ya no los necesitamos
rm(mess.paleo.130k, mess.paleo.21k, mess.futuro.csiro, mess.futuro.ccsm)
gc()


#############################
#############################
#ENTRENAMIENTO DE LOS MODELOS
#############################
#############################

#PREPARAMOS LAS FÓRMULAS
formula.regresion.poly<-as.formula(paste("presencia ~ poly(", paste(names(variables.presente), collapse=", 2) + poly("), ", 2)", collapse=""))

#CALCULAMOS PESOS PARA LOS BACKGROUND PONDERADOS DE GLM Y GAM
n.presencias<-nrow(presencia.background[presencia.background$presencia==1, ])
n.background<-nrow(presencia.background[presencia.background$presencia==0, ])
pesos<-c(rep(1/n.presencias, n.presencias), rep(1/n.background, n.background))


#CALIBRAMOS MODELOS
#GLM
m.glm<-glm(formula.regresion.poly, family=quasibinomial(link=logit), data=presencia.background, weights=pesos)

#PROYECTAMOS SOBRE LAS VARIABLES DEL PRESENTE
m.glm.map.presente<-predict(variables.presente, m.glm, type="response")

#PROYECTAMOS SOBRE LAS VARIABLES DEL 130k
m.glm.map.paleo.130k<-predict(variables.paleo.130k, m.glm, type="response")

#PROYECTAMOS SOBRE LAS VARIABLES DEL 21k
m.glm.map.paleo.21k<-predict(variables.paleo.21k, m.glm, type="response")

#PROYECTAMOS SOBRE LAS VARIABLES DEL FUTURO CSIRO
m.glm.map.futuro.csiro<-predict(variables.futuro.csiro, m.glm, type="response")

#PROYECTAMOS SOBRE LAS VARIABLES DEL FUTURO CSIRO
m.glm.map.futuro.ccsm<-predict(variables.futuro.ccsm, m.glm, type="response")


#PLOTEAMOS LOS MODELOS
######################
par(mfrow=c(1,5), mar=c(1,1,1,1), oma=c(3,3,4,3))
#glm
plot(m.glm.map.paleo.130k, main="GLM - 130 kyr BP")
plot(m.glm.map.paleo.21k, main="GLM - 21 kyr BP")
plot(m.glm.map.presente, main="GLM - Present")
plot(m.glm.map.futuro.ccsm, main="GLM - 2060 CCSM")
plot(m.glm.map.futuro.csiro, main="GLM - 2060 CSIRO")

#borramos los mapas individuales
rm(m.glm.map.presente, m.gam.map.presente, m.mx.map.presente, m.glm.map.paleo.130k, m.gam.map.paleo.130k, m.mx.map.paleo.130k, m.glm.map.paleo.21k, m.gam.map.paleo.21k, m.mx.map.paleo.21k, m.glm.map.futuro.csiro, m.gam.map.futuro.csiro, m.mx.map.futuro.csiro, m.glm.map.futuro.ccsm, m.gam.map.futuro.ccsm, m.mx.map.futuro.ccsm)
gc()

