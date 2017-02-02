f###################################
#ESTABLECE EL DIRECTORIO DE TRABAJO
###################################
#DIRECTORIO DE TRABAJO
setwd("C:/taller2")
setwd("/home/blas/Dropbox/DOCENCIA/CURSOS_MDE_GBIF/2014/sesiones/sesion_2/taller2")

#CREA CARPETA PARA GUARDAR LOS MODELOS
dir.create("./resultados/modelos")

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
install.packages("mgcv", dep=TRUE)
install.packages("kernlab", dep=TRUE)
install.packages("randomForest", dep=TRUE)
install.packages("party", dep=TRUE)
install.packages("HH", dep=TRUE)
install.packages("vegan", dep=TRUE)
install.packages("tree", dep=TRUE)


#CARGA LAS LIBRERIAS NECESARIAS (EJECUTAR SIEMPRE QUE TRABAJES CON EL SCRIPT)
library(dismo) #LIBRERIA PARA MODELOS DE DISTRIBUCION
library(plotmo) #LIBRERIA PARA VISUALIZACION DE CURVAS DE RESPUESTA
library(mgcv) #OTRA LIBRERIA PARA GAM
library(kernlab) #SUPPORT VECTOR MACHINES
library(randomForest) #RANDOM FOREST
library(party) #ÁRBOLES DE CLASIFICACIÓN Y REGRESIÓN
library(vegan) #NMDS
library(tree) #ÁRBOLES DE REGRESIÓN

#CARGA LAS PRESENCIAS Y LAS VARIABLES
load("./resultados/1_prepara_presencia.Rdata")


################################################
#EXPLORANDO LOS DATOS PARA UNA VARIABLE AMBIENTAL
################################################

#guardamos todo en un solo pdf
pdf("./resultados/analisis_exploratorio1.pdf", width=20, height=15, pointsize=30)
#itera por las variables
for (variable in names(variables)){

#organizamos el gráfico en varias ventanas (panel de 3x3)
par(mfrow=c(3,3), mar=c(4,4,4,4), oma=c(2,2,2,2))

#límites de la variable en los gráficos
variable.min<-min(presencia.background[, variable])
variable.max<-max(presencia.background[, variable])

#scatterplot
#------------
#presencias vs. background
plot(presencia.background[, variable],presencia.background$presencia, main="background", xlim=c(variable.min,variable.max), xlab=variable, ylab="")

#presencias vs ausencias
plot(presencia.ausencia[, variable],presencia.ausencia$presencia, main="ausencia", xlim=c(variable.min,variable.max), xlab=variable, ylab="")

#presencias vs pseudoausencias
plot(presencia.pseudoausencia[, variable],presencia.pseudoausencia$presencia, main="pseudo-ausencia", xlim=c(variable.min,variable.max), xlab=variable, ylab="")

#boxplot
#------------
#presencias vs. background
boxplot(presencia.background[, variable]~presencia.background$presencia, notch=TRUE, horizontal=TRUE, yxlim=c(variable.min,variable.max), col=c("red","forestgreen"), xlab=variable)

#presencias vs ausencias
boxplot(presencia.ausencia[, variable]~presencia.ausencia$presencia, notch=TRUE, horizontal=TRUE, ylim=c(variable.min,variable.max), col=c("red","forestgreen"), xlab=variable)

#presencias vs pseudoausencias
boxplot(presencia.pseudoausencia[, variable]~presencia.pseudoausencia$presencia, notch=TRUE, horizontal=TRUE, ylim=c(variable.min,variable.max), col=c("red","forestgreen"), xlab=variable)

#density plot
#------------
#presencias vs. background
d0<-density(presencia.background[which(presencia.background$presencia==0), variable], from=variable.min, to=variable.max)
d1<-density(presencia.background[which(presencia.background$presencia==1), variable], from=variable.min, to=variable.max)
plot(d0, col="red", xlim=c(variable.min,variable.max), lwd=3, main="", xlab=variable, ylim=c(0, max(d0$y, d1$y)))
lines(d1, col="forestgreen", lwd=3)

#presencias vs ausencias
d0<-density(presencia.ausencia[which(presencia.ausencia$presencia==0), variable], from=variable.min, to=variable.max)
d1<-density(presencia.ausencia[which(presencia.ausencia$presencia==1), variable], from=variable.min, to=variable.max)
plot(d0, col="red", xlim=c(variable.min,variable.max), lwd=3, main="", xlab=variable, ylim=c(0, max(d0$y, d1$y)))
lines(d1, col="forestgreen", lwd=3)

#presencias vs pseudoausencias
d0<-density(presencia.pseudoausencia[which(presencia.pseudoausencia$presencia==0), variable], from=variable.min, to=variable.max)
d1<-density(presencia.pseudoausencia[which(presencia.pseudoausencia$presencia==1), variable], from=variable.min, to=variable.max)
plot(d0, col="red", xlim=c(variable.min,variable.max), lwd=3, main="", xlab=variable, ylim=c(0, max(d0$y, d1$y)))
lines(d1, col="forestgreen", lwd=3)
}
dev.off()


#################################
#EXPLORANDO LOS DATOS CON UN NMDS
#################################
#EXPLICACIÓN EN LAS DIAPOS

#USAMOS LA TABLA presencia.pseudoausencia HACIENDO UNA COPIA EN tabla.nmds PORQUE VAMOS A MODIFICARLA
#OJO, UN NMDS VA MUY LENTO SI SE HACE CON MÁS DE 1000 PUNTOS
tabla.nmds<-presencia.pseudoausencia
#la ordenamos para que las presencias queden al final de la tabla, esto mejora los gráficos, porque los puntos se van dibujando de arriba a abajo, y no queremos que los puntos de las pseudoausencias tapen a las presencias
tabla.nmds<-tabla.nmds[order(tabla.nmds$presencia, decreasing=FALSE), ]

#ASIGNAMOS COLORES A LAS PRESENCIAS Y AUSENCIAS
tabla.nmds$color<-"gray60" #gris para todo
tabla.nmds[which(tabla.nmds$presencia==1),]$color<-"red4" #rojo para las presencias

#ASIGNAMOS TAMAÑO DE PUNTO DISTINTOS A LAS PRESENCIAS Y AUSENCIAS
tabla.nmds$cex<-0.6
tabla.nmds[which(tabla.nmds$presencia==1),]$cex<-0.8

#CON SCATTERPLOTS PODEMOS VER EL ESPACIO ECOLÓGICO POR PAREJAS DE VARIABLES
par(mfrow=c(2,2))
plot(tabla.nmds$bio5 ~ tabla.nmds$bio6, col=tabla.nmds$color, cex=tabla.nmds$cex, ylab="bio5", xlab="bio6", pch=20)
plot(tabla.nmds$bio5 ~ tabla.nmds$bio14, col=tabla.nmds$color, cex=tabla.nmds$cex, ylab="bio5", xlab="bio14", pch=20)
plot(tabla.nmds$landcover_veg_tree ~ tabla.nmds$ndvi_range, col=tabla.nmds$color, cex=tabla.nmds$cex, ylab="landcover_veg_tree", xlab="ndvi_range", pch=20)
plot(tabla.nmds$human_footprint ~ tabla.nmds$diversidad_topo, col=tabla.nmds$color, cex=tabla.nmds$cex, ylab="human_footprint", xlab="diversidad_topo", pch=20)

#NMDS
nmds<-metaMDS(tabla.nmds[,names(variables)], distance="euclidean")
#reglas para la interpretación del stress:
#stress < 0.05: excellent representation
#stress < 0.1: good representation
#stress < 0.2: acceptable representation,
#stress > 0.3: unsatisfactory representation

#GRAFICO
pdf("./resultados/nmds.pdf", width=10, height=7, pointsize=15)
#dibuja los ejes
plot(nmds, type="n", main="NMDS")
#dibuja todos los puntos
points(nmds$points, col=tabla.nmds$color, cex=tabla.nmds$cex, pch=19)
#dibuja una de las variables
ordisurf(nmds, tabla.nmds$bio14, add=T, col="blue", lwd=2, cex=3)
ordisurf(nmds, tabla.nmds$human_footprint, add=T, col="forestgreen", lwd=2, cex=3)
#ponemos la leyenda
legend("bottomright", c("pseudoabsence","presence"), pch=19, col=c("gray60","red4"))
legend("topright", title="Lines", c("Rainfall of warmest month", "Human footprint"), lty=1, lwd=2, col=c("blue", "forestgreen"))
dev.off()

#BORRAMOS OBJETOS QUE NO VAMOS A NECESITAR
rm(d0, d1, nmds, variable, variable.max, variable.min, tabla.nmds)
gc()


############################################################################
############################################################################
#CALIBRANDO MODELOS
############################################################################
############################################################################

#NOTA IMPORTANTE, VAMOS A CALIBRAR TODOS LOS MODELOS CON LAS TABLAS presencia.*.entrenamiento, PORQUE LOS EVALUAREMOS TODOS JUNTOS AL FINAL PARA COMPARARLOS


########
#BIOCLIM
########
#para ajustar el modelo solo necesitamos la tabla presencia con los valores de las variables
m.bioclim<-bioclim(presencia.entrenamiento[, names(variables)])

#ploteamos resultados
par(mfrow=c(2,2))
plot(m.bioclim, a=1, b=2, p=0.85)
plot(m.bioclim, a=2, b=7, p=0.85)
plot(m.bioclim, a=1, b=6, p=0.85)
plot(m.bioclim, a=1, b=7, p=0.85)

#predecimos a un mapa
m.bioclim.map<-predict(variables, m.bioclim)

#dibujamos el mapa
dev.off()
plot(m.bioclim.map)
points(presencia.entrenamiento[, c("x","y")], cex=0.1)

#guardamos el raster
writeRaster(m.bioclim.map, filename="./resultados/modelos/bioclim.asc", format="ascii", overwrite=TRUE)

#borra el mapa y el modelo de la memoria
rm(m.bioclim, m.bioclim.map)
gc()


#######
#DOMAIN
#######
#para ajustar el modelo solo necesitamos la tabla presencia con los valores de las variables
m.domain<-domain(presencia.entrenamiento[, names(variables)])

#predecimos a un mapa
m.domain.map<-predict(variables, m.domain)

#dibujamos el mapa
plot(m.domain.map)
points(presencia.entrenamiento[, c("x","y")], cex=0.5)

#guardamos el raster
writeRaster(m.domain.map, filename="./resultados/modelos/domain.asc", format="ascii", overwrite=TRUE)

#borra el mapa y el modelo de la memoria
rm(m.domain, m.domain.map)
gc()



###############################################################
###############################################################
#MÉTODOS DE REGRESIÓN
###############################################################
###############################################################

################################
#GENERALIZED LINEAR MODELS (GLM)
################################
#AJUSTE DE LOS MODELOS
#---------------------
#AYUDA GLM
help(glm)

#VAMOS POCO A POCO, EMPEZAMOS CON UNA VARIABLE
#---------------------------------------------
m.glm.temp<-glm(presencia ~ bio5, family=binomial(link=logit), data=presencia.pseudoausencia.entrenamiento)

#vemos el resumen del resultado
summary(m.glm.temp)

#devianza explicada
((m.glm.temp$null.deviance-m.glm.temp$deviance)/m.glm.temp$null.deviance)*100

#curva de respuesta
plotmo(m.glm.temp, se=1)

#predecimos a un mapa
m.glm.temp.mapa<-predict(variables, m.glm.temp, type="response")

#plot
plot(m.glm.temp.mapa)
points(presencia[, c("x","y")], cex=0.1)


#AHORA CON DOS VARIABLES
#-----------------------
m.glm.temp<-glm(presencia ~ bio5 + ndvi_range, family=binomial(link=logit), data=presencia.pseudoausencia.entrenamiento)

#vemos el resumen del resultado
summary(m.glm.temp)

#devianza explicada
((m.glm.temp$null.deviance-m.glm.temp$deviance)/m.glm.temp$null.deviance)*100

#curva de respuesta
plotmo(m.glm.temp, se=1)

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
((m.glm.temp$null.deviance-m.glm.temp$deviance)/m.glm.temp$null.deviance)*100

#curva de respuesta (poner all2=TRUE solo cuando haya interacciones en el modelo)
plotmo(m.glm.temp, all2=TRUE, se=1)

#predecimos a un mapa
m.glm.temp.mapa<-predict(variables, m.glm.temp, type="response")

#plot
plot(m.glm.temp.mapa)
points(presencia[, c("x","y")], cex=0.1)


#AHORA CON DOS VARIABLES CON TRANSFORMACIÓN POLINOMIAL (grado 2)
#----------------------------------------------------
m.glm.temp<-glm(presencia ~ poly(bio5, 2) + poly(ndvi_range, 2), family=binomial(link=logit), data=presencia.pseudoausencia.entrenamiento)

#vemos el resumen del resultado
summary(m.glm.temp)

#devianza explicada
((m.glm.temp$null.deviance-m.glm.temp$deviance)/m.glm.temp$null.deviance)*100

#curva de respuesta
plotmo(m.glm.temp, se=1)

#predecimos a un mapa
m.glm.temp.mapa<-predict(variables, m.glm.temp, type="response")

#plot
plot(m.glm.temp.mapa)
points(presencia[, c("x","y")], cex=0.1)


#AHORA CON DOS VARIABLES CON TRANSFORMACIÓN POLINOMIAL (grado 4)
#----------------------------------------------------
m.glm.temp<-glm(presencia ~ poly(bio5, 4) + poly(ndvi_range, 4), family=binomial(link=logit), data=presencia.pseudoausencia.entrenamiento)

#vemos el resumen del resultado
summary(m.glm.temp)

#devianza explicada
((m.glm.temp$null.deviance-m.glm.temp$deviance)/m.glm.temp$null.deviance)*100

#curva de respuesta
plotmo(m.glm.temp, se=1)

#predecimos a un mapa
m.glm.temp.mapa<-predict(variables, m.glm.temp, type="response")

#plot
plot(m.glm.temp.mapa)
points(presencia[, c("x","y")], cex=0.1)


#AHORA CON DOS VARIABLES QUE INTERACTÚAN CON TRANSFORMACIÓN POLINOMIAL (grado 4)
#----------------------------------------------------
m.glm.temp<-glm(presencia ~ poly(bio5, 4) * poly(ndvi_range, 4), family=binomial(link=logit), data=presencia.pseudoausencia.entrenamiento)

#vemos el resumen del resultado
summary(m.glm.temp)

#devianza explicada
((m.glm.temp$null.deviance-m.glm.temp$deviance)/m.glm.temp$null.deviance)*100

#curva de respuesta
plotmo(m.glm.temp, all2=TRUE, se=1)

#predecimos a un mapa
m.glm.temp.mapa<-predict(variables, m.glm.temp, type="response")

#plot
plot(m.glm.temp.mapa)
points(presencia[, c("x","y")], cex=0.1)


#VAMOS A TRABAJAR CON TODAS LAS VARIABLES
#----------------------------------------
#GENERA FÓRMULAS PARA LOS MODELOS DE REGRESIÓN
#fórmula de regresión sin interacciones
formula.regresion<-as.formula(paste("presencia ~ ", paste(names(variables), collapse="+"), collapse=""))
formula.regresion

#formula de regresión polinomial de grado 2 sin interacciones
formula.regresion.poly<-as.formula(paste("presencia ~ poly(", paste(names(variables), collapse=", 2) + poly("), ", 2)", collapse=""))
formula.regresion.poly


#VAMOS A PONDERAR LAS PRESENCIAS Y LOS BACKGROUND DE presencia.background.entrenamiento PARA QUE LA SUMA DE LOS PESOS DE LAS PRESENCIAS Y LOS PUNTOS DE BACKGROUND SUME 1
#¿cuantas presencias hay?
n.presencias<-nrow(presencia.background.entrenamiento[presencia.background.entrenamiento$presencia==1, ])

#¿cuantos puntos de background hay?
n.background<-nrow(presencia.background.entrenamiento[presencia.background.entrenamiento$presencia==0, ])

#generamos un vector con los los pesos
pesos<-c(rep(1/n.presencias, n.presencias), rep(1/n.background, n.background))


#AJUSTAMOS LOS MODELOS
#modelo con datos de background
m.glm.background<-glm(formula.regresion, family=binomial(link=logit), data=presencia.background.entrenamiento)

#modelo con datos de background ponderados, ojo a weights=pesos
#para casos ponderados ponemos family=quasibinomial
m.glm.backgroundw<-glm(formula.regresion, family=quasibinomial(link=logit), data=presencia.background.entrenamiento, weights=pesos)

#modelo de ausencia
m.glm.ausencia<-glm(formula.regresion, family=binomial(link=logit), data=presencia.ausencia.entrenamiento)

#modelo de pseudoausencia
m.glm.pseudoausencia<-glm(formula.regresion, family=binomial(link=logit), data=presencia.pseudoausencia.entrenamiento)


#DIBUJAMOS LAS CURVAS DE RESPUESTA
help(plotmo)
#fíjate en las diferencias entre las curvas de respuesta de los modelos de background
plotmo(m.glm.background)
plotmo(m.glm.backgroundw)
plotmo(m.glm.ausencia)
plotmo(m.glm.pseudoausencia)
#¿NOTAS ALGO RARO EN ESTAS CURVAS?


#VAMOS A PROBAR CON LA FÓRMULA POLINOMIAL
formula.regresion.poly

#AJUSTAMOS LOS MODELOS CON POLINOMIOS
#modelo con datos de background
m.glm.background<-glm(formula.regresion.poly, family=binomial(link=logit), data=presencia.background.entrenamiento)

#modelo con datos de background ponderados, ojo a weights=pesos
#cambiamos la familia a quasibinomial, que gestiona mejor los pesos
m.glm.backgroundw<-glm(formula.regresion.poly, family=quasibinomial(link=logit), data=presencia.background.entrenamiento, weights=pesos)

#modelo de ausencia
m.glm.ausencia<-glm(formula.regresion.poly, family=binomial(link=logit), data=presencia.ausencia.entrenamiento)

#modelo de pseudoausencia
m.glm.pseudoausencia<-glm(formula.regresion.poly, family=binomial(link=logit), data=presencia.pseudoausencia.entrenamiento)


#DIBUJAMOS LAS CURVAS DE RESPUESTA
plotmo(m.glm.background, se=1)
plotmo(m.glm.backgroundw, se=1)
plotmo(m.glm.ausencia, se=1)
plotmo(m.glm.pseudoausencia, se=1)
#¿CREES QUE TIENEN MÁS SENTIDO QUE LAS ANTERIORES?


#resumen de los modelos
summary(m.glm.background)
summary(m.glm.backgroundw)
summary(m.glm.ausencia)
summary(m.glm.pseudoausencia)


#devianza explicada
((m.glm.background$null.deviance-m.glm.background$deviance)/m.glm.background$null.deviance)*100
((m.glm.backgroundw$null.deviance-m.glm.backgroundw$deviance)/m.glm.backgroundw$null.deviance)*100
((m.glm.ausencia$null.deviance-m.glm.ausencia$deviance)/m.glm.ausencia$null.deviance)*100
((m.glm.pseudoausencia$null.deviance-m.glm.pseudoausencia$deviance)/m.glm.pseudoausencia$null.deviance)*100


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
writeRaster(m.glm.background.mapa, filename="./resultados/modelos/glm_background.asc", format="ascii", overwrite=TRUE)
writeRaster(m.glm.backgroundw.mapa, filename="./resultados/modelos/glm_backgroundw.asc", format="ascii", overwrite=TRUE)
writeRaster(m.glm.ausencia.mapa, filename="./resultados/modelos/glm_ausencia.asc", format="ascii", overwrite=TRUE)
writeRaster(m.glm.pseudoausencia.mapa, filename="./resultados/modelos/glm_pseudoausencia.asc", format="ascii", overwrite=TRUE)


#PARA AMPLIAR
#############

#SELECCIÓN DE VARIABLES
#----------------------
#install.packages("glmulti", dep=TRUE)
library(glmulti) #ojo, depende de java!

#ajustamos el modelo (uno pequeño, para que tarde menos)
m.glm.seleccion<-glm(presencia ~ poly(landcover_veg_tree, 2) * poly(bio6, 2) + poly(bio5, 2), family=binomial(link="logit"), data=presencia.ausencia.entrenamiento)

#glmulti comprueba distintas combinaciones de variables e interacciones
seleccion<-glmulti(m.glm.seleccion, chunk=1, chunks=20)

#vemos el resultado final con el mejor modelo
summary(seleccion)


#borra el mapa y el modelo de la memoria
rm(formula.regresion.poly, m.glm.ausencia, m.glm.ausencia.mapa, m.glm.background, m.glm.background.mapa, m.glm.backgroundw, m.glm.backgroundw.mapa, m.glm.interaccion, m.glm.pseudoausencia, m.glm.pseudoausencia.mapa, m.glm.seleccion, n.background, n.presencias)
gc()


#####################################
#MODELOS ADITIVOS GENERALIZADOS (GAM)
#####################################

#GENERA LA FÓRMULA PARA GAM
formula.gam<-as.formula(paste("presencia ~ s(", paste(names(variables), collapse=") + s("), ")", collapse=""))
formula.gam


#AJUSTAMOS LOS MODELOS
#para los modelos con background usaremos la función 'bam', o 'gam for big datasets'
#modelo con datos de background
m.gam.background<-bam(formula.gam, family=binomial(link=logit), data=presencia.background.entrenamiento)

#modelo con datos de background ponderados, ojo a weights=pesos
m.gam.backgroundw<-bam(formula.gam, family=quasibinomial(link=logit), data=presencia.background.entrenamiento, weights=pesos)

#modelo de ausencia
m.gam.ausencia<-gam(formula.gam, family=binomial(link=logit), data=presencia.ausencia.entrenamiento)

#modelo de pseudoausencia
m.gam.pseudoausencia<-gam(formula.gam, family=binomial(link=logit), data=presencia.pseudoausencia.entrenamiento)


#DIBUJAMOS LAS CURVAS DE RESPUESTA
vis.gam(m.gam.background, view=c("bio5","bio6"), type="response", plot.type="contour") #plotmo no funciona con objetos "bam"
vis.gam(m.gam.backgroundw, view=c("bio5","bio6"), type="response", plot.type="contour")
plotmo(m.gam.ausencia, se=1, type="response")
plotmo(m.gam.pseudoausencia, se=1, type="response")
#¿CREES QUE TIENEN MÁS SENTIDO QUE LAS ANTERIORES?


#resumen de los modelos
summary(m.gam.background)
summary(m.gam.backgroundw)
summary(m.gam.ausencia)
summary(m.gam.pseudoausencia)


#devianza explicada
((m.gam.background$null.deviance-m.gam.background$deviance)/m.gam.background$null.deviance)*100
((m.gam.backgroundw$null.deviance-m.gam.backgroundw$deviance)/m.gam.backgroundw$null.deviance)*100
((m.gam.ausencia$null.deviance-m.gam.ausencia$deviance)/m.gam.ausencia$null.deviance)*100
((m.gam.pseudoausencia$null.deviance-m.gam.pseudoausencia$deviance)/m.gam.pseudoausencia$null.deviance)*100


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
writeRaster(m.gam.background.mapa, filename="./resultados/modelos/gam_background.asc", format="ascii", overwrite=TRUE)
writeRaster(m.gam.backgroundw.mapa, filename="./resultados/modelos/gam_backgroundw.asc", format="ascii", overwrite=TRUE)
writeRaster(m.gam.ausencia.mapa, filename="./resultados/modelos/gam_ausencia.asc", format="ascii", overwrite=TRUE)
writeRaster(m.gam.pseudoausencia.mapa, filename="./resultados/modelos/gam_pseudoausencia.asc", format="ascii", overwrite=TRUE)


#borra el mapa y el modelo de la memoria
rm(m.gam.ausencia, m.gam.ausencia.mapa, m.gam.background, m.gam.background.mapa, m.gam.backgroundw, m.gam.backgroundw.mapa, m.gam.pseudoausencia, m.gam.pseudoausencia.mapa, pesos, formula.gam)
gc()



#####################################
#####################################
#ÁRBOLES DE CLASIFICACIÓN Y REGRESIÓN
#####################################
#####################################


############################
#ÁRBOL DE REGRESIÓN CON TREE
############################
#creamos dos árboles de regresión con distintos puntos de presencia para ver las diferencias
m.tree.background<-tree(formula.regresion, data=presencia.background)

m.tree.pseudoausencia<-tree(formula.regresion, data=presencia.pseudoausencia)

m.tree.ausencia<-tree(formula.regresion, data=presencia.ausencia)


#dibuja los árboles
par(mfrow=c(3,1))
plot(m.tree.background)
text(m.tree.background, all=TRUE, cex=1)
plot(m.tree.pseudoausencia)
text(m.tree.pseudoausencia, all=TRUE, cex=1)
plot(m.tree.ausencia)
text(m.tree.ausencia, all=TRUE, cex=1)


#predice los mapas
m.tree.background.mapa<-predict(variables, m.tree.background)
m.tree.pseudoausencia.mapa<-predict(variables, m.tree.pseudoausencia)
m.tree.ausencia.mapa<-predict(variables, m.tree.ausencia)


#los mapas
par(mfrow=c(3,1))
plot(m.tree.background.mapa)
plot(m.tree.pseudoausencia.mapa)
plot(m.tree.ausencia.mapa)


#los guarda
writeRaster(m.tree.background.mapa, filename="./resultados/modelos/tree_background.asc", format="ascii", overwrite=TRUE)
writeRaster(m.tree.ausencia.mapa, filename="./resultados/modelos/tree_ausencia.asc", format="ascii", overwrite=TRUE)
writeRaster(m.tree.pseudoausencia.mapa, filename="./resultados/modelos/tree_pseudoausencia.asc", format="ascii", overwrite=TRUE)


#borra objetos que no vamos a usar
rm(m.tree.ausencia, m.tree.ausencia.mapa, m.tree.background, m.tree.background.mapa, m.tree.pseudoausencia, m.tree.pseudoausencia.mapa)
gc()


##########################################
#ÁRBOL DE INFERENCIA CONDICIONAL CON PARTY
##########################################

#creamos dos árboles de regresión con distintos puntos de presencia para ver las diferencias
m.ctree.background<-ctree(formula.regresion, data=presencia.background)

m.ctree.pseudoausencia<-ctree(formula.regresion, data=presencia.pseudoausencia)

m.ctree.ausencia<-ctree(formula.regresion, data=presencia.ausencia)


#dibuja los plots
plot(m.ctree.background)
plot(m.ctree.pseudoausencia)
plot(m.ctree.ausencia)


#predice los mapas
m.ctree.background.mapa<-predict(variables, m.ctree.background)
m.ctree.pseudoausencia.mapa<-predict(variables, m.ctree.pseudoausencia)
m.ctree.ausencia.mapa<-predict(variables, m.ctree.ausencia)


#los mapas
par(mfrow=c(3,1))
plot(m.ctree.background.mapa)
plot(m.ctree.pseudoausencia.mapa)
plot(m.ctree.ausencia.mapa)


#los guarda
writeRaster(m.ctree.background.mapa, filename="./resultados/modelos/ctree_background.asc", format="ascii", overwrite=TRUE)
writeRaster(m.ctree.ausencia.mapa, filename="./resultados/modelos/ctree_ausencia.asc", format="ascii", overwrite=TRUE)
writeRaster(m.ctree.pseudoausencia.mapa, filename="./resultados/modelos/ctree_pseudoausencia.asc", format="ascii", overwrite=TRUE)


#borra objetos que no vamos a usar
rm(m.ctree.ausencia, m.ctree.ausencia.mapa, m.ctree.background, m.ctree.background.mapa, m.ctree.pseudoausencia, m.ctree.pseudoausencia.mapa)
gc()


###############
#RANDOM FORESTS
###############
#AYUDA DE randomForest
help(randomForest)

#AJUSTE DEL MODELO randomForest CON LOS DISTINTOS TIPOS DE PRESENCIA-AUSENCIA
m.rf.background<-randomForest(formula.regresion, data=presencia.background.entrenamiento, importance=TRUE, ntrees=5000, mtry=3, nodesize=10)

m.rf.ausencia<-randomForest(formula.regresion, data=presencia.ausencia.entrenamiento, importance=TRUE, ntrees=5000, mtry=3, nodesize=10)

m.rf.pseudoausencia<-randomForest(formula.regresion, data=presencia.pseudoausencia, importance=TRUE, ntrees=5000, mtry=3, nodesize=10)


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


########################
#SUPPORT VECTOR MACHINES
########################

#VEAMOS EL EFECTO DE DISTINTOS KERNELS (USAMOS SOLO LOS DATOS DE PRESENCIA-AUSENCIA POR SIMPLIFICAR)
#kernel lineal
m.svm.ausencia.linear<-ksvm(formula.regresion, data=presencia.ausencia.entrenamiento, kernel="vanilladot")
m.svm.ausencia.linear

#kernel radial
m.svm.ausencia.radial<-ksvm(formula.regresion, data=presencia.ausencia.entrenamiento, kernel="rbfdot", kpar=list(sigma=1))
m.svm.ausencia.radial

#kernel laplace
m.svm.ausencia.laplace<-ksvm(formula.regresion, data=presencia.ausencia.entrenamiento, kernel="laplacedot", kpar=list(sigma=1))
m.svm.ausencia.laplace

#PREDECIMOS A MAPAS
m.svm.ausencia.linear.mapa<-predict(variables, m.svm.ausencia.linear, type="response")
m.svm.ausencia.radial.mapa<-predict(variables, m.svm.ausencia.radial, type="response")
m.svm.ausencia.laplace.mapa<-predict(variables, m.svm.ausencia.laplace, type="response")

#VISUALIZAMOS
par(mfrow=c(1,3))
plot(m.svm.ausencia.linear.mapa, main="linear")
plot(m.svm.ausencia.radial.mapa, main="radial")
plot(m.svm.ausencia.laplace.mapa, main="laplace")


#AJUSTE DEL MODELO FINAL
########################
#background
m.svm.background<-ksvm(formula.regresion, data=presencia.background.entrenamiento, kernel="laplacedot", kpar=list(sigma=1))
m.svm.background #ojo a los training errors

#ausencia
m.svm.ausencia<-ksvm(formula.regresion, data=presencia.ausencia.entrenamiento, kernel="laplacedot", kpar=list(sigma=1))
m.svm.ausencia

#pseudoausencia
m.svm.pseudoausencia<-ksvm(formula.regresion, data=presencia.pseudoausencia.entrenamiento, kernel="laplacedot", kpar=list(sigma=1))
m.svm.pseudoausencia


#PREDICCIÓN GEOGRÁFICA
m.svm.background.mapa<-predict(variables, m.svm.background, type="response")
m.svm.ausencia.mapa<-predict(variables, m.svm.ausencia, type="response")
m.svm.pseudoausencia.mapa<-predict(variables, m.svm.pseudoausencia, type="response")


#MAPA
par(mfrow=c(1,3))
plot(m.svm.background.mapa, main="background")
plot(m.svm.ausencia.mapa, main="ausencia")
plot(m.svm.pseudoausencia.mapa, main="pseudoausencia")


#GUARDA EL MODELO
writeRaster(m.svm.background.mapa, filename="./resultados/modelos/svm_background.asc", format="ascii", overwrite=TRUE)
writeRaster(m.svm.ausencia.mapa, filename="./resultados/modelos/svm_ausencia.asc", format="ascii", overwrite=TRUE)
writeRaster(m.svm.pseudoausencia.mapa, filename="./resultados/modelos/svm_pseudoausencia.asc", format="ascii", overwrite=TRUE)


#borra objetos que no nos sirven
rm(m.svm.ausencia, m.svm.ausencia.mapa, m.svm.background, m.svm.background.mapa, m.svm.pseudoausencia, m.svm.pseudoausencia.mapa, m.svm.ausencia.linear, m.svm.ausencia.radial, m.svm.ausencia.laplace, m.svm.ausencia.linear.mapa, m.svm.ausencia.radial.mapa, m.svm.ausencia.laplace.mapa)
gc()


#######
#MAXENT
#######
#MAXENT ES UN SOFTWARE QUE SE DISTRIBUYE APARTE DE DISMO
#TENEMOS EL FICHERO maxent.jar (DE JAVA) EN LA CARPETA /taller2/maxent/maxent.jar
#DEBEMOS MOVER maxent.jar A LA CARPETA java DEL PAQUETE DISMO:
fichero<-"maxent/maxent.jar"
carpeta.destino<-system.file("java", package="dismo")
#copiamos
file.copy(from=fichero, to=carpeta.destino)
#comprobamos que la copia ha funcionado (TRUE si ha funcionado bien)
file.exists(paste(carpeta.destino, "maxent.jar", sep="/"))

#VEMOS LA AYUDA
help(maxent)

#AJUSTAMOS UN MODELO (NO ADMITE FÓRMULAS), Y REQUIERE SOLO LAS COORDENADAS DE PRESENCIA
#sacamos los puntos de presencia y background
puntos.presencia<-presencia.entrenamiento[, c("x","y")]
puntos.background<-presencia.background.entrenamiento[presencia.background.entrenamiento$presencia==0, c("x","y")]
#ajustamos el modelo
m.maxent<-maxent(x=variables, p=puntos.presencia, a=puntos.background, args=c("nohinge","nothreshold"))
#fíjate en args=c("nohinge","nothreshold") LA LISTA COMPLETA DE ARGUMENTOS ESTÁ EN LA AYUDA DE MAXENT

#PLOTEAMOS LA CONTRIBUCIÓN DE LAS VARIABLES
plot(m.maxent)

#PREDECIMOS A UN MAPA
mapa<-predict(variables, m.maxent, args=c("outputformat=raw"))

#PLOTEAMOS
plot(m.maxent.mapa.raw)

#LO ESCALAMOS DE 0 A 1
mapa.minimo<-cellStats(mapa, 'min')
mapa.maximo<-cellStats(mapa, 'max')
intervalo.minimo<-0
intervalo.maximo<-1

#((values-currentMin)/(currentMax-currentMin))*(desiredMax-desiredMin)+desiredMin
mapa.escalado<-((mapa-mapa.minimo)/(mapa.maximo-mapa.minimo))*(intervalo.maximo-intervalo.minimo)+intervalo.minimo

#PLOTEAMOS EL RESULTADO
plot(mapa.escalado)


#NOTA: SI LE FALTA MEMORIA A MAXENT PARA EJECUTARSE, LA SIGUIENTE LÍNEA DEBE IR ANTES DE CARGAR EL PAQUETE DISMO
options(java.parameters = "-Xmx1g" ) #donde 1g es 1GB. Puedes ponerle hasta 2GB.
