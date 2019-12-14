#ELIMINA NOTACIÓN CIENTÍFICA
options(scipen = 999)

#AJUSTA LA CARPETA DE TRABAJO A LA LOCALIZACIÓN DE ESTE SCRIPT
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#CARGA LIBRERÍAS
library(dismo)
library(ggplot2)
library(cowplot)
theme_set(theme_cowplot())
library(plotmo)
library(purrr)

#CARGA FUNCIONES
source("funciones.R")

#CARGANDO LOS DATOS DE LA ESPECIE VIRTUAL Y LAS VARIABLES
load("data/presencia_y_variables.RData")


#PREPARANDO DATOS DE ENTRENAMIENTO
#########################################################
#########################################################
#presencia y background
pb <- rbind(
  sp$presencia, 
  sp$background
)[, c("presencia", "x", "y", sp$variables.seleccionadas)]

#Nota: para calibrar GLMs con background necesitamos ponderar las presencias y los puntos de background en función de su número.
#Como hay muchos más puntos de background (ceros) que de presencias, el modelo dará mucha más importancia al background. Añadir pesos (weights) al modelo soluciona este problema.
pb.w <- weightCases(presence = pb$presencia)

#presencia y pseudoausencias
pp <- rbind(
  sp$presencia, 
  sp$pseudoausencia
)[, c("presencia", "x", "y", sp$variables.seleccionadas)]




#################################################################
#################################################################
#CALIBRANDO MODELOS
#################################################################
#################################################################

#brick y lista para guardar modelos
modelos.brick <- brick()
modelos.list <- list()
modelos.formulas <- list()



#NUESTRO PRIMER MODELO DE LA A A LA Z
#####################################
#####################################

#VER SECCIÓN DE BIOCLIM EN LAS DIAPOSITIVAS


#1.- CALIBRANDO EL MODELO
#####################################
#calibramos un modelo BIOCLIM que solo requiere presencias
#NOTA IMPORTANTE, VAMOS A CALIBRAR TODOS LOS MODELOS CON LAS TABLAS presencia.*.entrenamiento, PORQUE LOS EVALUAREMOS TODOS JUNTOS AL FINAL PARA COMPARARLOS
temp.bioclim <- dismo::bioclim(
  x = variables$brick, 
  p = sp$xy #solo coordenadas de presencia
  )

#vemos el objeto por dentro
str(temp.bioclim)


#2.- EXAMINANDO EL MODELO
#####################################
#para plotear el modelo, a y b son los índices de las variables en names(variables).
#en VERDE las presencias dentro del hipercubo según el modelo.
#en ROJO las presencias fuera del hipercubo según el modelo.
#en AZUL el nicho óptimo según las variables a y b del plot.
#aunque las presencias en rojo estén dentro del nicho óptimo para unos determinados a y b, estarán fuera para otros.
x11(width = 15, height = 10)
par(mfrow=c(2,2), mar=c(4,4,3,3))
plot(temp.bioclim, a=1, b=2)
plot(temp.bioclim, a=1, b=3)
plot(temp.bioclim, a=1, b=4)
plot(temp.bioclim, a=1, b=5)


#3.- PROYECTANDO EL MODELO A UN MAPA
#####################################
temp.bioclim.map <- raster::predict(
  object = variables$brick, 
  model = temp.bioclim
  )

#le ponemos nombre al mapa
names(temp.bioclim.map) <- "bioclim"

#ploteamos el mapa
dev.off()
x11(width = 15, height = 10)
plot(
  temp.bioclim.map, 
  col = viridis::viridis(
    100, 
    direction = -1
    )
)


#CERRANDO SECCIÓN
#-----------------------------------

#guardando modelos
names(temp.bioclim.map) <- "bioclim"
modelos.brick$bioclim <- temp.bioclim.map
modelos.list[["bioclim"]] <- temp.bioclim

#cierra ventanas gráficas
graphics.off() 
rm(temp.bioclim, temp.bioclim.map)
 



################################
################################
#GENERALIZED LINEAR MODELS (GLM)
################################
################################

#AYUDA GLM
help(glm)


#EMPEZAMOS POCO A POCO, SOLO CON UNA VARIABLE
#############################################
#############################################

#AJUSTE DEL MODELO
#---------------------------------------------------
#ndvi_minimum: rango anual de temperatura
temp.glm <- glm(
  presencia ~ ndvi_minimum,
  family = quasibinomial(link = logit), # -> PORQUE ESTAMOS HACIENDO REGRESIÓN LOGÍSTICA!
  data = pb, 
  weights = pb.w
  )

#RESUMEN DE RESULTADOS
summary(temp.glm)
#NOTA: los coeficientes no se pueden interpretar como medidas relativas de influencia si las variables no están en la misma escala.

#coeficientes estandarizados
lm.beta::lm.beta(temp.glm)


#CURVAS DE RESPUESTA
#---------------------------------------------------
#con plotmo
x11(width = 15, height = 10)
plotmo::plotmo(
  temp.glm, 
  level = 0.68
  )

#vemos curva de respuesta sobre densidad
x11(width = 15, height = 10)
plotRespuesta(
  modelo = temp.glm, 
  presencias = pb, 
  variable = "ndvi_minimum"
  )
#algo no va del todo bien, el modelo predice máxima probabilidad donde no hay valores para las variables.
#esa curva necesita ser algo más flexible


#AJUSTAMOS MODELO DE NUEVO
#---------------------------------------------------
#convertimos la variable en un polinomio de grado 2
temp.glm <- glm(
  presencia ~ poly(ndvi_minimum, 2, raw = TRUE),
  family = quasibinomial(link = logit), 
  data = pb, 
  weights = pb.w
)

#volvemos a dibujar
x11(width = 15, height = 10)
plotRespuesta(
  modelo = temp.glm, 
  presencias = pb, 
  variable = "ndvi_minimum"
)

#devianza
Dsquared(temp.glm)

#está un poco mejor, pero vamos a darle más flexiblidad


#AJUSTAMOS MODELO DE NUEVO
#---------------------------------------------------
#convertimos la variable en un polinomio de grado 3
temp.glm <- glm(
  presencia ~ poly(ndvi_minimum, 3, raw = TRUE),
  family = quasibinomial(link = logit), 
  data = pb, 
  weights = pb.w
)

#volvemos a dibujar
x11(width = 15, height = 10)
plotRespuesta(
  modelo = temp.glm, 
  presencias = pb, 
  variable = "ndvi_minimum"
)

#devianza
Dsquared(temp.glm)
#casi!


#AJUSTAMOS MODELO DE NUEVO
#---------------------------------------------------
#convertimos la variable en un polinomio de grado 4
temp.glm <- glm(
  presencia ~ poly(ndvi_minimum, 4, raw = TRUE),
  family = quasibinomial(link = logit), 
  data = pb, 
  weights = pb.w
)

#volvemos a dibujar
x11(width = 15, height = 10)
plotRespuesta(
  modelo = temp.glm, 
  presencias = pb, 
  variable = "ndvi_minimum"
)

#devianza
Dsquared(temp.glm) #fíjate que ahora Dsquared es un poco más




#INTERACCIÓN ENTRE VARIABLES
################################
################################

#AJUSTAMOS MODELO
#---------------------------------------------------
#ajustamos modelo
temp.glm <- glm(
  presencia ~ ndvi_minimum * bio15,
  family = quasibinomial(link = logit), 
  data = pb, 
  weights = pb.w
)

#resultado
summary(temp.glm)

#coeficientes estandarizados
lm.beta::lm.beta(temp.glm)

#D cuadrado
Dsquared(temp.glm)


#CURVAS DE RESPUESTA
#---------------------------------------------------
#con plotmo
x11(width = 15, height = 10)
plotmo::plotmo(
  temp.glm, 
  level = 0.68, 
  all2 = TRUE
)

#interacción
x11(width = 15, height = 10)
p1 <- plotInteraction(
  model = temp.glm, 
  data = pb, 
  x = "ndvi_minimum", 
  y = "bio15", 
  z = "presencia", 
  grid = 100, 
  point.size.range = c(0.5, 6), 
  print = TRUE
)


#AHORA CON DOS VARIABLES POLINOMIALES (GRADO 2) QUE INTERACCIONAN
################################################################
################################################################

#AJUSTAMOS MODELO
#---------------------------------------------------
temp.glm <- glm(
  presencia ~ poly(ndvi_minimum, 2, raw = TRUE) * poly(bio15, 2, raw = TRUE),
  family = quasibinomial(link = logit), 
  data = pb, 
  weights = pb.w
)

#resultado (NOTA: AHORA CONTIENE MÁS TÉRMINOS! POR QUÉ?)
summary(temp.glm)

#coeficientes estandarizados
lm.beta::lm.beta(temp.glm)

#D cuadrado
Dsquared(temp.glm)


#CURVAS DE RESPUESTA
#---------------------------------------------------
#con plotmo
x11(width = 15, height = 10)
plotmo::plotmo(
  temp.glm, 
  level = 0.68, 
  all2 = TRUE
)

#interacción
x11(width = 15, height = 10)
p1 <- plotInteraction(
  model = temp.glm, 
  data = pb, 
  x = "ndvi_minimum", 
  y = "bio15", 
  z = "presencia", 
  grid = 100, 
  point.size.range = c(0.5, 6), 
  print = TRUE
)



#AHORA CON DOS VARIABLES POLINOMIALES (GRADO 4!!!) QUE INTERACCIONAN
################################################################
################################################################


#AJUSTE DEL MODELO
#---------------------------------------------------
temp.glm <- glm(
  presencia ~ poly(ndvi_minimum, 4, raw = TRUE) * poly(bio15, 4, raw = TRUE),
  family = quasibinomial(link = logit), 
  data = pb, 
  weights = pb.w
)

#resultado (NOTA: AHORA CONTIENE MÁS TÉRMINOS! POR QUÉ?)
summary(temp.glm)

#coeficientes estandarizados
lm.beta::lm.beta(temp.glm)

#D cuadrado
Dsquared(temp.glm)


#CURVAS DE RESPUESTA
#---------------------------------------------------
#curvas de respuesta
x11(width = 15, height = 10)
plotmo::plotmo(
  temp.glm, 
  level = 0.68, 
  all2 = TRUE
)

#otra forma de verlas
x11(width = 15, height = 10)
p1 <- plotInteraction(
  model = temp.glm, 
  data = pb, 
  x = "ndvi_minimum", 
  y = "bio15", 
  z = "presencia", 
  grid = 100, 
  point.size.range = c(0.5, 6), 
  print = TRUE
)



#ALGUNAS CONCLUSIONES:
#ES DIFÍCIL CAPTURAR LA COMPLEJIDAD DE UNA DISTRIBUCIÓN CON SOLO DOS VARIABLES
#AÑADIR INTERACCIONES INCREMENTA LA COMPLEJIDAD DEL MODELO
#AÑADIR TÉRMINOS POLINOMIALES A LAS VARIABLES INCREMENTA LA COMPLEJIDAD DEL MODELO
#INCREMENTOS INNECESARIOS DE COMPLEJIDAD NO AYUDAN A ALCANZAR UN MODELO INTERPRETABLE

rm(temp.glm, temp.glm.mapa)


#VAMOS A TRABAJAR CON TODAS LAS VARIABLES
################################################################
################################################################


#GENERA FÓRMULAS PARA LOS MODELOS DE REGRESIÓN
#---------------------------------------------------
#formula de regresión polinomial de grado 2 sin interacciones
glm.formula <- as.formula(
  paste(
    "presencia ~ poly(",
    paste(sp$variables.seleccionadas,
    collapse=", 2, raw=TRUE) + poly("), 
    ", 2, raw=TRUE)", 
    collapse=""
    )
  )
glm.formula


#AJUSTAMOS MODELO
#---------------------------------------------------
temp.glm <- glm(
  glm.formula,
  family = quasibinomial(link = logit), 
  data = pb, 
  weights = pb.w
)

#coeficientes estandarizados
summary(lm.beta::lm.beta(temp.glm))
#algunas variables no aportan nada:
#segundo término polinomial de ndvi_minimum
#bio15
#segundo término polinomial de bio13
#human footprint

#D cuadrado
Dsquared(temp.glm)


#CURVAS DE RESPUESTA
#---------------------------------------------------
x11(width = 15, height = 10)
plotmo::plotmo(
  temp.glm, 
  level = 0.68, 
  all2 = TRUE
  )
#la curva de respuesta de topo_slope no tiene sentido ecológico


#REHACEMOS LA FÓRMULA DEL MODELO
#---------------------------------------------------
glm.formula <- as.formula(
  presencia ~ 
    ndvi_minimum +
    bio6 +
    poly(landcover_veg_herb, 2, raw = TRUE) +
    topo_slope +
    bio13 +
    human_footprint
  )
glm.formula


#REHACEMOS LA FÓRMULA DEL MODELO
#---------------------------------------------------
temp.glm <- glm(
  glm.formula,
  family = quasibinomial(link = logit), 
  data = pb, 
  weights = pb.w
)

#coeficientes estandarizados
summary(lm.beta::lm.beta(temp.glm))

#D cuadrado
Dsquared(temp.glm)


#CURVAS DE RESPUESTA
#---------------------------------------------------
x11(width = 15, height = 10)
plotmo::plotmo(
  temp.glm, 
  level = 0.68, 
  all2 = TRUE
)

x11(width = 15, height = 10)
p1 <- plotInteraction(
  model = temp.glm, 
  data = pb, 
  x = "ndvi_minimum", 
  y = "bio6", 
  z = "presencia", 
  grid = 100, 
  point.size.range = c(0.5, 6), 
  print = TRUE
)


#PREDICCIÓN A MAPA
#---------------------------------------------------
#prediccion
temp.glm.mapa <- raster::predict(
  object = variables$brick, 
  model = temp.glm, 
  type="response"
)

#dibujamos mapa
x11(width = 15, height = 10)
par(mar = c(0, 0, 0, 0))
raster::plot(
  temp.glm.mapa, 
  col = viridis::viridis(100, direction = -1)
)


#BONUS TRACK: IMPORTANCIA LOCAL DE LAS VARIABLES
#################################################
#################################################
#esta función ajusta un modelo lineal localmente
#el modelo es presencia ~ variables
#se ajusta en un área definida por scale.factor
#scale.factor = 10 indica que el modelo lineal se ajusta en un área de 10x10 píxeles
#se ajustan tantos modelos como ventanas de 10x10 pixeles hay en las variables

#CÁLCULO DE LA IMPORTANCIA LOCAL
#---------------------------------------------------
local.importance <- mapLocalImportance(
  predictors.brick = variables$brick[[sp$variables.seleccionadas]], 
  response.raster = temp.glm.mapa, 
  scale.factor = 6 #mínimo es 5
  )

#qué hay dentro del resultado?
names(local.importance)

#veamos algunos ejemplos
x11(height = 10, width = 15)
par(mfrow = c(1, 2))
plot(local.importance[["bio6_R2_6"]],
     col = colorRampPalette(RColorBrewer::brewer.pal(9, "Blues"))(100),
     main = "R2")
plot(local.importance[["bio6_coef_6"]],
     col = colorRampPalette(RColorBrewer::brewer.pal(9, "RdBu"))(100),
     main = "Coefficient")


#CERRANDO SECCIÓN
#---------------------------------------------------
#guardando modelos
names(temp.glm.mapa) <- "glm"
modelos.brick$glm <- temp.glm.mapa
modelos.list$glm <- temp.glm
modelos.formulas$glm <- glm.formula

#cerrando gráficos y eliminando objetos
graphics.off()
rm(temp.glm, temp.glm.mapa, glm.formula, local.importance)




#####################################
#####################################
#MODELOS ADITIVOS GENERALIZADOS (GAM)
#####################################
#####################################
library(mgcv)

#GAM CON UNA SOLA VARIABLE
#---------------------------------------------------
temp.gam <- mgcv::gam(
  presencia ~ s(ndvi_minimum),
  family = quasibinomial(link = logit), 
  data = pb, 
  weights = pb.w
)

#resultado
lm.beta::lm.beta(temp.gam)

#vemos curva de respuesta sobre densidad
x11(width = 15, height = 10)
plotRespuesta(
  modelo = temp.gam, 
  presencias = pb, 
  variable = "ndvi_minimum"
)


#EL PARÁMETRO k
########################################################
########################################################
k.plots <- list()

#ITERAMOS POR VALORES DE K
#---------------------------------------------------
for(k in 1:16){
  
  #ajustamos modelo
  temp.gam <- mgcv::gam(
    presencia ~ s(ndvi_minimum, k = k),
    family = quasibinomial(link = logit), 
    data = pb, 
    weights = pb.w
  )
  
  #resultado
  lm.beta::lm.beta(temp.gam)
  
  #vemos curva de respuesta sobre densidad
  k.plots[[k]] <- plotRespuesta(
    modelo = temp.gam, 
    presencias = pb, 
    variable = "ndvi_minimum"
  )
  
}

#plotea todo junto
x11(width = 15, height = 10)
cowplot::plot_grid(plotlist = k.plots)



#UN MODELO CON TODAS LAS VARIABLES
####################################################################
####################################################################

#FÓRMULA
#---------------------------------------------------
formula.gam <- as.formula(
  paste(
    "presencia ~ s(", 
    paste(
      sp$variables.seleccionadas, 
      collapse = ") + s("
      ),
    ")", collapse=""
    )
  )
formula.gam
#Fíjate que requiere encapsular las variables en una función llamada "s()" (de "smoothing")
#Esto es un requerimiento de los modelos GAM, 


#AJUSTANDO EL MODELO
#---------------------------------------------------
#select=TRUE cada término puede ser penalizado a 0 si no aporta nada al modelo.
temp.gam <- mgcv::gam(
  formula.gam, 
  family = quasibinomial(link = logit), 
  data = pb, 
  select = TRUE, #regularización
  gamma = 1, #intensidad de la regularización
  weights = pb.w
  )

#resultado
summary(temp.gam)

#R cuadrado
summary(temp.gam)$r.sq

#devianza explicada
summary(temp.gam)$dev.expl

#UBRE - UnBiaed Risk Estimator
#INTERPRETACIÓN: valores más pequeños indican mejores modelos
summary(temp.gam)$sp.criterion


#CURVAS DE RESPUESTA
#---------------------------------------------------
x11(width = 15, height = 10)
plotmo::plotmo(
  temp.gam, 
  level = 0.68, 
  all2 = TRUE,
  type = "response",
  col = viridis(100)
)

x11(width = 15, height = 10)
p1 <- plotInteraction(
  model = temp.gam, 
  data = pb, 
  x = "ndvi_minimum", 
  y = "bio6", 
  z = "presencia", 
  grid = 100, 
  point.size.range = c(0.5, 6), 
  print = TRUE
)



#PREDICCIÓN A MAPA
#---------------------------------------------------
temp.gam.mapa <- raster::predict(
  object = variables$brick, 
  model = temp.gam, 
  type="response"
)

#dibujamos mapa
x11(width = 15, height = 10)
raster::plot(
  temp.gam.mapa,
  col = viridis::viridis(100, direction = -1)
)

#guardando modelos
names(temp.gam.mapa) <- "gam"
modelos.brick$gam <- temp.gam.mapa
modelos.list$gam <- temp.gam
modelos.formulas$gam <- formula.gam



#EFECTO DE LA REGULARIZACIÓN (análisis de sensibilidad)
#######################################################
#######################################################
#vectores de resultados
Gamma <- 1:8
Rsquared <- Dsquare <- UBRE <- rep(NA, length(Gamma))

#iteramos por los valores de G
#---------------------------------------------------
for(Gamma.i in Gamma){
  
  #ajustamos modelo
  temp.gam <- mgcv::gam(
    formula.gam, 
    family = quasibinomial(link = logit), 
    data = pb, 
    select = TRUE,
    gamma = Gamma.i, #cambiamos este valor cada vez!
    weights = pb.w
  )
  
  #escribimos resultados
  Rsquared[Gamma.i] <- summary(temp.gam)$r.sq
  Dsquare[Gamma.i] <- summary(temp.gam)$dev.expl
  UBRE[Gamma.i] <- summary(temp.gam)$sp.criterion
  
}#fin de iteraciones

#a dataframe en formato largo
sensitivity <- tidyr::gather(
  data.frame(Gamma, Rsquared, Dsquare, UBRE), 
  variable, 
  valor, 
  2:4
  )

#plot
x11(width = 15, height = 10)
ggplot(
  data = sensitivity,
  aes(
    x = Gamma, 
    y = valor
    )
  ) + 
  geom_line() + 
  facet_wrap(
    "variable", 
    scales = "free", 
    ncol = 1
    ) + 
  scale_x_continuous(breaks = Gamma)


#CERRANDO SECCIÓN
#---------------------------------------------------
graphics.off()
rm(k.plots, sensitivity, temp.gam, temp.gam.mapa, k, Gamma, Gamma.i, UBRE, Rsquared, Dsquare, formula.gam)




#####################################
#####################################
#MAXENT
#####################################
#####################################
library(maxnet)

#MODELO CON UNA VARIABLE
#####################################
#ajustamos el modelo con sus argumentos por defecto
#nota sobre argumento "data": tiene que ser un dataframe!

#AJUSTE DEL MODELO
#---------------------------------------------------
temp.maxent <- maxnet::maxnet(
  p = pb$presencia, 
  data = data.frame(ndvi_minimum = pb[, "ndvi_minimum"]),
  regmult = 0.01
  )

#qué hay dentro del modelo?
str(temp.maxent)

#coeficientes del predictor y sus transformaciones
data.frame(
  importance = sort(
    abs(temp.maxent$betas)[1:10], 
    decreasing = TRUE
    )
  )


#CURVAS DE RESPUESTA
#---------------------------------------------------
x11(width = 15, height = 10)
plot(temp.maxent, type="cloglog")

x11(width = 15, height = 10)
plotRespuesta(
  modelo = temp.maxent, 
  presencias = pb, 
  variable = "ndvi_minimum"
)


#PREDICCIÓN
#---------------------------------------------------
temp.maxent.mapa <- raster::predict(
  object = variables$brick, 
  model = temp.maxent, 
  type="cloglog"
  )

#dibujamos mapa
x11(width = 15, height = 10)
raster::plot(
  temp.maxent.mapa,
  col = viridis::viridis(100, direction = -1)
)




#MAXENT TIENE UN PARÁMETRO PRAA CONTROLAR LA COMPLEJIDAD DEL MODELO: "regmult"
#EL PARÁMETRO regmult
########################################################
########################################################

#lista para guardar resultados y valores del parámetro a explorar
k.plots <- list()
regmult.values <- seq(
  0.01, 
  15, 
  length.out = 12
  )
regmult.values

#iteramos sobre valores de regmult
#---------------------------------------------------
for(i in 1:length(regmult.values)){
  
  #ajustamos modelo
  temp.maxent <- maxnet::maxnet(
    p = pb$presencia, 
    data = data.frame(ndvi_minimum = pb[, "ndvi_minimum"]),
    regmult = regmult.values[i]
  )
  
  #vemos curva de respuesta sobre densidad
  k.plots[[i]] <- plotRespuesta(
    modelo = temp.maxent, 
    presencias = pb, 
    variable = "ndvi_minimum"
  )
  
}

#plotea todo junto
x11(width = 15, height = 10)
cowplot::plot_grid(plotlist = k.plots)



#MAXENT CON TODAS LAS VARIABLES
##############################################################
##############################################################

#AJUSTE DEL MODELO
#---------------------------------------------------
#ajustamos el modelo con sus argumentos por defecto
#nota sobre argumento "data": tiene que ser un dataframe!
temp.maxent <- maxnet::maxnet(
  p = pb$presencia, 
  data = pb[, sp$variables.seleccionadas],
  regmult = 1
)

#coeficientes del predictor y sus transformaciones
data.frame(
  importance = sort(
    abs(temp.maxent$betas)[1:10], 
    decreasing = TRUE
  )
)


#CURVAS DE RESPUESTA
#---------------------------------------------------
x11(width = 15, height = 10)
p1 <- plotInteraction(
  model = temp.maxent, 
  data = pb, 
  x = "ndvi_minimum", 
  y = "bio6", 
  z = "presencia", 
  grid = 100, 
  point.size.range = c(0.5, 6), 
  print = TRUE
)

#curvas de respuesta
x11(width = 15, height = 10)
plot(temp.maxent, type="cloglog")


#PREDICCIÓN A MAPA
#---------------------------------------------------
temp.maxent.mapa <- raster::predict(
  object = variables$brick, 
  model = temp.maxent, 
  type="cloglog"
)

#dibujamos mapa
x11(width = 15, height = 10)
raster::plot(
  temp.maxent.mapa,
  col = viridis::viridis(100, direction = -1)
)


#CERRANDO SECCIÓN
#---------------------------------------------------
#guardando modelos
names(temp.maxent.mapa) <- "maxent"
modelos.brick$maxent <- temp.maxent.mapa
modelos.list[["maxent"]] <- temp.maxent

#cerrando gráficos y eliminando objetos
graphics.off()
rm(temp.maxent, temp.maxent.mapa, k.plots, i, regmult.values)




#####################################
#####################################
#ÁRBOLES DE PARTICIÓN RECURSIVA
#####################################
#####################################
library(rpart)
library(rpart.plot)
#https://cran.r-project.org/web/packages/rpart/vignettes/longintro.pdf


#MODELO CON TRES VARIABLES (hacerlo con una no tiene sentido)
#---------------------------------------------------
temp.rpart <- rpart(
  formula = presencia ~ bio6 + ndvi_minimum + topo_slope,
  data = pb,
  weights = pb.w,
  control = rpart.control(minbucket = 10)
  )

#resumen
summary(temp.rpart)
#Variable importance

#ploteamos el árbol
x11(width = 15, height = 10)
rpart.plot(
  temp.rpart, 
  type = 5, 
  box.palette = viridis::viridis(
    n = 100, 
    alpha = 0.6, 
    direction = -1,
    begin = 0.1),
  cex = 1.5
  )


#SUPERFICIES DE RESPUESTA
#---------------------------------------------------
p1 <- plotInteraction(
  model = temp.rpart, 
  data = pb, 
  x = "ndvi_minimum", 
  y = "bio6", 
  z = "presencia", 
  grid = 100, 
  point.size.range = c(0.5, 6), 
  print = FALSE
  )

p2 <- plotInteraction(
  model = temp.rpart, 
  data = pb, 
  x = "topo_slope", 
  y = "bio6", 
  z = "presencia", 
  grid = 100, 
  point.size.range = c(0.5, 6), 
  print = FALSE
)

p3 <- plotInteraction(
  model = temp.rpart, 
  data = pb, 
  x = "ndvi_minimum", 
  y = "topo_slope", 
  z = "presencia", 
  grid = 100, 
  point.size.range = c(0.5, 6), 
  print = FALSE
)

x11(width = 15, height = 10)
cowplot::plot_grid(p1, p2, p3, nrow = 1)



#PREDICCIÓN A MAPA
#---------------------------------------------------
temp.rpart.mapa <- predict(
  object = variables$brick, 
  model = temp.rpart
)

#dibujamos mapa
x11(width = 15, height = 10)
raster::plot(
  temp.rpart.mapa,
  col = viridis::viridis(
    n = 100,
    direction = -1
    )
  )



#CON TODAS LAS VARIABLES
########################################################
########################################################

#FÓRMULA
#---------------------------------------------------
formula.rpart <- as.formula(
  paste(
    "presencia ~ ", 
    paste(
      sp$variables.seleccionadas, 
      collapse = " + "
    )
    , collapse=""
  )
)
formula.rpart


#AJUSTE DEL MODELO
#---------------------------------------------------
temp.rpart <- rpart(
  formula = formula.rpart,
  data = pb[, c("presencia", sp$variables.seleccionadas)],
  weights = pb.w,
  control = rpart.control(minbucket = 10)
)


x11(width = 10, height = 6)
rpart.plot(
  temp.rpart, 
  type = 5, 
  box.palette = viridis::viridis(
    n = 100, 
    alpha = 0.6, 
    direction = -1,
    begin = 0.1),
  cex = 1
)


#PREDICCIÓN A MAPA
#---------------------------------------------------
temp.rpart.mapa <- dismo::predict(
  object = variables$brick, 
  model = temp.rpart,
  type = "prob"
)

#dibujamos mapa
x11(width = 15, height = 10)
raster::plot(
  temp.rpart.mapa,
  col = viridis::viridis(
    n = 100,
    direction = -1
  )
)


#CERRANDO SECCIÓN
#---------------------------------------------------
#guardando modelos
names(temp.rpart.mapa) <- "rpart"
modelos.brick$rpart <- temp.rpart.mapa
modelos.list$rpart <- temp.rpart
modelos.formulas$rpart <- formula.rpart

#cerrando gráficos y eliminando objetos
graphics.off()
rm(p1, p2, p3, temp.rpart, temp.rpart.mapa, formula.rpart)



#####################################
#####################################
#RANDOM FOREST
#####################################
#####################################
library(ranger)
library(pdp) #curvas de respuesta: https://bgreenwell.github.io/pdp/articles/pdp.html


#FÓRMULA
#---------------------------------------------------
formula.rf <- as.formula(
  paste(
    "presencia ~ ", 
    paste(
      sp$variables.seleccionadas, 
      collapse = " + "
    )
    , collapse=""
  )
)
formula.rf


#MODELO CON TODAS LAS VARIABLES
#---------------------------------------------------
temp.rf <- ranger::ranger(
  formula = formula.rf,
  data = pb,
  case.weights = pb.w,
  num.trees = 5000,
  min.node.size = 40, #<- parámetro importante
  mtry = 3,           #<- parámetro importante
  importance = "permutation", 
  scale.permutation.importance = TRUE
)


#ANÁLISIS DE SENSIBILIDAD A ESOS PARÁMETROS
#---------------------------------------------------
#Random Forest tiene 2 parámetros clave
#mtry: variables seleccionadas en cada split
#min.node.size: número de casos en un nodo terminal
#vamos a ver como afectan al modelo
p <- sensibilidadRF(
  formula = formula.rf,
  presencias =  pb,
  tipo = "background",
  pesos = pb.w,
  mtry.list = 1:length(sp$variables.seleccionadas),
  mns.list = c(10, 20, 30, 40, 50),
  num.trees = 5000
)


#REPETIMOS EL MODELO CON LOS PARÁMETROS ELEGIDOS
#---------------------------------------------------
#repetimos el modelo con los parámetros que más incrementan R-squared
temp.rf <- ranger::ranger(
  formula = formula.rf,
  data = pb,
  case.weights = pb.w,
  num.trees = 5000,
  min.node.size = 20,
  mtry = 2, 
  importance = "permutation", 
  scale.permutation.importance = TRUE
)

#resultado
print(temp.rf)

#R-cuadrado (obtenido del out-of-bag)
#es irrelevante cuando el modelo se entrena con background con pesos
#pero funciona correctamente cuando se usan pseudoausencias
temp.rf$r.squared

#miramos un árbol por dentro
treeInfo(temp.rf, tree=1)


#IMPORTANCIA DE LAS VARIABLES
#---------------------------------------------------
#está guardada en el modelo
temp.rf$variable.importance 

#lo reordenamos
importancia <- sort(
  temp.rf$variable.importance,
  decreasing = TRUE
)

#a dataframe
importancia <- data.frame(
  variable = names(importancia),
  importancia = importancia
)
row.names(importancia) <- NULL
importancia

#ploteamos la importancia de las variables
x11(width = 10, height = 10)
ggplot(
  data = importancia, 
  aes(
    x = variable, 
    y = importancia,
    fill = importancia)
)+ 
  geom_bar(
    stat="identity", 
    position="dodge"
  ) + 
  viridis::scale_fill_viridis(direction = -1) +
  theme(legend.position = "none") +
  coord_flip()


#BONUS TRACK: importancia de las variables para una observación concreta con breakDown (link to paper: https://arxiv.org/pdf/1804.01955.pdf)
##################################################################################
##################################################################################
#función para predecir probabilidad
predict.rf <- function(model, new_observation){
  predict(model, new_observation, type = "response")$predictions
}

#elegimos una presencia por su índice
presencia.i <- 31

#predicción de probabilidad para una presencia
prediccion.presencia.1 <- predict.rf(
  model = temp.rf,
  new_observation = pb[presencia.i, ]
)
prediccion.presencia.1

#importancia de las variables
explicacion.presencia.1 <- breakDown::broken(
  model = temp.rf,
  new_observation = pb[presencia.i, ],
  data = pb[, sp$variables.seleccionadas],
  predict.function = predict.rf,
  baseline = "intercept"
)
x11(height = 10, width = 15)
plot(explicacion.presencia.1) + theme(text = element_text(size = 40))


#CURVAS DE RESPUESTA PARA UNA VARIABLE
#---------------------------------------------------
x11(width = 10, height = 10)
ggplot2::autoplot(
  pdp::partial(
    temp.rf, 
    pred.var = "ndvi_minimum"
  ), 
  ylab = "Response"
) 


#CURVAS DE RESPUESTA PARA TODAS LAS VARIABLES
#---------------------------------------------------
plot.list <- list()
for(variable.i in sp$variables.seleccionadas){
  plot.list[[variable.i]] <- ggplot2::autoplot(
    pdp::partial(
      temp.rf, 
      pred.var = variable.i
    ), 
    ylab = "Response"
  ) 
}
x11(width = 15, height = 10)
cowplot::plot_grid(plotlist = plot.list)


#INTERACCIÓN ENTRE VARIABLES
#---------------------------------------------------
x11(width = 15, height = 10)
plotInteraction(
  model = temp.rf, 
  data = pb, 
  x = "ndvi_minimum", 
  y = "bio6", 
  z = "presencia", 
  grid = 100, 
  point.size.range = c(0.5, 6), 
  print = FALSE
)


#PREDICCIÓN A MAPA
#---------------------------------------------------
temp.rf.mapa <- raster::predict(
  variables$brick, 
  temp.rf, 
  type = 'response', 
  fun = function(temp.rf, ...) predict(temp.rf, ...)$predictions #<- OJO CON ESTO!!
)

#plotear
x11(width = 10, height = 10)
raster::plot(
  temp.rf.mapa,
  col = viridis::viridis(
    n = 100,
    direction = -1
  )
)


#CERRANDO SECCIÓN
#---------------------------------------------------
#guardando modelos
names(temp.rf.mapa) <- "rf"
modelos.brick$rf <- temp.rf.mapa
modelos.list$rf <- temp.rf
modelos.formulas$rf <- formula.rf

#cerrando gráficos y eliminando objetos
graphics.off()
rm(temp.rf, temp.rf.mapa, plot.list, variable.i, importancia, formula.rf, explicacion.presencia.1, prediccion.presencia.1, presencia.i)

#VAMOS A GUARDAR LOS MODELOS EN LA LISTA sp
sp$modelos$formulas <- modelos.formulas
sp$modelos$mapas <- modelos.brick
sp$modelos$modelos <- modelos.list
rm(modelos.formulas, modelos.brick, modelos.list, p)




#BONUS TRACK: MODELO RANDOM FOREST PARA VARIAS ESPECIES
########################################################
########################################################
#Datos de presencia de varias especies de Quercus en Europa tomados de la base de datos EU-forest
load("data/quercus.RData")
str(quercus)

#CUANTAS PRESENCIAS TENEMOS DE CADA ESPECIE? 
quercus.n <- data.frame(table(quercus$species))
names(quercus.n) <- c("species", "n")
quercus.n

#CREACIÓN DE PESOS
quercus.n$pesos <- nrow(quercus) / quercus.n$n

#unimos los pesos a la tabla de presencias
quercus <- merge(quercus, quercus.n[, c("species", "pesos")], by = "species")

#AÑADIMOS LOS VALORES DE LAS VARIABLES
quercus <- na.omit(
  data.frame(
    quercus,
    raster::extract(
      x = variables$brick,
      y = quercus[, c("x", "y")],
      df = TRUE
    )
  )
)

#vemos la estructura de la tabla
str(quercus)

#preparamos la fórmula
#formula rpart
quercus.formula <- as.formula(
  paste(
    "species ~ ", 
    paste(
      sp$variables.seleccionadas, 
      collapse = " + "
    )
    , collapse=""
  )
)
quercus.formula

#AJUSTE DEL MODELO
quercus.rf <- ranger::ranger(
  formula = quercus.formula,
  data = quercus,
  case.weights = quercus$pesos,
  probability = TRUE,
  importance = "permutation", 
  scale.permutation.importance = TRUE
)

#miramos el error de predicción
quercus.rf

#importancia de las variables
sort(
  quercus.rf$variable.importance,
  decreasing = TRUE
)

#prediciendo la probabilidad de cada clase
quercus.prob = predict(
  object = quercus.rf, 
  data = na.omit(
    as.data.frame(
      variables$brick
    )
  )
)$predictions

#curvas de respuesta
#dataframe para guardar resultado
quercus.curves <- NULL

#variable a plotear
variable <- "bio6"

#iterando por cada clase
for(i in 1:length(colnames(quercus.prob))){
  
  #partial dependence plot
  pdp.temp <- pdp::partial(
    object = quercus.rf, 
    type = "classification",
    prob = TRUE,
    pred.var = variable,
    which.class = i, 
    grid.resolution = 20, 
    train = quercus
    )
  
  #cambiamos nombre
  colnames(pdp.temp) <- c("variable.valor", "probabilidad")
  
  #a dataframe
  pdp.temp$species <- colnames(quercus.prob)[i]
  pdp.temp$variable.nombre <- variable
  quercus.curves <- rbind(quercus.curves, pdp.temp)
}

#plot
x11(height = 10, width = 15)
ggplot(
  quercus.curves, 
  aes(
    x = variable.valor, 
    y = probabilidad, 
    color = probabilidad,
    group = species
    )
) +
  geom_line(size = 1) + 
  viridis::scale_color_viridis(direction = -1) +
  facet_wrap("species", scales = "free_y", ncol = 1) + 
  ylab("Probability")


#les ponemos coordenadas a esas probabilidades
#por qué funciona esto? el dataframe de probabilidades tiene el mismo orden que las celdas de variables$brick que tienen valores válidos
quercus.prob <- data.frame(
  na.omit(
    raster::as.data.frame(
      variables$brick, xy = TRUE
      )
    )[, c("x", "y")],
  quercus.prob
)

#lo pasamos a SpatialPointsDataFrame
sp::coordinates(quercus.prob) <- ~ x + y
sp::gridded(quercus.prob) <- TRUE

#lo pasamos a un brick
quercus.brick <- raster::brick(quercus.prob)

#ploteamos
x11(height = 10, width = 15)
plot(quercus.brick, col = viridis::viridis(100, direction = -1))

#mapa con el máximo en cada celda
quercus.brick.max <- raster::which.max(quercus.brick)

#le ponemos una paleta de color más intuitiva
#creamos una paleta con 8 colores
paleta.color <- rev(RColorBrewer::brewer.pal(
  n = length(names(quercus.brick)),
  name = "Accent"
))

#plot con leyenda
x11(width = 15, height = 10)
raster::plot(
  quercus.brick.max, 
  col = paleta.color, 
  legend = FALSE
)
legend(
  "bottomleft", 
  legend = names(quercus.brick), 
  pch = 15, 
  col = paleta.color, 
  cex = 1, 
  border = ""
)


rm(quercus, quercus.brick, quercus.brick.max, quercus.curves, quercus.formula, quercus.rf, quercus.n, quercus.prob, pdp.temp, i, paleta.color, variable)




##################################
##################################
#EVALUACION DE MODELOS
##################################
##################################

###########################################################
#CALCULANDO AUC CON BOOTSTRAP
###########################################################

#PARÁMETROS GENERALES DE LA EVALUACIÓN
#lista para guardar resultados de evaluacion
sp$evaluacion$parametros <- list(
  repeticiones = 10, 
  porcentaje.evaluacion = 40, 
  radio.seleccion.ausencias = 5
  )

#EVALUACION BIOCLIM
#-----------------------------------
sp$evaluacion$bioclim <- bootstrapSDM(
  brick = variables$brick, 
  presencias = pb, 
  respuesta = "presencia", 
  predictores = sp$variables.seleccionadas, 
  formula.modelo = NULL, 
  modelo.nombre = "bioclim", 
  repeticiones = sp$evaluacion$parametros$repeticiones, 
  porcentaje.evaluacion = sp$evaluacion$parametros$porcentaje.evaluacion,
  radio.seleccion.ausencias = sp$evaluacion$parametros$radio.seleccion.ausencias
)

#la función devuelve varios objetos:
#1. un brick con todos los mapas de los modelos calibrados
evaluacion$bioclim$mapas
x11()
plot(
  evaluacion$bioclim$mapas, 
  col = viridis::viridis(100, direction = -1)
)

#2. una lista con los modelos
sp$evaluacion$bioclim$modelos$glm_1
summary(sp$evaluacion$bioclim$modelos$glm_1)
plotmo(sp$evaluacion$bioclim$modelos$glm_1)

#3. un dataframe con los valores de AUC, Boyce index, y el umbral de corte (threshold)
evaluacion$bioclim$evaluacion
plot(density(sp$evaluacion$bioclim$evaluacion$auc))
plot(density(sp$evaluacion$bioclim$evaluacion$boyce))
plot(density(sp$evaluacion$bioclim$evaluacion$threshold))


#EVALUACION GLM
#---------------------------------------
sp$evaluacion$glm <- bootstrapSDM(
  brick = variables$brick, 
  presencias = pb, 
  respuesta = "presencia", 
  predictores = sp$variables.seleccionadas, 
  formula.modelo = sp$modelos$formulas$glm, 
  modelo.nombre = "glm", 
  repeticiones = sp$evaluacion$parametros$repeticiones, 
  porcentaje.evaluacion = sp$evaluacion$parametros$porcentaje.evaluacion,
  radio.seleccion.ausencias = sp$evaluacion$parametros$radio.seleccion.ausencias
)

rm(glm.formula)


#EVALUACION GAM (lleva un rato largo)
#---------------------------------------
sp$evaluacion$gam <- bootstrapSDM(
  brick = variables$brick, 
  presencias = pb, 
  respuesta = "presencia", 
  predictores = sp$variables.seleccionadas, 
  formula.modelo = NULL, 
  modelo.nombre = "gam", 
  repeticiones = sp$evaluacion$parametros$repeticiones, 
  porcentaje.evaluacion = sp$evaluacion$parametros$porcentaje.evaluacion,
  radio.seleccion.ausencias = sp$evaluacion$parametros$radio.seleccion.ausencias
)


#MAXENT
#---------------------------------------
sp$evaluacion$maxent <- bootstrapSDM(
  brick = variables$brick, 
  presencias = pb, 
  respuesta = "presencia", 
  predictores = sp$variables.seleccionadas, 
  formula.modelo = NULL, 
  modelo.nombre = "maxent", 
  repeticiones = sp$evaluacion$parametros$repeticiones, 
  porcentaje.evaluacion = sp$evaluacion$parametros$porcentaje.evaluacion,
  radio.seleccion.ausencias = sp$evaluacion$parametros$radio.seleccion.ausencias
)


#RANDOM FOREST
#---------------------------------------
sp$evaluacion$rf <- bootstrapSDM(
  brick = variables$brick, 
  presencias = pb, 
  respuesta = "presencia", 
  predictores = sp$variables.seleccionadas, 
  formula.modelo = NULL, 
  modelo.nombre = "rf", 
  repeticiones = sp$evaluacion$parametros$repeticiones, 
  porcentaje.evaluacion = sp$evaluacion$parametros$porcentaje.evaluacion,
  radio.seleccion.ausencias = sp$evaluacion$parametros$radio.seleccion.ausencias,
  mtry = 2,
  min.node.size = 20,
  num.trees = 5000
)


#¿CUAL ES EL MODELO CON MAYOR AUC?
##################################################################################
##################################################################################
#para contestarla, unimos todos los dataframes "evaluacion" en una sola tabla
sp$evaluacion$tabla.evaluacion <- do.call(
  "rbind", 
  purrr::map(
    sp$evaluacion[1:length(sp$evaluacion)], 
    "evaluacion"
    )
  )

#a formato largo
temp.df.long <- tidyr::gather(
  sp$evaluacion$tabla.evaluacion , 
  medida, 
  valor, 
  c("auc", "boyce")
  )

#y hacemos un boxplot con los resultados
sp$evaluacion$boxplot <- ggplot(data = temp.df.long,
       aes(
         x = modelo,
         y = valor,
         group = modelo,
         fill = modelo
         )
       ) + 
  geom_boxplot(notch = TRUE, alpha = 0.5) +
  viridis::scale_fill_viridis(discrete = TRUE) + 
  theme(legend.position = "none",
        text = element_text(size = 40),
        axis.text = element_text(size = 40),
        plot.margin = unit(c(2,2,2,2), "lines"),
        panel.grid.major.x = element_line()) + 
  coord_flip() + 
  facet_wrap("medida", scales = "free_x") + 
  xlab("") + 
  ylab("")
x11(width = 15, height = 10)
sp$evaluacion$boxplot

rm(temp.df.long)


#CORRESPONDE AUC CON LA CAPACIDAD DEL MODELO PARA PREDECIR LA DISTRIBUCIÓN DE LA ESPECIE?
############################################################################################
############################################################################################
#tenemos los bricks con los mapas, los valores de auc, y el mapa de distribución "real" de la especie virtual.
#vamos a calcular la correlación entre cada mapa y el mapa "real" de la especie virtual
#y comparar esos valores de correlación con AUC

#creamos nuevo campo en evaluacion.auc
sp$evaluacion$tabla.evaluacion$cor <- NA

#valores de las presencias en el mapa de distribución "real"
idoneidad.real <- as.vector(raster::extract(
    x = sp$nicho.mapa$suitab.raster,
    y = sp$xy
    )
  )

#iteramos por líneas de evaluacion.auc
#---------------------------------------
for(i in 1:nrow(sp$evaluacion$tabla.evaluacion)){
  
  #modelo
  modelo <- sp$evaluacion$tabla.evaluacion[i, "modelo"]
  id <- sp$evaluacion$tabla.evaluacion[i, "id"]
  
  #mapas a vector
  idoneidad.predicha <- as.vector(
    raster::extract(
      x = sp$evaluacion[[modelo]]$mapas[[id]],
      y = sp$xy
      )
    )
  
  #calcula correlación
  sp$evaluacion$tabla.evaluacion[i, "cor"] <- cor(
    idoneidad.real, 
    idoneidad.predicha
    )
}

#dataframe temporal para hacer el plot
temp.resultados.individuales <- tidyr::gather(
  sp$evaluacion$tabla.evaluacion, 
  key = evaluacion, 
  value = evaluacion.valor, 
  c("auc", "boyce")
  )

#aggregamos por grupo
temp.resultados.medios <- sp$evaluacion$tabla.evaluacion %>% 
  group_by(modelo) %>% 
  summarise(auc = mean(auc),
            boyce  = mean(boyce),
            cor = mean(cor)
            )
temp.resultados.medios <- tidyr::gather(
  temp.resultados.medios,
  key = evaluacion, 
  value = evaluacion.valor, 
  c("auc", "boyce")
  )

#y hacemos un boxplot con los resultados
evaluacion.auc.vs.cor <- ggplot(data = temp.resultados.individuales,
                                 aes(
                                   x = evaluacion.valor,
                                   y = cor,
                                   group = modelo,
                                   fill = modelo
                                 )
                                ) + 
  geom_point(
    alpha = 0.3, 
    size = 7, 
    shape = 21
    ) +
  geom_point(data = temp.resultados.medios,
    alpha = 1, 
    size = 10, 
    shape = 21
  ) +
  viridis::scale_fill_viridis(discrete = TRUE, alpha = 0.5) + 
  theme(text = element_text(size = 40),
        axis.text = element_text(size = 40),
        panel.grid.major = element_line()
        ) + 
  facet_wrap("evaluacion", scales = "free_x")
x11(width = 15, height = 10)
evaluacion.auc.vs.cor



#cerrando sesión
#-------------------------
graphics.off()
rm(temp.resultados.individuales, temp.resultados.medios, evaluacion.auc.vs.cor, i, modelo, id, pesos, idoneidad.predicha, idoneidad.real)


#EVALUACIÓN CON BLOQUES
###################################################################
###################################################################
#vignette: http://htmlpreview.github.io/?https://github.com/rvalavi/blockCV/blob/master/vignettes/BlockCV_for_SDM.html
#paper: https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/2041-210X.13107
library(blockCV)

#hay que convertir los datos de presencia a SpatialPointsDataFrame
pb.spdf <- sp::SpatialPointsDataFrame(
  coords = pb[, c("x", "y")], #las coordenadas
  data = pb, #los datos
  proj4string = raster::crs(variables$brick) #el sistema de referencia
  )

#generamos bloques
#------------------------------
pb.blocks <- spatialBlock(
  speciesData = pb.spdf,
  species = "presencia",
  rasterLayer = variables$brick,
  rows = 5,
  cols = 6,
  k = 5,
  selection = "random",
  maskBySpecies = TRUE
  )

#extraemos los grupos
grupos <- pb.blocks$folds

#son indices de presencias y background correspondientes con los bloques
grupos
length(grupos)


#evaluación de un modelo maxnet
#---------------------------------
#vector para guardar valores de auc
output.auc <- vector()

#iteramos por los grupos
for(grupo.i in 1:length(grupos)){
  
  #indices de los datos de entrenamiento
  set.entrenamiento <- unlist(grupos[[grupo.i]][1])
  
  #indices de los datos de evaluación
  set.evaluacion <- unlist(grupos[[grupo.i]][2])
  
  #ajuste del modelo
  modelo.i <- maxnet::maxnet(
    p = pb$presencia[set.entrenamiento], 
    data = pb[set.entrenamiento, sp$variables.seleccionadas],
    regmult = 1
  )
  
  #predice mapa
  mapa.i <- raster::predict(
    object = variables$brick, 
    model = modelo.i, 
    type="cloglog"
  )
  
  #extrae valores del modelo sobre presencias y ausencias de evaluación
  mapa.i.valores <- data.frame(
    presencia = pb[set.evaluacion, "presencia"],
    idoneidad = raster::extract(
      x = mapa.i,
      y = pb[set.evaluacion, c("x", "y")]
    )
  )
  
  #evalúa modelo
  evaluacion.i <- dismo::evaluate(
    p = mapa.i.valores[mapa.i.valores$presencia == 1, "idoneidad"], 
    a = mapa.i.valores[mapa.i.valores$presencia == 0, "idoneidad"]
  )
  
  #extrae auc
  output.auc[grupo.i] <- evaluacion.i@auc
  
}

#promedio y desviación de AUC
mean(output.auc)
sd(output.auc)

#los comparamos con el que ya tenemos
mean(sp$evaluacion$maxent$evaluacion$auc)
sd(sp$evaluacion$maxent$evaluacion$auc)


#cerrando sesión
rm(evaluacion.i, grupos, mapa.i, mapa.i.valores, modelo.i, pb.blocks, pb.spdf, grupo.i, output.auc, set.entrenamiento, set.evaluacion)

#######################################################################################
#######################################################################################
#ENSAMBLADO DE MODELOS
#######################################################################################
#######################################################################################


#SIMILITUD ENTRE LOS MODELOS
#-----------------------------------
#transforma los modelos en dataframe eliminando nulos
modelos.df <- na.omit(
  raster::as.data.frame(
    sp$modelos$mapas
  )
)

#calcula matriz de correlaciones
modelos.cor <- cor(modelos.df)

#transforma matriz de correlación en distancias
modelos.dis <- abs(
  as.dist(
    modelos.cor
  )
)

#dibuja el árbol de correlación
x11(width = 15, height = 10)
plot(
  as.dendrogram(
    hclust(
      1 - modelos.dis
    )
  ), horiz=T
)



#ENSAMBLADO MEDIANTE PROMEDIO
#---------------------------------------
ensamblado.media <- raster::calc(
  x = sp$modelos$mapas, 
  fun = mean
)

#DESVIACIÓN ESTÁNDAR
ensamblado.desviacion <- raster::calc(
  x = sp$modelos$mapas, 
  fun = sd
)

#PLOT
x11(width = 15, height = 10)
par(mfrow=c(1,2), mar=c(1,1,1,1), oma=c(1,1,1,1))
raster::plot(
  ensamblado.media, 
  main = "Media",
  col = viridis::viridis(100)
)
raster::plot(
  ensamblado.desviacion, 
  main = "Desviación", 
  col = viridis::viridis(100, option = "A")
)

#PODEMOS FUNDIR AMBOS MAPAS
x11(width = 15, height = 10)
plotEnsamblado(
  media = ensamblado.media, 
  desviacion = ensamblado.desviacion
)

#BORRAMOS OBJETOS QUE YA NO VAMOS A USAR
graphics.off()
rm(ensamblado.desviacion, ensamblado.media, modelos.df, modelos.cor, modelos.dis)
gc()



#ENSAMBLANDO MEDIANTE MEDIA PONDERADA
#---------------------------------------
#TENEMOS LOS VALORES DE AUC POR MODELO
#los promediamos
auc.promedio <- sp$evaluacion$tabla.evaluacion %>% 
  group_by(modelo) %>% 
  summarise(auc = mean(auc))

#reescalamos los valores entre 0 y 1 para que el peor modelo tenga peso 0, y el mejor tenga peso 1.
auc.promedio$auc.w <- escalaPesos(
  pesos = auc.promedio$auc, 
  new.max = 1, 
  new.min = 0, 
  old.max = max(auc.promedio$auc), 
  old.min = min(auc.promedio$auc)
  )
auc.promedio$auc.w

#con esos datos podemos crear un ensamblado con media ponderada con la función raster::weighted.mean
ensemble.ponderado <- raster::weighted.mean(
  x = sp$modelos$mapas[[auc.promedio$modelo]], 
  w = auc.promedio$auc.w
  )
x11(height = 10, width = 10)
raster::plot(
  ensemble.ponderado,
  col = viridis::viridis(100, direction = -1)
)

#el AUC de este ensamblado es la media ponderada de los AUC originales y sus pesos
ensemble.auc <- weighted.mean(
  x = auc.promedio$auc,
  w = auc.promedio$auc.w
)
ensemble.auc

#guardamos el ensamblado en sp
sp$modelos$modelos$ensamblado <- ensemble.ponderado
sp$evaluacion$auc.ensamblado <- ensemble.auc

rm(ensemble.ponderado, ensemble.auc, auc.promedio)


#APLICANDO THRESHOLDS
##############################################
##############################################
#ya vimos que la función de evaluación devuelve una columna llamada threshold
sp$evaluacion$tabla.evaluacion

#esos valores están en unidades de idoneidad, y determinan el valor de la idoneidad para el que la proporción de presencias detectadas por el modelo (TRUE POSITIVE RATE, TPR) iguala a la proporción de ausencias detectadas por el modelo (TRUE NEGATIVE RATE, TNR).
#si calculamos la media ponderada por auc de esos valores
threshold.promedio <- sp$evaluacion$tabla.evaluacion %>%
  group_by(modelo) %>% 
  summarise(threshold.medio = weighted.mean(threshold, auc)) %>% 
  as.data.frame()

#ploteamos los modelos con threshold
x11(width = 15, height = 10)
par(mfrow=c(2, 3))

#---------------------------------------
for(i in 1:nrow(threshold.promedio)){
  
  #extrae mapa y threshold
  map <- sp$modelos$mapas[[threshold.promedio[i, "modelo"]]]
  threshold <- threshold.promedio[i, "threshold.medio"]
  
  #plotea mapa
  raster::plot(
    map >= threshold,
    col = viridis(2, direction = -1),
    main = threshold.promedio[i, "modelo"],
    )
}

rm(map, threshold.promedio, threshold, i)


######################################################################################
######################################################################################
#PROYECCIÓN EN EL ESPACIO
######################################################################################
######################################################################################

#IMPORTAMOS LAS VARIABLES DE NORTEAMÉRICA
######################################################
load("data/variables_norteamerica.RData")

#plot
x11(width = 15, height = 10)
plot(variables.na)


#ANÁLISIS MESS (multivariate environmental similarity surfaces, Elith et al. 2010)
#---------------------------------------
#UNA PREGUNTA IMPORTANTE: EXISTEN LAS CONDICIONES REQUERIDAS POR LA ESPECIE EN LA REGIÓN A LA QUE VAMOS A PROYECTAR EL MODELO?
#necesita registros de presencia con los valores de las variables, y un brick con las variables de la región de destino
sp$proyeccion$mess <- dismo::mess(
  x = variables.na[[sp$variables.seleccionadas]], 
  v = sp$presencia[, sp$variables.seleccionadas], 
  full=TRUE
  )

#nombres para los mapas contenidos en mes
names(sp$proyeccion$mess) <- c(sp$variables.seleccionadas, "mess")

#ploteamos el resultado del análisis
x11(width = 15, height = 10)
raster::plot(
  sp$proyeccion$mess, 
  col = viridis::viridis(100, option = "A", direction = -1)
  )

#INTERPRETACIÓN: VALORES MAYORES QUE 20 Y MENORES QUE -20 INDICAN COMBINACIONES DE CONDICIONES AMBIENTALES NO REPRESENTADAS EN LAS PRESENCIAS DE ENTRENAMIENTO.
#ploteamos el resultado

#reclasificamos valores entre -20 y 20
sp$proyeccion$mess.reclass <- raster::reclassify(
  x = sp$proyeccion$mess,
  rcl = matrix(c(-20, 20, 1,
                 -Inf, -20, 0,
                 20, Inf, 0), byrow = TRUE, ncol = 3)
  )

x11(height = 10, width = 15)
raster::plot(
  sp$proyeccion$mess.reclass, 
  col = viridis::viridis(2, direction = -1)
  )

#hacemos un polígono con este mess reclass
sp$proyeccion$mess.reclass.poligono <- rasterToPolygons(
  sp$proyeccion$mess.reclass$mess, 
  fun = function(x){x == 1}, 
  dissolve = TRUE
  )
plot(sp$proyeccion$mess.reclass.poligono)


#QUÉ VARIABLE TIENE EL MESS MÁS EXTREMO EN CADA LUGAR?
#---------------------------------------
sp$proyeccion$mess.maximo <- raster::which.max(abs(sp$proyeccion$mess[[sp$variables.seleccionadas]]))
sp$proyeccion$mess.maximo <- mask(sp$proyeccion$mess.maximo, mask = variables.na[[1]])

#le ponemos una paleta de color más intuitiva
#creamos una paleta con 8 colores
paleta.color <- RColorBrewer::brewer.pal(
  n = length(sp$variables.seleccionadas),
  name = "Set1"
  )

#plot con leyenda
x11(width = 15, height = 10)
raster::plot(
  sp$proyeccion$mess.maximo, 
  col = paleta.color, 
  legend = FALSE
  )
legend(
  "bottomleft", 
  c(sp$variables.seleccionadas), 
  pch = 15, 
  col = paleta.color, 
  cex = 1, 
  border = ""
  )

rm(paleta.color)



#PROYECCIÓN DE MODELOS
#######################
#######################

#solo hay que predecir sobre las variables nuevas
maxent.na <- predict(variables.na, sp$modelos$modelos$maxent, type = "cloglog")

#ploteamos la proyección
x11(width = 30, height = 20)
par(mfrow = c(1, 2))
plot(
  maxent.na, 
  col = viridis(100, direction = -1),
  main="Bioclim (Norte América)"
  )
lines(sp$proyeccion$mess.reclass.poligono) #líneas del mess
plot(
  sp$modelos$mapas$maxent, 
  col = viridis(100, direction = -1), 
  main="Bioclim (Europa)"
  )
#ESTAMOS EXTRAPOLANDO SOBRE COMBINACIONES NÓVELES DE LAS VARIABLES EN LA COSTA DE CANADÁ Y ALASKA!

#multiplicamos el mapa proyectado por el mess reclasificado
maxent.na <- maxent.na * sp$proyeccion$mess.reclass$mess

#ploteamos de nuevo
x11(width = 15, height = 10)
par(mfrow = c(1, 2))
plot(
  maxent.na, 
  col = viridis(100, direction = -1),
  main="Bioclim (Norte América)"
)
lines(sp$proyeccion$mess.reclass.poligono)
plot(
  sp$modelos$mapas$maxent, 
  col = viridis(100, direction = -1), 
  main="Bioclim (Europa)"
)


#cerrando sección
#---------------------------
graphics.off()
rm(maxent.na)


