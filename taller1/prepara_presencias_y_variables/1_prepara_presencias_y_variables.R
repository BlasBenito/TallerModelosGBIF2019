
# 1 INSTALACIÓN Y CARGA DE PAQUETES -----------------------------------------
#NOTA: SOLO ES NECESARIO INSTALARLOS UNA VEZ. DESACTIVA ESTAS LÍNEAS PARA LA PRÓXIMA SESIÓN
#INSTALA PAQUETE DISMO Y TODAS SUS DEPENDENCIAS (EJECUTAR UNA SOLA VEZ)
#install.packages(c("rgeos", "HH", "rgbif", "sf", "magrittr", "gistr", "leaflet", "ALA4R", "ape", "geosphere", "ggdendro"), dep=TRUE)

#AJUSTA LA CARPETA DE TRABAJO A LA LOCALIZACIÓN DE ESTE SCRIPT
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#CARGA LAS LIBRERIAS NECESARIAS (EJECUTAR SIEMPRE QUE TRABAJES CON EL SCRIPT)
library(ggplot2)
library(cowplot)
theme_set(theme_cowplot())
library(magrittr)
library(leaflet)
library(dplyr)
library(raster)
library(SDMworkshop)
source("funciones.R")


# 2 PREPARACIÓN DE LAS PRESENCIAS -------------------------------------------

# * 2.1 Importa variables predictivas ---------------------------------------

#descromprime variables
unzip("./1_variables.zip", exdir="./variables", junkpaths=TRUE)
#ahora todas las variables están en el directorio variables

#lista para guardar la info relacionada con las variables
variables <- list()

#IMPORTANDO LOS FICHEROS .asc
variables$brick <- SDMworkshop::importASC(
  folder = "variables",
  crs = "+init=epsg:4326",
  to.memory = TRUE
)

#RESOLUCION DE LAS VARIABLES
variables$resolucion.km <- raster::xres(variables$brick)*111.19
variables$resolucion.km

#plotea una variable
plotVariable(brick = variables$brick, variable = "bio1")

#plotea todas
plot(
  variables$brick,
  maxnl = length(names(variables$brick)),
  col = viridis::viridis(100)
)

#TRANSFORMA LOS MAPAS EN UNA TABLA
# variables$df <- na.omit(
#   raster::as.data.frame(
#     variables$brick
#     )
#   )

#NOMBRES DE LAS VARIABLES
variables$names <- names(variables$brick)
variables$names

# bio1 = Annual Mean Temperature
# bio2 = Mean Diurnal Range (Mean of monthly (max temp - min temp))
# bio3 = Isothermality (BIO2/BIO7) (* 100)
# bio4 = Temperature Seasonality (standard deviation *100)
# bio5 = Max Temperature of Warmest Month
# bio6 = Min Temperature of Coldest Month
# bio7 = Temperature Annual Range (BIO5-BIO6)
# bio8 = Mean Temperature of Wettest Quarter
# bio9 = Mean Temperature of Driest Quarter
# bio10 = Mean Temperature of Warmest Quarter
# bio11 = Mean Temperature of Coldest Quarter
# bio12 = Annual Precipitation
# bio13 = Precipitation of Wettest Month
# bio14 = Precipitation of Driest Month
# bio15 = Precipitation Seasonality (Coefficient of Variation)
# bio16 = Precipitation of Wettest Quarter
# bio17 = Precipitation of Driest Quarter
# bio18 = Precipitation of Warmest Quarter
# bio19 = Precipitation of Coldest Quarter
# diversidad_topo = diversidad topográfica (número de clases de elevación, pendiente, y orientación en un radio de 10 km)
# human_footprint = huella humana (https://sedac.ciesin.columbia.edu/data/set/wildareas-v3-2009-human-footprint)
# landcover_veg_bare = porcentaje de suelo desnudo (MODIS continuous fields)
# landcover_veg_herb = porcentaje de cobertura herbácea
# landcover_veg_tree = porcentaje de cobertura arbórea
# ndvi_average = media anual de actividad fotosintética de la vegetación (NDVI)
# ndvi_maximum = NDVI máximo
# ndvi_minimum = NDVI mínimo
# ndvi_range = rango anual de NDVI
# sun_rad_average = radiación solar potencial (modelo orbital y topográfico)
# sun_rad_maximum = radiación máxima anual
# sun_rad_minimum = radiación mínima
# sun_rad_range = rango de radiación solar
# topo_slope = pendiente topográfica



# * 2.2 Preparación de una especie virtual ----------------------------------

#vamos a diseñar una especie con un nicho ecológico basado en estas variables
#bio5 - temperatura del mes más cálido
#bio6 - temperatura del mes más frío
#bio12 - precipitación anual
#topo_slope - pendiente
#landcover_veg_herb - % de cobertura herbácea
#human_footprint - huella humana

#CREAMOS LISTA PARA GUARDAR RESULTADOS
sp <- list()

#niche dimensions
sp$nicho.dimensiones <- c(
  "bio12",
  "bio5",
  "bio6",
  "human_footprint",
  "topo_slope",
  "landcover_veg_herb"
)

#vemos las estadísticas de esas variables
#estadística descriptiva de las variables
summary(variables$df[, sp$nicho.dimensiones])

#descripción del nicho ecológico
#el primer número es la media de una distribución normal (nicho óptimo)
#el segundo número es la desviación estándar de la distribución normal (amplitud de nicho)
sp$nicho.parametros <- list(
  bio12 = c(500, 250),
  bio5 = c(240, 50),
  bio6 = c(10, 30),
  human_footprint = c(0, 30),
  topo_slope = c(0, 2),
  landcover_veg_herb = c(100, 35)
)

#genera la especie virtual
especie.virtual <- makeVirtualSpecies(
  variables = variables$brick,
  niche.parameters = sp$nicho.parametros,
  max.n = 200
)

#qué hay dentro de especie.virtual?
names(especie.virtual)
especie.virtual$niche.dimensions
especie.virtual$niche.parameters
plot(especie.virtual$suitability.raster)
especie.virtual$observed.presence

#añadimos esos objetos a sp
sp$suitability.raster <- especie.virtual$suitability.raster
sp$xy <- especie.virtual$observed.presence
rm(especie.virtual)

#ploteamos presencias
SDMworkshop::plotRaster(
  x = variables$brick[["human_footprint"]],
  points.x = sp$xy$x,
  points.y = sp$xy$y,
  points.size = 5
)

#NOTA: fíjate que la función virtualspecies::sampleOccurrences solo genera un punto por celda



#DESCARGANDO PRESENCIAS DE GBIF CON rgbif
#########################################
#########################################
#tutorial: https://ropensci.org/tutorials/rgbif_tutorial/

#miramos la taxonomía de un nombre de un género
rgbif::name_backbone(name = "Tilia tomentosa")

#qué especies hay dentro del género?
taxon <- rgbif::name_suggest(q = "Tilia tomentosa")

#cuantos registros hay?
rgbif::occ_count(
  taxonKey = taxon$key[1], #primera especie
  georeferenced = TRUE
)

#acotamos la descarga a nuestro área de trabajo
#creando un polígono que usa las variables como referencia
area.wkt <-
  sf::st_bbox(variables$brick) %>% #extension
  sf::st_as_sfc() %>% #geometría
  sf::st_as_text() #texto
area.wkt
#IMPORTANTE: las coordenadas deben ir en contra de las agujas del reloj (así GBIF las interpreta como polígono)

#downloading data
tilia <- rgbif::occ_search(
  taxonKey = taxon$key[1],
  return = "data",
  hasCoordinate = TRUE,
  geometry = area.wkt,
  fields = c(
    "acceptedScientificName",
    "decimalLatitude",
    "decimalLongitude",
    "coordinatePrecision",
    "basisOfRecord",
    "taxonkey",
    "year",
    "country")
)

#comprobamos si hay duplicados en coordenadas
sum(duplicated(tilia[, c("decimalLatitude", "decimalLongitude")]))

#los eliminamos
tilia <- tilia[!duplicated(tilia[, c("decimalLatitude", "decimalLongitude")]),]

#ploteamos presencias
plotPresencia(
  brick = variables$brick,
  variable = "human_footprint",
  lon = tilia$decimalLongitude,
  lat = tilia$decimalLatitude
)

#NOTA: haz zoom, y verás que en a veces hay varios puntos por cuadrícula. Guarda este detalle para luego

rm(taxon)



#DESCARGANDO PRESENCIAS CON ALA4R
#########################################
#########################################
#ALGUNOS RECURSOS
# https://cran.r-project.org/web/packages/ALA4R/vignettes/ALA4R.html
# https://www.ala.org.au/faq/spatial-portal/spatial-portal-case-studies/ala4r/

#NOTA: La configuración de ALA4R en el nodo español de GBIF usa los datos del portal nacional (https://datos.gbif.es), el cual contiene datos compartidos en GBIF por proveedores españoles, así como datos de proveedores extranjeros para el territorio español. En total unos 30 millones de datos aprox.

#PRIMERO DEFINIMOS UNA CARPETA PARA GUARDAR ARCHIVOS TEMPORALES
dir.create("ALA4R_cache")
ALA4R::ala_config(cache_directory = "ALA4R_cache")

#configuramos el servidor
#tomado de: # https://www.gbif.es/trabajar-con-info-de-portal-nacional-utilizando-r/
configuraALA4R()

#qué información hay sobre un género concreto?
taxon <- search_fulltext("Tilia")$data

#descargar información de presencia
presencia <- occurrences(
  taxon = "Tilia tomentosa",
  wkt = area.wkt,
  qa = "none",
  download_reason_id = "testing"
)$data

#filtramos las que tienen rank == genus
presencia <- presencia[presencia$rank == "species" & presencia$genus == "Tilia", ]

#ploteamos
plotPresencia(
  brick = variables$brick,
  variable = "human_footprint",
  lon = presencia$longitude,
  lat = presencia$latitude,
  group = presencia$scientificName
)

rm(taxon, area.wkt)




#######################################
#######################################
#AUTOCORRELACIÓN ESPACIAL
#######################################
#######################################
#"Everything is related to everything else, but near things are more related than distant things."

#hay zonas en las que los puntos están muy agregados
#probablemente hay mucha autocorrelación espacial entre grupos de puntos debido a sesgo en el muestreo
#puntos agregados artificialmente debido a muestreo poco homogéneo añade bias a las estimaciones del nicho ecológico
#vamos a reducir la autocorrelación espacial de los puntos de Tilia

#creamos un dataframe nuevo más simple para Tilia
#solo las columnas "x" e "y"
xy <- tilia[, c("decimalLongitude", "decimalLatitude")]
colnames(xy) <- c("x", "y")

#vamos a darle un vistazo a los puntos de presencia de Tilia
plotPresencia(
  brick = variables$brick,
  variable = "human_footprint",
  lon = xy$x,
  lat = xy$y
)

#función para calcular autocorrelación
#se basa en la función ape::Moran.I()
xy.moran <- autocor(
  brick = variables$brick,
  xy = xy
)
xy.moran
#si observed es mayor que 0 y p.value es menor de 0.05, se considera que hay autocorrelación espacial

#según este resultado, todas las variables presentan una autocorrelación significativa

#THINNING (nombre en inglés de lo que vamos a hacer)
#aplicar thinning a un conjunto de datos de presencia implica incrementar la distancia media entre puntos eliminando aquellos que son redundantes.
xy.thin <- thinning(
  xy = xy,
  brick = variables$brick,
  separacion = 5 #celdas entre puntos cercanos
)
#NOTA: esta función conserva los puntos que tiene valores extremos para cualquiera de las variables (para conservar los extremos del nicho ecológico de la especie). Esos puntos no cumplen necesariamente la regla de distancia mínima

#NOTA: el paquete spThin (URL: https://cran.r-project.org/web/packages/spThin) también hace thinning para modelos de distribución.

#vemos las presencias de nevo
plotPresencia(
  brick = variables$brick,
  variable = "human_footprint",
  lon = xy.thin$x,
  lat = xy.thin$y
)

#calculamos autocorrelacion otra vez
xy.moran <- autocor(
  brick = variables$brick,
  xy = xy.thin
)
xy.moran
#NOTA: aquí no hay un criterio fijo, realmente no se puede eliminar completamente la autocorrelación espacial, mitigarla ya es suficiente.


rm(xy, xy.moran, xy.thin, tilia)



################################################################
################################################################
#PREPARACION DE DATOS PARA AJUSTAR LOS MODELOS
################################################################
################################################################


#PRESENCIA
######################################
######################################

#valores de las variables para sp$xy
sp$presencia <- data.frame(
  sp$xy,
  raster::extract(
    x = variables$brick,
    y = sp$xy,
    df = TRUE,
    cellnumbers = FALSE
  )
)
sp$presencia$ID <- NULL

#añadimos columna de presencia (presencia = 1)
sp$presencia$presencia <- 1


#BACKGROUND
###################################
###################################
#la función dismo::randomPoints es muy útil para generar puntos de background
#el número de puntos de background debe ser suficiente para asegurarnos que estamos muestreando variables$brik bien
#en este caso muestreamos un 2% del total de las celdas de las variables, pero luego nos aseguramos de tener los máximos y mínimos de todas las variables
background <- data.frame(
  dismo::randomPoints(
    mask = variables$brick,
    n = floor(
      (2 * nrow(variables$df)) / 100
    )
  )
)

#buscamos los casos que contienen los extremos de cada una de las variables
########################################################################
#iteramos por cada variable
for(variable in names(variables$df)){

  #buscamos las coordenadas de la celda con el menor valor
  xy.min <- raster::xyFromCell(object = variables$brick,
                               cell = raster::which.min(variables$brick[[variable]])[1]
  )
  #buscamos las coordenadas de la celda con el mayor valor
  xy.max <- raster::xyFromCell(object = variables$brick,
                               cell = raster::which.max(variables$brick[[variable]])[1]
  )

  #las unimos al background
  background <- rbind(background, xy.min, xy.max)
}

#eliminamos duplicados
background <- background[!duplicated(background), ]

#vemos el background
plotPresencia(
  brick = variables$brick,
  variable = "human_footprint",
  lon = background$x,
  lat = background$y
)

#le añadimos los valores de las variables
sp$background <- data.frame(
  background,
  raster::extract(
    x = variables$brick,
    y = background,
    df = TRUE,
    cellnumbers = FALSE
  )
)
sp$background$ID <- NULL

#añadimos columna de presencia (presencia = 1)
sp$background$presencia <- 0

rm(background, xy.max, xy.min, variable)



#BACKGROUND RESTRINGIDO
#############################
#background limitado a las zonas a las que la especie puede acceder mediante migración
background.restringido <- backgroundRestringido(
  xy = sp$xy,
  brick = variables$brick,
  buffer.km = 200, #máxima distancia migratoria
  percent = 2 #porcentaje de área muestreada
)

#vemos el background
plotPresencia(
  brick = variables$brick,
  variable = "human_footprint",
  lon = background.restringido$x,
  lat = background.restringido$y
)

#le añadimos los valores de las variables
sp$background.restringido <- data.frame(
  background.restringido,
  raster::extract(
    x = variables$brick,
    y = background.restringido,
    df = TRUE,
    cellnumbers = FALSE
  )
)
sp$background.restringido$ID <- NULL

#añadimos columna de presencia (presencia = 1)
sp$background.restringido$presencia <- 0

rm(background.restringido)



#PSEUDOAUSENCIAS
##############################
#usamos sp$xy en lugar de sp$xy porque sp$xy tiene todas las presencias posibles.
#sin embargo, queremos el doble de pseudo-ausencias como presencias hay en sp$xy, de ahí n=nrow(sp$xy)
pseudoausencia <- data.frame(
  randomPoints(
    mask = variables$brick,
    n = nrow(sp$xy) * 2, #doble del número de presencias
    p = sp$xy, #puntos a excluir
    excludep = TRUE #excluir p
  )
)

#buscamos los casos que contienen los extremos de cada una de las variables
########################################################################
#iteramos por cada variable
for(variable in names(variables$df)){

  #buscamos las coordenadas de la celda con el menor valor
  xy.min <- raster::xyFromCell(object = variables$brick,
                               cell = raster::which.min(variables$brick[[variable]])[1]
  )
  #buscamos las coordenadas de la celda con el mayor valor
  xy.max <- raster::xyFromCell(object = variables$brick,
                               cell = raster::which.max(variables$brick[[variable]])[1]
  )

  #las unimos al background
  pseudoausencia <- rbind(pseudoausencia, xy.min, xy.max)
}

#eliminamos duplicados
pseudoausencia <- pseudoausencia[!duplicated(pseudoausencia), ]

#vemos las pseudoausencias
plotPresencia(
  brick = variables$brick,
  variable = "human_footprint",
  lon = pseudoausencia$x,
  lat = pseudoausencia$y
)


#le añadimos los valores de las variables
sp$pseudoausencia <- data.frame(
  pseudoausencia,
  raster::extract(
    x = variables$brick,
    y = pseudoausencia,
    df = TRUE,
    cellnumbers = FALSE
  )
)
sp$pseudoausencia$ID <- NULL

#añadimos columna de presencia (presencia = 1)
sp$pseudoausencia$presencia <- 0

rm(pseudoausencia, xy.min, xy.max, variable)



#GENERAMOS LAS AUSENCIAS
########################
#NO TENEMOS AUSENCIAS REALES (EN GBIF NO SUELE HABERLAS), PERO VAMOS A SIMULAR UN CONJUNTO DE AUSENCIAS, COMO SI HUBIÉRAMOS REALIZADO MUESTREOS ALREDEDOR DE LAS ZONAS DE PRESENCIA
ausencia <- simulaAusencia(
  xy = sp$xy,
  brick = variables$brick,
  pob.muestreadas = 60, #porcentaje de poblaciones muestreadas
  buffer.km = 100, #distancia máxima desde poblaciones muestreadas
  distancia.thinning = 4 #separación de las ausencias en número de celdas
)

#vemos las ausencias con las presencias
plotPresencia(
  brick = variables$brick,
  variable = "human_footprint",
  lon = c(sp$xy$x, ausencia$x),
  lat = c(sp$xy$y, ausencia$y),
  group = c(
    rep("presencia", nrow(sp$xy)),
    rep("ausencia", nrow(ausencia))
  )
)

#le añadimos los valores de las variables
sp$ausencia <- data.frame(
  ausencia,
  raster::extract(
    x = variables$brick,
    y = ausencia,
    df = TRUE,
    cellnumbers = FALSE
  )
)
sp$ausencia$ID <- NULL

#lo añadimos a sp
sp$ausencia$presencia <- 0

rm(ausencia)




##################################################################
##################################################################
#SELECCION DE VARIABLES
##################################################################
##################################################################
#Las variables a seleccionar deben:
#deben tener algún vínculo causal con la distribución de la especie
#no estar correlacionadas entre sí (baja colinealidad)

#NOTA: de ahora en adelante asumimos que no sabemos cuales son las variables importantes para la especie virtual!!

#1. CURVAS DE USO VS DISPONIBILIDAD
###################################################
sp$variables.uso.disponibilidad <- plotUsoDisponibilidad(
  presencias = rbind(sp$presencia, sp$background),
  presencia = "presencia",
  variables = names(variables$brick)
)


#2. POINT BISERIAL CORRELATION
###################################################
#BISERIAL CORRELATION es la correlación entre una variable binaria (presencia con sus unos y ceros) y una variable contínua (cualquiera de las variables)
#Pendientes positivas o negativas indican que las medias de las presencias y las ausencias son distintas para una variable dada
#guardamos el resultado directamente en la especie virtual
sp$variables.biserial.correlation <- biserialCorrelation(
  presencias = rbind(sp$presencia, sp$background),
  presencia = "presencia",
  variables = names(variables$brick)
)
#la función devuelve el plot y un dataframe
#sp$biserial.correlation$plot
#sp$biserial.correlation$df


#3. DENDROGRAMA DE CORRELACIÓN
###################################################
correlacion.variables <- dendrogramaCorrelacion(
  variables.df = variables$df,
  biserial.correlation = sp$variables.biserial.correlation$df,
  seleccion.automatica = TRUE,
  correlacion.maxima = 0.50
)

#si seleccion.automatica = TRUE, devuelve una lista con dos slots
correlacion.variables$variables.dendrograma
correlacion.variables$variables.seleccionas
#si seleccion.automatica = FALSE, devuelve solo el plot

#IMPORTANTE: la selección automática no es perfecta!
#es necesario evaluar críticamente cada elección del algoritmo.
#POR EJEMPLO: entre bio4 y bio7, la primera está seleccionada automáticamente, pero la segunda presenta mejores propiedades cuando miramos las curvas de uso-disponibilidad.

#guardamos el dendrograma en la especie virtual
sp$variables.dendrograma <- correlacion.variables$variables.dendrograma

#seguimos trabajando con variables seleccionadas
variables.seleccionadas <- correlacion.variables$variables.seleccionas

rm(correlacion.variables)


#4. VARIANCE INFLATION FACTOR (VIF)
###################################
#evalúa si hay variables que son combinaciones lineales de otras variables
#valores mayores de 5 (esto varía entre librerías que calculan VIF) indican que sí
temp.vif <- HH::vif(variables$df[, variables.seleccionadas])
temp.vif

#1. eliminamos landcover_veg_bare porque tiene un biserial correlaion muy bajo
variables.seleccionadas <- variables.seleccionadas[variables.seleccionadas != "landcover_veg_bare"]

#repetimos
temp.vif <- HH::vif(variables$df[, variables.seleccionadas])
temp.vif


#2. eliminamos ndvi_average porque está relacionada con landcover_veg_herb
variables.seleccionadas <- variables.seleccionadas[variables.seleccionadas != "ndvi_average"]

#repetimos
temp.vif <- HH::vif(variables$df[, variables.seleccionadas])
temp.vif

#guardamos variables.seleccionadas en sp
sp$variables.seleccionadas <- variables.seleccionadas

#y hasta aquí, ya tenemos nuestras presencias y variables!
#guardamos el resultado
save(sp, variables, file = "presencia_y_variables.RData")
