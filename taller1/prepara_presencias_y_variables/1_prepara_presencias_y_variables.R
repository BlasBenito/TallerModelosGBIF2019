
# 1 INSTALACIÓN Y CARGA DE PAQUETES -----------------------------------------
#NOTA: solo es necesario instalarlos una vez!
#install.packages(c("rgeos", "HH", "rgbif", "sf", "magrittr", "gistr", "leaflet", "ALA4R", "ape", "geosphere", "ggdendro", "here"), dep=TRUE)


#carga librerías
library(ggplot2)
library(cowplot)
theme_set(theme_cowplot())
library(magrittr)
library(leaflet)
library(dplyr)
library(raster)
library(here)
source("funciones.R")
here::here()
library(devtools)
devtools::install_github("blasbenito/SDMworkshop", force = TRUE)
library(SDMworkshop)


# 2 PREPARACIÓN DE LAS PRESENCIAS -------------------------------------------

# * 2.1 Importa variables predictivas ---------------------------------------

#descromprime mapas ratser al directorio variables
unzip("./1_variables.zip", exdir="./variables", junkpaths=TRUE)

#lista para guardar la info relacionada con las variables
variables <- list()

#importando ficheros raster .asc
variables$brick <- SDMworkshop::importASC(
  folder = "variables",
  crs = "+init=epsg:4326",
  to.memory = TRUE
)

#viendo resolución de las variables
variables$resolucion.km <- raster::xres(variables$brick)*111.19
variables$resolucion.km

#plotea una variable
plotVariable(brick = variables$brick, variable = "bio1")

#plotea todas
x11()
plot(
  variables$brick,
  maxnl = length(names(variables$brick)),
  col = viridis::viridis(100)
)

#TRANSFORMA LOS MAPAS EN UNA TABLA
variables$df <- na.omit(
  raster::as.data.frame(
    variables$brick
    )
  )

#nombres de las variables
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
especie.virtual <- SDMworkshop::makeVirtualSpecies(
  variables = variables$brick,
  niche.parameters = sp$nicho.parametros,
  max.n = 200
)

#qué hay dentro de especie.virtual?
names(especie.virtual)
especie.virtual$niche.dimensions
especie.virtual$niche.parameters
plot(especie.virtual$suitability.raster, col = viridis::viridis(100, direction = -1))
especie.virtual$observed.presence

#añadimos esos objetos a sp
sp$suitability.raster <- especie.virtual$suitability.raster
sp$xy <- especie.virtual$observed.presence
rm(especie.virtual)

#ploteamos presencias
SDMworkshop::plotVariable(
  variable = variables$brick[["human_footprint"]],
  points.x = sp$xy$x,
  points.y = sp$xy$y,
  points.size = 5
)



# * BONUS: Descarga de presencias de GBIF ----------------------------------
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
#NOTA: si esta línea da problemas:
#raster::crs(variables$brick) <- NA
area.wkt <-
  sf::st_bbox(variables$brick, crs = sf::st_crs(4326)) %>% #extension
  sf::st_as_sfc() %>% #geometría
  sf::st_as_text() #texto
area.wkt
#IMPORTANTE: las coordenadas deben ir en contra de las agujas del reloj (así GBIF las interpreta como polígono)

#descargando los datos
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
#NOTA: si esta línea da problemas:
#raster::crs(variables$brick) <- "+init=epsg:4326"
SDMworkshop::plotVariable(
  variable = variables$brick[["human_footprint"]],
  points.x = tilia$decimalLongitude,
  points.y = tilia$decimalLatitude
)

#NOTA: haz zoom, y verás que en a veces hay varios puntos por cuadrícula. Guarda este detalle para luego



# * BONUS: Descarga de presencias con ALA4R ----------------------------------

#recursos:
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
presencia <- ALA4R::occurrences(
  taxon = "Tilia tomentosa",
  wkt = area.wkt,
  qa = "none",
  download_reason_id = "testing"
)$data

#filtramos las que tienen rank == genus
presencia <- presencia[presencia$rank == "species" & presencia$genus == "Tilia", ]

#ploteamos
SDMworkshop::plotVariable(
  variable = variables$brick[["human_footprint"]],
  points.x = presencia$longitude,
  points.y = presencia$latitude
)

rm(taxon, area.wkt)



# * 2.3 Autocorrelación espacial ----------------------------------
#"Everything is related to everything else, but near things are more related than distant things."

#creamos un dataframe nuevo más simple para Tilia
#solo las columnas "x" e "y"
xy <- tilia[, c("decimalLongitude", "decimalLatitude")]
colnames(xy) <- c("x", "y")

#vamos a darle un vistazo a los puntos de presencia de Tilia
SDMworkshop::plotVariable(
  variable = variables$brick[["human_footprint"]],
  points.x = xy$x,
  points.y = xy$y
)

#hay zonas en las que los puntos están muy agregados
#probablemente hay mucha autocorrelación espacial entre grupos de puntos debido a sesgo en el muestreo
#puntos agregados artificialmente debido a muestreo poco homogéneo añade bias a las estimaciones del nicho ecológico
xy.moran <- SDMworkshop::testSpatialCorrelation(
  xy = xy,
  variables = variables$brick
)
xy.moran
#si observed es mayor que 0 y p.value es menor de 0.05, se considera que hay autocorrelación espacial significativa
#según este resultado, todas las variables presentan una autocorrelación significativa

#THINNING (nombre en inglés de lo que vamos a hacer)
#NOTA: el paquete spThin (URL: https://cran.r-project.org/web/packages/spThin) también hace thinning para modelos de distribución.
#aplicar thinning a un conjunto de datos de presencia implica incrementar la distancia media entre puntos eliminando aquellos que son redundantes.
xy.thin <- SDMworkshop::reduceSpatialCorrelation(
  xy = xy,
  variables = variables$brick,
  minimum.distance = 1 #distancia a imponer entre puntos adyacentes, 1 grado
)
#NOTA: esta función conserva los puntos que tiene valores extremos para cualquiera de las variables (para conservar los extremos del nicho ecológico de la especie). Esos puntos no cumplen necesariamente la regla de distancia mínima
SDMworkshop::plotVariable(
  variable = variables$brick[["human_footprint"]],
  points.x = xy.thin$x,
  points.y = xy.thin$y
)

#vemos que ahora hay menos correlación espacial
xy.moran <- SDMworkshop::testSpatialCorrelation(
  xy = xy.thin,
  variables = variables$brick
)
xy.moran


#NOTA: aquí no hay un criterio fijo, realmente no se puede eliminar completamente la autocorrelación espacial, mitigarla ya es suficiente.
rm(xy, xy.moran, xy.thin, tilia)


# * 2.4 Preparación de datos de entrenamiento ----------------------------------

#usaremos la función "prepareTrainingData"
help(prepareTrainingData)

#presencia
sp$presencia <- SDMworkshop::prepareTrainingData(
  xy = sp$xy,
  variables = variables$brick,
  presence.only = TRUE
)

#background
sp$background <- SDMworkshop::prepareTrainingData(
  xy = sp$xy,
  variables = variables$brick,
  n = 10000,
  background = TRUE
)

#background restringido a zonas accesibles mediante migracción
sp$background.restringido <- SDMworkshop::prepareTrainingData(
  xy = sp$xy,
  variables = variables$brick,
  n = 10000,
  restricted.background = TRUE,
  restricted.background.buffer = 300
)

#pseudoausencias
sp$pseudoausencia <- SDMworkshop::prepareTrainingData(
  xy = sp$xy,
  variables = variables$brick,
  n = nrow(sp$presencia) * 2,
  background = TRUE
)


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
sp$variables.biserial.correlation <- SDMworkshop::corPB(
  x = rbind(sp$presencia, sp$background),
  presence.column = "presencia",
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
