#Simula ausencias
#############################################################################################
simulaAusencia <- function(xy, brick, pob.muestreadas, buffer.km, distancia.thinning){
  
  #librerías requeridas
  require(raster)
  require(rgeos)
  require(dismo)
  require(sp)
  
  #seleccionamos al azar un porcentaje de poblaciones muestreadas
  pob.muestreadas <- pob.muestreadas / 100
  xy <- xy[sample(nrow(xy), floor(nrow(xy) * pob.muestreadas), replace = FALSE), ]
  
  #radio a metros
  buffer.m <- buffer.km * 1000
  
  #convertir la tabla de xy en objeto "sp" (clase SpatialPoints)
  sp::coordinates(xy) <- c("x", "y")
  
  #hacer buffer
  buffer <- dismo::circles(xy, d = buffer.km*1000, lonlat = TRUE)
  buffer.dissolve <- rgeos::gUnaryUnion(buffer@polygons)
  
  #extraemos los identificadores de las celdas
  celdas <- unlist(raster::cellFromPolygon(object = brick, p = buffer.dissolve))
  
  #tomamos uno de los mapas de brick como plantilla para la máscara
  mascara.temp <- raster::raster(brick[[1]])
  
  #creamos vector de valores
  valores <- rep(NaN, raster::ncell(mascara.temp))
  valores[celdas] <- 1
  
  #mascara a partir de valores
  mascara.temp <- raster::setValues(mascara.temp, values = valores)
  mascara.temp <- raster::mask(mascara.temp, mask = brick[[1]])
  
  #generamos las ausencias
  ausencia <- data.frame(
    dismo::randomPoints(
      mask = mascara.temp, 
      n = floor(length(celdas)),
      p = xy,
      excludep = TRUE
      )
    )
  
  #ausencia thinning a las ausencias también
  ausencia <- thinning(
    xy = ausencia,
    brick = variables$brick,
    separacion = distancia.thinning
  )
  
  return(ausencia)
}


#Genera background restringido a una distancia alrededor de los puntos conocidos de presencia
#############################################################################################
backgroundRestringido <- function(xy, brick, buffer.km, percent){
  
  #librerías requeridas
  require(raster)
  require(rgeos)
  require(dismo)
  require(sp)
  
  #convertir la tabla de xy en objeto "sp" (clase SpatialPoints)
  sp::coordinates(xy) <- c("x", "y")
  
  #hacer buffer
  buffer <- dismo::circles(
    xy, 
    d = buffer.km*1000, 
    lonlat = TRUE
    )
  buffer.dissolve <- rgeos::gUnaryUnion(buffer@polygons)
  
  #extraemos los identificadores de las celdas
  celdas <- unlist(raster::cellFromPolygon(
    object = brick, 
    p = buffer.dissolve)
    )
  
  #tomamos uno de los mapas de brick como plantilla para la máscara
  mascara.temp <- raster::raster(brick[[1]])
  
  #creamos vector de valores
  valores <- rep(NaN, raster::ncell(mascara.temp))
  valores[celdas] <- 1
  
  #mascara a partir de valores
  mascara.temp <- raster::setValues(
    mascara.temp, 
    values = valores
    )
  mascara.temp <- raster::mask(
    mascara.temp, 
    mask = brick[[1]]
    )
  
  #generamos background
  background <- data.frame(
    dismo::randomPoints(
      mask = mascara.temp, 
      n = floor((percent * length(celdas))/100)
      )
    )
  
  #tomamos los valores extremos de las variables
  #hacemos primero version con máscara del brick
  brick.mask <- raster::mask(
    x = brick, 
    mask = mascara.temp
    )
  
  #iteramos por cada variable
  for(variable in names(brick.mask)){
    
    #buscamos las coordenadas de la celda con el menor valor
    xy.min <- raster::xyFromCell(object = brick.mask,
                                 cell = raster::which.min(brick.mask[[variable]])[1]
    )
    #buscamos las coordenadas de la celda con el mayor valor
    xy.max <- raster::xyFromCell(object = brick.mask,
                                 cell = raster::which.max(brick.mask[[variable]])[1]
    )
    
    #las unimos al background
    background <- rbind(background, xy.min, xy.max)
  }
  #eliminamos duplicados
  background <- background[!duplicated(background), ]
  
  
  return(background)
}



#Calcula la autocorrelación espacial de un conjunto de puntos de presencia para cada una de las variables en un brick
##############################################################################################
##############################################################################################
autocor <- function(brick = NULL, xy = NULL){
  
  require(ape)
  require(geosphere)
  
  #remove scientific notation
  options(scipen = 9999)
  
  #extrae valores de las variables para los puntos de presencia
  xy.variables <- na.omit(
    data.frame(
      xy,
      raster::extract(
        x = brick,
        y = xy,
        df = TRUE,
        cellnumbers = FALSE
      )
    )
  )
  
  #quitando una variable que no necesitamos
  xy.variables$ID <- NULL
    
  #matriz de distancias inversas entre presencias
  xy.distancias <- 1/ (geosphere::distm(
    x = xy.variables[, c("x", "y")],
    fun = distGeo
  ) / 1000)
  
  #reemplazando Inf por cero
  xy.distancias[!is.finite(xy.distancias)] <- 0
  
  #diagonal a 0
  diag(xy.distancias) <- 0
  
  #calcula I de Moran para cada columna
  #check: https://cran.r-project.org/web/packages/ape/vignettes/MoranI.pdf
  moran.i <- apply(
    xy.variables, 
    2, 
    FUN = function(x)  ape::Moran.I(x, xy.distancias, na.rm = TRUE)
    )
  
  #a dataframe
  output.df <- do.call("rbind", moran.i)
  
  #eliminando una columna que no es importante
  output.df$sd <- NULL
  
  return(output.df)
  
}


#configura servidor de ALA4R siguiendo las instrucciones de esta página
#######################################################################
#######################################################################
#https://www.gbif.es/trabajar-con-info-de-portal-nacional-utilizando-r/
configuraALA4R <- function(){
  require(ALA4R)
  server_config <- list(
    max_occurrence_records = 500000,
    server_max_url_length = 8150,
    brand = "ALA4R",
    notify = "Please use https://github.com/AtlasOfLivingAustralia/ALA4R/issues/ or email to support@ala.org.au",
    support_email = "infogbifspain@gmail.com",
    reasons_function = "ala_reasons",
    fields_function = "ala_fields",
    occurrences_function = "occurrences",
    config_function = "ala_config",
    base_url_spatial = "https://geo-ws.gbif.es/layers-service",
    base_url_bie = "https://especies-ws.gbif.es",
    base_url_biocache = "https://registros-ws.gbif.es/",
    base_url_images = "https://images.gbif.es/",
    base_url_logger = "https://logger.gbif.es/service/logger",
    biocache_version = "2.1.15",
    verbose = TRUE,
    download_reason_id = 10,
    caching = "off"
  )
}



#plotea una variable
#########################################
#########################################
plotVariable <- function(brick, variable){
  
  require(raster)
  require(leaflet)
  
  x <- variables$brick[[variable]]
  x.values <- na.omit(values(x))
  pal <- colorNumeric(
    palette = viridis::viridis(100), 
    domain = x.values,
    na.color = "transparent"
    )
  
  leaflet() %>%
    addTiles() %>%
    addRasterImage(
      x, 
      colors = pal, 
      opacity = 0.5
      ) %>%
    addLegend(
      pal = pal, 
      values = x.values,
      title = variable
      )
}


#plotea las presencias y una variable con leaflet
###################################################
###################################################
plotPresencia <- function(brick = NULL, variable = NULL, lon = NULL, lat = NULL, group = NULL){
  
  require(leaflet)
  require(raster)
  require(viridis)
  
  #preparando raster para mapa
  x <- brick[[variable]]
  x.values <- na.omit(raster::values(x))
  pal.raster <- colorNumeric(
    palette = viridis::viridis(100), 
    domain = x.values,
    na.color = "transparent"
  )
  
  #preparando paleta para grupos
  if(is.null(group)){
    group = rep(1, length(x))
  }
  pal.groups <- colorFactor(
    palette = viridis::plasma(length(unique(group))),
    domain = unique(group),
    na.color = "transparent"
  )
  
  #mapa
  leaflet() %>%
    setView(
      lng = mean(lon), 
      lat = mean(lat), 
      zoom = 04
    ) %>% 
    leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
    addTiles() %>%
    addRasterImage(
      x, 
      colors = pal.raster, 
      opacity = 0.4
      ) %>%
    addLegend(
      pal = pal.raster, 
      values = x.values,
      title = variable,
      opacity = 1
      ) %>%
    addCircles(
      lng = lon, 
      lat = lat,
      weight = 10,
      radius = 10,
      color = pal.groups(group),
      stroke = TRUE,
      opacity = 1,
      fillOpacity = 0.2
      ) %>%
    addLegend(
      pal = pal.groups, 
      values = unique(group),
      title = "Taxon", 
      opacity = 1
    )
  
  # addMarkers(
  #   lng = lon, 
  #   lat = lat,
  #   group = group)
  
}




#Generate virtual species
##########################################################
##########################################################
generaEspecieVirtual <- function(variables.brick, nicho, prevalencia){
  
  #carga librerías
  require(raster)
  require(virtualspecies)
  require(viridis)
  require(ggplot2)
  theme_set(theme_cowplot())
  require(cowplot)
  
  #variables del nicho
  nicho.variables <- names(nicho)
  
  #variables brick a tabla
  variables.df <- na.omit(as.data.frame(variables.brick[[nicho.variables]]))
  
  #error si los nombres de las variables en nicho están mal
  if(sum(names(nicho) %in% names(variables.brick)) != length(nicho.variables)){
    stop("Algún nombre del objecto nicho está mal definido")
  }
  
  #a long format
  variables.df.long <- tidyr::gather(
    data = variables.df[, nicho.variables], 
    key = variable, 
    value = valor)
  
  #creamos los nichos para cada variable para visualizarlos
  n <- nrow(variables.df)
  variables.nicho <- list()
  for(i in nicho.variables){
    variables.nicho[[i]] <- rnorm(
      n, 
      mean = nicho[[i]][1], 
      sd = nicho[[i]][2]
    )
  }
  variables.nicho <- data.frame(do.call("cbind", variables.nicho))

  
  #a formato largo
  variables.nicho <- tidyr::gather(
    data = variables.nicho, 
    key = variable, 
    value = valor
  )
  
  #ploteamos con ggplot
  plot.densidad <- ggplot(
    data = variables.df.long, 
    aes(
      x = valor, 
      group = variable
    )
  ) + 
    geom_density(
      fill = viridis::viridis(1, begin = 0.99), 
      alpha = 1,
      size = 0
    ) +
    facet_wrap("variable", scales = "free") + 
    geom_density(
      data = variables.nicho,
      aes(
        x = valor, 
        group = variable
      ),
      fill = viridis::viridis(1, begin = 0.3), 
      alpha = 0.5,
      size = 0
    ) 
  
  #wrapper for virtualspecies::formatFunctions
  args <- list()
  for(i in 1:length(nicho)){
    args[[i]] <- c(fun = "dnorm", mean = nicho[[i]][1], sd = nicho[[i]][2])
  }
  names(args) <- names(nicho)
  
  #generating the details file for virtualspecies::generateSpFromFun
  details <- list()
  for (i in names(args)){
    details[[i]]$fun <- args[[i]]["fun"]
    args[[i]] <- args[[i]][-(names(args[[i]]) %in% "fun")]
    details[[i]]$args <- as.list(args[[i]])
    details[[i]]$args <- sapply(details[[i]]$args, as.numeric)
  }
  
  #generating nicho map from details
  virtual.species <- virtualspecies::generateSpFromFun(
    raster.stack = variables.brick[[nicho.variables]],
    parameters = details,
    rescale = TRUE
  )
  
  #variables brick a tabla
  nicho.map.df <- na.omit(
    as.data.frame(virtual.species$suitab.raster, xy = TRUE)
    )
  
  #plot mapa
  plot.mapa <- ggplot(data = nicho.map.df,
                      aes(x = x, 
                          y = y, 
                          fill = layer)
                      ) +
    geom_tile() + 
    viridis::scale_fill_viridis(direction = -1) +
    labs(fill = "Suitability") + 
    xlab("Longitude") + 
    ylab("Latitude")
  
  #multipanel plot
  plot.nicho <- cowplot::plot_grid(
    plot.densidad, 
    plot.mapa, 
    ncol = 1, 
    axis = "l", 
    align = "hv")
  print(plot.nicho)
  
  #convertir a presencia ausencia
  virtual.species.rango <- virtualspecies::convertToPA(
    x = virtual.species,
    species.prevalence = prevalencia,
    plot = FALSE
  )
  
  virtual.species.rango$nicho.plot <- plot.nicho
  
  return(virtual.species.rango)
  
}

#############################################################################
#thinning
#This function reduces the spatial clustering of a set of presence records. It is intended to reduce spatial autocorrelation, and reduce sampling bias, specially at larger geographical scales.

#It requires two different arguments:
#data.table: a table with two fields representing latitude (named 'y') and longitude (named 'x')

#a min.dist value, provided in the same units as the coordinates, that will define the search radius when looking for pairs of coordinates within search distance to get rid of. Hint: the minimum distance can be extracted from the resolution of a raster containint the environmental factors, like "min.dist<-xres(v.brick.20km)"
thinning = function(xy, brick, separacion){
  
  #cambia nombres de columnas
  names(xy) <- c("x", "y")
    
  #calcula distancia mínima entre puntos
  min.dist = raster::xres(brick) * separacion
  
  #extrae valores de las variables para los puntos de presencia
  xy <- na.omit(
    data.frame(
      xy,
      raster::extract(
        x = brick,
        y = xy,
        df = TRUE
      )
    )
  )
  
  #remove column
  xy$ID <- NULL

  #genera el dataframe de extremos
  indices.extremos <- vector()
  for(variable in names(xy)){
    indices.extremos <- c(indices.extremos, which.min(xy[, variable]), which.max(xy[, variable]))
  }
  
  #quitamos repetidos
  indices.extremos <- unique(indices.extremos)
  
  #generamos el dataframe xy.extremos
  xy.extremos <- xy[indices.extremos, ]
  
  #los eliminamos de xy
  xy <- xy[-indices.extremos, ]
  
  #count rows
  row<-1
  
  #repite la operación hasta que se cumple la condición de salida
  repeat{
    
    #contenido de la fila (para no tirar de toda la tabla en todas las operaciones)
    f<-xy[row, ]
    
    #genera los límites de la cuadrícula de búsqueda
    ymax<-f$y + min.dist
    ymin<-f$y - min.dist
    xmax<-f$x + min.dist
    xmin<-f$x - min.dist
    
    #selecciona de la tabla los datos con coordenadas dentro del rectángulo que no tienen las mismas coordenadas que la fila con la que estamos trabajando, y las elimina de la tabla
    xy <- xy[!((xy$y <= ymax) & (xy$y >= ymin) & (xy$x <= xmax) & (xy$x >= xmin) & (xy$y != f$y | xy$x != f$x)), ]
    
    #estima de filas por procesar
    print(paste("Processed rows: ", row, " out of ", nrow(xy), sep=""))
    
    #suma 1 al contador de la fila
    row<-row+1
    
    #condición de salida cuando llega a la última fila
    if(row>=nrow(xy))break
  }
  
  #añadiendo extremos
  xy <- rbind(xy.extremos, xy)[, c("x", "y")]
  
  return(xy)

}

#############################################################
#CALCULA BISERIAL CORRELATION PARA UNA TABLA DE PRESENCIAS
#presencias: dataframe con presencias y ausencias para un conjunto de variables
#presencia: nombre de la columna de presencia
#variables: nombres de las variables a analizar
biserialCorrelation <- function(presencias, presencia, variables){
  
  require(tidyr)
  require(ggplot2)
  require(viridis)
  
  #a formato long
  presencias.long <- tidyr::gather(
    data = presencias, 
    key = variable, 
    value = valor, 
    variables
    )
  
  #presencia a factor
  presencias.long[, presencia] <- factor(presencias.long[, presencia])
  
  #ploteamos primero
  biserial.plot <- ggplot(
    data = presencias.long, 
    aes_string(
      x = get(presencia), 
      y = "valor",
      group = "variable",
      color = get(presencia)
    )
  ) + 
    geom_point(
      alpha = 0.05,
      size = 3
    ) +
    facet_wrap("variable", scales = "free") +
    viridis::scale_color_viridis(
      discrete = TRUE, 
      direction = -1
    ) + 
    geom_smooth(
      method = "lm",
      size = 2,
      color = viridis::viridis(1, begin = 0.5)) +
    guides(colour = guide_legend(override.aes = list(alpha = 1))) +
    ylab("Variable") + 
    xlab("Presencia")
  
  #plot a pantalla
  print(biserial.plot)
  
  #calcula biserial correlation para cada variable
  biserial.correlation <- data.frame(
    variable = variables,
    R2 = NA,
    p = NA,
    stringsAsFactors = FALSE
  )
  
  #iteramos por todas las variables
  for(variable in biserial.correlation$variable){
    
    #calcula correlacion
    temp.cor <- cor.test(presencias[, presencia], presencias[, variable])
    
    #guarda resultado
    biserial.correlation[
      biserial.correlation$variable == variable ,
      c("R2", "p")
      ] <- c(abs(temp.cor$estimate), round(temp.cor$p.value, 3))
    
  }
  
  #ordena por R2
  biserial.correlation <- biserial.correlation[
    order(
      biserial.correlation$R2, 
      decreasing = TRUE
    ), ]
  
  #nuevos nombres de fila
  row.names(biserial.correlation) <- 1:nrow(biserial.correlation)
  
  #a pantalla
  print(biserial.correlation)
  
  #lista de resultados
  output.list <- list()
  output.list$plot <- biserial.plot
  output.list$df <- biserial.correlation
  
  return(output.list)
  
}


###################################################################
#PLOTEA DENSIDADES DE PRESENCIAS Y AUSENCIAS
#presencias: dataframe con presencias, ausencias, y sus valores de las variables
#presencia: nombre de la columna de presencia
#variables: nombres de las columnas de variables
plotUsoDisponibilidad <- function(presencias, presencia, variables){
  
  require(tidyr)
  require(ggplot2)
  require(viridis)
  
  #a formato long
  temp.long <- tidyr::gather(
    data = presencias, 
    key = variable, 
    value = valor, 
    variables
  )
  
  #presencia a factor
  temp.long[, presencia] <- factor(temp.long[, presencia])
  
  #ploteamos con ggplot
  curvas.respuesta <- ggplot(
    data = temp.long, 
    aes_string(
      x = "valor", 
      group = get(presencia),
      fill = get(presencia)
    )
  ) + 
    geom_density(
      alpha = 0.5,
      size = 0.2
    ) +
    facet_wrap("variable", scales = "free") +
    viridis::scale_fill_viridis(
      discrete = TRUE, 
      direction = -1
    )
  x11(width = 20, height = 15, pointsize = 20)
  print(curvas.respuesta)
  
  return(curvas.respuesta)

}


######################################################################
#DENDROGRAMA DE CORRELACIÓN
#variables.df: dataframe de las variables sin valores nulos originado de un brick
#biserial.correlation: dataframe resultante de la función biserialCorrelation
#cor.threshold: umbral de correlación para separar grupos
dendrogramaCorrelacion <- function(variables.df, biserial.correlation, seleccion.automatica = FALSE, correlacion.maxima = 0.5){
  
  require(dplyr)
  require(magrittr)
  require(ggplot2)
  require(ggdendro)
  
  #obtiene variables con biserial.correlation significativa
  variables.seleccionadas <- biserial.correlation[biserial.correlation$p < 0.05, "variable"]
  
  #1: dendrograma de correlación de las variables seleccionadas
  #############################################################
  #calculamos la matriz de correlación con cor
  temp.cor <- cor(variables.df[, variables.seleccionadas])
  
  #lo convertimos en matriz de distancias (menor distancia = mayor correlación)
  temp.dist <- as.dist(abs(temp.cor))
  
  #cluster (1 - correlación) para convertir correlación en distancia
  temp.cluster <- hclust(1 - temp.dist)
  
  
  #2: seleccion automatica
  ##############################################################
  if(seleccion.automatica == TRUE){
    
    #tabla de grupos
    temp.cluster.groups <- data.frame(group = cutree(temp.cluster, h = 1 - correlacion.maxima))
    temp.cluster.groups$variable <- row.names(temp.cluster.groups)
    temp.cluster.groups <- temp.cluster.groups[
      order(
        temp.cluster.groups$group, 
        decreasing = FALSE
      ), ]
    row.names(temp.cluster.groups) <- 1:nrow(temp.cluster.groups)
    
    #le une biserial correlation
    #añadimos los valores de biserial correlation a las etiquetas del cluster
    temp.cluster.groups$R2 <- biserial.correlation$R2[
      match(
        temp.cluster.groups$variable,     #etiquetas cluster
        biserial.correlation$variable #variables biserial.correlation
      )
      ]
    
    #se queda con el máximo de cada grupo
    variables.seleccionadas <- temp.cluster.groups %>%
      dplyr::group_by(group) %>%
      dplyr::slice(which.max(R2)) %>% 
      .$variable
    
  } #fin de selección automática
  
  
  #3: plotea cluster
  ##############################################################
  #datos requeridos para plotear el cluster
  temp.cluster.data <- ggdendro::dendro_data(temp.cluster)
  
  #añadimos los valores de biserial correlation a las etiquetas del cluster
  temp.cluster.data$labels$R2 <- biserial.correlation$R2[
    match(
      temp.cluster.data$labels$label,     #etiquetas cluster
      biserial.correlation$variable #variables biserial.correlation
    )
    ]
  
  #extrae etiquetas
  labs <- ggdendro::label(temp.cluster.data)
  
  #añade señal a la etiqueta si la variable está seleccionada
  if(seleccion.automatica == TRUE){
    labs$label <- as.character(labs$label)
    for(i in 1:nrow(labs)){
      if(labs[i, "label"] %in% variables.seleccionadas){
        labs[i, "label"] <- paste("→ ", labs[i, "label"], sep = "")
      }
    }
  }
  labs$label <- factor(labs$label)
  
  
  #plotea dendrograma
  cluster.plot <- ggplot(ggdendro::segment(temp.cluster.data)) +
    geom_segment(
      aes(
        x = x, 
        y = y, 
        xend = xend, 
        yend = yend)
    ) +
    geom_text(
      data = ggdendro::label(temp.cluster.data),
      aes(
        label = labs$label, 
        x = x, 
        y = 0, 
        colour = labs$R2,
        hjust = 1
      ),
      size = 10
    ) + 
    coord_flip(ylim = c(-0.2, 1)) +
    viridis::scale_colour_viridis(direction = -1, end = 0.9)  + 
    theme(
      axis.text.y = element_blank(),
      axis.line.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title.y = element_blank(),
      plot.margin = unit(c(2,2,2,2), "lines"),
      axis.text.x = element_text(size = 20),
      legend.position = "left"
    ) + 
    labs(colour = "R2") + 
    geom_hline(
      yintercept = 1 - correlacion.maxima, 
      col = "red4", 
      linetype = "dashed",
      size = 2,
      alpha = 0.5
      ) + 
    scale_y_continuous(breaks = c(1 - correlacion.maxima, 0, 0.25, 0.5, 0.75, 1)) + 
    ylab("1 - correlación")
  
  x11(width = 15, height = 10, pointsize = 20)
  print(cluster.plot)
  
  #lista de salida
  if(seleccion.automatica == TRUE){
    output.list <- list()
    output.list$variables.dendrograma <- cluster.plot
    output.list$variables.seleccionas <- variables.seleccionadas
    return(output.list)
  } else {
    return(cluster.plot)
  }
  
}



rasterNet <- function(x, resolution=NULL, xbin=NULL, ybin=NULL, mask=FALSE, degree=111325, xOffset=NULL, yOffset=NULL,
                      checkerboard=FALSE, maxpixels=250000){
  ext <- raster::extent(x)
  extRef <- raster::extent(x)
  if(is.na(sp::proj4string(x))){
    mapext <- raster::extent(x)[1:4]
    if(mapext >= -180 && mapext <= 180){
      resolution <- resolution / degree
      warning("The input layer has no CRS defined. Based on the extent of the input map it is assumed to have an un-projected reference system")
    } else {
      resolution <- resolution
      warning("The input layer has no CRS defined. Based on the extent of the input map it is assumed to have a projected reference system")
    }
  } else{
    if(sp::is.projected(sp::SpatialPoints((matrix(1:10, 5, byrow=FALSE)), proj4string=crs(x)))){
      resolution <- resolution
    } else{
      resolution <- resolution / degree
    }
  }
  if(!is.null(xbin) && is.null(ybin)){
    rasterNet <- raster::raster(ext, nrow=1, ncol=xbin, crs=crs(x))
  } else if(is.null(xbin) && !is.null(ybin)){
    rasterNet <- raster::raster(ext, nrow=ybin, ncol=1, crs=crs(x))
  } else if(!is.null(xbin) && !is.null(ybin)){
    rasterNet <- raster::raster(ext, nrow=ybin, ncol=xbin, crs=crs(x))
  } else if(is.null(xbin) && is.null(ybin) && !is.null(resolution)){
    xrange <- raster::xmax(x) - raster::xmin(x) # number of columns
    yrange <- raster::ymax(x) - raster::ymin(x) # number of rows
    xPix <- ceiling(xrange / resolution)
    yPix <- ceiling(yrange / resolution)
    xdif <- ((xPix * resolution) - xrange) / 2 # the difference of extent divided by 2 to split on both sides
    ydif <- ((yPix * resolution) - yrange) / 2
    ext@xmin <- raster::xmin(x) - xdif
    ext@xmax <- raster::xmax(x) + xdif
    ext@ymin <- raster::ymin(x) - ydif
    ext@ymax <- raster::ymax(x) + ydif
    if(!is.null(xOffset)){
      if(xOffset > 1 || xOffset < 0){stop("xOffset should be between 0 and 1")}
      ext@xmin <- ext@xmin + (resolution * xOffset)
      ext@xmax <- ext@xmax + (resolution * xOffset)
    }
    if(!is.null(yOffset)){
      if(yOffset > 1 || yOffset < 0){stop("yOffset should be between 0 and 1")}
      ext@ymin <- ext@ymin + (resolution * yOffset)
      ext@ymax <- ext@ymax + (resolution * yOffset)
    }
    # adding cells if needed
    if(ext@xmin > extRef@xmin){ # add one column by increasing the extent and number of bins
      ext@xmin <- ext@xmin - resolution
      xPix <- xPix + 1
    }
    if(ext@ymin > extRef@ymin){
      ext@ymin <- ext@ymin - resolution
      yPix <- yPix + 1
    }
    rasterNet <- raster::raster(ext, nrow=yPix, ncol=xPix, crs=crs(x))
  } else stop("A value should be specified for the block size")
  if(checkerboard == TRUE){
    values(rasterNet) <- 1:ncell(rasterNet)
    m <- as.matrix(rasterNet)
    for(i in 1:ncol(rasterNet)){
      if(i %% 2 == 0){
        m[,i] <- rep(1:2, nrow(m))[1:nrow(m)]
      } else{
        m[,i] <- rep(2:1, nrow(m))[1:nrow(m)]
      }
    }
    rasterNet[] <- m
  } else{
    values(rasterNet) <- 1:ncell(rasterNet)
  }
  rasterNet <- raster::rasterToPolygons(rasterNet)
  if(mask==TRUE){
    if(methods::is(x, 'Raster')){
      points <- raster::rasterToPoints(x[[1]], spatial=TRUE)
      if(nrow(points) > 750000){
        points2 <- points[sample(1:nrow(points), maxpixels, replace=FALSE), ]
        rasterNet <- raster::intersect(rasterNet, points2)
      } else  rasterNet <- raster::intersect(rasterNet, points)
    } else{
      rasterNet <- raster::intersect(rasterNet, x)
    }
  }
  return(rasterNet)
}



multiplot <- function(..., plotlist=NULL, file, cols=2, layout=NULL) {
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  if (is.null(layout)) {
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  if (numPlots==1) {
    print(plots[[1]])
  } else {
    grid::grid.newpage()
    grid::pushViewport(grid::viewport(layout = grid::grid.layout(nrow(layout), ncol(layout))))
    for (i in 1:numPlots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plots[[i]], vp = grid::viewport(layout.pos.row = matchidx$row,
                                            layout.pos.col = matchidx$col))
    }
  }
}

#' Measure spatial autocorrelation in the predictor raster files
#'
#' This function provides a quantitative basis for choosing block size. The spatial autocorrelation in all continuous
#' predictor variables available as raster layers is assessed and reported. The function estimates spatial autocorrelation
#' ranges of all input raster layers. This is the range over which observations are independent and is determined by
#' constructing the empirical variogram, a fundamental geostatistical tool for measuring spatial autocorrelation.
#' The empirical variogram models the structure of spatial autocorrelation by measuring variability between all possible
#' pairs of points (O’Sullivan and Unwin, 2010). Results are plotted. See the details section for further information.
#'
#'
#' The input raster layers should be continuous for computing the variograms and estimating the range of spatial
#' autocorrelation. The input rasters should also have a specified coordinate reference system. However, if the reference
#' system is not specified, the function attempts to guess it based on the extent of the map. It assumes an unprojected
#' reference system for layers with extent lying between -180 and 180, and a projected reference system otherwise.
#'
#' Variograms are calculated based on the distances between pairs of points, so unprojected rasters (in degrees) will
#' not give an accurate result (especially over large latitudinal extents). For unprojected rasters, the great circle
#' distance (rather than Euclidian distance) is used to calculate the spatial distances between pairs of points. To
#' enable more accurate estimate, it is recommended to transform unprojected maps (geographic coordinate
#' system / latitude-longitude) to a projected metric reference system (e.g. UTM, Lambert) where it is possible.
#' See \code{\link[automap]{autofitVariogram}} from \pkg{automap} and \code{\link[gstat]{variogram}} from \pkg{gstat} packages
#' for further information.
#'
#'
#' @param rasterLayer RasterLayer, RasterBrick or RasterStack of covariates to find spatial autocorrelation range.
#' @param sampleNumber Integer. The number of sample points of each raster layer to fit variogram models. It is 5000 by default,
#' however it can be increased by user to represent their region well (relevant to the extent and resolution of rasters).
#' @param border A SpatialPolygons* or sf object for clipping output blocks. This increases the computation time slightly.
#' @param speciesData A spatial or sf object. If provided, the \code{sampleNumber} is ignored and
#' variograms are created based on species locations. This option is not recommended if the species data is not
#' evenly distributed across the whole study area and/or the number of records is low.
#' @param showPlots Logical. Show final plot of spatial blocks and autocorrelation ranges.
#' @param maxpixels Number of random pixels to select the blocks over the study area.
#' @param plotVariograms Logical. Plot fitted variograms. This can also be done after the analysis. Set to \code{FALSE} by default.
#' @param doParallel Logical. Run in parallel when more than one raster layer is available. Given multiple CPU cores, it is
#' recommended to set it to \code{TRUE} when there is a large number of rasters to process.
#' @param nCores Integer. Number of CPU cores to run in parallel. If \code{nCores = NULL} half of available cores in your
#' machine will be used.
#' @param progress Logical. Shows progress bar. It works only when \code{doParallel = FALSE}.
#' @param degMetre Integer. The conversion rate of metres to degree. This is for constructing spatial
#' blocks for visualisation. When the input map is in geographic coordinate system (decimal degrees), the block size is
#' calculated based on deviding the calculated \emph{range} by this value to convert to the input map's unit
#' (by default 111325; the standard distance of a degree in metres, on the Equator).
#'
#' @import automap
#' @import foreach
#' @import doParallel
#'
#' @references O’Sullivan, D., Unwin, D.J., 2010. Geographic Information Analysis, 2nd ed. John Wiley & Sons.
#'
#' Roberts et al., 2017. Cross-validation strategies for data with temporal, spatial, hierarchical,
#' or phylogenetic structure. Ecography. 40: 913-929.
#'
#' @return An object of class S3. A list object including:
#'     \itemize{
#'     \item{range - the suggested range, which is the median of all calculated ranges}
#'     \item{rangeTable - a table of input covariates names and their autocorrelation range}
#'     \item{plots - the output plot (the plot is shown by default)}
#'     \item{sampleNumber}
#'     \item{variograms - fitted variograms for all layers}
#'     }
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # load the example raster data
#' awt <- raster::brick(system.file("extdata", "awt.grd", package = "blockCV"))
#' # import presence-absence species data
#' PA <- read.csv(system.file("extdata", "PA.csv", package = "blockCV"))
#' # coordinate reference system
#' Zone55s <- "+proj=utm +zone=55 +south +ellps=GRS80 +units=m +no_defs"
#' # make a SpatialPointsDataFrame object from data.frame
#' pa_data <- sp::SpatialPointsDataFrame(PA[,c("x", "y")], PA, proj4string=CRS(Zone55s))
#'
#' # run the model in parallel
#' range1 <- spatialAutoRange(rasterLayer = awt,
#'                            sampleNumber = 5000, # number of cells to be used
#'                            doParallel = TRUE,
#'                            nCores = NULL, # use half of the CPU cores
#'                            plotVariograms = FALSE,
#'                            showPlots = TRUE)
#'
#' range2 <- spatialAutoRange(rasterLayer = awt,
#'                            speciesData = pa_data, # use species locations to create variogram(s)
#'                            doParallel = TRUE)
#'
#' # run the model with no parallel
#' range3 <- spatialAutoRange(rasterLayer = awt,
#'                            sampleNumber = 5000,
#'                            doParallel = FALSE,
#'                            showPlots = TRUE,
#'                            progress = TRUE)
#'
#' # show the result
#' summary(range1)
#' }
spatialAutoRange <- function(rasterLayer, sampleNumber=5000, border=NULL, speciesData=NULL,
                             doParallel=TRUE, nCores=NULL, showPlots=TRUE, degMetre=111325,
                             maxpixels=1e+05, plotVariograms=FALSE, progress=TRUE){
  if(!is.null(speciesData)){
    if((methods::is(speciesData, "SpatialPoints") || methods::is(speciesData, "sf"))==FALSE){
      stop("speciesData should be SpatialPoints* or sf object")
    }
  }
  if(methods::is(rasterLayer, 'Raster')){
    if(any(is.factor(rasterLayer))){warning("rasterLayer should not include any factor layer")}
    numLayer <- raster::nlayers(rasterLayer)
    if(is.na(sp::proj4string(rasterLayer))){
      mapext <- raster::extent(rasterLayer)[1:4]
      if(mapext >= -180 && mapext <= 180){
        raster::crs(rasterLayer) <- sp::CRS("+init=epsg:4326")
        warning("The input layer has no CRS defined. Based on the extent of the input map it is assumed to have geographic coordinate system")
      }
    }
    # reduce the sampleNumber if the raster does not have enough cells
    if(raster::ncell(rasterLayer) < 10 * sampleNumber){
      rp <- raster::rasterToPoints(rasterLayer[[1]])
      if(nrow(rp) < sampleNumber){
        sampleNumber <- nrow(rp)
        message("The sampleNumber reduced to ", sampleNumber, ", the total number of available cells")
      }
    }
    if(numLayer==1){
      if(is.null(speciesData)){
        rasterPoints <- raster::rasterToPoints(rasterLayer, spatial=TRUE)
        set.seed(2017)
        points <- rasterPoints[sample(1:nrow(rasterPoints), sampleNumber, replace=FALSE), ]
        names(points) <- 'target'
      } else{
        points <- raster::extract(rasterLayer, speciesData, na.rm=TRUE, sp=TRUE)
        names(points)[ncol(points)] <- "target"
      }
      fittedVar = automap::autofitVariogram(target~1, points)
      theRange <- fittedVar$var_model[2,3]
      if(plotVariograms==TRUE){
        plot(fittedVar)
      }
    } else if(numLayer>1){
      df <- data.frame(layers=1:numLayer, range=1:numLayer, sill=1:numLayer)
      variogramList <- list()
      message(paste('There are', numLayer, 'raster layers'))
      if(doParallel==TRUE){
        if(is.null(nCores)){
          nCores <- ceiling((parallel::detectCores()) / 2)
        }
        cl <- parallel::makeCluster(nCores) # use snow clusters
        doParallel::registerDoParallel(cl) # set up a parallel backend for foreach package
        pp <- foreach::foreach(r = 1:numLayer, .inorder=TRUE, .packages=c('raster', 'automap')) %dopar% {
          if(is.null(speciesData)){
            rasterPoints <- raster::rasterToPoints(rasterLayer[[r]], spatial=TRUE)
            set.seed(2017)
            points <- rasterPoints[sample(1:nrow(rasterPoints), sampleNumber, replace=FALSE), ]
            names(points) <- 'target'
          } else{
            points <- raster::extract(rasterLayer[[r]], speciesData, na.rm=TRUE, sp=TRUE)
            names(points)[ncol(points)] <- "target"
          }
          fittedVar <- automap::autofitVariogram(target~1, points)
        }
        for(v in 1:length(pp)){
          df$range[v] <- pp[[v]]$var_model[2,3]
          df$sill[v] <- pp[[v]]$var_model[2,2]
          df$layers[v] <- names(rasterLayer)[v]
        }
        variogramList <- pp # save variogram of all layer
        parallel::stopCluster(cl)
        foreach::registerDoSEQ()
      } else{
        if(progress==TRUE){
          pb <- progress::progress_bar$new(format = " Progress [:bar] :percent in :elapsed",
                                           total=numLayer, clear=FALSE, width=75) # add progress bar
        }
        for(r in 1:numLayer){
          name <- names(rasterLayer[[r]])
          if(is.null(speciesData)){
            rasterPoints <- raster::rasterToPoints(rasterLayer[[r]], spatial=TRUE)
            set.seed(2017)
            points <- rasterPoints[sample(1:nrow(rasterPoints), sampleNumber, replace=FALSE), ]
            names(points) <- 'target'
          } else{
            points <- raster::extract(rasterLayer[[r]], speciesData, na.rm=TRUE, sp=TRUE)
            names(points)[ncol(points)] <- "target"
          }
          fittedVar <- automap::autofitVariogram(target~1, points)
          variogramList[[r]] <- fittedVar
          df$range[r] <- fittedVar$var_model[2,3]
          df$sill[r] <- fittedVar$var_model[2,2]
          df$layers[r] <- name;
          if(progress==TRUE){
            pb$tick() # update progress bar
          }
        }
        variogramList <- variogramList # save variogram of all layer
      }
      # calculate all ranges and mean them for block size
      theRange <- stats::median(df$range)
      modelInfo <- df[order(df$range),] # save range and sill of all layers
      if(plotVariograms==TRUE){
        for(v in 1:numLayer){
          plot(variogramList[[v]])
        }
      }
    } else stop('The raster layer is empty!')
  } else stop('The input file is not a valid R raster file')
  # creating the blocks based on calculated autocorrelation range
  # check the spatial reference sytem for specifiying block size
  if(is.na(sp::proj4string(rasterLayer))){
    mapext <- raster::extent(rasterLayer)[1:4]
    if(mapext >= -180 && mapext <= 180){
      theRange2 <- theRange * 1000
      if(numLayer>1){
        modelInfo$range <- modelInfo$range * 1000
      }
      xaxes <- "Longitude"
      yaxes <- "Latitude"
    } else{
      theRange2 <- theRange
      xaxes <- "Easting"
      yaxes <- "Northing"
    }
  } else{
    if(sp::is.projected(sp::SpatialPoints((matrix(1:10, 5, byrow=FALSE)), proj4string=raster::crs(rasterLayer)))){
      theRange2 <- theRange
      xaxes <- "Easting"
      yaxes <- "Northing"
    } else{
      theRange2 <- theRange * 1000
      if(numLayer>1){
        modelInfo$range <- modelInfo$range * 1000
      }
      xaxes <- "Longitude"
      yaxes <- "Latitude"
    }
  }
  if(is.null(border)){
    subBlocks <- rasterNet(rasterLayer[[1]], resolution=theRange2, degree=degMetre, mask=TRUE, maxpixels =maxpixels)
  } else{
    net <- rasterNet(rasterLayer[[1]], resolution=theRange2, degree=degMetre, mask=FALSE)
    if(methods::is(border, "sf")){
      border <- sf::as_Spatial(border)
    }
    subBlocks <- raster::crop(net, border)
  }
  if(numLayer>1){
    if(is.null(speciesData)){
      ptnum <- sampleNumber
    } else{
      ptnum <- nrow(speciesData)
    }
    p1 <- ggplot2::ggplot(data=modelInfo, aes(x=stats::reorder(factor(layers), range), y=range, fill=range))+
      geom_bar(stat="identity")+
      xlab("Layers") + ylab("Range (m)") +
      theme(axis.text.x = element_text(angle=75, hjust=1)) +
      ggtitle('Autocorrelation range',
              subtitle=paste('Based on', ptnum, 'sample points'))+
      guides(fill=FALSE)+
      geom_hline(yintercept=theRange2, color='red', size=0.9, linetype=2)+
      annotate("text", x=floor(nrow(modelInfo)/3), y= (theRange2 + (max(modelInfo$range)/20)),
               label="Block size", color='red')
  }
  # plot raster file in ggplot2
  samp <- raster::sampleRegular(rasterLayer[[1]], 5e+05, asRaster=TRUE)
  map.df <- raster::as.data.frame(samp, xy=TRUE, centroids=TRUE, na.rm=TRUE)
  colnames(map.df) <- c("Easting", "Northing", "MAP")
  mid <- mean(map.df$MAP)
  p2 <- ggplot2::ggplot(data=map.df, aes(y=Northing, x=Easting)) +
    geom_raster(aes(fill=MAP)) +
    coord_fixed() +
    scale_fill_gradient2(low="darkred", mid="yellow", high="darkgreen", midpoint=mid) +
    guides(fill=FALSE) +
    ggtitle('Spatial blocks', subtitle=paste("Based on", round(theRange2), "metres distance")) +
    geom_polygon(aes(x = long, y = lat, group=id),
                 data = subBlocks, color ="red",
                 fill ="orangered4",
                 alpha = 0.04,
                 size = 0.2) +
    xlab(xaxes) + ylab(yaxes)
  if(showPlots==TRUE){
    if(numLayer>1){
      multiplot(p1, p2)
    } else{
      plot(p2)
    }
  }
  if(numLayer>1){
    finalList <- list(range=theRange2, rangeTable=modelInfo, plots=list(barchart = p1, mapplot = p2),
                      sampleNumber=sampleNumber, variograms=variogramList)
  } else{
    finalList <- list(range=theRange2, plots=list(mapplot = p2), sampleNumber=sampleNumber, variograms=fittedVar)
  }
  # gc() # to release the occupied RAM in windows OS
  # specify the output class
  class(finalList) <- c("SpatialAutoRange", class(finalList))
  return(finalList)
}


#' @export
print.SpatialAutoRange <- function(x, ...){
  print(class(x))
}


#' @export
plot.SpatialAutoRange <- function(x, y, ...){
  if(length(x$plots) == 2){
    multiplot(x$plots$barchart, x$plots$mapplot)
  } else{
    plot(x$plots$mapplot)
  }
}

#' @export
summary.SpatialAutoRange <- function(object, ...){
  print("Summary statistics of spatial autocorrelation ranges of all input layers")
  print(summary(object$rangeTable$range))
  print(object$rangeTable[,1:2])
}