#general
install.packages(c("tidyverse", "data.table"))

#librerías gráficas
install.packages(c("cowplot", "ggdendro", "ggfortify", "viridis"))

#librerías GIS
install.packages(c("raster", "rgdal", "rgeos", "sf", "leaflet", "geosphere"))

#librerías para ajustar modelos
install.packages(c("ranger", "maxnet", "rpart", "mgcv"))

#librerías para analizar modelos
install.packages(c("rpart.plot", "plotmo", "pdp", "lm.beta", "breakDown", "vegan", "HH"))

#librerías sdm
install.packages(c("dismo", "ecospat", "rgbif", "ALA4R", "virtualspecies"))

#installing blockCV
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
install.packages(c("automap", "RStoolbox", "shinydashboard"))
#descargado de https://github.com/rvalavi/blockCV/archive/v1.0.tar.gz
install.packages("blockCV-1.0.tar.gz", repos = NULL, type = "source")

#chequeando que se instalaron bien
library(tidyverse)
library(data.table)
library(cowplot)
library(ggdendro)
library(ggfortify)
library(viridis)
library(raster)
library(rgdal)
library(rgeos)
library(sf)
library(leaflet)
library(geosphere)
library(ranger)
library(maxnet)
library(rpart)
library(mgcv)
library(rpart.plot)
library(plotmo)
library(pdp)
library(lm.beta)
library(breakDown)
library(vegan)
library(HH)
library(dismo)
library(ecospat)
library(rgbif)
library(ALA4R)
library(virtualspecies)
library(blockCV)
