setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("functions.R")
source("graphics.R")

# ==============================================
#  PROCESAMIENTO DE DATOS
# ==============================================

#  CONSOLIDADO DE DATOS POR AÑO
anios <- 2014:2023
anios_pre_pandemia <- c("2014", "2015", "2016", "2017", "2018", "2019")
anios_pandemia <- c("2020", "2021", "2022", "2023")

load("RData/datos.RData")
load("RData/datos_atipicos.RData")
load("RData/resultados_eficiencia.RData")
load(paste0("RData/",carpeta,"/malmquist.RData"))
load(paste0("RData/",carpeta,"/determinantes.RData"))



