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

load("dataR/datos.RData")
load("dataR/datos_atipicos.RData")
load("dataR/resultados_eficiencia.RData")

# Estos deben variar segun carpeta:
# - originales
# - output_vrs
# - output_crs
# - input_vrs
# - input_crs

carpeta <- "output_vrs"

load(paste0("dataR/",carpeta,"/malmquist.RData"))
load(paste0("dataR/",carpeta,"/determinantes.RData"))



