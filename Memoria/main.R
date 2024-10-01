library(readxl)
library(dplyr)
library(Benchmarking)
library(corrplot)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("functions.R")
#source("graphics.R")
source("graphics2.R")

data_2019 <- consolidar_datos_por_anio(2019)
resultados_2019_in <- analisis_dea_in(data_2019)

chile_vrs(resultados_2019_in) 
region <- region_vrs(resultados_2019_in, 13)
print(region)

mapa_interactivo <- ggplotly(region, tooltip = "text")



