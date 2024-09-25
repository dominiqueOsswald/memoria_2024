library(readxl)
library(dplyr)
library(Benchmarking)
library(corrplot)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("functions.R")
source("graphics.R")



data_2019 <- consolidar_datos_por_anio(2019)

resultados_2019_in <- analisis_dea_in(data_2019)

resultados_2019_out <- analisis_dea_out(data_2019)

resultados_2019_graph <- analisis_dea_graph(data_2019)

