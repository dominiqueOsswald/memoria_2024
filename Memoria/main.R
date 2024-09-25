library(readxl)
library(dplyr)
library(Benchmarking)
library(corrplot)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("functions.R")
source("graphics.R")



data_2019 <- consolidar_datos_por_anio(2019)

resultados_2019 <- analisis_dea(data_2019)


