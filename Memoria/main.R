library(readxl)
library(dplyr)
library(Benchmarking)
library(corrplot)
library(purrr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("functions.R")
source("graphics2.R")

# ----------------------------------------------- #
# Periodo previo a pandemia #
data_2014 <- consolidar_datos_por_anio(2014)
resultados_2014_in <- analisis_dea_in(data_2014)

data_2015 <- consolidar_datos_por_anio(2015)
resultados_2015_in <- analisis_dea_in(data_2015)

data_2016 <- consolidar_datos_por_anio(2016)
resultados_2016_in <- analisis_dea_in(data_2016)

data_2017 <- consolidar_datos_por_anio(2017)
resultados_2017_in <- analisis_dea_in(data_2017)

data_2018 <- consolidar_datos_por_anio(2018)
resultados_2018_in <- analisis_dea_in(data_2018)

data_2019 <- consolidar_datos_por_anio(2019)
resultados_2019_in <- analisis_dea_in(data_2019)


# Convertir los vectores 'vrs' en dataframes con una columna para 'IdEstablecimiento' y otra para los valores
# Asegúrate de que 'IdEstablecimiento' esté disponible y sea consistente en todas las listas.


iteracion_1 <- comparativa(resultados_2014_in, resultados_2015_in, resultados_2017_in, resultados_2018_in, resultados_2019_in) 
cor(iteracion_1[, c("vrs_2014", "vrs_2015", "vrs_2017", "vrs_2018", "vrs_2019")])

# ----------------------------------------------- #
# Periodo pandémico #

data_2020 <- consolidar_datos_por_anio(2020)
resultados_2020_in <- analisis_dea_in(data_2020)














chile_vrs(resultados_2019_in) 
region <- region_vrs(resultados_2019_in, 13)
print(region)

mapa_interactivo <- ggplotly(region, tooltip = "text")



