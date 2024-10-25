library(corrplot)
library(purrr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("functions.R")
source("graphics.R")

# ----------------------------------------------- #
# Periodo previo a pandemia #

# Datos #
data_2014 <- consolidar_datos_por_anio(2014)
data_2015 <- consolidar_datos_por_anio(2015)
data_2016 <- consolidar_datos_por_anio(2016)
data_2017 <- consolidar_datos_por_anio(2017)
data_2018 <- consolidar_datos_por_anio(2018)
data_2019 <- consolidar_datos_por_anio(2019)


# DEA - IN
resultados_2014_in <- analisis_dea_in(data_2014)
resultados_2015_in <- analisis_dea_in(data_2015)
resultados_2016_in <- analisis_dea_in(data_2016)
resultados_2017_in <- analisis_dea_in(data_2017)
resultados_2018_in <- analisis_dea_in(data_2018)
resultados_2019_in <- analisis_dea_in(data_2019)



resultados_2014_in_2 <- sensibilidad_parametro(data_2014, resultados_2014_in, FALSE, 0.99)
resultados_2015_in_2 <- sensibilidad_parametro(data_2015, resultados_2015_in, FALSE, 0.99)
resultados_2016_in_2 <- sensibilidad_parametro(data_2016, resultados_2016_in, FALSE, 0.99)
resultados_2017_in_2 <- sensibilidad_parametro(data_2017, resultados_2017_in, FALSE, 0.99)
resultados_2018_in_2 <- sensibilidad_parametro(data_2018, resultados_2018_in, FALSE, 0.99)
resultados_2019_in_2 <- sensibilidad_parametro(data_2019, resultados_2019_in, FALSE, 0.99)



resultados_2014_in_3 <- sensibilidad_parametro(data_2014, resultados_2014_in_2$resultados, FALSE, 0.99)
resultados_2015_in_3 <- sensibilidad_parametro(data_2015, resultados_2015_in_2$resultados, FALSE, 0.99)
resultados_2016_in_3 <- sensibilidad_parametro(data_2016, resultados_2016_in_2$resultados, FALSE, 0.99)
resultados_2017_in_3 <- sensibilidad_parametro(data_2017, resultados_2017_in_2$resultados, FALSE, 0.99)
resultados_2018_in_3 <- sensibilidad_parametro(data_2018, resultados_2018_in_2$resultados, FALSE, 0.99)
resultados_2019_in_3 <- sensibilidad_parametro(data_2019, resultados_2019_in_2$resultados, FALSE, 0.99)




# DEA - OUT












# Graficas #
region_rm_2014 <- region_vrs(resultados_2014_in, 13, 2014)
print(region_rm_2014)

region_rm_2015 <- region_vrs(resultados_2015_in, 13, 2015)
print(region_rm_2015)

region_rm_2016 <- region_vrs(resultados_2016_in, 13, 2016)
print(region_rm_2016)

region_rm_2017 <- region_vrs(resultados_2017_in, 13, 2017)
print(region_rm_2017)

region_rm_2018 <- region_vrs(resultados_2018_in, 13, 2018)
print(region_rm_2018)

region_rm_2019 <- region_vrs(resultados_2019_in, 13, 2019)
print(region_rm_2019)













# ----------------------------------------------- #
# Periodo pandémico #

data_2020 <- consolidar_datos_por_anio(2020)
resultados_2020_in <- analisis_dea_in(data_2020)

# Falta esta data
#data_2021 <- consolidar_datos_por_anio(2021)
#resultados_2021_in <- analisis_dea_in(data_2021)





region_rm_2020 <- region_vrs(resultados_2020_in, 13, 2020)
print(region_rm_2020)

#region_rm_2021 <- region_vrs(resultados_2021_in, 13, 2021)
#print(region_rm_2021)





# -------------------------------------------- #

mapa_interactivo <- ggplotly(region, tooltip = "text")
print(mapa_interactivo)

# ------------------------------------------- #
