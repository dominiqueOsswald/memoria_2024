library(corrplot)
library(purrr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("functions.R")
source("graphics.R")

# ----------------------------------------------- #
# Periodo previo a pandemia #

# Datos #
datos <- list(
  "2014" = consolidar_datos_por_anio(2014),
  "2015" = consolidar_datos_por_anio(2015),
  "2016" = consolidar_datos_por_anio(2016),
  "2017" = consolidar_datos_por_anio(2017),
  "2018" = consolidar_datos_por_anio(2018),
  "2019" = consolidar_datos_por_anio(2019)
)

# -------------------------------------------- #
# -------------------------------------------- #

# DEA - INPUT
resultados_in <- aplicar_analisis_dea(datos, "io")

# SENSIBILIDAD - VRS 
resultados_in_2_vrs <- aplicar_sensibilidad(datos, lapply(resultados_in, `[[`, "data"), 0.99, "io", "vrs", FALSE)
resultados_in_3_vrs <- aplicar_sensibilidad(datos, lapply(resultados_in_2_vrs, `[[`, "data"), 0.99, "io", "vrs", FALSE)

# SENSIBILIDAD - CRS 
resultados_in_2_crs <- aplicar_sensibilidad(datos, lapply(resultados_in, `[[`, "data"), 0.99, "io", "crs", FALSE)
resultados_in_3_crs <- aplicar_sensibilidad(datos, lapply(resultados_in_2_crs, `[[`, "data"), 0.99, "io", "crs", FALSE)

# SENSIBILIDAD - ESCALA 
# resultados_in_2_esc <- aplicar_sensibilidad(datos, lapply(resultados_in, `[[`, "data"), 0.99, "io", "esc")
# resultados_in_3_esc <- aplicar_sensibilidad(datos, lapply(resultados_in_2_esc, `[[`, "data"), 0.99, "io", "esc")



# -------------------------------------------- #
# -------------------------------------------- #
# DEA - OUTPUT
resultados_out <- aplicar_analisis_dea(datos, "oo")

# SENSIBILIDAD - VRS 
# SE ELIMINAN AQUELLOS DMU QUE SON EFICIENTES
resultados_out_2_vrs <- aplicar_sensibilidad(datos, lapply(resultados_out, `[[`, "data"), 1, "oo", "vrs", TRUE)
resultados_out_3_vrs <- aplicar_sensibilidad(datos, lapply(resultados_out_2_vrs, `[[`, "data"), 1, "oo", "vrs", TRUE)

# SENSIBILIDAD - CRS 
resultados_out_2_crs <- aplicar_sensibilidad(datos, lapply(resultados_out, `[[`, "data"), 1, "oo", "crs", TRUE)
resultados_out_3_crs <- aplicar_sensibilidad(datos, lapply(resultados_out_2_crs, `[[`, "data"), 1, "oo", "crs", TRUE)

# SENSIBILIDAD - ESCALA 
# resultados_in_2_esc <- aplicar_sensibilidad(datos, lapply(resultados_in, `[[`, "data"), 0.99, "io", "esc")
# resultados_in_3_esc <- aplicar_sensibilidad(datos, lapply(resultados_in_2_esc, `[[`, "data"), 0.99, "io", "esc")









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
