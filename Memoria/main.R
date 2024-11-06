library(corrplot)
library(gridExtra)
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


# Llamar a la función
lista_resultados_combinados_in <- combinar_resultados_iteraciones(resultados_in, resultados_in_2_vrs, resultados_in_3_vrs, resultados_in_2_crs, resultados_in_3_crs)


# ------------------------------------------------------------- #
# CALCULO Y VISUALIZACION DE CORRELACION DE DATOS SENSIBILIZADOS

# Revisar la correlación de los datos dentro de cada dataframe en la lista
correlaciones_lista <- lapply(lista_resultados_combinados, function(df) {
  # Convertir las columnas de VRS y CRS a numéricas (en caso de que sean texto debido a "NO APLICA")
  df_num <- df %>%
    select(-IdEstablecimiento) %>%  # Excluir la columna IdEstablecimiento
    mutate(across(starts_with("vrs_iteracion_"), ~ as.numeric(replace(., . == "NO APLICA", NA)))) %>%
    mutate(across(starts_with("crs_iteracion_"), ~ as.numeric(replace(., . == "NO APLICA", NA))))
  
  # Calcular la correlación solo con columnas numéricas, excluyendo NA
  cor(df_num[, sapply(df_num, is.numeric)], use = "complete.obs")
})

# Nombrar la lista con los años para identificación
names(correlaciones_lista) <- names(lista_resultados_combinados)

# Mostrar las correlaciones
correlaciones_lista




# Instalar y cargar la librería corrplot si no está instalada
if (!require(corrplot)) install.packages("corrplot")
library(corrplot)


# Definir la cuadrícula de gráficos (por ejemplo, 2 filas x 3 columnas)
num_graficos <- length(correlaciones_lista)
filas <- ceiling(sqrt(num_graficos))
columnas <- ceiling(num_graficos / filas)

# Ajustar la ventana gráfica
par(mfrow = c(filas, columnas), mar = c(1, 1, 2, 1))  # Ajusta los márgenes y la cuadrícula

# Graficar todas las matrices de correlación
for (anio in names(correlaciones_lista)) {
  corrplot(correlaciones_lista[[anio]], method = "color", title = paste("Matriz de Correlación - Año", anio))
}

# Restablecer la configuración gráfica por defecto
par(mfrow = c(1, 1))

# ------------------------------------------------------------- #
# ------------------------------------------------------------- #

library(reshape2)
library(ggplot2)

# Crear una lista para almacenar las correlaciones entre matrices de distintos años
correlacion_entre_anios <- list()

# Calcular la correlación entre las matrices de correlación de cada par de años consecutivos
for (i in 1:(length(anios) - 1)) {
  anio_actual <- anios[i]
  anio_siguiente <- anios[i + 1]
  
  # Tomar las matrices de correlación y convertirlas a vectores para comparación
  matriz_actual <- as.vector(correlaciones_lista[[anio_actual]])
  matriz_siguiente <- as.vector(correlaciones_lista[[anio_siguiente]])
  
  # Calcular la correlación entre las dos matrices (vectores)
  correlacion <- cor(matriz_actual, matriz_siguiente, use = "complete.obs")
  correlacion_entre_anios[[paste(anio_actual, "vs", anio_siguiente)]] <- correlacion
}

# Mostrar la correlación entre los años
correlacion_entre_anios

# Graficar la correlación entre los años como un gráfico de barras
correlacion_df <- data.frame(
  Comparacion = names(correlacion_entre_anios),
  Correlacion = unlist(correlacion_entre_anios)
)

ggplot(correlacion_df, aes(x = Comparacion, y = Correlacion, fill = Correlacion)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  labs(title = "Correlación entre matrices de correlación de distintos años", x = "Comparación de Años", y = "Correlación") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# ------------------------------------------------------------ #
# ------------------------------------------------------------ #



# Graficas #
# Generar y mostrar gráficos VRS
graficos_vrs <- list(
  generar_graficos_iteracion(resultados_in, "Input VRS", "vrs"),
  generar_graficos_iteracion(resultados_in_2_vrs, "Input VRS", "vrs"),
  generar_graficos_iteracion(resultados_in_3_vrs, "Input VRS", "vrs")
)

# Mostrar gráficos VRS
lapply(graficos_vrs, function(graficos) {
  grid.arrange(grobs = graficos, ncol = 3)
})

# Generar y mostrar gráficos CRS
graficos_crs <- list(
  generar_graficos_iteracion(resultados_in, "Input CRS", "crs"),
  generar_graficos_iteracion(resultados_in_2_crs, "Input CRS", "crs"),
  generar_graficos_iteracion(resultados_in_3_crs, "Input CRS", "crs")
)

# Mostrar gráficos CRS
lapply(graficos_crs, function(graficos) {
  grid.arrange(grobs = graficos, ncol = 3)
})


# Iterar sobre los años y mostrar los gráficos
anios <- c("2014", "2015", "2016", "2017", "2018", "2019")
for (anio in anios) {
  graficos_vrs <- generar_graficos_por_anio(anio, "Input - VRS")
  # Mostrar los gráficos en una fila (3 gráficos por año)
  grid.arrange(grobs = graficos_vrs, ncol = 3, top = paste("Comparación de Eficiencia Input VRS - Año", anio))
}


for (anio in anios) {
  graficos_crs <- generar_graficos_por_anio_crs(anio, "CRS")
  # Mostrar los gráficos en una fila (3 gráficos por año)
  grid.arrange(grobs = graficos, ncol = 3, top = paste("Comparación de Eficiencia Input CRS - Año", anio))
}








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
