setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("functions.R")
source("graphics.R")

# ==============================================
#  PRE PROCESAMIENTO DE DATOS
# ==============================================

#  CONSOLIDADO DE DATOS POR AÑO
anios <- 2014:2020
datos_iniciales <- lapply(anios, consolidar_datos_por_anio)
names(datos_iniciales) <- as.character(anios)

#anio_2020 <- consolidar_datos_por_anio(2020)
anio_2022 <- consolidar_datos_por_anio(2022)

# Encontrar las DMUs comunes en todos los años y filtrar los datos para incluir solo esas DMUs
dmus_comunes <- Reduce(intersect, lapply(datos_iniciales, `[[`, "IdEstablecimiento"))
datos <- lapply(datos_iniciales, function(data) data[data$IdEstablecimiento %in% dmus_comunes, ])

# ==============================================
#  CÁLCULO DEA
# ==============================================

#  SENSIBILIDAD - ELIMINACION EFICIENTES

resultados <- list(
  io = resultados_iteracion(datos, "io"),
  oo = resultados_iteracion(datos, "oo")
)

#  COMPARACION DE VALORES ORIGINALES INPUT - OUTPUT - VRS - CRS 

input_output_original <- combinar_resultados_in_out(resultados$io[["original"]], resultados$oo[["original"]])
graficas_in_out <- calcular_y_graficar_correlaciones(input_output_original, anios, "ambos")


#  SENSIBILIDAD - ELIMINACIÓN DE DATOS ATÍPICOS

resultados_cortados <- list(
  io = resultados_corte(resultados$io, "io"),
  oo = resultados_corte(resultados$oo, "oo")
)


# ==============================================
#  MALMQUIST 
# ==============================================

malmquist_indices <- list(
  in_vrs = malmquist("vrs", "in"),
  in_crs = malmquist("crs", "in"),
  out_vrs = malmquist("vrs", "out"),
  out_crs = malmquist("crs", "out")
)

procesar_y_graficar(malmquist_indices)

# ==============================================
#  DETERMINANTES
# ==============================================
# -------------------------------------------- #
#  CONFIGURACIÓN Y MODELO RANDOM FOREST
# -------------------------------------------- #

# Definir años de interés
anios <- 2014:2020

# Aplicar Random Forest para cada año
random_forest <- lapply(anios, function(anio) {
  analize_rf(anio, resultados_in = resultados$io, 500)
})

# Asignar nombres a la lista de modelos
names(random_forest) <- paste0("random_forest_", anios)

# -------------------------------------------- #
#  EXTRACCIÓN DE VARIABLES POR AÑO
# -------------------------------------------- #

variables_random_forest <- lapply(random_forest, rownames)
names(variables_random_forest) <- paste0("variables_random_forest_", anios)

# -------------------------------------------- #
#  COMBINACIÓN Y ANÁLISIS DE VARIABLES
# -------------------------------------------- #

# Combinar todas las variables en una sola lista
todas_las_variables <- unlist(variables_random_forest)

# Calcular las frecuencias de cada variable
frecuencias <- table(todas_las_variables)
frecuencias_filtradas <- head(sort(frecuencias, decreasing = TRUE)[frecuencias > 1], 100)


# -------------------------------------------- #
#  VISUALIZACIÓN DE FRECUENCIAS
# -------------------------------------------- #

library(ggplot2)

# Convertir las frecuencias filtradas en un dataframe
df_frecuencias <- as.data.frame(frecuencias_filtradas)
colnames(df_frecuencias) <- c("Variable", "Frecuencia")

# Graficar las frecuencias
ggplot(df_frecuencias, aes(x = reorder(Variable, -Frecuencia), y = Frecuencia)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(
    title = "Frecuencia de variables entre años", 
    x = "Variable", 
    y = "Frecuencia"
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#

# ==============================================
#  RESULTADOS
# ==============================================

dataframes <- lapply(names(dataframes), function(year) {
  dataframes[[year]] %>%
    select(IdEstablecimiento, vrs) %>%
    rename(!!paste0("vrs_", year) := vrs)
})


gran_dataframe <- reduce(dataframes, full_join, by = "IdEstablecimiento")

gran_dataframe <- gran_dataframe %>%
  rowwise() %>%
  mutate(promedio_vrs = mean(c_across(starts_with("vrs_")), na.rm = TRUE))




library(openxlsx)

# Crear un workbook
wb <- createWorkbook()

# Agregar hojas al workbook
addWorksheet(wb, "Eficiencias")
addWorksheet(wb, "Indice Malmquist")
addWorksheet(wb, "Determinantes")

# Escribir dataframes en las hojas
writeData(wb, "Eficiencias", gran_dataframe)
writeData(wb, "Indice Malmquist", malmquist_indices[["in_vrs"]][["index"]])  # Reemplaza con tu segundo dataframe
writeData(wb, "Determinantes", frecuencias_filtradas)  # Reemplaza con tu segundo dataframe

# Guardar el archivo Excel
saveWorkbook(wb, "Resultados.xlsx", overwrite = TRUE)





# GRAFICA DE PROMEDIO DE EFICIENCIAS
datos <- na.omit(gran_dataframe$promedio_vrs)
mediana <- median(datos)

# Crear el gráfico
grafico <- ggplot(data.frame(x = datos), aes(x = x)) +
  geom_density(fill = "blue", alpha = 0.5, color = "blue") +
  geom_vline(aes(xintercept = mediana), color = "green", linetype = "dashed", size = 1) +
  ggtitle("Densidad") +
  xlab("Valores") +
  ylab("Densidad") +
  theme_minimal() +
  annotate("text", x = mediana, y = 0.15, 
           label = paste0("Mediana: ", round(mediana, 2)), color = "green", hjust = -0.1)

# Renderizar el gráfico
print(grafico)

# ==============================================
#    GRAFICAS
# ==============================================
#  TODOS - GRAFICA DEA INPUT VRS

lapply(anios, function(anio) {
  chile_map_plot(resultados$io[["original"]][[as.character(anio)]][["data"]], anio, "vrs")
})


#    MEJORES RESULTADOS 

mejores_25 <- list("in_vrs" =top_eficiencia(resultados$io, "vrs", 25, TRUE),
                   "in_crs" = top_eficiencia(resultados$io, "crs", 25, TRUE),
                   "out_vrs" = top_eficiencia(resultados$oo, "vrs", 25, TRUE),
                   "out_crs" = top_eficiencia(resultados$oo, "crs", 25, TRUE)) 


#    VRS INPUT
lapply(anios, function(anio) {
  chile_map_plot(mejores_25[["in_vrs"]][[as.character(anio)]], anio, "vrs")
})


#    REGION COLOREADA POR PORCENTAJE DENTRO DE 25 MEJOR
resumen <- resumen_eficiencia(mejores_25$in_vrs)
colorear_region(resumen)


# -------------------------------------------- #

#mapa_interactivo <- ggplotly(region, tooltip = "text")
#print(mapa_interactivo)

# ------------------------------------------- #

# ==============================================