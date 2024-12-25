setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("functions.R")
source("graphics.R")

# ==============================================
#  PRE PROCESAMIENTO DE DATOS
# ==============================================

#  CONSOLIDADO DE DATOS POR AÑO
anios <- c(2014:2023)
datos_iniciales <- lapply(anios, consolidar_datos_por_anio)
names(datos_iniciales) <- as.character(anios)

# Encontrar las DMUs comunes en todos los años y filtrar los datos para incluir solo esas DMUs
dmus_comunes <- Reduce(intersect, lapply(datos_iniciales, `[[`, "IdEstablecimiento"))
datos <- lapply(datos_iniciales, function(data) data[data$IdEstablecimiento %in% dmus_comunes, ])

# ==============================================
#  CÁLCULO DEA
# ==============================================

#  SENSIBILIDAD - ELIMINACION EFICIENTES

resultados <- list(
  io = resultados_iteracion(datos, "io")
  #oo = resultados_iteracion(datos, "oo")
)

resultados2 <- list(
  oo = resultados_iteracion(datos[1:2], "oo")
  #oo = resultados_iteracion(datos, "oo")
)

resultados3 <- list(
  oo = resultados_iteracion(datos[2:3], "oo")
  #oo = resultados_iteracion(datos, "oo")
)

resultados4 <- list(
  oo = resultados_iteracion(datos[4:5], "oo")
  #oo = resultados_iteracion(datos, "oo")
)

resultados5 <- list(
  oo = resultados_iteracion(datos[6:7], "oo")
  #oo = resultados_iteracion(datos, "oo")
)

resultados6 <- list(
  oo = resultados_iteracion(datos[8:10], "oo")
  #oo = resultados_iteracion(datos, "oo")
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
#year, resultados_in, n_top, trees, tipo, orientacion
random_forest_results <- list(
  input_vrs = setNames(
    lapply(anios, function(anio) {analize_rf(anio, resultados$io, 50, 300, "vrs", "Input")}),anios
  ),
  input_crs = setNames(
    lapply(anios, function(anio) {analize_rf(anio, resultados$io, 50, 300, "crs", "Input")}),anios
  ),
  output_vrs = setNames(
    lapply(anios, function(anio) {analize_rf(anio, resultados$oo, 50, 300, "vrs", "Output")}),anios
  ),
  output_crs = setNames(
    lapply(anios, function(anio) {analize_rf(anio, resultados$oo, 50, 300, "crs", "Output")}),anios
  )
)

# -------------------------------------------- #
#  EXTRACCIÓN DE VARIABLES POR AÑO
# -------------------------------------------- #

# Extraer variables de cada modelo dentro de random_forest_results
variables_random_forest <- lapply(random_forest_results, function(modelos_por_anio) {
  lapply(modelos_por_anio, rownames)
})

# Asignar nombres dinámicos
names(variables_random_forest) <- paste0("variables_random_forest_", names(random_forest_results))


# -------------------------------------------- #
#  COMBINACIÓN Y ANÁLISIS DE VARIABLES
# -------------------------------------------- #

# Crear una lista para almacenar las tablas de frecuencias por método
frecuencias_por_metodo <- lapply(variables_random_forest, function(variables_por_anio) {
  # Combinar todas las variables para un método específico
  todas_las_variables <- unlist(variables_por_anio)
  
  # Calcular las frecuencias de las variables
  frecuencias <- table(todas_las_variables)
  
  # Filtrar y ordenar las variables más frecuentes
  frecuencias_filtradas <- head(sort(frecuencias[frecuencias > 1], decreasing = TRUE), 100)
  
  # Retornar las frecuencias filtradas
  return(frecuencias_filtradas)
})

# Asignar nombres a las tablas de frecuencias
names(frecuencias_por_metodo) <- paste0("frecuencias_", names(variables_random_forest))

# Verificar los resultados
print(frecuencias_por_metodo)


# -------------------------------------------- #
#  VISUALIZACIÓN DE FRECUENCIAS
# -------------------------------------------- #


# Crear un dataframe para combinar las frecuencias por método
df_frecuencias <- bind_rows(
  lapply(names(frecuencias_por_metodo), function(metodo) {
    # Convertir la tabla de frecuencias a dataframe
    df <- as.data.frame(frecuencias_por_metodo[[metodo]])
    colnames(df) <- c("Variable", "Frecuencia")
    # Agregar el nombre del método como columna
    df$Metodo <- metodo
    return(df)
  })
)


# Crear gráficos separados para cada método
for (metodo in names(frecuencias_por_metodo)) {
  # Convertir la tabla de frecuencias en un dataframe
  df_frecuencias <- as.data.frame(frecuencias_por_metodo[[metodo]])
  colnames(df_frecuencias) <- c("Variable", "Frecuencia")
  
  # Crear el gráfico
  plot <- ggplot(df_frecuencias, aes(x = reorder(Variable, -Frecuencia), y = Frecuencia)) +
    geom_bar(stat = "identity", fill = "blue") +
    labs(
      title = paste("Frecuencia de Variables -", metodo),
      x = "Variable",
      y = "Frecuencia"
    ) +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1),
      plot.title = element_text(hjust = 0.5)
    )
  
  # Mostrar el gráfico
  print(plot)
  
  # Opcional: Guardar el gráfico en un archivo
  ggsave(filename = paste0(metodo, "_frecuencia_variables.png"), plot = plot, width = 10, height = 6)
}


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