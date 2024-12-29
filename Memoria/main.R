setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("functions.R")
source("graphics.R")

# ==============================================
#  PROCESAMIENTO DE DATOS
# ==============================================

#  CONSOLIDADO DE DATOS POR AÑO
anios <- 2014:2023
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
  io = resultados_iteracion(datos, "io"),
  oo = resultados_iteracion(datos, "oo")
)


#  SENSIBILIDAD - ELIMINACIÓN DE DATOS ATÍPICOS

resultados_cortados <- list(
  io = resultados_corte(resultados$io, "io"),
  oo = resultados_corte(resultados$oo, "oo")
)




graficas_sensibilidad <- list(
  # GRAFICA DE SENSIBILIDAD POR EFICIENCIA
  eficiencia_io = graficar_correlaciones(resultados[["io"]][["resultados_correlacion"]][["correlaciones_lista"]], "io", c("vrs_i1", "vrs_i2", "vrs_i3", "crs_i1", "crs_i2",  "crs_i3"), "Sensibilidad por eliminación de DMU eficientes"),
  eficiencia_oo = graficar_correlaciones(resultados[["oo"]][["resultados_correlacion"]][["correlaciones_lista"]], "oo", c("vrs_i1", "vrs_i2", "vrs_i3", "crs_i1", "crs_i2",  "crs_i3"),  "Sensibilidad por eliminación de DMU eficientes"),
  
  # GRAFICA DE SENSIBILIDAD POR ELIMINACION DE DATOS ATIPICOS
  atipicos_io = graficar_correlaciones(resultados_cortados[["io"]][["resultados_correlacion"]][["correlaciones_lista"]], "io", c("vrs_i1", "vrs_i2", "vrs_i3", "crs_i1", "crs_i2",  "crs_i3"),  "Sensibilidad por eliminación de datos atípicos"),
  atipicos_oo = graficar_correlaciones(resultados_cortados[["oo"]][["resultados_correlacion"]][["correlaciones_lista"]], "oo", c("vrs_i1", "vrs_i2", "vrs_i3", "crs_i1", "crs_i2",  "crs_i3"), "Sensibilidad por eliminación de datos atípicos")

)

#print(graficas_sensibilidad$eficiencia_io)

# CORRELACION DE VALORES ORIGINALES PARA TODAS LAS COMBINACIONES EN TODOS LOS AÑOS
input_output_original <- combinar_resultados_in_out(resultados$io[["original"]], resultados$oo[["original"]])

correlacion_todos_metodos <- calcular_correlaciones_all(input_output_original)

grafica_correlacion_metodos <- graficar_correlaciones(correlacion_todos_metodos[["correlaciones_lista"]], "ambos", c("vrs_io", "vrs_oo", "crs_io", "crs_oo"))


# ==============================================
#  MALMQUIST 
# ==============================================

malmquist_indices <- list(
  in_vrs = malmquist("vrs", "in"),
  in_crs = malmquist("crs", "in"),
  out_vrs = malmquist("vrs", "out"),
  out_crs = malmquist("crs", "out")
)

malmquist_graficas <- procesar_y_graficar(malmquist_indices)

# ==============================================
#  DETERMINANTES
# ==============================================
# -------------------------------------------- #
#  CONFIGURACIÓN Y MODELO RANDOM FOREST
# -------------------------------------------- #

# Aplicar Random Forest para cada año
random_forest <- list(
  io_vrs = lapply(anios, function(anio) {analize_rf(anio, resultados_in = resultados$io, 500, "vrs")}),
  io_crs = lapply(anios, function(anio) {analize_rf(anio, resultados_in = resultados$io, 500, "crs")}),
  oo_vrs = lapply(anios, function(anio) {analize_rf(anio, resultados_in = resultados$oo, 500, "vrs")}),
  oo_crs = lapply(anios, function(anio) {analize_rf(anio, resultados_in = resultados$oo, 500, "crs")})
)

# Asignar nombres a la lista de modelos
names(random_forest$io_vrs) <- paste0(anios)
names(random_forest$io_crs) <- paste0(anios)
names(random_forest$oo_vrs) <- paste0(anios)
names(random_forest$oo_crs) <- paste0(anios)

# -------------------------------------------- #
#  EXTRACCIÓN DE VARIABLES POR AÑO
# -------------------------------------------- #


variables_random_forest <- list()

# Iterar sobre cada método/posición
for (metodo in names(random_forest)) {
  variables_random_forest[[metodo]] <- lapply(random_forest[[metodo]], function(modelo) {
    names(modelo$top_IncMSE)  # Extraer los nombres de filas de 'importancia'
  })
}




# -------------------------------------------- #
#  COMBINACIÓN Y ANÁLISIS DE VARIABLES
# -------------------------------------------- #


# Crear listas para almacenar resultados
resultados_IncNodePurity <- list()
resultados_IncMSE <- list()

# Definir períodos
anios_pre_pandemia <- c("2014", "2015", "2016", "2017", "2018", "2019")
anios_pandemia <- c("2020", "2021", "2022", "2023")

# Iterar sobre cada método
for (metodo in names(random_forest)) {
  
  # Crear dataframes vacíos para almacenar resultados
  df_IncNodePurity <- data.frame(Variable = character(), stringsAsFactors = FALSE)
  df_IncMSE <- data.frame(Variable = character(), stringsAsFactors = FALSE)
  
  # Recopilar datos por año
  for (anio in names(random_forest[[metodo]])) {
    # Obtener importancia
    importancia <- random_forest[[metodo]][[anio]]$importancia
    
    # --- IncNodePurity ---
    temp_IncNodePurity <- data.frame(
      Variable = rownames(importancia),
      Valor = importancia[, "IncNodePurity"],  # Seleccionar columna
      Año = anio
    )
    df_IncNodePurity <- rbind(df_IncNodePurity, temp_IncNodePurity)
    
    # --- %IncMSE ---
    temp_IncMSE <- data.frame(
      Variable = rownames(importancia),
      Valor = importancia[, "%IncMSE"],  # Seleccionar columna
      Año = anio
    )
    df_IncMSE <- rbind(df_IncMSE, temp_IncMSE)
  }
  
  # --- Procesar IncNodePurity ---
  pivot_IncNodePurity <- reshape(df_IncNodePurity, 
                                 idvar = "Variable", 
                                 timevar = "Año", 
                                 direction = "wide")
  colnames(pivot_IncNodePurity) <- gsub("Valor\\.", "", colnames(pivot_IncNodePurity))
  
  # Calcular frecuencia y promedios
  pivot_IncNodePurity$Frecuencia <- table(df_IncNodePurity$Variable)[pivot_IncNodePurity$Variable]
  pivot_IncNodePurity$Promedio_Pre_Pandemia <- rowMeans(pivot_IncNodePurity[, anios_pre_pandemia], na.rm = TRUE)
  pivot_IncNodePurity$Promedio_Pandemia <- rowMeans(pivot_IncNodePurity[, anios_pandemia], na.rm = TRUE)
  
  # Ordenar y seleccionar las 50 más importantes
  pivot_IncNodePurity <- pivot_IncNodePurity[order(-pivot_IncNodePurity$Promedio_Pre_Pandemia), ]
  resultados_IncNodePurity[[metodo]] <- head(pivot_IncNodePurity, 50)
  
  # --- Procesar %IncMSE ---
  pivot_IncMSE <- reshape(df_IncMSE, 
                          idvar = "Variable", 
                          timevar = "Año", 
                          direction = "wide")
  colnames(pivot_IncMSE) <- gsub("Valor\\.", "", colnames(pivot_IncMSE))
  
  # Calcular frecuencia y promedios
  pivot_IncMSE$Frecuencia <- table(df_IncMSE$Variable)[pivot_IncMSE$Variable]
  pivot_IncMSE$Promedio_Pre_Pandemia <- rowMeans(pivot_IncMSE[, anios_pre_pandemia], na.rm = TRUE)
  pivot_IncMSE$Promedio_Pandemia <- rowMeans(pivot_IncMSE[, anios_pandemia], na.rm = TRUE)
  
  # Ordenar y seleccionar las 50 más importantes
  pivot_IncMSE <- pivot_IncMSE[order(-pivot_IncMSE$Promedio_Pre_Pandemia), ]
  resultados_IncMSE[[metodo]] <- head(pivot_IncMSE, 50)
}

# Mostrar resultados
head(resultados_IncNodePurity[["io_vrs"]])  # Ver resultados para IncNodePurity
head(resultados_IncMSE[["io_vrs"]])        # Ver resultados para %IncMSE




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
dataframes <- list("2014" = resultados$io[["original"]][["2014"]][["data"]], 
                   "2015" = resultados$io[["original"]][["2015"]][["data"]], 
                   "2016" = resultados$io[["original"]][["2016"]][["data"]], 
                   "2017" = resultados$io[["original"]][["2017"]][["data"]], 
                   "2018" = resultados$io[["original"]][["2018"]][["data"]], 
                   "2019" = resultados$io[["original"]][["2019"]][["data"]],
                   "2020" = resultados$io[["original"]][["2020"]][["data"]])


gran_dataframe <- reduce(dataframes, full_join, by = c("IdEstablecimiento", "vrs"))

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