setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Estos deben variar segun carpeta:
# - originales
# - output_vrs
# - output_crs
# - input_vrs
# - input_crs

carpeta <- "output_vrs"

source("data.R")


# ==============================================
#  CÁLCULO DEA
# ==============================================
correlaciones_eficiencia_grafica(resultados_usar[["io"]][["resultados_correlacion"]][["correlaciones_lista"]], "io", c("VRS iteracion 1", "VRS iteracion 2", "VRS iteracion 3", "CRS iteracion 1", "CRS iteracion 2",  "CRS iteracion 3"), "Sensibilidad por eliminación de DMU eficientes")
correlaciones_eficiencia_grafica(resultados_usar[["oo"]][["resultados_correlacion"]][["correlaciones_lista"]], "oo", c("VRS iteracion 1", "VRS iteracion 2", "VRS iteracion 3", "CRS iteracion 1", "CRS iteracion 2",  "CRS iteracion 3"),  "Sensibilidad por eliminación de DMU eficientes")

# ---- SOLO VRS OO

resultados_vrs_oo <- resultados_sin_atipicos[["vrs_oo"]]

# ==============================================
#  CÁLCULO
# ==============================================

# Vector de años de interés
years <- 2014:2023

# Extraemos la submatriz de correlaciones vrs para cada año
vrs_cor_list <- lapply(years, function(y) {
  # Accedemos a la matriz de correlaciones del año y
  cor_matriz <- resultados_usar[["oo"]][["resultados_correlacion"]][["correlaciones_lista"]][[as.character(y)]]
  
  # Filtramos filas y columnas que inician con "vrs"
  cor_matriz_vrs <- cor_matriz[grepl("^vrs", rownames(cor_matriz)),
                               grepl("^vrs", colnames(cor_matriz))]
  
  return(cor_matriz_vrs)
})

# Asignamos nombres a cada elemento de la lista para identificar el año
names(vrs_cor_list) <- years

# Ahora vrs_cor_list es una lista en la que cada elemento es la submatriz vrs de cada año
vrs_cor_list

resultados_vrs_oo[["oo"]][["resultados_correlacion"]][["correlaciones_lista"]] <- vrs_cor_list

# ==============================================
#  CÁLCULO
# ==============================================

correlaciones_eficiencia_grafica(resultados_vrs_oo[["oo"]][["resultados_correlacion"]][["correlaciones_lista"]], "oo", c("VRS iteracion 1", "VRS iteracion 2", "VRS iteracion 3"),  "Sensibilidad por eliminación de DMU eficientes")
correlaciones_eficiencia_grafica(correlacion_todos_metodos_atipicos[["vrs_oo"]][["original_vs_sin_atipicos"]][["oo"]][["correlaciones_lista"]], "ambos", c("VRS original", "VRS sin atipicos", "CRS original", "CRS sin atipicos"), "Comparación Original vs sin atipicos - Orientación Outputs VRS")



# Distribucion de eficiencias
eficiencias_grafica(resultados_usar)





# ==============================================
#    GRAFICAS
# ==============================================
#  TODOS - GRAFICA DEA INPUT VRS

#resultados_usar <- resultados
lapply(anios, function(anio) {
  eficiencias_chile_grafica(resultados_usar$io[["original"]][[as.character(anio)]][["data"]], anio, "vrs", "Gráfica Chile - Eficiencia técnica ", "Modelo orientado a entradas - VRS - ")
})

lapply(anios, function(anio) {
  eficiencias_chile_grafica(resultados_usar$io[["original"]][[as.character(anio)]][["data"]], anio, "crs", "Gráfica Chile - Eficiencia técnica ", "Modelo orientado a entradas - CRS -")
})

lapply(anios, function(anio) {
  eficiencias_chile_grafica(resultados_usar$oo[["original"]][[as.character(anio)]][["data"]], anio, "vrs", "Gráfica Chile - Eficiencia técnica ", "Modelo orientado a salidas - VRS -")
})

lapply(anios, function(anio) {
  eficiencias_chile_grafica(resultados_usar$oo[["original"]][[as.character(anio)]][["data"]], anio, "crs", "Gráfica Chile - Eficiencia técnica ", "Modelo orientado a salidas - CRS -")
})


# ==============================================
#  MALMQUIST 
# ==============================================


malmquist_graficas(malmquist_indices)
#save(malmquist_indices,datos_usar,file="malmquist.RData")





# -------------------------------------------- #
#  VISUALIZACIÓN DE FRECUENCIAS
# -------------------------------------------- #


# Graficar para IncNodePurity
determinantes_grafica(resultados_IncNodePurity[["io_vrs"]], "Top 10 Determinantes - IncNodePurity", "Modelo orientado a entradas - VRS -")
determinantes_grafica(resultados_IncNodePurity[["io_crs"]], "Top 10 Determinantes - IncNodePurity", "Modelo orientado a entradas - CRS -")
determinantes_grafica(resultados_IncNodePurity[["oo_vrs"]], "Top 10 Determinantes - IncNodePurity", "Modelo orientado a salidas - VRS -")
determinantes_grafica(resultados_IncNodePurity[["oo_crs"]], "Top 10 Determinantes - IncNodePurity", "Modelo orientado a salidas - CRS -")

# Graficar para %IncMSE
determinantes_grafica(resultados_IncMSE[["io_vrs"]], "Top 10 Determinantes - IncMSE", "Modelo orientado a entradas - VRS -")
determinantes_grafica(resultados_IncMSE[["io_crs"]], "Top 10 Determinantes - IncMSE", "Modelo orientado a entradas - CRS -")
determinantes_grafica(resultados_IncMSE[["oo_vrs"]], "Top 10 Determinantes - IncMSE", "Modelo orientado a salidas - VRS -")
determinantes_grafica(resultados_IncMSE[["oo_crs"]], "Top 10 Determinantes - IncMSE", "Modelo orientado a salidas - CRS -")



# ==============================================
#  RESULTADOS
# ==============================================

# Generación de excel con valores de eficiencias y determinantes

# Procesar OUTPUT
guardar_resultados(
  dataframes = resultados_usar[["oo"]],
  resultados_IncNodePurity = resultados_IncNodePurity,
  resultados_pre_IncNodePurity = resultados_pre_IncNodePurity,
  resultados_post_IncNodePurity = resultados_post_IncNodePurity,
  resultados_IncMSE = resultados_IncMSE,
  resultados_pre_IncMSE = resultados_pre_IncMSE,
  resultados_post_IncMSE = resultados_post_IncMSE,
  malmquist_vrs = malmquist_indices[["out_vrs"]][["index"]],
  malmquist_crs = malmquist_indices[["out_crs"]][["index"]],
  archivo_salida = "RESULTADOS OUTPUT.xlsx",
  prefijo = "oo"
)

# Procesar INPUT
guardar_resultados(
  dataframes = resultados_usar[["io"]],
  resultados_IncNodePurity = resultados_IncNodePurity,
  resultados_IncMSE = resultados_IncMSE,
  archivo_salida = "RESULTADOS INPUT.xlsx",
  prefijo = "io"
)
