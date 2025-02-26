setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("functions.R")
source("graphics.R")

orientacion <- "oo"
retorno <- "vrs"
columna <- "vrs_oo"

resultados_usar <- resultados_sin_atipicos[[columna]]

# ---- SOLO VRS OO

# Vector de años de interés
years <- 2014:2023

# Extraemos la submatriz de correlaciones vrs para cada año
vrs_cor_list <- lapply(years, function(y) {
  # Accedemos a la matriz de correlaciones del año y
  cor_matriz <- resultados_usar[[orientacion]][["resultados_correlacion"]][["correlaciones_lista"]][[as.character(y)]]
  
  # Filtramos filas y columnas que inician con "vrs"
  cor_matriz_vrs <- cor_matriz[grepl("^vrs", rownames(cor_matriz)),
                               grepl("^vrs", colnames(cor_matriz))]
  
  return(cor_matriz_vrs)
})

# Asignamos nombres a cada elemento de la lista para identificar el año
names(vrs_cor_list) <- years

# Ahora vrs_cor_list es una lista en la que cada elemento es la submatriz vrs de cada año
vrs_cor_list

resultados_usar[[orientacion]][["resultados_correlacion"]][["correlaciones_lista"]] <- vrs_cor_list



correlaciones_eficiencia_grafica(resultados_usar[[orientacion]][["resultados_correlacion"]][["correlaciones_lista"]], orientacion, c("VRS iteracion 1", "VRS iteracion 2", "VRS iteracion 3"),  "Sensibilidad por eliminación de DMU eficientes")
correlaciones_eficiencia_grafica(correlacion_todos_metodos_atipicos[[columna]][["original_vs_sin_atipicos"]][[orientacion]][["correlaciones_lista"]], "ambos", c("VRS original", "VRS sin atipicos", "CRS original", "CRS sin atipicos", "ESC original", "ESC sin atipicos"), "Comparación Original vs sin atipicos - Orientación Outputs VRS")



# GRAFICA DE DISTRIBUCIÓN DE EFICIENCIAS

eficiencias_grafica(resultados_usar)


# ==============================================
#  MALMQUIST 
# ==============================================

# DATOS COMPLETOS / ORIGINALES

datos_usar <- datos_sin_atipicos[[columna]]

# DATOS SIN ATIPICOS PARA VRS OO
malmquist_indices <- malmquist(datos_usar,"vrs", "out")

generar_graficas_malmquist(malmquist_indices$index,"out_vrs")


# ==============================================
#  DETERMINANTES
# ==============================================

# Aplicar Random Forest para cada año
random_forest <- lapply(anios, function(anio) {analize_rf(anio, resultados_in = resultados_usar$oo, 500, "vrs", "Salidas")})

# Asignar nombres a la lista de modelos
names(random_forest) <- paste0(anios)


# -------------------------------------------- #
#  EXTRACCIÓN DE VARIABLES POR AÑO
# -------------------------------------------- #

# Llamar a la función
resultados_importancia <- determinantes_importancia_single(random_forest, anios_pre_pandemia, anios_pandemia)



# Acceder a los resultados
resultados_IncNodePurity <- resultados_importancia$IncNodePurity
resultados_IncMSE <- resultados_importancia$IncMSE

resultados_pre_IncNodePurity <- resultados_importancia$IncNodePurity_Pre
resultados_post_IncNodePurity <- resultados_importancia$IncNodePurity_Post

resultados_pre_IncMSE <- resultados_importancia$IncMSE_Pre
resultados_post_IncMSE <- resultados_importancia$IncMSE_Post

# -------------------------------------------- #
#  VISUALIZACIÓN DE FRECUENCIAS
# -------------------------------------------- #


# Graficar para IncNodePurity

# Graficar para %IncMSE
determinantes_grafica(resultados_importancia$IncMSE, "Top 10 Determinantes - IncMSE", "Modelo orientado a salidas - VRS -")




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



# ==============================================
#    GRAFICAS
# ==============================================
#  TODOS - GRAFICA DEA INPUT VRS

#resultados_usar <- resultados

lapply(anios, function(anio) {
  eficiencias_chile_grafica(resultados_usar[[orientacion]][["original"]][[as.character(anio)]][["data"]], anio, retorno, "Gráfica Chile - Eficiencia técnica ", "Modelo orientado a salidas - VRS -")
})



