setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("functions.R")
source("graphics.R")
load("data.RData")

# ==============================================
#  INICIO 
# ==============================================



orientacion <- "io"
orientacion2 <- "in" 
retorno <- "vrs"
columna <- paste0(retorno,"_",orientacion)
columna2 <- paste0(orientacion,"_",retorno)
titulos <- "Inputs VRS"
titulos2 <- "Inputs"

resultados_usar <- resultados_sin_atipicos[[columna]]

#correlaciones_eficiencia_grafica_vertical(correlacion_todos_metodos_atipicos[[columna]][["comparacion"]][["correlaciones_lista"]], "todos", c("VRS I", "VRS O", "CRS I", "CRS O","ESC I", "ESC O"),  "", "TODOS_sin_atipicos")

# ---- Grafica de comparacion de resultados

# Vector de años de interés
years <- 2014:2023
# --------------------------- #

# Extraemos la submatriz de correlaciones vrs para cada año
vrs_cor_list <- lapply(years, function(y) {
  # Accedemos a la matriz de correlaciones del año y
  cor_matriz <- resultados_usar[[orientacion]][["resultados_correlacion"]][["correlaciones_lista"]][[as.character(y)]]
  
  # Filtramos filas y columnas que inician con "vrs"
  cor_matriz_vrs <- cor_matriz[grepl(paste0("^",retorno), rownames(cor_matriz)),
                               grepl(paste0("^",retorno), colnames(cor_matriz))]
  
  return(cor_matriz_vrs)
})

# Asignamos nombres a cada elemento de la lista para identificar el año
names(vrs_cor_list) <- years

# Ahora vrs_cor_list es una lista en la que cada elemento es la submatriz vrs de cada año
vrs_cor_list

resultados_usar[[orientacion]][["resultados_correlacion"]][["correlaciones_lista"]] <- vrs_cor_list



#correlaciones_eficiencia_grafica(resultados_usar[[orientacion]][["resultados_correlacion"]][["correlaciones_lista"]], "", c("Iteracion 1", "Iteracion 2", "Iteracion 3"),  "Sensibilidad por eliminación de DMU eficientes", "iteraciones",2.5,1.5, 3500,6000, 3,4)


# --------------------------- #


# Extraemos la submatriz de correlaciones vrs para cada año
vrs_atp_list <- lapply(years, function(y) {
  # Accedemos a la matriz de correlaciones del año y

  cor_matriz <- correlacion_todos_metodos_atipicos[[columna]][["original_vs_sin_atipicos"]][[orientacion]][["correlaciones_lista"]][[as.character(y)]]
  
  # Filtramos filas y columnas que inician con "vrs"
  cor_matriz_vrs <- cor_matriz[grepl(paste0("^",retorno), rownames(cor_matriz)),
                               grepl(paste0("^",retorno), colnames(cor_matriz))]
  
  return(cor_matriz_vrs)
})

# Asignamos nombres a cada elemento de la lista para identificar el año
names(vrs_atp_list) <- years

# Ahora vrs_cor_list es una lista en la que cada elemento es la submatriz vrs de cada año
vrs_atp_list

correlacion_todos_metodos_atipicos[[columna]][["original_vs_sin_atipicos"]][[orientacion]][["correlaciones_lista"]] <- vrs_atp_list


#correlaciones_eficiencia_grafica(correlacion_todos_metodos_atipicos[[columna]][["original_vs_sin_atipicos"]][[orientacion]][["correlaciones_lista"]], "ambos", c("Original", "Sin atipicos"), paste0("Comparación Original vs sin atipicos - Orientación ", titulos), "sin_atipicos",3,2, 3500,6000, 3,4)


# GRAFICA DE DISTRIBUCIÓN DE EFICIENCIAS

eficiencias_grafica(resultados_usar)


# ==============================================
#  MALMQUIST 
# ==============================================

# DATOS COMPLETOS / ORIGINALES

datos_usar <- datos_sin_atipicos[[columna]]

# DATOS SIN ATIPICOS PARA VRS OO
malmquist_indices <- malmquist(datos_usar,retorno, orientacion2)

#generar_graficas_malmquist(malmquist_indices$index,columna2)


# ==============================================
#  DETERMINANTES
# ==============================================

# Aplicar Random Forest para cada año
random_forest <- lapply(anios, function(anio) {analize_rf(anio, resultados_in = resultados_usar[[orientacion]], 500, retorno, titulos2)})

# Asignar nombres a la lista de modelos
names(random_forest) <- paste0(anios)


# -------------------------------------------- #
#  EXTRACCIÓN DE VARIABLES POR AÑO
# -------------------------------------------- #

# Llamar a la función
resultados_importancia <- importancia_dataframe(random_forest)




# ==============================================
#    GRAFICAS
# ==============================================
#  TODOS - GRAFICA DEA INPUT VRS


lapply(anios, function(anio) {
  eficiencias_chile_grafica(resultados_usar[[orientacion]][["original"]][[as.character(anio)]][["data"]], anio, retorno, "Gráfica Chile - Eficiencia técnica ", paste0(titulos," - "))
})



# ---------------------------------------- #
# ---------------------------------------- #
# ---------------------------------------- #
