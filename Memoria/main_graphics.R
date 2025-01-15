setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("functions.R")
source("graphics.R")

# ==============================================
#  PROCESAMIENTO DE DATOS
# ==============================================

#  CONSOLIDADO DE DATOS POR AÑO
anios <- 2014:2023
anios_pre_pandemia <- c("2014", "2015", "2016", "2017", "2018", "2019")
anios_pandemia <- c("2020", "2021", "2022", "2023")

load("dataR/datos.RData")
load("dataR/datos_atipicos.RData")
load("dataR/resultados_eficiencia.RData")
load("dataR/malmquist.RData")
load("dataR/determinantes.RData")
#load("dataR/resultados_eficiencia_atipicos.RData")

# ==============================================
#  CÁLCULO DEA
# ==============================================

# GRAFICA DE SENSIBILIDAD POR EFICIENCIA
graficar_correlaciones(resultados[["io"]][["resultados_correlacion"]][["correlaciones_lista"]], "io", c("vrs_i1", "vrs_i2", "vrs_i3", "crs_i1", "crs_i2",  "crs_i3"), "Sensibilidad por eliminación de DMU eficientes")
graficar_correlaciones(resultados[["oo"]][["resultados_correlacion"]][["correlaciones_lista"]], "oo", c("vrs_i1", "vrs_i2", "vrs_i3", "crs_i1", "crs_i2",  "crs_i3"),  "Sensibilidad por eliminación de DMU eficientes")

# CORRELACION DE VALORES ORIGINALES PARA TODAS LAS COMBINACIONES EN TODOS LOS AÑOS
graficar_correlaciones(correlacion_todos_metodos[["correlaciones_lista"]], "ambos", c("vrs_io", "vrs_oo", "crs_io", "crs_oo"))

# GRAFICA DE DISTRIBUCIÓN DE EFICIENCIAS
grafica_eficiencias(resultados)



# GRAFICA DE SENSIBILIDAD POR ELIMINACION DE DATOS ATIPICOS
graficar_correlaciones(resultados_sin_atipicos_vrs_io[["io"]][["resultados_correlacion"]][["correlaciones_lista"]], "io", c("vrs_i1", "vrs_i2", "vrs_i3", "crs_i1", "crs_i2",  "crs_i3"), "Sensibilidad por eliminación de DMU eficientes - Sin datos atipicos VRS Input")
graficar_correlaciones(resultados_sin_atipicos_vrs_io[["oo"]][["resultados_correlacion"]][["correlaciones_lista"]], "oo", c("vrs_i1", "vrs_i2", "vrs_i3", "crs_i1", "crs_i2",  "crs_i3"), "Sensibilidad por eliminación de DMU eficientes - Sin datos atipicos VRS Input")

graficar_correlaciones(resultados_sin_atipicos_crs_io[["io"]][["resultados_correlacion"]][["correlaciones_lista"]], "io", c("vrs_i1", "vrs_i2", "vrs_i3", "crs_i1", "crs_i2",  "crs_i3"), "Sensibilidad por eliminación de DMU eficientes - Sin datos atipicos CRS Input")
graficar_correlaciones(resultados_sin_atipicos_crs_io[["oo"]][["resultados_correlacion"]][["correlaciones_lista"]], "oo", c("vrs_i1", "vrs_i2", "vrs_i3", "crs_i1", "crs_i2",  "crs_i3"), "Sensibilidad por eliminación de DMU eficientes - Sin datos atipicos CRS Input" )

graficar_correlaciones(resultados_sin_atipicos_vrs_oo[["io"]][["resultados_correlacion"]][["correlaciones_lista"]], "io", c("vrs_i1", "vrs_i2", "vrs_i3", "crs_i1", "crs_i2",  "crs_i3"), "Sensibilidad por eliminación de DMU eficientes - Sin datos atipicos VRS Output")
graficar_correlaciones(resultados_sin_atipicos_vrs_oo[["oo"]][["resultados_correlacion"]][["correlaciones_lista"]], "oo", c("vrs_i1", "vrs_i2", "vrs_i3", "crs_i1", "crs_i2",  "crs_i3"), "Sensibilidad por eliminación de DMU eficientes - Sin datos atipicos VRS Output")

graficar_correlaciones(resultados_sin_atipicos_crs_oo[["io"]][["resultados_correlacion"]][["correlaciones_lista"]], "io", c("vrs_i1", "vrs_i2", "vrs_i3", "crs_i1", "crs_i2",  "crs_i3"), "Sensibilidad por eliminación de DMU eficientes - Sin datos atipicos CRS Output")
graficar_correlaciones(resultados_sin_atipicos_crs_oo[["oo"]][["resultados_correlacion"]][["correlaciones_lista"]], "oo", c("vrs_i1", "vrs_i2", "vrs_i3", "crs_i1", "crs_i2",  "crs_i3"), "Sensibilidad por eliminación de DMU eficientes - Sin datos atipicos CRS Output")



# ==============================================
#    GRAFICAS
# ==============================================
#  TODOS - GRAFICA DEA INPUT VRS

lapply(anios, function(anio) {
  chile_map_plot(resultados$io[["original"]][[as.character(anio)]][["data"]], anio, "vrs", "Gráfica Chile - Eficiencia técnica ", "Modelo orientado a entradas - VRS - ")
})

lapply(anios, function(anio) {
  chile_map_plot(resultados$io[["original"]][[as.character(anio)]][["data"]], anio, "crs", "Gráfica Chile - Eficiencia técnica ", "Modelo orientado a entradas - CRS -")
})

lapply(anios, function(anio) {
  chile_map_plot(resultados$oo[["original"]][[as.character(anio)]][["data"]], anio, "vrs", "Gráfica Chile - Eficiencia técnica ", "Modelo orientado a salidas - VRS -")
})

lapply(anios, function(anio) {
  chile_map_plot(resultados$oo[["original"]][[as.character(anio)]][["data"]], anio, "crs", "Gráfica Chile - Eficiencia técnica ", "Modelo orientado a salidas - CRS -")
})


# ==============================================
#  MALMQUIST 
# ==============================================

procesar_y_graficar(malmquist_indices)

# ==============================================
#  DETERMINANTES
# ==============================================

# -------------------------------------------- #
#  VISUALIZACIÓN DE FRECUENCIAS
# -------------------------------------------- #


# Graficar para IncNodePurity
graficar_top_10(resultados_IncNodePurity[["io_vrs"]], "Top 10 Determinantes - IncNodePurity", "Modelo orientado a entradas - VRS -")
graficar_top_10(resultados_IncNodePurity[["io_crs"]], "Top 10 Determinantes - IncNodePurity", "Modelo orientado a entradas - CRS -")
graficar_top_10(resultados_IncNodePurity[["oo_vrs"]], "Top 10 Determinantes - IncNodePurity", "Modelo orientado a salidas - VRS -")
graficar_top_10(resultados_IncNodePurity[["oo_crs"]], "Top 10 Determinantes - IncNodePurity", "Modelo orientado a salidas - CRS -")

# Graficar para %IncMSE
graficar_top_10(resultados_IncMSE[["io_vrs"]], "Top 10 Determinantes - %IncMSE", "Modelo orientado a entradas - VRS -")
graficar_top_10(resultados_IncMSE[["io_crs"]], "Top 10 Determinantes - %IncMSE", "Modelo orientado a entradas - CRS -")
graficar_top_10(resultados_IncMSE[["oo_vrs"]], "Top 10 Determinantes - %IncMSE", "Modelo orientado a salidas - VRS -")
graficar_top_10(resultados_IncMSE[["oo_crs"]], "Top 10 Determinantes - %IncMSE", "Modelo orientado a salidas - CRS -")




# ==============================================
#  RESULTADOS
# ==============================================

# Generación de excel con valores de eficiencias y determinantes

# Procesar OUTPUT
procesar_y_guardar_resultados(
  dataframes = crear_dataframes(resultados, "oo"),
  resultados_IncNodePurity = resultados_IncNodePurity,
  resultados_IncMSE = resultados_IncMSE,
  archivo_salida = "RESULTADOS OUTPUT.xlsx",
  prefijo = "oo"
)

# Procesar INPUT
procesar_y_guardar_resultados(
  dataframes = crear_dataframes(resultados, "io"),
  resultados_IncNodePurity = resultados_IncNodePurity,
  resultados_IncMSE = resultados_IncMSE,
  archivo_salida = "RESULTADOS INPUT.xlsx",
  prefijo = "io"
)


