setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("functions.R")
source("graphics.R")

# ==============================================
#  PRE PROCESAMIENTO DE DATOS
# ==============================================

#  CONSOLIDADO DE DATOS POR AÑO
anios <- 2014:2023
anios_pre_pandemia <- c("2014", "2015", "2016", "2017", "2018", "2019")
anios_pandemia <- c("2020", "2021", "2022", "2023")

datos_iniciales <- lapply(anios, consolidar_datos_por_anio)
names(datos_iniciales) <- as.character(anios)

# Encontrar las DMUs comunes en todos los años y filtrar los datos para incluir solo esas DMUs
dmus_comunes <- Reduce(intersect, lapply(datos_iniciales, `[[`, "IdEstablecimiento"))
datos <- lapply(datos_iniciales, function(data) data[data$IdEstablecimiento %in% dmus_comunes, ])



# ==============================================
#  CÁLCULO DEA
# ==============================================

#  SENSIBILIDAD - ELIMINACION EFICIENTES
resultados <- list(io = resultados_iteracion(datos, "io"),oo = resultados_iteracion(datos, "oo"))

# GRAFICA DE SENSIBILIDAD POR EFICIENCIA
correlaciones_eficiencia_grafica(resultados[["io"]][["resultados_correlacion"]][["correlaciones_lista"]], "io", c("VRS iteracion 1", "VRS iteracion 2", "VRS iteracion 3", "CRS iteracion 1", "CRS iteracion 2",  "CRS iteracion 3"), "Sensibilidad por eliminación de DMU eficientes")
correlaciones_eficiencia_grafica(resultados[["oo"]][["resultados_correlacion"]][["correlaciones_lista"]], "oo", c("VRS iteracion 1", "VRS iteracion 2", "VRS iteracion 3", "CRS iteracion 1", "CRS iteracion 2",  "CRS iteracion 3"),  "Sensibilidad por eliminación de DMU eficientes")


# CORRELACION DE VALORES ORIGINALES PARA TODAS LAS COMBINACIONES EN TODOS LOS AÑOS
resultados_combinaciones <- combinar_resultados_in_out(resultados$io[["original"]], resultados$oo[["original"]])
correlacion_todos_metodos <- calcular_correlaciones_all(resultados_combinaciones)

correlaciones_eficiencia_grafica(correlacion_todos_metodos[["correlaciones_lista"]], "ambos", c("VRS Input", "VRS Output", "CRS Input", "CRS Output"))




#  ELIMINACIÓN DE DATOS ATÍPICOS
datos_sin_atipicos <- datos_filtrados_atipicos(datos,resultados)

resultados_sin_atipicos <- list(
  vrs_io = list(io = resultados_iteracion(datos_sin_atipicos[["vrs_io"]], "io"),oo = resultados_iteracion(datos_sin_atipicos[["vrs_io"]], "oo")),
  crs_io = list(io = resultados_iteracion(datos_sin_atipicos[["crs_io"]], "io"),oo = resultados_iteracion(datos_sin_atipicos[["crs_io"]], "oo")),
  vrs_oo = list(io = resultados_iteracion(datos_sin_atipicos[["vrs_oo"]], "io"),oo = resultados_iteracion(datos_sin_atipicos[["vrs_oo"]], "oo")),
  crs_oo = list(io = resultados_iteracion(datos_sin_atipicos[["crs_oo"]], "io"),oo = resultados_iteracion(datos_sin_atipicos[["crs_oo"]], "oo"))
  
)


# GRAFICA DE SENSIBILIDAD POR EFICIENCIA
correlaciones_eficiencia_grafica(resultados_sin_atipicos[["vrs_io"]][["io"]][["resultados_correlacion"]][["correlaciones_lista"]], "io", c("VRS iteracion 1", "VRS iteracion 2", "VRS iteracion 3", "CRS iteracion 1", "CRS iteracion 2",  "CRS iteracion 3"), "Sensibilidad por eliminación de DMU eficientes - Sin datos atipicos VRS Input")
correlaciones_eficiencia_grafica(resultados_sin_atipicos[["vrs_io"]][["oo"]][["resultados_correlacion"]][["correlaciones_lista"]], "oo", c("VRS iteracion 1", "VRS iteracion 2", "VRS iteracion 3", "CRS iteracion 1", "CRS iteracion 2",  "CRS iteracion 3"), "Sensibilidad por eliminación de DMU eficientes - Sin datos atipicos VRS Input")

correlaciones_eficiencia_grafica(resultados_sin_atipicos[["crs_io"]][["io"]][["resultados_correlacion"]][["correlaciones_lista"]], "io", c("VRS iteracion 1", "VRS iteracion 2", "VRS iteracion 3", "CRS iteracion 1", "CRS iteracion 2",  "CRS iteracion 3"), "Sensibilidad por eliminación de DMU eficientes - Sin datos atipicos CRS Input")
correlaciones_eficiencia_grafica(resultados_sin_atipicos[["crs_io"]][["oo"]][["resultados_correlacion"]][["correlaciones_lista"]], "oo", c("VRS iteracion 1", "VRS iteracion 2", "VRS iteracion 3", "CRS iteracion 1", "CRS iteracion 2",  "CRS iteracion 3"), "Sensibilidad por eliminación de DMU eficientes - Sin datos atipicos CRS Input" )

correlaciones_eficiencia_grafica(resultados_sin_atipicos[["vrs_oo"]][["io"]][["resultados_correlacion"]][["correlaciones_lista"]], "io", c("VRS iteracion 1", "VRS iteracion 2", "VRS iteracion 3", "CRS iteracion 1", "CRS iteracion 2",  "CRS iteracion 3"), "Sensibilidad por eliminación de DMU eficientes - Sin datos atipicos VRS Output")
correlaciones_eficiencia_grafica(resultados_sin_atipicos[["vrs_oo"]][["oo"]][["resultados_correlacion"]][["correlaciones_lista"]], "oo", c("VRS iteracion 1", "VRS iteracion 2", "VRS iteracion 3", "CRS iteracion 1", "CRS iteracion 2",  "CRS iteracion 3"), "Sensibilidad por eliminación de DMU eficientes - Sin datos atipicos VRS Output")

correlaciones_eficiencia_grafica(resultados_sin_atipicos[["crs_oo"]][["io"]][["resultados_correlacion"]][["correlaciones_lista"]], "io", c("VRS iteracion 1", "VRS iteracion 2", "VRS iteracion 3", "CRS iteracion 1", "CRS iteracion 2",  "CRS iteracion 3"), "Sensibilidad por eliminación de DMU eficientes - Sin datos atipicos CRS Output")
correlaciones_eficiencia_grafica(resultados_sin_atipicos[["crs_oo"]][["oo"]][["resultados_correlacion"]][["correlaciones_lista"]], "oo", c("VRS iteracion 1", "VRS iteracion 2", "VRS iteracion 3", "CRS iteracion 1", "CRS iteracion 2",  "CRS iteracion 3"), "Sensibilidad por eliminación de DMU eficientes - Sin datos atipicos CRS Output")



# -----------------------------------------------------------------------------------------------------------#
# CORRELACION ENTRE EFICIENCIAS DE ATIPICOS Y TODOS LOS DATOS

resultados_combinaciones_sin_atipicos <- list(
  vrs_oo = list(
    original_vs_sin_atipicos =   list(
      oo= combinar_resultados_in_out(resultados$oo[["original"]], resultados_sin_atipicos[["vrs_oo"]]$oo[["original"]]),
      io= combinar_resultados_in_out(resultados$io[["original"]], resultados_sin_atipicos[["vrs_oo"]]$io[["original"]])
    ),
    comparacion = combinar_resultados_in_out(resultados_sin_atipicos[["vrs_oo"]]$io[["original"]], resultados_sin_atipicos[["vrs_oo"]]$oo[["original"]])
  ),
  crs_oo = list(
    original_vs_sin_atipicos =   list(
    oo = combinar_resultados_in_out(resultados$oo[["original"]], resultados_sin_atipicos[["crs_oo"]]$oo[["original"]]),
    io = combinar_resultados_in_out(resultados$io[["original"]], resultados_sin_atipicos[["crs_oo"]]$io[["original"]])
  ),
  comparacion = combinar_resultados_in_out(resultados_sin_atipicos[["crs_oo"]]$io[["original"]], resultados_sin_atipicos[["crs_oo"]]$oo[["original"]])
  ),
  vrs_io = list(
    original_vs_sin_atipicos =   list(
      oo = combinar_resultados_in_out(resultados$oo[["original"]], resultados_sin_atipicos[["vrs_io"]]$oo[["original"]]),
      io = combinar_resultados_in_out(resultados$io[["original"]], resultados_sin_atipicos[["vrs_io"]]$io[["original"]])
  ),
  comparacion = combinar_resultados_in_out(resultados_sin_atipicos[["vrs_io"]]$io[["original"]], resultados_sin_atipicos[["vrs_io"]]$oo[["original"]])
  ),
  crs_io = list(
    original_vs_sin_atipicos =   list(
    oo = combinar_resultados_in_out(resultados$oo[["original"]], resultados_sin_atipicos[["crs_io"]]$oo[["original"]]),
    io = combinar_resultados_in_out(resultados$io[["original"]], resultados_sin_atipicos[["crs_io"]]$io[["original"]])
  ),
  comparacion = combinar_resultados_in_out(resultados_sin_atipicos[["crs_io"]]$io[["original"]], resultados_sin_atipicos[["crs_io"]]$oo[["original"]])
  )
)




correlacion_todos_metodos_atipicos <- list(
  vrs_oo = list(
    original_vs_sin_atipicos =   list(
      oo = calcular_correlaciones_all(resultados_combinaciones_sin_atipicos[["vrs_oo"]][["original_vs_sin_atipicos"]][["oo"]]),
      io = calcular_correlaciones_all(resultados_combinaciones_sin_atipicos[["vrs_oo"]][["original_vs_sin_atipicos"]][["io"]])
    ),
    comparacion = calcular_correlaciones_all(resultados_combinaciones_sin_atipicos[["vrs_oo"]][["comparacion"]])
  ),
  
  crs_oo = list(
    original_vs_sin_atipicos =   list(
    oo = calcular_correlaciones_all(resultados_combinaciones_sin_atipicos[["crs_oo"]][["original_vs_sin_atipicos"]][["oo"]]),
    io = calcular_correlaciones_all(resultados_combinaciones_sin_atipicos[["crs_oo"]][["original_vs_sin_atipicos"]][["io"]])
  ),
    comparacion = calcular_correlaciones_all(resultados_combinaciones_sin_atipicos[["crs_oo"]][["comparacion"]])
  ),
  
  vrs_io = list(
    original_vs_sin_atipicos =   list(
      oo = calcular_correlaciones_all(resultados_combinaciones_sin_atipicos[["vrs_io"]][["original_vs_sin_atipicos"]][["oo"]]),
      io = calcular_correlaciones_all(resultados_combinaciones_sin_atipicos[["vrs_io"]][["original_vs_sin_atipicos"]][["io"]])
    ),
    comparacion = calcular_correlaciones_all(resultados_combinaciones_sin_atipicos[["vrs_io"]][["comparacion"]])
  ),
  crs_io = list(
    original_vs_sin_atipicos =   list(
      oo = calcular_correlaciones_all(resultados_combinaciones_sin_atipicos[["crs_io"]][["original_vs_sin_atipicos"]][["oo"]]),
      io = calcular_correlaciones_all(resultados_combinaciones_sin_atipicos[["crs_io"]][["original_vs_sin_atipicos"]][["io"]])
    ),
    comparacion = calcular_correlaciones_all(resultados_combinaciones_sin_atipicos[["crs_io"]][["comparacion"]])
  )
)

resultados_usar <- resultados
#resultados_usar <- resultados_sin_atipicos[["vrs_oo"]]


# GRAFICA DE SENSIBILIDAD POR EFICIENCIA
correlaciones_eficiencia_grafica(resultados_usar[["io"]][["resultados_correlacion"]][["correlaciones_lista"]], "io", c("VRS iteracion 1", "VRS iteracion 2", "VRS iteracion 3", "CRS iteracion 1", "CRS iteracion 2",  "CRS iteracion 3"), "Sensibilidad por eliminación de DMU eficientes")
correlaciones_eficiencia_grafica(resultados_usar[["oo"]][["resultados_correlacion"]][["correlaciones_lista"]], "oo", c("VRS iteracion 1", "VRS iteracion 2", "VRS iteracion 3", "CRS iteracion 1", "CRS iteracion 2",  "CRS iteracion 3"),  "Sensibilidad por eliminación de DMU eficientes")


correlaciones_eficiencia_grafica(correlacion_todos_metodos_atipicos[["vrs_oo"]][["original_vs_sin_atipicos"]][["oo"]][["correlaciones_lista"]], "ambos", c("VRS original", "VRS sin atipicos", "CRS original", "CRS sin atipicos"), "Comparación Original vs sin atipicos - Orientación Outputs VRS")



# GRAFICA DE DISTRIBUCIÓN DE EFICIENCIAS

eficiencias_grafica(resultados)
eficiencias_grafica(resultados_usar)


# ==============================================
#  MALMQUIST 
# ==============================================

# DATOS COMPLETOS / ORIGINALES

datos_usar <- datos
#datos_usar <- datos_sin_atipicos[["vrs_io"]]

# DATOS SIN ATIPICOS PARA VRS OO
malmquist_indices <- list(
  in_vrs = malmquist(datos_usar,"vrs", "in"),
  in_crs = malmquist(datos_usar,"crs", "in"),
  out_vrs = malmquist(datos_usar,"vrs", "out"),
  out_crs = malmquist(datos_usar,"crs", "out")
)


#save(datos_usar,malmquist_indices, file="malmquist_io_vrs.RData")

malmquist_graficas(malmquist_indices)
save(malmquist_indices,datos_usar,file="malmquist.RData")
# ==============================================
#  DETERMINANTES
# ==============================================
# -------------------------------------------- #
#  CONFIGURACIÓN Y MODELO RANDOM FOREST
# -------------------------------------------- #


# Aplicar Random Forest para cada año
random_forest <- list(
  io_vrs = lapply(anios, function(anio) {analize_rf(anio, resultados_in = resultados_usar$io, 500, "vrs", "Entradas")}),
  io_crs = lapply(anios, function(anio) {analize_rf(anio, resultados_in = resultados_usar$io, 500, "crs", "Entradas")}),
  oo_vrs = lapply(anios, function(anio) {analize_rf(anio, resultados_in = resultados_usar$oo, 500, "vrs", "Salidas")}),
  oo_crs = lapply(anios, function(anio) {analize_rf(anio, resultados_in = resultados_usar$oo, 500, "crs", "Salidas")})
)

# Asignar nombres a la lista de modelos
names(random_forest$io_vrs) <- paste0(anios)
names(random_forest$io_crs) <- paste0(anios)
names(random_forest$oo_vrs) <- paste0(anios)
names(random_forest$oo_crs) <- paste0(anios)

# -------------------------------------------- #
#  EXTRACCIÓN DE VARIABLES POR AÑO
# -------------------------------------------- #

# Llamar a la función
resultados_importancia <- determinantes_importancia(random_forest, anios_pre_pandemia, anios_pandemia)

# Acceder a los resultados
resultados_IncNodePurity <- resultados_importancia$IncNodePurity
resultados_IncMSE <- resultados_importancia$IncMSE


#save(resultados_usar,resultados_importancia, resultados_IncNodePurity, resultados_IncMSE, file="determinantes_io_vrs.RData")
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
  resultados_IncMSE = resultados_IncMSE,
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


# MEJORES RESULTADOS 

#mejores_25 <- list("in_vrs" =top_eficiencia(resultados$io, "vrs", 25, TRUE),
#                   "in_crs" = top_eficiencia(resultados$io, "crs", 25, TRUE),
#                   "out_vrs" = top_eficiencia(resultados$oo, "vrs", 25, TRUE),
#                   "out_crs" = top_eficiencia(resultados$oo, "crs", 25, TRUE)) 


# VRS INPUT
#lapply(anios, function(anio) {
#  chile_map_plot(mejores_25[["in_vrs"]][[as.character(anio)]], anio, "vrs")
#})


# REGION COLOREADA POR PORCENTAJE DENTRO DE 25 MEJOR
#resumen <- resumen_eficiencia(mejores_25$in_vrs)
#colorear_region(resumen)


# -------------------------------------------- #

#mapa_interactivo <- ggplotly(region, tooltip = "text")
#print(mapa_interactivo)

# ------------------------------------------- #

# ==============================================