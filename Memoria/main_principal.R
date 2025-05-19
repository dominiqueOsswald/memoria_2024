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

datos_completos <- lapply(datos_iniciales, function(data) data[data$IdEstablecimiento %in% dmus_comunes, ])



# Aplicando logaritmo a los valores antes de normalizar
datos_normalizados <- lapply(datos_completos, function(data) {
  cols_a_normalizar <- 5:10
  
  # Aplicamos logaritmo a las columnas seleccionadas
  for (col in cols_a_normalizar) {
    data[[col]] <- log1p(data[[col]])  # log(1 + valor), evita log(0)
  }
  
  # Luego, normalizamos entre 0 y 1
  for (col in cols_a_normalizar) {
    min_col <- min(data[[col]], na.rm = TRUE)
    max_col <- max(data[[col]], na.rm = TRUE)
    data[[col]] <- (data[[col]] - min_col) / (max_col - min_col)
  }
  
  return(data)
})                                                                       

datos <- datos_normalizados

# ==============================================
#  CÁLCULO DEA
# ==============================================

#  CALCULO DE EFICIENCIA EN TODOS LOS AÑOS Y REVISIÓN DE
#  SENSIBILIDAD - ELIMINACION EFICIENTES
#resultados <- list(oo = resultados_iteracion(datos_normalizados, "oo"))

resultados <- list(io = resultados_iteracion(datos_normalizados, "io"),
                   oo = resultados_iteracion(datos_normalizados, "oo"))

# CORRELACION DE VALORES ORIGINALES PARA TODAS LAS COMBINACIONES EN TODOS LOS AÑOS
resultados_combinaciones <- combinar_resultados_in_out(resultados$io[["original"]], resultados$oo[["original"]])
correlacion_todos_metodos <- calcular_correlaciones_all(resultados_combinaciones)

correlaciones_eficiencia_grafica(correlacion_todos_metodos[["correlaciones_lista"]], "todos", c("VRS I", "VRS O", "CRS I", "CRS O","ESC I", "ESC O"),  "", "TODOS", 2,1.5, 4000,6500, 3,4)


#  NUEVO CONJUNTO DE DATOS A PARTIR DE ELIMINACIÓN DE ATÍPICOS 

datos_sin_atipicos <- datos_filtrados_atipicos(datos_normalizados,resultados)


resultados_sin_atipicos <- list( vrs_oo = vrs_oo)
vrs_oo = list(io = resultados_iteracion(datos_sin_atipicos[["vrs_oo"]], "io"),oo = resultados_iteracion(datos_sin_atipicos[["vrs_oo"]], "oo"))

resultados_sin_atipicos <- list(
  vrs_io = list(io = resultados_iteracion(datos_sin_atipicos[["vrs_io"]], "io"),oo = resultados_iteracion(datos_sin_atipicos[["vrs_io"]], "oo")),
  crs_io = list(io = resultados_iteracion(datos_sin_atipicos[["crs_io"]], "io"),oo = resultados_iteracion(datos_sin_atipicos[["crs_io"]], "oo")),
  vrs_oo = list(io = resultados_iteracion(datos_sin_atipicos[["vrs_oo"]], "io"),oo = resultados_iteracion(datos_sin_atipicos[["vrs_oo"]], "oo")),
  crs_oo = list(io = resultados_iteracion(datos_sin_atipicos[["crs_oo"]], "io"),oo = resultados_iteracion(datos_sin_atipicos[["crs_oo"]], "oo")),
  esc_io = list(io = resultados_iteracion(datos_sin_atipicos[["esc_io"]], "io"),oo = resultados_iteracion(datos_sin_atipicos[["esc_io"]], "oo")),
  esc_oo = list(io = resultados_iteracion(datos_sin_atipicos[["esc_oo"]], "io"),oo = resultados_iteracion(datos_sin_atipicos[["esc_oo"]], "oo"))
)




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
  ),
  esc_io = list(
    original_vs_sin_atipicos =   list(
      oo = calcular_correlaciones_all(resultados_combinaciones_sin_atipicos[["crs_io"]][["original_vs_sin_atipicos"]][["oo"]]),
      io = calcular_correlaciones_all(resultados_combinaciones_sin_atipicos[["crs_io"]][["original_vs_sin_atipicos"]][["io"]])
    ),
    comparacion = calcular_correlaciones_all(resultados_combinaciones_sin_atipicos[["crs_io"]][["comparacion"]])
  ),
  esc_io = list(
    original_vs_sin_atipicos =   list(
      oo = calcular_correlaciones_all(resultados_combinaciones_sin_atipicos[["crs_io"]][["original_vs_sin_atipicos"]][["oo"]]),
      io = calcular_correlaciones_all(resultados_combinaciones_sin_atipicos[["crs_io"]][["original_vs_sin_atipicos"]][["io"]])
    ),
    comparacion = calcular_correlaciones_all(resultados_combinaciones_sin_atipicos[["crs_io"]][["comparacion"]])
  )
)


