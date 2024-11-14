
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("functions.R")
source("graphics.R")

# ----------------------------------------------- #

anios <- c("2014", "2015", "2016", "2017", "2018", "2019","2020")
# Datos #
datos_iniciales <- list(
  "2014" = consolidar_datos_por_anio(2014),
  "2015" = consolidar_datos_por_anio(2015),
  "2016" = consolidar_datos_por_anio(2016),
  "2017" = consolidar_datos_por_anio(2017),
  "2018" = consolidar_datos_por_anio(2018),
  "2019" = consolidar_datos_por_anio(2019),
  "2020" = consolidar_datos_por_anio(2020)
  
)


# Encontrar las DMUs comunes en todos los años y filtrar los datos para incluir solo esas DMUs
dmus_comunes <- Reduce(intersect, lapply(datos_iniciales, `[[`, "IdEstablecimiento"))
datos <- lapply(datos_iniciales, function(data) data[data$IdEstablecimiento %in% dmus_comunes, ])



# -------------------------------------------- #
#  CÁLCULO DEA INPUT
# -------------------------------------------- #


resultados_in <- resultados_iteracion(datos)

# Eliminacion de datos atipicos

datos_cut <- lapply(datos, function(df) {
  df %>% filter(!(IdEstablecimiento %in% resultados_in[["vector_outliers"]]))
})



resultados_in_cut <- resultados_iteracion(datos_cut)









# Graficas #
# Generar y mostrar gráficos VRS
graficos_vrs <- list(
  generar_graficos_iteracion(resultados_in, "Input VRS", "vrs", "in"),
  generar_graficos_iteracion(resultados_in_2_vrs, "Input VRS", "vrs", "in"),
  generar_graficos_iteracion(resultados_in_3_vrs, "Input VRS", "vrs", "in")
)

# Mostrar gráficos VRS
lapply(graficos_vrs, function(graficos) {
  grid.arrange(grobs = graficos, ncol = 3)
})

# Generar y mostrar gráficos CRS
graficos_crs <- list(
  generar_graficos_iteracion(resultados_in, "Input CRS", "crs", "in"),
  generar_graficos_iteracion(resultados_in_2_crs, "Input CRS", "crs", "in"),
  generar_graficos_iteracion(resultados_in_3_crs, "Input CRS", "crs", "in")
)

# Mostrar gráficos CRS
lapply(graficos_crs, function(graficos) {
  grid.arrange(grobs = graficos, ncol = 3)
})


# Iterar sobre los años y mostrar los gráficos
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


# Llamar a la función
lista_resultados_combinados_out <- combinar_resultados_iteraciones(resultados_out, resultados_out_2_vrs, resultados_out_3_vrs, resultados_out_2_crs, resultados_out_3_crs)







# ------------------------------------------------------------- #
# CALCULO Y VISUALIZACION DE CORRELACION DE DATOS SENSIBILIZADOS
resultados <- calcular_y_graficar_correlaciones(lista_resultados_combinados_out, anios)









# Graficas #
# Generar y mostrar gráficos VRS
graficos_vrs_out <- list(
  generar_graficos_iteracion(resultados_out, "Output VRS", "vrs", "out"),
  generar_graficos_iteracion(resultados_out_2_vrs, "Output VRS", "vrs", "out"),
  generar_graficos_iteracion(resultados_out_3_vrs, "Output VRS", "vrs", "out")
)

# Mostrar gráficos VRS
lapply(graficos_vrs_out, function(graficos) {
  grid.arrange(grobs = graficos, ncol = 3)
})

# Generar y mostrar gráficos CRS
graficos_crs_out <- list(
  generar_graficos_iteracion(resultados_out, "Output CRS", "crs"),
  generar_graficos_iteracion(resultados_out_2_crs, "Output CRS", "crs"),
  generar_graficos_iteracion(resultados_out_3_crs, "Output CRS", "crs")
)

# Mostrar gráficos CRS
lapply(graficos_crs_out, function(graficos) {
  grid.arrange(grobs = graficos, ncol = 3)
})


# Iterar sobre los años y mostrar los gráficos
anios <- c("2014", "2015", "2016", "2017", "2018", "2019")
for (anio in anios) {
  graficos_vrs <- generar_graficos_por_anio(anio, "Output - VRS")
  # Mostrar los gráficos en una fila (3 gráficos por año)
  grid.arrange(grobs = graficos_vrs, ncol = 3, top = paste("Comparación de Eficiencia Output VRS - Año", anio))
}


for (anio in anios) {
  graficos_crs <- generar_graficos_por_anio_crs(anio, "CRS")
  # Mostrar los gráficos en una fila (3 gráficos por año)
  grid.arrange(grobs = graficos, ncol = 3, top = paste("Comparación de Eficiencia Output CRS - Año", anio))
}


#------------------------------------- #

# Crear un dataframe para almacenar los valores de VRS y CRS por cada año
in_vrs_df <- data.frame(ID = resultados_in[["2014"]][["data"]][["IdEstablecimiento"]])
in_crs_df <- data.frame(ID = resultados_in[["2014"]][["data"]][["IdEstablecimiento"]])

# Iterar sobre cada año para llenar los dataframes
for (year in names(resultados_in)) {
  in_vrs_df[[year]] <- resultados_in[[year]][["data"]][["vrs"]]
  in_crs_df[[year]] <- resultados_in[[year]][["data"]][["crs"]]
}

out_vrs_df <- data.frame(ID = resultados_out[["2014"]][["data"]][["IdEstablecimiento"]])
out_crs_df <- data.frame(ID = resultados_out[["2014"]][["data"]][["IdEstablecimiento"]])

# Iterar sobre cada año para llenar los dataframes
for (year in names(resultados_in)) {
  out_vrs_df[[year]] <- resultados_out[[year]][["data"]][["vrs"]]
  out_crs_df[[year]] <- resultados_out[[year]][["data"]][["crs"]]
}


# ----------------------------------- #
#    MALMQUIST 


malmquist_in_vrs <- calcular_malmquist(datos, "vrs", "in")
malmquist_in_crs <- calcular_malmquist(datos, "crs", "in")
malmquist_out_vrs <- calcular_malmquist(datos, "vrs", "out")
malmquist_out_crs <- calcular_malmquist(datos, "crs", "out")





correlaciones <- sapply(names(in_vrs_df)[-1], function(year) {
  cor(in_vrs_df[[year]], malmquist_in_vrs[[year]], use = "complete.obs")
})

correlaciones




# Falta esta data
#data_2021 <- consolidar_datos_por_anio(2021)
#resultados_2021_in <- analisis_dea_in(data_2021)





#region_rm_2020 <- region_vrs(resultados_2020_in, 13, 2020)
#print(region_rm_2020)

#region_rm_2021 <- region_vrs(resultados_2021_in, 13, 2021)
#print(region_rm_2021)





# -------------------------------------------- #

#mapa_interactivo <- ggplotly(region, tooltip = "text")
#print(mapa_interactivo)

# ------------------------------------------- #
