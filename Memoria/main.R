setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("functions.R")
source("graphics.R")

# ----------------------------------------------- #

# -------------------------------------------- #
#  CONSOLIDADO DE DATOS POR AÑO
# -------------------------------------------- #

anios <- c("2014", "2015", "2016", "2017", "2018", "2019","2020")

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

resultados_in <- resultados_iteracion(datos, "io")

# -------------------------------------------- #
#  CÁLCULO DEA OUTPUT
# -------------------------------------------- #

resultados_out <- resultados_iteracion(datos, "oo")


# -------------------------------------------- #
#    MALMQUIST 
# -------------------------------------------- #


malmquist_in_vrs <- calcular_malmquist(datos, "vrs", "in")
malmquist_in_crs <- calcular_malmquist(datos, "crs", "in")
malmquist_out_vrs <- calcular_malmquist(datos, "vrs", "out")
malmquist_out_crs <- calcular_malmquist(datos, "crs", "out")




# -------------------------------------------- #
#    COMPARACIÓN DE METODOS 
# -------------------------------------------- #


# Crear un dataframe para almacenar los valores de VRS y CRS por cada año
in_vrs_df <- data.frame(ID = resultados_in[["original"]][["2014"]][["data"]][["IdEstablecimiento"]])
in_crs_df <- data.frame(ID = resultados_in[["original"]][["2014"]][["data"]][["IdEstablecimiento"]])

# Iterar sobre cada año para llenar los dataframes
for (year in names(resultados_in[["original"]])) {
  print(year)
  in_vrs_df[[year]] <- resultados_in[["original"]][[year]][["data"]][["vrs"]]
  in_crs_df[[year]] <- resultados_in[["original"]][[year]][["data"]][["crs"]]
}




out_vrs_df <- data.frame(ID = resultados_out[["original"]][["2014"]][["data"]][["IdEstablecimiento"]])
out_crs_df <- data.frame(ID = resultados_out[["original"]][["2014"]][["data"]][["IdEstablecimiento"]])

# Iterar sobre cada año para llenar los dataframes
for (year in names(resultados_out[["original"]])) {
  out_vrs_df[[year]] <- resultados_out[["original"]][[year]][["data"]][["vrs"]]
  out_crs_df[[year]] <- resultados_out[["original"]][[year]][["data"]][["crs"]]
}



correlaciones <- sapply(names(in_vrs_df)[-1], function(year) {
  cor(in_vrs_df[[year]], malmquist_in_vrs[[year]], use = "complete.obs")
})

correlaciones




# Creamos una lista vacía para almacenar los resultados

mejores_25 <- list("in_vrs" =top_eficiencia(resultados_in, "vrs", 25, TRUE),
                   "in_crs" = top_eficiencia(resultados_in, "crs", 25, TRUE),
                   "out_vrs" = top_eficiencia(resultados_out, "vrs", 25, TRUE),
                   "out_crs" = top_eficiencia(resultados_out, "crs", 25, TRUE)) 




resumen <- resumen_eficiencia(mejores_25$in_vrs)



colorear_region(resumen)







































# -------------------------------------------- #
#  ELIMINACIÓN DE DATOS ATÍPICOS INPUT
# -------------------------------------------- #


datos_cut_in_vrs <- lapply(datos, function(df) {
  df %>% filter(!(IdEstablecimiento %in% resultados_in[["vector_outliers_vrs"]]))
})

datos_cut_in_crs <- lapply(datos, function(df) {
  df %>% filter(!(IdEstablecimiento %in% resultados_in[["vector_outliers_crs"]]))
})



resultados_in_cut_vrs <- resultados_iteracion(datos_cut_in_vrs, "io")
resultados_in_cut_crs <- resultados_iteracion(datos_cut_in_crs, "io")

# -------------------------------------------- #
#  ELIMINACIÓN DE DATOS ATÍPICOS OUTPUT
# -------------------------------------------- #

datos_cut_out_vrs <- lapply(datos, function(df) {
  df %>% filter(!(IdEstablecimiento %in% resultados_out[["vector_outliers_vrs"]]))
})

datos_cut_out_crs <- lapply(datos, function(df) {
  df %>% filter(!(IdEstablecimiento %in% resultados_out[["vector_outliers_crs"]]))
})



resultados_out_cut_vrs <- resultados_iteracion(datos_cut_out_vrs, "oo")
resultados_out_cut_crs <- resultados_iteracion(datos_cut_out_crs, "oo")









# Crear un dataframe para almacenar los valores de VRS y CRS por cada año
in_vrs_df <- data.frame(ID = resultados_in[["original"]][["2014"]][["data"]][["IdEstablecimiento"]])
in_crs_df <- data.frame(ID = resultados_in[["original"]][["2014"]][["data"]][["IdEstablecimiento"]])

in_vrs_df_cut <-
in_vrs_df_cut <-  
  
# Iterar sobre cada año para llenar los dataframes
for (year in names(resultados_in[["original"]])) {
  print(year)
  in_vrs_df[[year]] <- resultados_in[["original"]][[year]][["data"]][["vrs"]]
  in_crs_df[[year]] <- resultados_in[["original"]][[year]][["data"]][["crs"]]
}




out_vrs_df <- data.frame(ID = resultados_out[["original"]][["2014"]][["data"]][["IdEstablecimiento"]])
out_crs_df <- data.frame(ID = resultados_out[["original"]][["2014"]][["data"]][["IdEstablecimiento"]])

# Iterar sobre cada año para llenar los dataframes
for (year in names(resultados_out[["original"]])) {
  out_vrs_df[[year]] <- resultados_out[["original"]][[year]][["data"]][["vrs"]]
  out_crs_df[[year]] <- resultados_out[["original"]][[year]][["data"]][["crs"]]
}



correlaciones <- sapply(names(in_vrs_df)[-1], function(year) {
  cor(in_vrs_df[[year]], malmquist_in_vrs[[year]], use = "complete.obs")
})

correlaciones













graficos_vrs <- list(
  generar_graficos_iteracion(resultados_in, "Input VRS", "vrs", "in"),
  generar_graficos_iteracion(resultados_in_2_vrs, "Input VRS", "vrs", "in"),
  generar_graficos_iteracion(resultados_in_3_vrs, "Input VRS", "vrs", "in")
)



# -------------------------------------------- #
#  GRAFICA DEA INPUT
# -------------------------------------------- #


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
#  GRAFICA DEA OUTPUT
# -------------------------------------------- #



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
