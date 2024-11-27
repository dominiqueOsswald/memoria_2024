setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("functions.R")
source("graphics.R")

# ----------------------------------------------- #
# ==============================================
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


# ==============================================
# -------------------------------------------- #
#  CÁLCULO DEA - SENSIBILIDAD
# -------------------------------------------- #

#  INPUT

resultados_in <- resultados_iteracion(datos, "io")

#  OUTPUT

resultados_out <- resultados_iteracion(datos, "oo")

#  COMPARACION DE VALORES ORIGINALES INPUT - OUTPUT - VRS - CRS 

input_output_original <- combinar_resultados_in_out(resultados_in[["original"]], resultados_out[["original"]])
graficas_in_out <- calcular_y_graficar_correlaciones(input_output_original, anios, "ambos")

# ==============================================
# -------------------------------------------- #
#  ELIMINACIÓN DE DATOS ATÍPICOS
# -------------------------------------------- #

# INPUT

datos_cut_in_vrs <- lapply(datos, function(df) {df %>% filter(!(IdEstablecimiento %in% resultados_in[["vector_outliers_vrs"]]))})
datos_cut_in_crs <- lapply(datos, function(df) {df %>% filter(!(IdEstablecimiento %in% resultados_in[["vector_outliers_crs"]]))})

resultados_in_cut_vrs <- resultados_iteracion(datos_cut_in_vrs, "io")
resultados_in_cut_crs <- resultados_iteracion(datos_cut_in_crs, "io")

# OUTPUT

datos_cut_out_vrs <- lapply(datos, function(df) {df %>% filter(!(IdEstablecimiento %in% resultados_out[["vector_outliers_vrs"]]))})
datos_cut_out_crs <- lapply(datos, function(df) {df %>% filter(!(IdEstablecimiento %in% resultados_out[["vector_outliers_crs"]]))})

resultados_out_cut_vrs <- resultados_iteracion(datos_cut_out_vrs, "oo")
resultados_out_cut_crs <- resultados_iteracion(datos_cut_out_crs, "oo")

# ==============================================
# -------------------------------------------- #
#    COMPARACIÓN DE METODOS 
# -------------------------------------------- #

# Crear dataframes base de ID
in_vrs_df <- data.frame(ID = resultados_in[["original"]][["2014"]][["data"]][["IdEstablecimiento"]])
in_crs_df <- data.frame(ID = resultados_in[["original"]][["2014"]][["data"]][["IdEstablecimiento"]])
out_vrs_df <- data.frame(ID = resultados_out[["original"]][["2014"]][["data"]][["IdEstablecimiento"]])
out_crs_df <- data.frame(ID = resultados_out[["original"]][["2014"]][["data"]][["IdEstablecimiento"]])

in_vrs_por_anio_cut <- data.frame(ID = resultados_in_cut_vrs[["original"]][["2014"]][["data"]][["IdEstablecimiento"]])
in_crs_por_anio_cut <- data.frame(ID = resultados_in_cut_crs[["original"]][["2014"]][["data"]][["IdEstablecimiento"]])
out_vrs_por_anio_cut <- data.frame(ID = resultados_out_cut_vrs[["original"]][["2014"]][["data"]][["IdEstablecimiento"]])
out_crs_por_anio_cut <- data.frame(ID = resultados_out_cut_crs[["original"]][["2014"]][["data"]][["IdEstablecimiento"]])

# Llenar los dataframes con los valores de VRS y CRS por cada año
for (year in names(resultados_in[["original"]])) {
  in_vrs_df[[year]] <- resultados_in[["original"]][[year]][["data"]][["vrs"]]
  in_crs_df[[year]] <- resultados_in[["original"]][[year]][["data"]][["crs"]]
  
  in_vrs_por_anio_cut[[year]] <- resultados_in_cut_vrs[["original"]][[year]][["data"]][["vrs"]]
  in_crs_por_anio_cut[[year]] <- resultados_in_cut_crs[["original"]][[year]][["data"]][["crs"]]
  
  out_vrs_df[[year]] <- resultados_out[["original"]][[year]][["data"]][["vrs"]]
  out_crs_df[[year]] <- resultados_out[["original"]][[year]][["data"]][["crs"]]
  
  out_vrs_por_anio_cut[[year]] <- resultados_out_cut_vrs[["original"]][[year]][["data"]][["vrs"]]
  out_crs_por_anio_cut[[year]] <- resultados_out_cut_crs[["original"]][[year]][["data"]][["crs"]]
}

# Calcular las correlaciones
correlaciones_in_vrs <- calcular_correlaciones(in_vrs_df, in_vrs_por_anio_cut)
correlaciones_in_crs <- calcular_correlaciones(in_crs_df, in_crs_por_anio_cut)
correlaciones_out_vrs <- calcular_correlaciones(out_vrs_df, out_vrs_por_anio_cut)
correlaciones_out_crs <- calcular_correlaciones(out_crs_df, out_crs_por_anio_cut)

# Imprimir las correlaciones 

print(correlaciones_in_vrs)
print(correlaciones_in_crs)
print(correlaciones_out_vrs)
print(correlaciones_out_crs)

# ==============================================
# -------------------------------------------- #
#    MALMQUIST 
# -------------------------------------------- #


malmquist_in_vrs <- calcular_malmquist(datos, "vrs", "in")
malmquist_in_crs <- calcular_malmquist(datos, "crs", "in")
malmquist_out_vrs <- calcular_malmquist(datos, "vrs", "out")
malmquist_out_crs <- calcular_malmquist(datos, "crs", "out")


malmquist_in_vrs[["index"]][, -1] <- lapply(malmquist_in_vrs[["index"]][, -1], as.numeric)

# Calcular la tasa de crecimiento año a año
tasa_crecimiento <- malmquist_in_vrs[["index"]]
#tasa_crecimiento_pre <- malmquist_in_vrs[["index"]][, -ncol(malmquist_in_vrs[["index"]])]
for (i in 3:ncol(malmquist_in_vrs[["index"]])) {
  tasa_crecimiento[[i]] <- (malmquist_in_vrs[["index"]][[i]] - malmquist_in_vrs[["index"]][[i-1]]) / malmquist_in_vrs[["index"]][[i-1]]
  #tasa_crecimiento_pre[[i]] <- (malmquist_in_vrs[["index"]][[i]] - malmquist_in_vrs[["index"]][[i-1]]) / malmquist_in_vrs[["index"]][[i-1]]
}


columnas <- colnames(malmquist_in_vrs[["index"]])[-1] 
nuevos_nombres <- paste(columnas[-length(columnas)], columnas[-1], sep = "_")
nuevos_nombres <- c("2014_2015", nuevos_nombres)

colnames(tasa_crecimiento)[-1] <- nuevos_nombres
tasa_promedio <- rowMeans(tasa_crecimiento[, -c(1, ncol(tasa_crecimiento))], na.rm = TRUE)
tasa_crecimiento$Tasa_Promedio_Pre_Pandemia <- tasa_promedio



# ==============================================
# -------------------------------------------- #
#    GRAFICA DE MEJORES RESULTADOS 
# -------------------------------------------- #

mejores_25 <- list("in_vrs" =top_eficiencia(resultados_in, "vrs", 25, TRUE),
                   "in_crs" = top_eficiencia(resultados_in, "crs", 25, TRUE),
                   "out_vrs" = top_eficiencia(resultados_out, "vrs", 25, TRUE),
                   "out_crs" = top_eficiencia(resultados_out, "crs", 25, TRUE)) 


resumen <- resumen_eficiencia(mejores_25$in_vrs)

colorear_region(resumen)

# ==============================================
# -------------------------------------------- #
#  GRAFICA DEA INPUT
# -------------------------------------------- #


# Graficas #
# Generar y mostrar gráficos VRS

grafica <- generar_graficos_iteracion(resultados_in[["original"]], "Input VRS", "vrs", "in")
print(grafica)


do.call(grid.arrange, c(grafica, nrow = 2, ncol = 4)) # Ajusta según tus necesidades

# Mostrar gráficos VRS
lapply(graficos_vrs, function(graficos) {
  grid.arrange(grobs = graficos, ncol = 3)
})



# Iterar sobre los años y mostrar los gráficos
for (anio in anios) {
  graficos_vrs <- generar_graficos_por_anio(anio, "Input - VRS")
  # Mostrar los gráficos en una fila (3 gráficos por año)
  grid.arrange(grobs = graficos_vrs, ncol = 3, top = paste("Comparación de Eficiencia Input VRS - Año", anio))
}



# ==============================================
#-------------------------------------#
# DETERMINANTES #
#-------------------------------------#


datos_iniciales <- list(
  "2014" = analyze_tobit_model(resultados_in = resultados_in,year = 2014,top_n = 50),
  "2015" = analyze_tobit_model(resultados_in = resultados_in,year = 2015,top_n = 5),

  
)

TOBIT_2014 <- analyze_tobit_model(resultados_in = resultados_in,year = 2014,top_n = 50)
TOBIT_2015 <- analyze_tobit_model(resultados_in = resultados_in,year = 2015,top_n = 5)
TOBIT_2016 <- analyze_tobit_model(resultados_in = resultados_in,year = 2016,top_n = 50)
TOBIT_2017 <- analyze_tobit_model(resultados_in = resultados_in,year = 2017,top_n = 5)
TOBIT_2018 <- analyze_tobit_model(resultados_in = resultados_in,year = 2018,top_n = 5)
TOBIT_2019 <- analyze_tobit_model(resultados_in = resultados_in,year = 2019,top_n = 5)
TOBIT_2020 <- analyze_tobit_model(resultados_in = resultados_in,year = 2020,top_n = 5)




# Ver las top 10 variables con mayor coeficiente
print(TOBIT_2014$top_coefficients)
print(TOBIT_2015$top_coefficients)
print(TOBIT_2016$top_coefficients)
print(TOBIT_2017$top_coefficients)
print(TOBIT_2018$top_coefficients)
print(TOBIT_2019$top_coefficients)
print(TOBIT_2020$top_coefficients)






library(ggplot2)

# Crear un gráfico de barras para las 10 variables con mayor impacto
ggplot(top_coef, aes(x = reorder(Variable, abs(Coeficiente)), y = Coeficiente)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "Variables con Mayor Coeficiente en el Modelo Tobit",
    x = "Variable",
    y = "Coeficiente"
  ) +
  theme_minimal()









#region_rm_2020 <- region_vrs(resultados_2020_in, 13, 2020)
#print(region_rm_2020)

#region_rm_2021 <- region_vrs(resultados_2021_in, 13, 2021)
#print(region_rm_2021)





# -------------------------------------------- #

#mapa_interactivo <- ggplotly(region, tooltip = "text")
#print(mapa_interactivo)

# ------------------------------------------- #
