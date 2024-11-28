setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("functions.R")
source("graphics.R")

# -------------------------------------------- #
#  CONSOLIDADO DE DATOS POR AÑO
# -------------------------------------------- #
# ==============================================

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
# SENSIBILIDAD
# -------------------------------------------- #
# ==============================================
# -------------------------------------------- #
#  CÁLCULO DEA - ELIMINACION EFICIENTES
# -------------------------------------------- #

#  INPUT

resultados_in <- resultados_iteracion(datos, "io")

#  OUTPUT

resultados_out <- resultados_iteracion(datos, "oo")

#  COMPARACION DE VALORES ORIGINALES INPUT - OUTPUT - VRS - CRS 

input_output_original <- combinar_resultados_in_out(resultados_in[["original"]], resultados_out[["original"]])
graficas_in_out <- calcular_y_graficar_correlaciones(input_output_original, anios, "ambos")

# -------------------------------------------- #
#  CÁLCULO DEA - ELIMINACIÓN DE DATOS ATÍPICOS
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


# -------------------------------------------- #
#    COMPARACIÓN DE SENSIBILIDAD VS ELIMINACION ATIPICOS 
# -------------------------------------------- #

# Usar la función
resultados <- procesar_datos(
  resultados_in, resultados_out, 
  resultados_in_cut_vrs, resultados_in_cut_crs, 
  resultados_out_cut_vrs, resultados_out_cut_crs
)

# Imprimir resultados
print(resultados$correlaciones_in_vrs)
print(resultados$correlaciones_in_crs)
print(resultados$correlaciones_out_vrs)
print(resultados$correlaciones_out_crs)

# -------------------------------------------- #
#    MALMQUIST 
# -------------------------------------------- #
# ==============================================

malmquist_in_vrs <- calcular_malmquist(datos, "vrs", "in")
malmquist_in_crs <- calcular_malmquist(datos, "crs", "in")
malmquist_out_vrs <- calcular_malmquist(datos, "vrs", "out")
malmquist_out_crs <- calcular_malmquist(datos, "crs", "out")


malmquist_in_vrs[["index"]][, -1] <- lapply(malmquist_in_vrs[["index"]][, -1], as.numeric)


index_valores_vrs <- malmquist_in_vrs[["index"]]
columnas <- colnames(malmquist_in_vrs[["index"]])[-1] 
nuevos_nombres <- paste(columnas[-length(columnas)], columnas[-1], sep = "_")
nuevos_nombres <- c("2014_2015", nuevos_nombres)

colnames(index_valores_vrs)[-1] <- nuevos_nombres
tasa_promedio <- rowMeans(index_valores_vrs[, -c(1, ncol(index_valores_vrs))], na.rm = TRUE)
index_valores_vrs$Tasa_Promedio_Pre_Pandemia <- tasa_promedio


#summary(index_valores_vrs$Tasa_Promedio_Pre_Pandemia)


columnas <- colnames(index_valores_vrs[,-1])

for (col in columnas) {
  datos <- na.omit(index_valores_vrs[[col]])
  media <- mean(datos)
  mediana <- median(datos)
  
  # Crear el gráfico
  grafico <- ggplot(data = data.frame(x = datos), aes(x = x)) +
    geom_density(fill = "blue", alpha = 0.5, color = "blue") +
    geom_vline(aes(xintercept = mediana), color = "green", linetype = "dashed", size = 1) +
    ggtitle(paste("Densidad", col)) +
    xlab("Valores") +
    ylab("Densidad") +
    theme_minimal() +
    # Añadir etiquetas con los valores numéricos
    annotate("text", x = mediana, y = 0.15, label = paste0("Mediana: ", round(mediana, 2)), 
             color = "green", hjust = -0.1)
  
  # Imprimir el gráfico
  print(grafico)
}




# -------------------------------------------- #
#    GRAFICAS
# -------------------------------------------- #
# ==============================================
# -------------------------------------------- #
#    MEJORES RESULTADOS 
# -------------------------------------------- #

mejores_25 <- list("in_vrs" =top_eficiencia(resultados_in, "vrs", 25, TRUE),
                   "in_crs" = top_eficiencia(resultados_in, "crs", 25, TRUE),
                   "out_vrs" = top_eficiencia(resultados_out, "vrs", 25, TRUE),
                   "out_crs" = top_eficiencia(resultados_out, "crs", 25, TRUE)) 


# VRS INPUT
chile_map_plot(mejores_25[["in_vrs"]][["2014"]], 2014, "vrs")
chile_map_plot(mejores_25[["in_vrs"]][["2015"]], 2015, "vrs")
chile_map_plot(mejores_25[["in_vrs"]][["2016"]], 2016, "vrs")
chile_map_plot(mejores_25[["in_vrs"]][["2017"]], 2017, "vrs")
chile_map_plot(mejores_25[["in_vrs"]][["2018"]], 2018, "vrs")
chile_map_plot(mejores_25[["in_vrs"]][["2019"]], 2019, "vrs")
chile_map_plot(mejores_25[["in_vrs"]][["2020"]], 2020, "vrs")



# REGION COLOREADA POR PORCENTAJE DENTRO DEK 25 MEJOR
resumen <- resumen_eficiencia(mejores_25$in_vrs)
colorear_region(resumen)


# -------------------------------------------- #
#  TODOS - GRAFICA DEA INPUT VRS
# -------------------------------------------- #


chile_map_plot(resultados_in[["original"]][["2014"]][["data"]], 2014, "vrs")
chile_map_plot(resultados_in[["original"]], 2015, "vrs")
chile_map_plot(resultados_in[["original"]], 2016, "vrs")
chile_map_plot(resultados_in[["original"]], 2017, "vrs")
chile_map_plot(resultados_in[["original"]], 2018, "vrs")
chile_map_plot(resultados_in[["original"]], 2019, "vrs")
chile_map_plot(resultados_in[["original"]], 2020, "vrs")
      

# -------------------------------------------- #
#  TODOS - GRAFICA DEA OUTPUT VRS
# -------------------------------------------- #
chile_map_plot(resultados_out[["original"]], 2014, "vrs")
chile_map_plot(resultados_out[["original"]], 2015, "vrs")
chile_map_plot(resultados_out[["original"]], 2016, "vrs")
chile_map_plot(resultados_out[["original"]], 2017, "vrs")
chile_map_plot(resultados_out[["original"]], 2018, "vrs")
chile_map_plot(resultados_out[["original"]], 2019, "vrs")
chile_map_plot(resultados_out[["original"]], 2020, "vrs")





#-------------------------------------#
# DETERMINANTES #
#-------------------------------------#
# ==============================================



TOBIT_2014 <- analyze_tobit_model(resultados_in = resultados_in,year = 2014,top_n = 50)
TOBIT_2015 <- analyze_tobit_model(resultados_in = resultados_in,year = 2015,top_n = 50)
TOBIT_2016 <- analyze_tobit_model(resultados_in = resultados_in,year = 2016,top_n = 50)
TOBIT_2017 <- analyze_tobit_model(resultados_in = resultados_in,year = 2017,top_n = 5)
TOBIT_2018 <- analyze_tobit_model(resultados_in = resultados_in,year = 2018,top_n = 5)
TOBIT_2019 <- analyze_tobit_model(resultados_in = resultados_in,year = 2019,top_n = 5)
TOBIT_2020 <- analyze_tobit_model(resultados_in = resultados_in,year = 2020,top_n = 5)




# Ver las top 10 variables con mayor coeficiente
summary(TOBIT_2014$tobit_model)
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








# -------------------------------------------- #

#mapa_interactivo <- ggplotly(region, tooltip = "text")
#print(mapa_interactivo)

# ------------------------------------------- #
