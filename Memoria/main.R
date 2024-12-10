setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("functions.R")
source("graphics.R")


# ==============================================

#  CONSOLIDADO DE DATOS POR AÑO
anios <- 2014:2020
datos_iniciales <- lapply(anios, consolidar_datos_por_anio)
names(datos_iniciales) <- as.character(anios)


# Encontrar las DMUs comunes en todos los años y filtrar los datos para incluir solo esas DMUs
dmus_comunes <- Reduce(intersect, lapply(datos_iniciales, `[[`, "IdEstablecimiento"))
datos <- lapply(datos_iniciales, function(data) data[data$IdEstablecimiento %in% dmus_comunes, ])


# ==============================================
#  CÁLCULO DEA - ELIMINACION EFICIENTES


resultados <- list(
  io = resultados_iteracion(datos, "io"),
  oo = resultados_iteracion(datos, "oo")
)

# Supongamos que tus dataframes son df1, df2, ..., df6
dataframes <- list("2014" = resultados$io[["original"]][["2014"]][["data"]], 
                   "2015" = resultados$io[["original"]][["2015"]][["data"]], 
                   "2016" = resultados$io[["original"]][["2016"]][["data"]], 
                   "2017" = resultados$io[["original"]][["2017"]][["data"]], 
                   "2018" = resultados$io[["original"]][["2018"]][["data"]], 
                   "2019" = resultados$io[["original"]][["2019"]][["data"]],
                   "2020" = resultados$io[["original"]][["2020"]][["data"]])



# Combinar todos los dataframes por las columnas ID y VRS
gran_dataframe <- reduce(dataframes, full_join, by = c("IdEstablecimiento", "vrs"))


#  COMPARACION DE VALORES ORIGINALES INPUT - OUTPUT - VRS - CRS 

input_output_original <- combinar_resultados_in_out(resultados$io[["original"]], resultados$oo[["original"]])
graficas_in_out <- calcular_y_graficar_correlaciones(input_output_original, anios, "ambos")




#  CÁLCULO DEA - ELIMINACIÓN DE DATOS ATÍPICOS


resultados_cortados <- list(
  io = resultados_corte(resultados$io, "io"),
  oo = resultados_corte(resultados$oo, "oo")
)


# -------------------------------------------- #
#    COMPARACIÓN DE SENSIBILIDAD VS ELIMINACION ATIPICOS 
# -------------------------------------------- #
# PENDIENTE DE VER SI ES NECESARIO
# Usar la función
#comparacion <- procesar_datos(
#  resultados_in, resultados_out, 
#  resultados_in_cut_vrs, resultados_in_cut_crs, 
#  resultados_out_cut_vrs, resultados_out_cut_crs
#)

# Imprimir resultados
#print(resultados$correlaciones_in_vrs)
#print(resultados$correlaciones_in_crs)
#print(resultados$correlaciones_out_vrs)
#print(resultados$correlaciones_out_crs)

# -------------------------------------------- #
#    MALMQUIST 
# -------------------------------------------- #
# ==============================================


malmquist_indices <- list(
  in_vrs = malmquist("vrs", "in"),
  in_crs = malmquist("crs", "in"),
  out_vrs = malmquist("vrs", "out"),
  out_crs = malmquist("crs", "out")
)



procesar_y_graficar(malmquist_indices)



# ==============================================
#    GRAFICAS

#  TODOS - GRAFICA DEA INPUT VRS

lapply(anios, function(anio) {
  chile_map_plot(resultados$io[["original"]][[as.character(anio)]][["data"]], anio, "vrs")
})


#    MEJORES RESULTADOS 

mejores_25 <- list("in_vrs" =top_eficiencia(resultados$io, "vrs", 25, TRUE),
                   "in_crs" = top_eficiencia(resultados$io, "crs", 25, TRUE),
                   "out_vrs" = top_eficiencia(resultados$oo, "vrs", 25, TRUE),
                   "out_crs" = top_eficiencia(resultados$oo, "crs", 25, TRUE)) 


#    VRS INPUT
lapply(anios, function(anio) {
  chile_map_plot(mejores_25[["in_vrs"]][[as.character(anio)]], anio, "vrs")
})


#    REGION COLOREADA POR PORCENTAJE DENTRO DE 25 MEJOR
resumen <- resumen_eficiencia(mejores_25$in_vrs)
colorear_region(resumen)



#-------------------------------------#
# DETERMINANTES #
#-------------------------------------#
# ==============================================


# ESTO NO FUNCIONA ;-;
#TOBIT_2014 <- analyze_tobit_model(resultados_in = resultados_in,year = 2014,top_n = 50)
#TOBIT_2015 <- analyze_tobit_model(resultados_in = resultados_in,year = 2015,top_n = 50)
#TOBIT_2016 <- analyze_tobit_model(resultados_in = resultados_in,year = 2016,top_n = 50)
#TOBIT_2017 <- analyze_tobit_model(resultados_in = resultados_in,year = 2017,top_n = 5)
#TOBIT_2018 <- analyze_tobit_model(resultados_in = resultados_in,year = 2018,top_n = 5)
#TOBIT_2019 <- analyze_tobit_model(resultados_in = resultados_in,year = 2019,top_n = 5)
#TOBIT_2020 <- analyze_tobit_model(resultados_in = resultados_in,year = 2020,top_n = 5)



test_2014 <- analize_rf(2014,resultados_in = resultados_in_cut_vrs, 500)
test_2015 <- analize_rf(2015,resultados_in = resultados_in, 500)
test_2016 <- analize_rf(2016,resultados_in = resultados_in, 500)
test_2017 <- analize_rf(2017,resultados_in = resultados_in, 500)
test_2018 <- analize_rf(2018,resultados_in = resultados_in, 500)
test_2019 <- analize_rf(2019,resultados_in = resultados_in, 500)
test_2020 <- analize_rf(2020,resultados_in = resultados_in, 500)


#top_50_2014 <- test_2014[order(test_2014[, "%IncMSE"], decreasing = TRUE), ][1:50, ]
#top_50_2015 <- test_2015[order(test_2015[, "%IncMSE"], decreasing = TRUE), ][1:50, ]
#top_50_2016 <- test_2016[order(test_2016[, "%IncMSE"], decreasing = TRUE), ][1:50, ]
#top_50_2017 <- test_2017[order(test_2017[, "%IncMSE"], decreasing = TRUE), ][1:50, ]
#top_50_2018 <- test_2018[order(test_2018[, "%IncMSE"], decreasing = TRUE), ][1:50, ]
#top_50_2019 <- test_2019[order(test_2019[, "%IncMSE"], decreasing = TRUE), ][1:50, ]
#top_50_2020 <- test_2020[order(test_2020[, "%IncMSE"], decreasing = TRUE), ][1:50, ]





# Extraer nombres de las variables
variables_2014 <- rownames(test_2014)
variables_2015 <- rownames(test_2015)
variables_2016 <- rownames(test_2016)
variables_2017 <- rownames(test_2017)
variables_2018 <- rownames(test_2018)
variables_2019 <- rownames(test_2019)
variables_2020 <- rownames(test_2020)






# Combinar todas las variables en una sola lista
todas_las_variables <- c(variables_2014, variables_2015, variables_2016, 
                         variables_2017, variables_2018, variables_2019, variables_2020)

# Calcular las frecuencias de cada variable
frecuencias <- table(todas_las_variables)

# Ordenar por frecuencia en orden descendente
frecuencias_ordenadas <- sort(frecuencias, decreasing = TRUE)

# Mostrar las variables con sus frecuencias
print(frecuencias_ordenadas)

# Opcional: filtrar las variables que se repiten en al menos dos años
frecuencias_filtradas <- frecuencias_ordenadas[frecuencias_ordenadas > 1]
print("Variables que se repiten en al menos dos años:")
print(frecuencias_filtradas)




library(ggplot2)

# Convertir las frecuencias en un dataframe
df_frecuencias <- as.data.frame(frecuencias_filtradas)
colnames(df_frecuencias) <- c("Variable", "Frecuencia")

# Graficar las frecuencias
ggplot(df_frecuencias, aes(x = reorder(Variable, -Frecuencia), y = Frecuencia)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Frecuencia de variables entre años", x = "Variable", y = "Frecuencia") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))








# -------------------------------------------- #

#mapa_interactivo <- ggplotly(region, tooltip = "text")
#print(mapa_interactivo)

# ------------------------------------------- #
