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
# --------------------------- #

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



correlaciones_eficiencia_grafica(resultados_usar[[orientacion]][["resultados_correlacion"]][["correlaciones_lista"]], orientacion, c("VRS iteracion 1", "VRS iteracion 2", "VRS iteracion 3"),  "Sensibilidad por eliminación de DMU eficientes", "iteraciones")
# --------------------------- #


# Extraemos la submatriz de correlaciones vrs para cada año
vrs_atp_list <- lapply(years, function(y) {
  # Accedemos a la matriz de correlaciones del año y
  cor_matriz <- correlacion_todos_metodos_atipicos[[columna]][["original_vs_sin_atipicos"]][[orientacion]][["correlaciones_lista"]][[as.character(y)]]
  
  # Filtramos filas y columnas que inician con "vrs"
  cor_matriz_vrs <- cor_matriz[grepl("^vrs", rownames(cor_matriz)),
                               grepl("^vrs", colnames(cor_matriz))]
  
  return(cor_matriz_vrs)
})

# Asignamos nombres a cada elemento de la lista para identificar el año
names(vrs_atp_list) <- years

# Ahora vrs_cor_list es una lista en la que cada elemento es la submatriz vrs de cada año
vrs_atp_list

correlacion_todos_metodos_atipicos[[columna]][["original_vs_sin_atipicos"]][[orientacion]][["correlaciones_lista"]] <- vrs_atp_list


correlaciones_eficiencia_grafica(correlacion_todos_metodos_atipicos[[columna]][["original_vs_sin_atipicos"]][[orientacion]][["correlaciones_lista"]], "ambos", c("VRS original", "VRS sin atipicos"), "Comparación Original vs sin atipicos - Orientación Outputs VRS", "sin_atipicos")



# GRAFICA DE DISTRIBUCIÓN DE EFICIENCIAS

eficiencias_grafica(resultados_usar)


# ==============================================
#  MALMQUIST 
# ==============================================

# DATOS COMPLETOS / ORIGINALES

datos_usar <- datos_sin_atipicos[[columna]]

# DATOS SIN ATIPICOS PARA VRS OO
malmquist_indices <- malmquist(datos_usar,retorno, "out")

generar_graficas_malmquist(malmquist_indices$index,"out_vrs")


# ==============================================
#  DETERMINANTES
# ==============================================

# Aplicar Random Forest para cada año
random_forest <- lapply(anios, function(anio) {analize_rf(anio, resultados_in = resultados_usar[[orientacion]], 500, retorno, "Entradas")})

# Asignar nombres a la lista de modelos
names(random_forest) <- paste0(anios)


# -------------------------------------------- #
#  EXTRACCIÓN DE VARIABLES POR AÑO
# -------------------------------------------- #

# Llamar a la función
resultados_importancia <- determinantes_importancia_single(random_forest, anios_pre_pandemia, anios_pandemia)

# ==============================================
#  RESULTADOS
# ==============================================



# Almacenar resultados en excel
guardar_resultados(
  dataframes = resultados_usar[[orientacion]],
  retorno,
  resultados_importancia,
  malmquist = malmquist_indices$index,
  carpeta="results/io_crs",
  archivo_salida = "RESULTADOS.xlsx",
  prefijo = orientacion
)



# ==============================================
#    GRAFICAS
# ==============================================
#  TODOS - GRAFICA DEA INPUT VRS

#resultados_usar <- resultados

lapply(anios, function(anio) {
  eficiencias_chile_grafica(resultados_usar[[orientacion]][["original"]][[as.character(anio)]][["data"]], anio, retorno, "Gráfica Chile - Eficiencia técnica ", "Modelo orientado a salidas - VRS -")
})



# ---------------------------------------- #
# ---------------------------------------- #
# ---------------------------------------- #
# PRUEBA DE HIPÓTESIS

verificar_normalidad <- function(df, columnas) {
  resultados <- data.frame(Columna = character(), p_valor = numeric(), Normalidad = character())
  for (col in columnas) {
    mat <- as.matrix(df[,col])
    vec <- as.numeric(mat) 
    
    shapiro <- shapiro.test(vec)
    resultados <- rbind(resultados, data.frame(
      Columna = col,
      p_valor = shapiro$p.value,
      Normalidad = ifelse(shapiro$p.value > 0.05, "Sí", "No")
    ))
  }
  return(resultados)
}




ef_tec <- guardar_dataframe_por_columna(resultados_usar[["oo"]], retorno)


normalidad_ef_tec <- verificar_normalidad(ef_tec, c(3:12))
print(normalidad_ef_tec)

qqnorm(ef_tec[["2014"]])
qqline(ef_tec[["2014"]], col = "red") 



mal_tec <- malmquist_indices[["index"]][,c(2:10)]

normalidad_ef_tec <- verificar_normalidad(mal_tec,c(1:9))
print(normalidad_ef_tec)


# NO SON NORMALES LOS DATOS



# REVISAR SI HAY DIFERENCIAS EN LAS MEDIANAS
df_long_ef <- ef_tec[,c("2014","2015","2016","2017","2018","2019","2020","2021","2022", "2023")] %>%
  mutate(DMU = row_number()) %>%        # Identificador del DMU
  pivot_longer(
    cols = starts_with("20"),          # Ajustar si tus columnas se llaman "2014", "2015", etc.
    names_to = "year",
    values_to = "efficiency"
  ) %>%
  mutate(
    year = as.factor(year)  # o as.numeric si prefieres, pero factor para la prueba
  )

# Aplicar la prueba de Kruskal-Wallis
resultado_kruskal <- kruskal.test(efficiency ~ year, data = df_long_ef)

# Mostrar el resultado
print(resultado_kruskal)



library(dunn.test)

# Aplicar la prueba de Dunn
resultado_dunn <- dunn.test(df_long_ef$efficiency, df_long_ef$year, method = "holm")

# Mostrar el resultado
print(resultado_dunn)

color_fijo <- brewer.pal(11, "RdYlGn")[9]
color_median <- brewer.pal(11, "RdYlGn")[11]

ggplot(df_long_ef, aes(x = factor(year), y = efficiency, fill = factor(year))) +
  geom_violin(fill = color_fijo, color = color_fijo, alpha = 0.6) +
  stat_summary(fun = median, geom = "point", size = 3, color = color_median) + # Agrega puntos de la mediana
  stat_summary(fun = median, geom = "crossbar", width = 0.2, color = color_median) + # Agrega una barra en la mediana
  labs(title = "Eficiencia técnica por año",
       x = "Años", y = "Índice Malmquist") +
  theme_minimal()

