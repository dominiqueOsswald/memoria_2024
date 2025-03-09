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
ef_tec <- guardar_dataframe_por_columna(resultados_usar[["oo"]], retorno)


# PARA VER SIHAY DIFERENCIAS ENTRE PRE Y POST
grupo_1_medianas <- apply(ef_tec[, 3:8], 1, median)
grupo_2_medianas <- apply(ef_tec[, 9:12], 1, median)

wilcox.test(grupo_1_medianas, grupo_2_medianas, paired = TRUE, alternative = "two.sided")

# TWO SIDED

#Dado que el p-valor = 0.8079 es mayor que 0.05, NO se rechaza la hipótesis nula 
#Esto significa que no hay suficiente evidencia estadística para afirmar que las medianas de los dos grupos son significativamente diferentes.



# IMPACTO ENTRE 20189 Y 2020
wilcox.test(ef_tec$'2019', ef_tec$'2020', paired = TRUE, alternative = "greater")


# GREATER
#Dado que el p-valor = 0.01049 es menor que 0.05, se rechaza la hipótesis nula con un nivel de significancia del 5%.
# Esto indica que la mediana de la eficiencia técnica en 2020 es significativamente mayor que en 2019.


# IMPACTO ENTRE 20210 Y 2021
wilcox.test(ef_tec$'2020', ef_tec$'2021', paired = TRUE, alternative = "two.sided")


# TWO SIDED

#Dado que el V = 5452, p-value = 0.3587 es mayor que 0.05, NO se rechaza la hipótesis nula 

# Esto significa que no hay suficiente evidencia estadística para afirmar que la mediana de la eficiencia técnica en 2021 es menor que en 2020.

# ¿Qué significa en términos prácticos?
  
#No se puede concluir que hubo una disminución en la eficiencia técnica entre estos dos años.
#Aunque pueda haber una diferencia en los valores observados, esta no es estadísticamente significativa.
#Es posible que la eficiencia técnica se haya mantenido estable o que la diferencia observada sea producto del azar.









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

