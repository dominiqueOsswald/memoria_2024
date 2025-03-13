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
resultados_importancia <- importancia_dataframe(random_forest)
#resultados_importancia <- determinantes_importancia_single(random_forest, anios_pre_pandemia, anios_pandemia)
# ==============================================
#  RESULTADOS
# ==============================================



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
# PRUEBA DE HIPÓTESIS - EFICIENCIA
ef_tec <- guardar_dataframe_por_columna(resultados_usar[["oo"]], retorno)


# PARA VER SIHAY DIFERENCIAS ENTRE PRE Y POST
grupo_1_medianas <- apply(ef_tec[, 3:8], 1, median)
grupo_2_medianas <- apply(ef_tec[, 9:12], 1, median)

# Calcular la mediana de cada grupo
mediana_grupo1 <- median(grupo_1_medianas, na.rm = TRUE)
mediana_grupo2 <- median(grupo_2_medianas, na.rm = TRUE)


variacion <- 100 * ( (mediana_grupo2-mediana_grupo1)/mediana_grupo1 )

# Comparación
if (mediana_grupo1 > mediana_grupo2) {
  print("La mediana del grupo 1 es mayor que la del grupo 2.")
} else if (mediana_grupo1 < mediana_grupo2) {
  print("La mediana del grupo 2 es mayor que la del grupo 1.")
} else {
  print("Las medianas de ambos grupos son iguales.")
}




wilcox.test(grupo_1_medianas, grupo_2_medianas, paired = TRUE, alternative = "two.sided")

# TWO SIDED

#Dado que el p-valor = 0.8079 es mayor que 0.05, NO se rechaza la hipótesis nula 
#Esto significa que no hay suficiente evidencia estadística para afirmar que las medianas de los dos grupos son significativamente diferentes.



# IMPACTO ENTRE 2019 Y 2020
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








# MALMQUIST


  
mal_tec <- malmquist_indices[["index"]][,c(2:10)]

normalidad_malm <- verificar_normalidad(mal_tec,c(1:9))
print(normalidad_ef_tec)


# REVISAR SI HAY DIFERENCIAS EN LAS MEDIANAS
df_long_malm <- mal_tec %>%
  mutate(DMU = row_number()) %>%        # Identificador del DMU
  pivot_longer(
    cols = starts_with("20"),          # Ajustar si tus columnas se llaman "2014", "2015", etc.
    names_to = "year",
    values_to = "index"
  ) %>%
  mutate(
    year = as.factor(year)  # o as.numeric si prefieres, pero factor para la prueba
  )

# Aplicar la prueba de Kruskal-Wallis
resultado_kruskal_malm <- kruskal.test(index ~ year, data = df_long_malm)

# Mostrar el resultado
print(resultado_kruskal_malm)



library(dunn.test)

# Aplicar la prueba de Dunn
resultado_dunn <- dunn.test(df_long_malm$index, df_long_malm$year, method = "holm")

# Mostrar el resultado
print(resultado_dunn)








# DETERMINANTES
# REVISAR SI HAY SIGNIFICANCIA POR AÑO EN DETERMINANTES:


df_incmse <- resultados_importancia[["df_incmse_10"]]
df_incmse_all <- df_incmse
df_incmse_pre <- df_incmse[,c(1:7)]
df_incmse_post <- df_incmse[,c(1,8:11)]


# Calcular la mediana de los valores en el periodo "pre" (columnas 2 a 7)
df_median_pre <- df_incmse_pre %>%
  mutate(Mediana_Pre = apply(df_incmse_pre[, -1], 1, median, na.rm = TRUE)) %>%
  select(Variable, Mediana_Pre)  # Seleccionar solo Variable y la mediana

# Calcular la mediana de los valores en el periodo "post" (columnas 8 a 11)
df_median_post <- df_incmse_post %>%
  mutate(Mediana_Post = apply(df_incmse_post[, -1], 1, median, na.rm = TRUE)) %>%
  select(Variable, Mediana_Post)  # Seleccionar solo Variable y la mediana


# Unir ambos dataframes en un único dataframe con las medianas
df_median <- left_join(df_median_pre, df_median_post, by = "Variable")
df_median <- calcular_estadisticas(df_median) %>% filter(Frecuencia > 1) 
df_median <- df_median[,-c(3,4)]

df_incmse_all_est <- calcular_estadisticas(df_incmse_all) %>% filter(Frecuencia > 1)
df_incmse_all <- df_incmse_all_est[,c(1:11)]

df_incmse_pre_est <- calcular_estadisticas(df_incmse_pre) %>% filter(Frecuencia > 1) 
df_incmse_pre <- df_incmse_pre_est[,c(1:7)]

df_incmse_post_est <- calcular_estadisticas(df_incmse_post) %>% filter(Frecuencia > 1)
df_incmse_post <- df_incmse_post_est[,c(1:5)]




# Convertir a formato largo (tidy)
df_long_all <- df_incmse_all %>% pivot_longer(-Variable, names_to = "Año", values_to = "Valor")
df_long_all <- na.omit(df_long_all)

df_long_pre <- df_incmse_pre %>% pivot_longer(-Variable, names_to = "Año", values_to = "Valor")
df_long_pre <- na.omit(df_long_pre)

df_long_post <- df_incmse_post %>% pivot_longer(-Variable, names_to = "Año", values_to = "Valor")
df_long_post <- na.omit(df_long_post)

df_long_pre_post <- df_median %>% pivot_longer(-Variable, names_to = "Periodo", values_to = "Valor")
df_long_pre_post <- na.omit(df_long_pre_post)


# Aplicar Kruskal-Wallis para cada fila (variable)
kruskal_results_all <- df_long_all %>%
  group_by(Variable) %>%
  summarise(
    p_value = kruskal.test(Valor ~ Año)$p.value
  )


# Aplicar Kruskal-Wallis para cada fila (variable)
kruskal_results_pre <- df_long_pre %>%
  group_by(Variable) %>%
  summarise(
    p_value = kruskal.test(Valor ~ Año)$p.value
  )


# Aplicar Kruskal-Wallis para cada fila (variable)
kruskal_results_post <- df_long_post %>%
  group_by(Variable) %>%
  summarise(
    p_value = kruskal.test(Valor ~ Año)$p.value
  )

# Aplicar Kruskal-Wallis para cada fila (variable)
kruskal_results_pre_post <- df_long_pre_post %>%
  group_by(Variable) %>%
  summarise(
    p_value = kruskal.test(Valor ~ Periodo)$p.value
  )



print(n=100,kruskal_results_all)
print(n=100,kruskal_results_pre)
print(n=100,kruskal_results_post)
print(n=100,kruskal_results_pre_post)


df_long_kruskal_all <- df_long_all %>% filter(Variable %in% kruskal_results_all$Variable)
df_long_kruskal_pre <- df_long_pre %>% filter(Variable %in% kruskal_results_pre$Variable)
df_long_kruskal_post <- df_long_post %>% filter(Variable %in% kruskal_results_post$Variable)

# Gráfico de evolución de las variables analizadas

library(ggplot2)
library(RColorBrewer)

grafica <- ggplot(df_long_kruskal_all, aes(x = Año, y = Variable, fill = Valor)) +
  geom_tile() +
  theme_minimal() +
  labs(title = "Matriz de valores por variable y año",
       x = "Año", y = "Variable", fill = "Valor") +
  scale_fill_gradientn(
    colors = brewer.pal(11, "RdYlGn"),  # Paleta de colores RdYlGn
    limits = range(df_long_kruskal_all$Valor, na.rm = TRUE)  # Escala de valores automática
  )



ggsave(paste0("determinantes","_","todos",".jpg"), plot = grafica, width = 10, height = 8, dpi = 300)



grafica <- ggplot(df_long_kruskal_pre, aes(x = Año, y = Variable, fill = Valor)) +
  geom_tile() +
  theme_minimal() +
  labs(title = "Matriz de valores por variable y año",
       x = "Año", y = "Variable", fill = "Valor") +
  scale_fill_gradientn(
    colors = brewer.pal(11, "RdYlGn"),  # Paleta de colores RdYlGn
    limits = range(df_long_kruskal_pre$Valor, na.rm = TRUE)  # Escala de valores automática
  )



ggsave(paste0("determinantes","_","pre",".jpg"), plot = grafica, width = 10, height = 8, dpi = 300)





grafica <- ggplot(df_long_kruskal_post, aes(x = Año, y = Variable, fill = Valor)) +
  geom_tile() +
  theme_minimal() +
  labs(title = "Matriz de valores por variable y año",
       x = "Año", y = "Variable", fill = "Valor") +
  scale_fill_gradientn(
    colors = brewer.pal(11, "RdYlGn"),  # Paleta de colores RdYlGn
    limits = range(df_long_kruskal_post$Valor, na.rm = TRUE)  # Escala de valores automática
  )



ggsave(paste0("determinantes","_","post",".jpg"), plot = grafica, width = 10, height = 8, dpi = 300)




df_incmse_est <- resultados_importancia[["df_incmse_est_10"]]



df_incmse_est <- df_incmse_est %>%
  left_join(kruskal_results_all, by = "Variable") %>%
  rename(P_value_all = p_value)  # Cambia el nombre de la columna

df_incmse_est <- df_incmse_est %>%
  left_join(kruskal_results_pre, by = "Variable") %>%
  rename(P_value_pre = p_value)  # Cambia el nombre de la columna

df_incmse_est <- df_incmse_est %>%
  left_join(kruskal_results_post, by = "Variable") %>%
  rename(P_value_post = p_value)  # Cambia el nombre de la columna




# Almacenar resultados en excel
guardar_resultados(
  dataframes = resultados_usar[[orientacion]],
  retorno,
  df_incmse_est,
  malmquist = malmquist_indices$index,
  carpeta="results/io_crs",
  archivo_salida = "RESULTADOS.xlsx",
  prefijo = orientacion
)











# Gráfico de evolución de las 10 variables
ggplot(df_long_10, aes(x = Año, y = Valor, group = Variable, color = Variable)) +
  geom_line() + 
  geom_point() + 
  theme_minimal() +
  labs(title = "Evolución de las primeras 10 variables (2014-2023)",
       x = "Año", y = "Valor de la Variable")













kruskal.test(IncMSE ~ Variable, data = df_importancia_2014)





library(dunn.test)
resultado_dunn <- dunn.test(df_importancia_2014$IncMSE, df_importancia_2014$Variable, method = "bonferroni", table = TRUE)

# Convertir los resultados a un dataframe
df_dunn <- data.frame(
  Comparacion = resultado_dunn$comparisons,
  p_value = resultado_dunn$P.adjusted
)

# Filtrar solo las comparaciones significativas (p < 0.05)
df_dunn_significativas <- df_dunn[df_dunn$p_value < 0.05, ]

# Mostrar solo las comparaciones significativas
print(df_dunn_significativas)
