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
    mat <- as.matrix(df[[col]])
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

columnas <- c("2014", "2015", "2016", "2017", "2018", "2019","2020","2021","2022", "2023")

normalidad_ef_tec <- verificar_normalidad(ef_tec, columnas)
print(normalidad_todas)

qqnorm(ef_tec[["2014"]])
qqline(ef_tec[["2014"]], col = "red") 



# NO SON NORMALES LOS DATOS


# SE REALIZA PRUEBA NO PARAMETRICA:


df_long <- ef_tec %>%
  mutate(DMU = row_number()) %>%        # Identificador del DMU
  pivot_longer(
    cols = starts_with("20"),          # Ajustar si tus columnas se llaman "2014", "2015", etc.
    names_to = "year",
    values_to = "efficiency"
  ) %>%
  mutate(
    DMU  = as.factor(DMU),
    year = as.factor(year)  # o as.numeric si prefieres, pero factor para la prueba
  )


friedman.test(efficiency ~ year | DMU, data = df_long)
# Si este test da un p-valor muy pequeño, indica que hay al menos un año diferente de los demás

# revisar como difieren los años
pairwise.wilcox.test(
  x      = df_long$efficiency,
  g      = df_long$year,
  paired = TRUE,
  p.adjust.method = "bonferroni",
  alternative = "less"  # si tu hipótesis es que X < Y para la comparación de años
)


#Pairwise comparisons using Wilcoxon signed rank test with continuity correction 

#data:  df_long$efficiency and df_long$year 

#2014  2015  2016  2017  2018  2019  2020  2021  2022 
#2015 0.035 -     -     -     -     -     -     -     -    
#  2016 1.000 1.000 -     -     -     -     -     -     -    
#  2017 1.000 1.000 1.000 -     -     -     -     -     -    
#  2018 1.000 1.000 1.000 1.000 -     -     -     -     -    
#  2019 1.000 1.000 1.000 1.000 1.000 -     -     -     -    
#  2020 1.000 1.000 1.000 1.000 0.542 0.472 -     -     -    
#  2021 1.000 1.000 1.000 1.000 0.781 0.149 1.000 -     -    
#  2022 1.000 1.000 1.000 1.000 0.310 0.132 1.000 1.000 -    
#  2023 1.000 1.000 1.000 1.000 1.000 1.000 1.000 1.000 1.000

#P value adjustment method: bonferroni 


pairwise.wilcox.test(
  x      = df_long$efficiency,
  g      = df_long$year,
  paired = TRUE,
  p.adjust.method = "bonferroni",
  alternative = "two.sided"  # si tu hipótesis es que X < Y para la comparación de años
)


#Pairwise comparisons using Wilcoxon signed rank test with continuity correction 

#data:  df_long$efficiency and df_long$year 

#2014    2015    2016    2017    2018    2019    2020    2021    2022   
#2015 0.06907 -       -       -       -       -       -       -       -      
#  2016 1.00000 0.59298 -       -       -       -       -       -       -      
#  2017 1.00000 0.01480 0.01394 -       -       -       -       -       -      
#  2018 0.14960 2.3e-05 1.3e-06 3.7e-05 -       -       -       -       -      
#  2019 0.16942 2.3e-05 6.2e-06 0.00228 1.00000 -       -       -       -      
#  2020 1.00000 0.17178 1.00000 1.00000 1.00000 0.94444 -       -       -      
#  2021 1.00000 0.43112 1.00000 1.00000 1.00000 0.29739 1.00000 -       -      
#  2022 1.00000 1.00000 1.00000 1.00000 0.61911 0.26358 1.00000 1.00000 -      
#  2023 0.00033 1.8e-07 2.8e-05 0.00062 0.30616 1.00000 0.01304 0.00014 2.2e-05

#P value adjustment method: bonferroni 







# MALMQUIST

hip_data <- malmquist_indices[["index"]]
hip_data_2020_2021 <- hip_data[["2020_2021"]]

hip_data_otros <- hip_data[,c(2,3,4,5,6,7,9,10)]

columnas_previas <- c("2014_2015", "2015_2016", "2016_2017", "2017_2018", "2018_2019", "2019_2020","2020_2021","2021_2022","2022_2023")


normalidad_todas <- verificar_normalidad(hip_data, columnas_previas)
print(normalidad_todas)


# ----------------------- #
# ----------------------- #

# Nombres de las columnas:
columnas_previas <- c("2014_2015", "2015_2016", "2016_2017", "2017_2018", "2018_2019", "2019_2020","2021_2022","2022_2023")
columna_critica <- "2020_2021"

# Crear un dataframe para guardar resultados
resultados <- data.frame(
  Periodo_Previo = character(),
  Estadistico_W = numeric(),
  p_valor = numeric(),
  stringsAsFactors = FALSE
)

# Bucle para comparar la columna crítica con cada periodo previo
for (periodo in columnas_previas) {
  # Aplicar Mann-Whitney (alternative = "less" para H₁: crítica < previo)
  prueba <- wilcox.test(
    x = hip_data[[columna_critica]], 
    y = hip_data[[periodo]], 
    alternative = "less",  # Evalúa si la crítica es menor que el periodo previo
    paired = FALSE  # Muestras independientes
  )
  
  # Guardar resultados
  resultados <- rbind(resultados, data.frame(
    Periodo_Previo = periodo,
    Estadistico_W = prueba$statistic,
    p_valor = prueba$p.value
  ))
}



resultados$p_valor_ajustado <- p.adjust(resultados$p_valor, method = "bonferroni")


print(resultados)








