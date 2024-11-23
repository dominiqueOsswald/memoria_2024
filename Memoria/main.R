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
graficas_in_out <- calcular_y_graficar_correlaciones(input_output_original, anios)

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

# Calcular las correlaciones utilizando la función creada
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

colnames(tasa_crecimiento)[-1] <- paste0("Crecimiento_", colnames(malmquist_in_vrs[["index"]])[-1])
tasa_promedio <- rowMeans(tasa_crecimiento[, -1], na.rm = TRUE)
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
datos_consolidados <- read.table("data/2014/2014_consolidated_data.csv", sep=";", header=TRUE)
df <- datos_consolidados
df[colnames(datos_consolidados)] <- lapply(df[colnames(datos_consolidados)], as.integer)


#variables_independientes <- colnames(df[,-1])




#setdiff(filtered_vars,colnames(datos_normalizados))

#valid_filtered_vars <- intersect(filtered_vars, colnames(datos_consolidados))
#print(valid_filtered_vars)


# No me tinca esto, revisar para que pueda traer todas kas variables
# filtered_vars <- intersect(filtered_vars, colnames(datos_normalizados))


# data_filtered_vars <- datos_consolidados %>% select(all_of(filtered_vars))

df_vrs <- resultados_in[["original"]][["2014"]][["data"]][, c("IdEstablecimiento", "vrs")] %>% 
  rename("idEstablecimiento" = "IdEstablecimiento")

df_w_vrs <- df %>%
  filter(idEstablecimiento %in% df_vrs$idEstablecimiento)

# Combinar los dataframes por la columna "ID"
df_merged <- merge(df_w_vrs, df_vrs, by = "idEstablecimiento", all.x = TRUE)

df_merged <- df_merged[, colSums(is.na(df_merged)) < nrow(df_merged)]


df_merged <- df_merged %>% select(where(~ !all(is.na(.))))


nas_por_hospital <- rowMeans(is.na(df_merged)) * 100
#df_merged[is.na(df_merged)] <- 0


analizar_nas <- function(datos) {
  # Porcentaje de NA's por variable
  nas_por_variable <- colMeans(is.na(datos)) * 100
  
  # Patrón de NA's por fila
  nas_por_fila <- rowMeans(is.na(datos)) * 100
  
  # Correlación entre NA's
  na_matriz <- is.na(datos) * 1
  correlacion_nas <- cor(na_matriz, use = "pairwise.complete.obs")
  
  # Clasificación de filas por porcentaje de NA's
  clasificacion <- data.frame(
    "Rango NA (%)" = c("0-10%", "10-25%", "25-50%", "50-75%", ">75%"),
    "Cantidad de Filas" = c(
      sum(nas_por_fila <= 10),
      sum(nas_por_fila > 10 & nas_por_fila <= 25),
      sum(nas_por_fila > 25 & nas_por_fila <= 50),
      sum(nas_por_fila > 50 & nas_por_fila <= 75),
      sum(nas_por_fila > 75)
    )
  )
  
  return(list(
    porcentaje_por_variable = sort(nas_por_variable, decreasing = TRUE),
    porcentaje_por_fila = nas_por_fila,
    correlacion = correlacion_nas,
    clasificacion = clasificacion
  ))
}


# Función para diferentes estrategias de manejo de NA's
comparar_estrategias_na <- function(datos, var_dependiente, vars_independientes) {
  # 1. Eliminar filas con NA
  datos_complete <- na.omit(datos[c(var_dependiente, vars_independientes)])
  
  # 2. Reemplazar con 0
  datos_zero <- datos
  datos_zero[is.na(datos_zero)] <- 0
  
  # 3. Reemplazar con mediana por grupo
  datos_median <- datos %>%
    group_by(tipo_hospital) %>%  # Asumiendo que tienes una columna tipo_hospital
    mutate(across(everything(), ~ifelse(is.na(.), median(., na.rm = TRUE), .))) %>%
    ungroup()
  
  # 4. Estrategia mixta: 0 para procedimientos no realizados, mediana para datos faltantes
  datos_mixed <- datos
  # Asumiendo que tienes una forma de identificar verdaderos ceros vs NA's
  
  # Ajustar modelos con diferentes tratamientos
  resultados <- list()
  
  # Función auxiliar para ajustar modelo
  ajustar_modelo <- function(datos_subset) {
    tryCatch({
      formula_str <- paste(var_dependiente, "~", 
                           paste(vars_independientes, collapse = " + "))
      modelo <- censReg(as.formula(formula_str),
                        data = datos_subset,
                        left = 0,
                        right = 1)
      return(modelo)
    }, error = function(e) {
      return(NULL)
    })
  }
  
  # Ajustar modelos con diferentes tratamientos
  modelos <- list(
    completo = ajustar_modelo(datos_complete),
    zeros = ajustar_modelo(datos_zero),
    medianas = ajustar_modelo(datos_median)
  )
  
  # Comparar resultados
  comparacion <- lapply(modelos, function(mod) {
    if(is.null(mod)) return(NULL)
    
    resumen <- summary(mod)
    list(
      coeficientes = resumen@coef,
      loglik = logLik(mod),
      n_obs = nobs(mod)
    )
  })
  
  return(list(
    modelos = modelos,
    comparacion = comparacion
  ))
}

# Función para análisis de sensibilidad
analisis_sensibilidad_nas <- function(datos, var_dependiente, vars_independientes) {
  # Crear varios escenarios de reemplazo
  escenarios <- list(
    zero = 0,
    min = function(x) min(x, na.rm = TRUE),
    median = function(x) median(x, na.rm = TRUE),
    mean = function(x) mean(x, na.rm = TRUE)
  )
  
  resultados <- list()
  
  for(nombre_escenario in names(escenarios)) {
    datos_temp <- datos
    reemplazo <- escenarios[[nombre_escenario]]
    
    if(is.function(reemplazo)) {
      for(var in vars_independientes) {
        datos_temp[[var]][is.na(datos_temp[[var]])] <- reemplazo(datos_temp[[var]])
      }
    } else {
      datos_temp[is.na(datos_temp)] <- reemplazo
    }
    
    formula_str <- paste(var_dependiente, "~", 
                         paste(vars_independientes, collapse = " + "))
    
    modelo <- tryCatch({
      censReg(as.formula(formula_str),
              data = datos_temp,
              left = 0,
              right = 1)
    }, error = function(e) NULL)
    
    if(!is.null(modelo)) {
      resultados[[nombre_escenario]] <- list(
        coeficientes = summary(modelo)@coef,
        loglik = logLik(modelo),
        aic = AIC(modelo)
      )
    }
  }
  
  return(resultados)
}



result <- analizar_nas(df_merged)



resultados_comparacion <- comparar_estrategias_na(df_merged, 
                                                  "vrs", 
                                                  variables_independientes)

# Análisis de sensibilidad
sensibilidad <- analisis_sensibilidad_nas(df_merged, 
                                          "vrs", 
                                          variables_independientes)





variables_independientes <- colnames(df_merged[,-1])





library(AER)
library(censReg)
library(VGAM)
library(dplyr)

# Función para preparar los datos y realizar análisis Tobit
realizar_analisis_tobit <- function(datos, var_dependiente, vars_independientes, 
                                    limite_inferior = 0, limite_superior = Inf) {
  
  datos <- df_merged
  var_dependiente <- "vrs"
  vars_independientes <- variables_independientes 
  
  # 1. Preparación de datos
  # Eliminar filas con NA
  datos_clean <- datos %>%
    select(all_of(c(var_dependiente, vars_independientes))) %>%
    na.omit()
  
  # 2. Crear fórmula para el modelo
  formula_str <- paste(var_dependiente, "~", 
                       paste(vars_independientes, collapse = " + "))
  options(expressions = 50000)
  formula_modelo <- as.formula(formula_str)
  
  # 3. Ajustar modelo Tobit
  tryCatch({
    # Intentar ajustar con censReg (más eficiente para datasets grandes)
    modelo_tobit <- censReg(formula_modelo, 
                            data = datos_clean,
                            left = limite_inferior,
                            right = limite_superior)
    
    # Resumen del modelo
    resumen <- summary(modelo_tobit)
    
    # Calcular pseudo R²
    loglik_full <- logLik(modelo_tobit)
    modelo_null <- censReg(as.formula(paste(var_dependiente, "~ 1")),
                           data = datos_clean,
                           left = limite_inferior,
                           right = limite_superior)
    loglik_null <- logLik(modelo_null)
    pseudo_r2 <- 1 - (loglik_full/loglik_null)
    
    # Crear lista con resultados
    resultados <- list(
      modelo = modelo_tobit,
      resumen = resumen,
      pseudo_r2 = pseudo_r2,
      datos_utilizados = nrow(datos_clean),
      vars_significativas = row.names(resumen@coef)[abs(resumen@coef[,"z value"]) > 1.96]
    )
    
    return(resultados)
    
  }, error = function(e) {
    # Si falla censReg, intentar con VGAM (más robusto pero más lento)
    mensaje <- paste("Error en censReg:", e$message, 
                     "\nIntentando con VGAM...")
    print(mensaje)
    
    modelo_tobit <- vglm(formula_modelo,
                         tobit(Lower = limite_inferior, Upper = limite_superior),
                         data = datos_clean)
    
    return(list(
      modelo = modelo_tobit,
      resumen = summary(modelo_tobit),
      datos_utilizados = nrow(datos_clean)
    ))
  })
}

# Ejemplo de uso
# datos <- tu_dataframe
# variables_independientes <- c("var1", "var2", "var3")
resultados <- realizar_analisis_tobit(df_merged, 
                                   "vrs",
                                   variables_independientes)
# 
# # Ver resultados
# print(resultados$resumen)
# print(paste("Pseudo R²:", resultados$pseudo_r2))
# print(paste("Variables significativas:", 
#            paste(resultados$vars_significativas, collapse=", ")))








correlations <- sapply(df_numeric, function(x) cor(x, df_numeric$vrs, use = "complete.obs"))
filtered_vars <- names(correlations[abs(correlations) > 0.1])  # Umbral ajustable







#datos_consolidados$idEstablecimiento








library(AER)



# Dividir las variables en bloques de 1000
chunk_size <- 1000
chunks <- split(variables_independientes, ceiling(seq_along(variables_independientes) / chunk_size))

variables_independientes[1:10]

formula <- as.formula(paste("vrs ~", paste(variables_independientes[1:600], collapse = " + ")))
tobit_1 <- tobit(formula, data = df_merged[,-1], left = 0)



sapply(df_merged, function(x) if (is.factor(x)) levels(x))

# Ajustar modelos por partes
resultados <- lapply(chunks, function(vars) {
  formula <- as.formula(paste("vrs ~", paste(vars, collapse = " + ")))
  tobit(formula, data = df_merged[,-1], left = 0)
})

# Combinar resultados (puedes analizar significancia o métricas)
summary(resultados[[1]])  # Ejemplo con el primer modelo















formula <- as.formula(paste("y ~", paste(variables_independientes, collapse = " + ")))



variances <- apply(datos_consolidados[, variables_independientes], 2, var)

# Explorar estadísticas descriptivas
summary(variances)
hist(variances, breaks = 50, main = "Distribución de Varianzas", xlab = "Varianza")





variances <- apply(datos_consolidados[, variables_independientes], 2, var)
threshold <- 1e-6  # Ajusta según el caso
filtered_vars <- names(variances[variances > threshold])





# Resultado
print(df_merged)


library(glmnet)

# Convertir datos a matrices
X <- as.matrix(datos_consolidados[, variables_independientes])
Y <- datos_consolidados$y

# Ajustar modelo LASSO
modelo_lasso <- cv.glmnet(X, Y, alpha = 1, family = "gaussian")  # Alpha = 1 para LASSO

# Variables seleccionadas
coeficientes <- coef(modelo_lasso, s = "lambda.min")
print(coeficientes)

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
