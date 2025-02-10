# Funciones auxiliares
library(randomForest)
library(Benchmarking)
library(gridExtra)
library(corrplot)
library(censReg)
library(Metrics)
library(readxl)
library(purrr)
library(tidyr)
library(dplyr)
library(caret)
library(deaR)

# ==============================================
#  
# ==============================================
filtrar_datos <- function(datos, vector_outliers) {
  datos_filtrados <- lapply(names(datos), function(anio) {
    datos[[anio]] %>% 
      filter(!(IdEstablecimiento %in% vector_outliers))
  })
  names(datos_filtrados) <- names(datos)
  return(datos_filtrados)
}

# ==============================================
#  
# ==============================================

guardar_dataframes_por_anio <- function(dataframes, columnas) {
  # Verificar que columnas sea un vector
  if (!is.vector(columnas)) {
    stop("El parámetro 'columnas' debe ser un vector de nombres de columnas.")
  }
  
  # Crear la lista de resultados
  resultados <- lapply(names(dataframes[["original"]]), function(anio) {
    message(paste("Procesando el año:", anio))
    df <- dataframes[["original"]][[anio]][["data"]]
    
    if (is.null(df)) {
      warning(paste("Datos nulos para el año", anio))
      return(NULL)
    }
    
    if (!("IdEstablecimiento" %in% colnames(df))) {
      warning(paste("El año", anio, "no contiene 'IdEstablecimiento'"))
      return(NULL)
    }
    
    # Verificar que todas las columnas estén presentes
    columnas_faltantes <- columnas[!columnas %in% colnames(df)]
    if (length(columnas_faltantes) > 0) {
      warning(paste("El año", anio, "no contiene las columnas:", paste(columnas_faltantes, collapse = ", ")))
      return(NULL)
    }
    
    message(paste("Datos válidos encontrados para el año", anio))
    df_seleccionado <- reemplazar_nulos(df[, c("IdEstablecimiento", columnas)])
    return(df_seleccionado)
  })
  
  # Asignar nombres a la lista de resultados
  names(resultados) <- names(dataframes[["original"]])
  
  # Filtrar resultados no nulos
  resultados <- Filter(Negate(is.null), resultados)
  
  if (length(resultados) == 0) {
    warning("No hay datos válidos para los años procesados")
    return(NULL)
  }
  
  return(resultados)
}





guardar_dataframe_por_columna <- function(dataframes, columna) {
  # Validar que la columna sea un único valor
  if (!is.character(columna) || length(columna) != 1) {
    stop("El parámetro 'columna' debe ser un único nombre de columna.")
  }
  
  # Extraer datos para la columna especificada
  resultados <- lapply(names(dataframes[["original"]]), function(anio) {
    message(paste("Procesando el año:", anio))
    df <- dataframes[["original"]][[anio]][["data"]]
    
    if (is.null(df)) {
      warning(paste("Datos nulos para el año", anio))
      return(NULL)
    }
    
    if (!("IdEstablecimiento" %in% colnames(df))) {
      warning(paste("El año", anio, "no contiene 'IdEstablecimiento'"))
      return(NULL)
    }
    
    if (!(columna %in% colnames(df))) {
      warning(paste("El año", anio, "no contiene la columna", columna))
      return(NULL)
    }
    
    message(paste("Datos válidos encontrados para el año", anio))
    df_seleccionado <- reemplazar_nulos(df[, c("IdEstablecimiento", columna)])
    colnames(df_seleccionado) <- c("IdEstablecimiento", anio)  # Renombrar columna con el año
    return(df_seleccionado)
  })
  
  # Filtrar resultados no nulos
  resultados <- Filter(Negate(is.null), resultados)
  
  if (length(resultados) == 0) {
    warning("No hay datos válidos para los años procesados")
    return(NULL)
  }
  
  # Combinar resultados en un solo dataframe
  df_final <- Reduce(function(x, y) merge(x, y, by = "IdEstablecimiento", all = TRUE), resultados)
  
  # Calcular el promedio por fila (ignorando nulos)
  df_final$Promedio <- rowMeans(df_final[, -1], na.rm = TRUE)
  
  return(df_final)
}

# ==============================================
#  
# ==============================================
normalize_min_max <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

# ==============================================
#  
# ==============================================
normalize_max_min <- function(x) {
  (max(x) - x) / (max(x) - min(x))
}

# ==============================================
#  
# ==============================================
reemplazar_nulos <- function(df) {
  df[is.na(df) | df == ""] <- "-" # Reemplaza NA o valores vacíos
  return(df)
}

# ==============================================
#  
# ==============================================
calcular_corte <- function(datos, vector_outliers) {
  lapply(datos, function(df) df %>% filter(!(IdEstablecimiento %in% vector_outliers)))
}

# ==============================================
#  
# ==============================================
top_eficiencia <- function(datos, tipo, cantidad, best){
  # Creamos una lista vacía para almacenar los resultados
  mejores_tipo <- list()
  
  # Iteramos sobre cada año en la lista principal
  for (año in names(datos[["original"]])) {
    # Accedemos al dataframe de cada año
    df_año <- datos[["original"]][[año]][["data"]]
    
    # Ordenamos el dataframe por la variable 'vrs' de forma descendente
    if (tipo == "vrs"){
      df_ordenado <- df_año[order(-df_año$vrs), ]
    }else{
      df_ordenado <- df_año[order(-df_año$crs), ]
    }
    
    if (best){mejores <- head(df_ordenado, cantidad)
    
    }else{mejores <- tail(df_ordenado, cantidad)}
    
    mejores_tipo[[año]] <- mejores
  }
  
  
  return (mejores_tipo)
}

# ==============================================
#  
# ==============================================
calcular_correlaciones_all <- function(lista_resultados_combinados_in) {
  # Calcular las matrices de correlación para cada dataframe en la lista
  correlaciones_lista <- lapply(lista_resultados_combinados_in, function(df) {
    df_num <- df %>%
      select(-IdEstablecimiento) %>%
      mutate(across(starts_with("vrs_iteracion_"), ~ as.numeric(replace(., . == "NO APLICA", NA)))) %>%
      mutate(across(starts_with("crs_iteracion_"), ~ as.numeric(replace(., . == "NO APLICA", NA))))
    
    cor(df_num[, sapply(df_num, is.numeric)], use = "complete.obs")
  })
  
  # Nombrar la lista con los años para identificación
  names(correlaciones_lista) <- names(lista_resultados_combinados_in)
  
  # Sumar todas las matrices con `Reduce`:
  suma_matrices <- Reduce("+", correlaciones_lista)
  
  # Calcular el promedio dividiendo por la cantidad de matrices
  n <- length(correlaciones_lista)
  promedio_matriz <- suma_matrices / n
  
  promedio_matriz
  
  
  # Retornar resultados de correlación entre matrices de distintos años
  return(list(correlaciones_lista = correlaciones_lista,
              promedio_correlacion = promedio_matriz))
}



# ==============================================
#  
# ==============================================
calcular_correlaciones <- function(df1, df2, id_col = "ID") {
  # Encontrar IDs comunes
  ids_comunes <- intersect(df1[[id_col]], df2[[id_col]])
  
  # Filtrar dataframes con los IDs comunes
  df1_filtrado <- df1[df1[[id_col]] %in% ids_comunes, ]
  df2_filtrado <- df2[df2[[id_col]] %in% ids_comunes, ]
  
  # Calcular correlaciones por año
  correlaciones <- sapply(names(df1_filtrado)[-1], function(year) {
    cor(df1_filtrado[[year]], df2_filtrado[[year]], use = "complete.obs")
  })
  
  return(correlaciones)
}

# ==============================================
#  
# ==============================================
resumen_eficiencia <- function(datos) {
  # Lista para almacenar las posiciones por establecimiento
  posiciones <- list()
  # Lista para almacenar los porcentajes por región
  porcentajes <- list()
  
  # Iteramos sobre cada año
  for (año in names(datos)) {
    df_año <- datos[[año]]
    
    # Guardamos la posición de cada establecimiento
    posiciones[[año]] <- data.frame(
      IdEstablecimiento = df_año$IdEstablecimiento,
      Region = df_año$Region,
      IDRegion = df_año$region_id,
      Posicion = seq_len(nrow(df_año)),
      Año = año
    )
    
    # Calculamos el porcentaje de ocurrencia de cada región
    porcentaje_region <- as.data.frame(prop.table(table(df_año$Region)) * 100)
    colnames(porcentaje_region) <- c("Region", "Porcentaje")
    
    # Añadimos la columna IDRegion basada en la relación con la región
    id_region <- unique(df_año[, c("Region", "region_id")])
    porcentaje_region <- merge(porcentaje_region, id_region, by = "Region")
    
    porcentaje_region$Año <- año
    porcentajes[[año]] <- porcentaje_region
  }
  
  # Unimos todas las posiciones en un solo dataframe
  posiciones_df <- do.call(rbind, posiciones)
  
  # Transformamos posiciones_df para que cada columna sea un año
  posiciones_wide <- posiciones_df %>%
    pivot_wider(names_from = Año, values_from = Posicion)
  
  # Unimos todos los porcentajes en un solo dataframe
  porcentajes_df <- do.call(rbind, porcentajes)
  
  # Transformamos porcentajes_df para que cada columna sea un año y conservamos IDRegion
  porcentajes_wide <- porcentajes_df %>%
    pivot_wider(names_from = Año, values_from = Porcentaje, values_fill = list(Porcentaje = 0))
  
  # Retornamos una lista con los resultados
  list(Posiciones = posiciones_wide, Porcentajes = porcentajes_wide)
}

# ==============================================
# Función para crear dataframes por período
# ==============================================
eficiencias_dataframe <- function(resultados, tipo, periodo) {
  # Seleccionar rango de años según el período
  rango <- switch(
    periodo,
    "todos" = 2014:2023,
    "pre"   = 2014:2019,
    "post"  = 2020:2023,
    stop("Período no válido. Usa 'todos', 'pre' o 'post'.")
  )
  
  # Combinar datos de los años especificados
  df <- do.call(rbind, lapply(rango, function(year) {
    data <- resultados[[tipo]][["original"]][[as.character(year)]][["data"]]
    data$year <- as.factor(year)  # Añade columna del año como factor
    return(data)
  }))
  return(df)
}

# ==============================================
#  
# ==============================================
malmquist_index <- function(index) {
  # Asegurarse de que las columnas sean numéricas
  index[, -1] <- lapply(index[, -1], as.numeric)
  
  # Renombrar las columnas de acuerdo a los años consecutivos
  columnas <- colnames(index)[-1]
  nuevos_nombres <- paste(columnas[-length(columnas)], columnas[-1], sep = "_")
  colnames(index)[-1] <- c("2014_2015", nuevos_nombres)
  
  # Calcular la tasa promedio pre-pandemia
  index$Tasa_Promedio_Pre_Pandemia <- rowMeans(index[, 2:6], na.rm = TRUE)
  index$Tasa_Promedio_Pandemia <- rowMeans(index[, 7:10], na.rm = TRUE)
  
  return(index)
}

# ===================================================
# SENSIBILIDAD
# ===================================================
aplicar_sensibilidad <- function(datos, resultados, umbral, orientacion, retorno, mayor) {
  mapply(function(data, resultado, anio) {
    # Mostrar el nombre del año en pantalla
    print(paste("Aplicando sensibilidad para el año:", anio))
    
    # Ejecutar la función principal
    sensibilidad_parametro_general(data, resultado, mayor, umbral, orientacion, retorno)
  },
  datos, resultados, names(datos), SIMPLIFY = FALSE) # Pasar los nombres de los datos como argumento
}
