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


# Función para crear dataframes por período
crear_dataframe <- function(resultados, tipo, periodo) {
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



# Función para generar resultados
procesar_y_guardar_resultados <- function(dataframes, resultados_IncNodePurity, resultados_IncMSE, archivo_salida, prefijo) {
  guardar_resultados(
    dataframes = dataframes,
    resultados_IncNodePurity = resultados_IncNodePurity,
    resultados_IncMSE = resultados_IncMSE,
    archivo_salida = archivo_salida,
    prefijo = prefijo
  )
}

# Crear listas para dataframes
crear_dataframes <- function(resultados, orientacion) {
  anios <- 2014:2023
  dataframes <- lapply(anios, function(anio) resultados[[orientacion]][["original"]][[as.character(anio)]][["data"]])
  names(dataframes) <- as.character(anios)
  return(dataframes)
}

guardar_resultados <- function(dataframes, resultados_IncNodePurity, resultados_IncMSE, archivo_salida, prefijo) {
  # Instalar y cargar el paquete necesario
  if (!require(openxlsx)) install.packages("openxlsx")
  library(openxlsx)
  
  # Función para reemplazar valores vacíos, NULL o NA por "-"
  reemplazar_nulos <- function(df) {
    df[is.na(df) | df == ""] <- "-" # Reemplaza NA o valores vacíos
    return(df)
  }
  
  # Crear un workbook
  wb <- createWorkbook()
  
  # Guardar resultados VRS y CRS
  vrs <- guardar_dataframes(dataframes, "vrs")
  crs <- guardar_dataframes(dataframes, "crs")
  
  # Añadir la hoja
  addWorksheet(wb, "VRS")
  writeData(wb, "VRS", vrs)
  
  addWorksheet(wb, "CRS")
  writeData(wb, "CRS", crs)
  
  ### GUARDAR IMPORTANCIA ###
  # IncNodePurity
  addWorksheet(wb, paste0("Determinantes IncNodePurity VRS"))
  writeData(wb, paste0("Determinantes IncNodePurity VRS"), 
            reemplazar_nulos(resultados_IncNodePurity[[paste0(prefijo, "_vrs")]]))
  
  addWorksheet(wb, paste0("Determinantes IncNodePurity CRS"))
  writeData(wb, paste0("Determinantes IncNodePurity CRS"), 
            reemplazar_nulos(resultados_IncNodePurity[[paste0(prefijo, "_crs")]]))
  
  # %IncMSE
  addWorksheet(wb, paste0("Determinantes IncMSE VRS"))
  writeData(wb, paste0("Determinantes IncMSE VRS"), 
            reemplazar_nulos(resultados_IncMSE[[paste0(prefijo, "_vrs")]]))
  
  addWorksheet(wb, paste0("Determinantes IncMSE CRS"))
  writeData(wb, paste0("Determinantes IncMSE CRS"), 
            reemplazar_nulos(resultados_IncMSE[[paste0(prefijo, "_crs")]]))
  
  # Guardar el archivo
  saveWorkbook(wb, archivo_salida, overwrite = TRUE)
  cat("Archivo guardado como:", archivo_salida, "\n")
}


guardar_dataframes <- function(dataframes, columna) {
  # Extraer el valor especificado (vrs o crs) de cada dataframe por año
  resultados <- lapply(names(dataframes), function(anio) {
    df <- dataframes[[anio]]
    
    # Verificar si el dataframe contiene las columnas requeridas
    if (!("IdEstablecimiento" %in% colnames(df)) || !(columna %in% colnames(df))) {
      warning(paste("El año", anio, "no contiene las columnas necesarias"))
      return(NULL)  # Retornar NULL si falta alguna columna
    }
    
    # Seleccionar columna especificada junto con IdEstablecimiento
    df_seleccionado <- reemplazar_nulos(df[, c("IdEstablecimiento", columna)])
    # Renombrar la columna con el año correspondiente
    colnames(df_seleccionado) <- c("IdEstablecimiento", anio)
    return(df_seleccionado)
  })
  
  # Eliminar entradas nulas
  resultados <- Filter(Negate(is.null), resultados)
  
  # Verificar que existan resultados antes de combinar
  if (length(resultados) == 0) {
    warning(paste("No hay datos válidos para la hoja"))
    return(NULL)  # Salir de la función si no hay resultados válidos
  }
  
  # Unir los resultados por columna (IdEstablecimiento como fila)
  df_final <- Reduce(function(x, y) merge(x, y, by = "IdEstablecimiento", all = TRUE), resultados)
  
  # Añadir columna de promedio
  df_final$Promedio <- rowMeans(df_final[, -1], na.rm = TRUE)
  
  return (df_final)
}



# ******************************* #
# Eliminación de datos atípicos y recalculo DEA
calcular_corte <- function(datos, vector_outliers) {
  lapply(datos, function(df) df %>% filter(!(IdEstablecimiento %in% vector_outliers)))
}

resultados_corte <- function(resultados, tipo) {
  resultados_iteracion(calcular_corte(datos, resultados[[paste0("vector_outliers_", tipo, "_vrs")]]), tipo)
}
# ===================================================
# MALMQUIST
# ===================================================
malmquist <- function(tipo, orientacion) {
  calcular_malmquist(datos, tipo, orientacion)
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
# ===================================================
# NORMALIZAR DATA
# ===================================================
normalize_min_max <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

normalize_max_min <- function(x) {
  (max(x) - x) / (max(x) - min(x))
}

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

# ===================================================
# CONSOLIDACIÓN DE DATOS
# ===================================================
consolidar_datos_por_anio <- function(anio) {
  
  # Establecer directorio de trabajo
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  
  # Definir rutas de archivos utilizando el año como variable
  path_hospitales <- paste0("data/", anio, "/", anio, "_hospitals.csv")
  path_predicciones_grd <- paste0("data/", anio, "/", anio, "_prediciones_grd.txt")
  path_datos_consolidados <- paste0("data/", anio, "/", anio, "_consolidated_data.csv")
  path_financiero <- paste0("data/", anio, "/", anio, "_financial_data.csv")
  path_estadisticas <- "data/Consolidado estadísticas hospitalarias 2014-2023.xlsx"
  path_consultas <- paste0("data/", anio, "/variables/", anio, "_consultas.txt")
  path_quirofano <- paste0("data/", anio, "/variables/", anio, "_quirofano.txt")
  
  # Cargar datos
  hospitales <- read.csv(path_hospitales) %>% rename("IdEstablecimiento" = "hospital_id")
  predicciones_grd <- read.csv(path_predicciones_grd, sep=",")
  datos_consolidados <- read.table(path_datos_consolidados, sep=";", header=TRUE)
  financiero <- read.csv(path_financiero) %>% 
    select(hospital_id, X21_value, X22_value) %>% rename("IdEstablecimiento" = "hospital_id")
  financiero$X21_value <- as.numeric(financiero$X21_value)
  financiero$X22_value <- as.numeric(financiero$X22_value)
  
  financiero <- financiero[rowSums(is.na(financiero)) < 2, ]
  
  
  estadisticas <- read_excel(path_estadisticas, sheet = (anio - 2014) + 1, skip = 1)  %>% 
    rename("IdEstablecimiento" = "Cód. Estab.", "Region" = "Nombre SS/SEREMI") %>%
    filter(`Nombre Nivel Cuidado` == "Datos Establecimiento") %>% 
    select(-"Cód. Nivel Cuidado", -"Cód. SS/SEREMI", -"Nombre Nivel Cuidado") %>%  
    semi_join(predicciones_grd, by = "IdEstablecimiento") %>%
    select(1:5)
  
  # Procesar estadísticas
  dias_cama_disponibles <- estadisticas %>% 
    filter(Glosa == "Dias Cama Disponibles") %>%  
    select(1:5) %>% rename("dias_cama_disponible" = "Acum") %>% select(-Glosa)
  
  egresos <- estadisticas %>% 
    filter(Glosa == "Numero de Egresos") %>%  
    select(1:5) %>% rename("egresos" = "Acum") %>% select(-Glosa)


  consultas <- unlist(strsplit(readLines(path_consultas), ","))
  # Seleccionar y convertir columnas válidas
  columnas_validas <- intersect(unlist(consultas), colnames(datos_consolidados))
  
  consultas_data <- subset(datos_consolidados, select = columnas_validas)
  
  # Identificar columnas tipo character
  cols_char <- sapply(consultas_data, is.character)
  
  # Convertir columnas character a numeric
  consultas_data[, cols_char] <- lapply(consultas_data[, cols_char], function(x) as.numeric(x))
  
  # Crear suma total de consultas
  consultas_data$sumaTotal <- rowSums(consultas_data[, -which(names(consultas_data) == "idEstablecimiento")], na.rm = TRUE)
  
  consultas <- data.frame(idEstablecimiento = consultas_data$idEstablecimiento, 
                          Consultas = consultas_data$sumaTotal) %>%
    rename("IdEstablecimiento" = "idEstablecimiento") %>%
    inner_join(predicciones_grd, by = "IdEstablecimiento") %>%
    select(IdEstablecimiento, Consultas)
  
  # Definir quirofano
  #quirofano <- list("idEstablecimiento", "X21220100", "X21220200", "X21220700", "X21220600", "X21400300", "X21220900",
  #                  "X21400500","X21400600","X21800810")
  
  quirofano <- list( "idEstablecimiento",
    "X21220100", "X21220200", "X21220700", "X21220600", "X21220800", "X21400300", "X21220900",
    "X21300100", "X21400400", "X21400500", "X21400600", "X21400700", "X21300600", "X21300700",
    "X21800810", "X21800820", "X21400800", "X21400900", "X21500100", "X21500200", "X21800830",
    "X21800840", "X21500300", "X21800850", "X21800860", "X21800870", "X21800880", "X21500600",
    "X21600600", "X21600700", "X21600800", "X21600900", "X21700100", "X21500700", "X21500800",
    "X21700300", "X21700400", "X21700500", "X21700600", "X21500900", "X21700700", "X21600100",
    "X21700800", "X21700900", "X21800100", "X21800200", "X21800300", "X21600200", "X21600300",
    "X21800500", "X21800600", "X21800700", "X21800800", "X21600400", "X21600500", "X21700701",
    "X21700702", "X21700703", "X21700704", "X21700705", "X21700706", "X21700707", "X21700708",
    "X21800890", "X21900100", "X21900110", "X21900120", "X21900130", "X21900140", "X21900150",
    "X21900160", "X21900170", "X21900180", "X21900190", "X21900200", "X21900210", "X21900220",
    "X21100010", "X21100020", "X21100030", "X21100040", "X21100050", "X21100060", "X21100070",
    "X21100080", "X21100090", "X29101381", "X29101382", "X29101383"
  )
  
  columnas_validas_q <- intersect(unlist(quirofano), colnames(datos_consolidados))
  #print(quirofano)
  quirofano_data <- subset(datos_consolidados, select = unlist(columnas_validas_q))
  
  #quirofano_data <- subset(datos_consolidados, select = unlist(quirofano))
  
  # Reemplazar comas por puntos y convertir a numérico
  quirofano_data <- quirofano_data %>%
    mutate(across(-idEstablecimiento, ~ as.integer(floor(as.numeric(gsub(",", ".", .))))))

  # Crear suma total de quirofano
  quirofano_data$sumaTotal <- rowSums(select(quirofano_data, -idEstablecimiento), na.rm = TRUE)
  
  quirofano <- data.frame(idEstablecimiento = quirofano_data$idEstablecimiento, 
                          Quirofano = quirofano_data$sumaTotal) %>%
    rename("IdEstablecimiento" = "idEstablecimiento") %>%
    inner_join(predicciones_grd, by = "IdEstablecimiento") %>%
    select(IdEstablecimiento, Quirofano)
  
  # Procesar egresos y predicciones GRD
  intermediate_df <- egresos %>%
    inner_join(predicciones_grd, by = "IdEstablecimiento") %>%
    mutate(Egresos.GRD = Prediction * egresos) %>%
    select("Region", IdEstablecimiento, "Nombre Establecimiento", Egresos.GRD)
  
  # Combinar datos financieros y días cama disponibles
  input <- left_join(financiero, dias_cama_disponibles %>% 
                       select(IdEstablecimiento, dias_cama_disponible), by = "IdEstablecimiento")
  
  # Combinar todas las salidas
  output <- intermediate_df %>%
    left_join(consultas, by = "IdEstablecimiento") %>%
    left_join(quirofano, by = "IdEstablecimiento")
  
  # Consolidar todos los datos
  all <- inner_join(output, input, by = "IdEstablecimiento") %>%
    left_join(hospitales %>% select(IdEstablecimiento, region_id, latitud, longitud), by = "IdEstablecimiento") %>%
    relocate(region_id, .after = Region)
  
  all_sin_duplicados <- distinct(all)
  return(all_sin_duplicados)
}

# ===================================================
#  APLICACIÓN DE ANÁLISIS ENVOLVENTE DE DATOS
#  PARA UN AÑO EN ESPECÍFICO
# ===================================================

analisis_dea_general <- function(data, orientation) {
  # --- Preparar inputs y outputs
  model <- make_deadata(data, ni=3, no="IdEstablecimiento", dmus=3, inputs=8:10, outputs=5:7)
  #browser()
  # ---Aplicar DEA con la orientación y RTS (VRS y CRS)
  resultado_dea_vrs <- model_basic(model, orientation=orientation, rts="vrs", dmu_eval = 1:nrow(data), dmu_ref = 1:nrow(data)) 
  resultado_dea_crs <- model_basic(model, orientation=orientation, rts="crs", dmu_eval = 1:nrow(data), dmu_ref = 1:nrow(data)) 
  
  #--- Calcular eficiencias
  eficiencia_vrs <- deaR::efficiencies(resultado_dea_vrs)
  eficiencia_crs <- deaR::efficiencies(resultado_dea_crs)
  
  if (orientation == "oo"){
    eficiencia_vrs <- normalize_max_min(eficiencia_vrs)
    eficiencia_crs <- normalize_max_min(eficiencia_crs)
  }
  
  
  # Crear dataframe con eficiencias y retorno a escala
  eficiencia_df <- data.frame(
    IdEstablecimiento = data$IdEstablecimiento,
    Nombre = data$'Nombre Establecimiento',
    Region = data$'Region',
    vrs = round(eficiencia_vrs, 3),
    crs = round(eficiencia_crs, 3),
    escala = round(eficiencia_vrs / eficiencia_crs, 3),
    latitud = data$latitud,
    longitud = data$longitud,
    region_id = data$region_id
  )
  

  
  return(list(data = eficiencia_df, 
              dea_vrs = resultado_dea_vrs,
              dea_crs = resultado_dea_crs))
}

# ===================================================
#  ELIMINACIÓN DE DATOS SEGÚN VALOR ENTREGADO Y SEGUN 
#  SU TIPO DE DEA (VRS O CRS)
# ===================================================

sensibilidad_parametro_general <- function(data, data_original, mayor, valor, orientacion, tipo) {
  # Determinar la columna a trabajar (vrs o crs)
  columna <- ifelse(tipo == "vrs", "vrs", ifelse(tipo == "crs", "crs", "esc"))
  
  # Filtrar los datos en función del parámetro `mayor` y el valor dado
  if (mayor) {
    data_filtrada <- subset(data_original, data_original[[columna]] > valor)
  } else {
    data_filtrada <- subset(data_original, data_original[[columna]] < valor)
  }
  
  # Filtrar el dataset por IdEstablecimiento
  data_set <- data[data$IdEstablecimiento %in% data_filtrada$IdEstablecimiento, ]

  # Aplicar el análisis DEA
  resultados_in  <- analisis_dea_general(data_set, orientacion)
  
  return (resultados = resultados_in)
}

# ==============================================
#  CÁLCULO DE INDICE MALMQUIST
# ==============================================
calcular_malmquist <- function(datos, tipo, orientacion) {
  # Extraer inputs, outputs, ID y TIME
  input_data <- do.call(rbind, lapply(names(datos), function(year) as.matrix(datos[[year]][, 8:10])))
  output_data <- do.call(rbind, lapply(names(datos), function(year) as.matrix(datos[[year]][, 5:7])))
  ID <- unlist(lapply(datos, function(data) data$IdEstablecimiento))
  TIME <- unlist(lapply(names(datos), function(year) rep(year, nrow(datos[[year]]))))
  
  # Eliminar duplicados
  unique_indices <- !duplicated(data.frame(ID = ID, TIME = TIME))
  input_data <- input_data[unique_indices, ]
  output_data <- output_data[unique_indices, ]
  ID <- ID[unique_indices]
  TIME <- TIME[unique_indices]
  
  # Calcular el índice Malmquist
  malmquist_index <- Benchmarking::malmquist(X = input_data, Y = output_data, ID = ID, TIME = TIME, 
                                             RTS = tipo, ORIENTATION = orientacion)
  
  # Crear dataframe de resultados
  resultados_df <- data.frame(
    ID = malmquist_index$id,
    Año = malmquist_index$time,
    MalmquistIndex = malmquist_index$m,
    Effch = malmquist_index$ec,
    Techch = malmquist_index$tc
  )
  
  # Crear dataframes pivotados
  pivotar_resultados <- function(data, column) {
    data %>%
      select(ID, Año, !!sym(column)) %>%
      pivot_wider(names_from = Año, values_from = !!sym(column)) %>%
      Filter(function(x) !all(is.na(x)), .)
  }
  
  malmquist_df <- pivotar_resultados(resultados_df, "MalmquistIndex")
  effch_df <- pivotar_resultados(resultados_df, "Effch")
  techch_df <- pivotar_resultados(resultados_df, "Techch")
  
  # Crear eficiencia en formato wide
  efficiency_df <- data.frame(
    ID = malmquist_index$id,
    TIME = malmquist_index$time,
    Efficiency = malmquist_index$e11
  ) %>%
    arrange(ID, TIME) %>%
    pivot_wider(names_from = TIME, values_from = Efficiency)
  
  print(5)
  
  # Se elimina la columna que no tienen ningun valor
  malmquist_df <- Filter(function(x) !all(is.na(x)),malmquist_df)
  effch_df <- Filter(function(x) !all(is.na(x)),effch_df)
  techch_df <- Filter(function(x) !all(is.na(x)), techch_df)
  
  # Ordenar el dataframe por ID y TIME
  efficiency_df <- efficiency_df[order(efficiency_df$ID, efficiency_df$TIME), ]
  
  # Transformar el dataframe en formato ancho (wide) con 'ID' como fila y 'TIME' como columnas
  efficiency_wide <- pivot_wider(efficiency_df, names_from = TIME, values_from = Efficiency)
  
  malmquist_df <- procesar_index(malmquist_df)
  
  return(list(eficiencia = efficiency_wide, 
              index = malmquist_df,
              tech = techch_df,
              eff = effch_df))
}

# ==============================================
#  COMBINACIÓN DE RESULTADO DE ITERACIONES
# ==============================================
combinar_resultados_iteraciones <- function(resultados_in, resultados_in_2_vrs, resultados_in_3_vrs, resultados_in_2_crs, resultados_in_3_crs) {
  # Crear una lista de dataframes, uno por cada año, con valores de VRS y CRS
  lista_resultados_combinados <- lapply(unique(names(resultados_in)), function(anio) {
    # Seleccionar los datos de las iteraciones de VRS
    df_vrs_1 <- resultados_in[[anio]]$data %>%
      select(IdEstablecimiento, vrs) %>%
      rename(vrs_iteracion_1 = vrs)
    
    df_vrs_2 <- resultados_in_2_vrs[[anio]]$data %>%
      select(IdEstablecimiento, vrs) %>%
      rename(vrs_iteracion_2 = vrs)
    
    df_vrs_3 <- resultados_in_3_vrs[[anio]]$data %>%
      select(IdEstablecimiento, vrs) %>%
      rename(vrs_iteracion_3 = vrs)
    
    # Unir los dataframes de VRS por IdEstablecimiento
    df_vrs_combinado <- df_vrs_1 %>%
      full_join(df_vrs_2, by = "IdEstablecimiento") %>%
      full_join(df_vrs_3, by = "IdEstablecimiento") %>%
      mutate(
        vrs_iteracion_1 = ifelse(is.na(vrs_iteracion_1), "NO APLICA", vrs_iteracion_1),
        vrs_iteracion_2 = ifelse(is.na(vrs_iteracion_2), "NO APLICA", vrs_iteracion_2),
        vrs_iteracion_3 = ifelse(is.na(vrs_iteracion_3), "NO APLICA", vrs_iteracion_3)
      )
    
    # Seleccionar los datos de las iteraciones de CRS
    df_crs_1 <- resultados_in[[anio]]$data %>%
      select(IdEstablecimiento, crs) %>%
      rename(crs_iteracion_1 = crs)
    
    df_crs_2 <- resultados_in_2_crs[[anio]]$data %>%
      select(IdEstablecimiento, crs) %>%
      rename(crs_iteracion_2 = crs)
    
    df_crs_3 <- resultados_in_3_crs[[anio]]$data %>%
      select(IdEstablecimiento, crs) %>%
      rename(crs_iteracion_3 = crs)
    
    # Unir los dataframes de CRS por IdEstablecimiento
    df_crs_combinado <- df_crs_1 %>%
      full_join(df_crs_2, by = "IdEstablecimiento") %>%
      full_join(df_crs_3, by = "IdEstablecimiento") %>%
      mutate(
        crs_iteracion_1 = ifelse(is.na(crs_iteracion_1), "NO APLICA", crs_iteracion_1),
        crs_iteracion_2 = ifelse(is.na(crs_iteracion_2), "NO APLICA", crs_iteracion_2),
        crs_iteracion_3 = ifelse(is.na(crs_iteracion_3), "NO APLICA", crs_iteracion_3)
      )
    
    # Unir los resultados de VRS y CRS en un solo dataframe por IdEstablecimiento
    df_combinado <- df_vrs_combinado %>%
      full_join(df_crs_combinado, by = "IdEstablecimiento")
    
    return(df_combinado)
  })
  
  # Nombrar la lista con los años para identificación
  names(lista_resultados_combinados) <- unique(names(resultados_in))
  
  return(lista_resultados_combinados)
}

# ==============================================
#  GENERACIÓN DE RESULTADOS DE ITERACION
# ==============================================
resultados_iteracion <- function(datos, orientacion){
  anios <- c("2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022","2023")
  original <-  sapply(datos, function(data) analisis_dea_general(data, orientacion), simplify = FALSE)
  

  #aplicar_analisis_dea(datos, orientacion)
  if (orientacion == "io"){
      print("INPUT")
      iteracion_1_vrs <- aplicar_sensibilidad(datos, lapply(original, `[[`, "data"), 0.99, orientacion, "vrs", FALSE)
      iteracion_2_vrs <- aplicar_sensibilidad(datos, lapply(iteracion_1_vrs, `[[`, "data"), 0.99, orientacion, "vrs", FALSE)
      iteracion_1_crs <- aplicar_sensibilidad(datos, lapply(original, `[[`, "data"), 0.99, orientacion, "crs", FALSE)
      iteracion_2_crs <- aplicar_sensibilidad(datos, lapply(iteracion_1_crs, `[[`, "data"), 0.99, orientacion, "crs", FALSE)
    
  }else{
    print("OUTPUT")
    
    iteracion_1_vrs <- aplicar_sensibilidad(datos, lapply(original, `[[`, "data"), 1, orientacion, "vrs", FALSE)
    iteracion_2_vrs <- aplicar_sensibilidad(datos, lapply(iteracion_1_vrs, `[[`, "data"), 1, orientacion, "vrs", FALSE)
    iteracion_1_crs <- aplicar_sensibilidad(datos, lapply(original, `[[`, "data"), 1, orientacion, "crs", FALSE)
    iteracion_2_crs <- aplicar_sensibilidad(datos, lapply(iteracion_1_crs, `[[`, "data"), 1, orientacion, "crs", FALSE)

    #print(iteracion_1_vrs[[year]][["data"]]$vrs)
    
        
    #print(iteracion_1_vrs[[year]][["data"]]$vrs)
  }
  print("A")
  resultados_combinados <- combinar_resultados_iteraciones(original, iteracion_1_vrs, iteracion_2_vrs, iteracion_1_crs, iteracion_2_crs)
  resultados_correlacion <- calcular_correlaciones_all(resultados_combinados)
  
  print("C")
  # Crear una lista vacía para almacenar los valores atípicos por año
  lista_outliers_vrs <- list()
  print("D")
  # Crear un vector vacío para almacenar todos los valores atípicos sin duplicados
  vector_outliers_vrs <- c()
  print("E")
  lista_outliers_crs <- list()
  # Crear un vector vacío para almacenar todos los valores atípicos sin duplicados
  print("F")
  vector_outliers_crs <- c()
  
  print("G")
  # Especificar los años que quieres iterar
  

  for (anio in anios) {
    
    # Generar y almacenar los valores atípicos de VRS
    outliers_vrs <- boxplot.stats(original[[anio]][["data"]]$vrs)$out
    print("J")
    # Filtrar el dataframe para obtener los IDs de los valores atípicos
    ids_outliers_vrs <- original[[anio]][["data"]] %>%
      filter(vrs %in% outliers_vrs) %>%
      select(IdEstablecimiento, vrs)
    print("K")
    # Añadir los valores atípicos del año actual a la lista, con el nombre del año
    lista_outliers_vrs[[anio]] <- ids_outliers_vrs
    print("L")
    # Añadir los IDs al vector de valores atípicos, asegurando que no se repitan
    vector_outliers_vrs <- unique(c(vector_outliers_vrs, ids_outliers_vrs$IdEstablecimiento))
    print("M")
    # Generar y almacenar los valores atípicos de CRS
    outliers_crs <- boxplot.stats(original[[anio]][["data"]]$crs)$out
    print("N")
    # Filtrar el dataframe para obtener los IDs de los valores atípicos
    ids_outliers_crs <- original[[anio]][["data"]] %>%
      filter(crs %in% outliers_crs) %>%
      select(IdEstablecimiento, crs)
    print("O")
    # Añadir los valores atípicos del año actual a la lista, con el nombre del año
    lista_outliers_crs[[anio]] <- ids_outliers_crs
    print("P")
    # Añadir los IDs al vector de valores atípicos, asegurando que no se repitan
    vector_outliers_crs <- unique(c(vector_outliers_crs, ids_outliers_crs$IdEstablecimiento))
  }

  list(
    original =  original,
    iteracion_1_vrs = iteracion_1_vrs,
    iteracion_2_vrs = iteracion_2_vrs,
    iteracion_1_crs = iteracion_1_crs,
    iteracion_2_crs = iteracion_2_crs,
    resultados_combinados = resultados_combinados,
    resultados_correlacion = resultados_correlacion,
    lista_outliers_vrs = lista_outliers_vrs,
    vector_outliers_vrs = vector_outliers_vrs,
    lista_outliers_crs = lista_outliers_crs,
    vector_outliers_crs = vector_outliers_crs
  )
}

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
  
  # Retornar resultados de correlación entre matrices de distintos años
  return(list(correlaciones_lista = correlaciones_lista))
}

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
#  COMBINAR RESULTADOS IN OUT
# ==============================================
combinar_resultados_in_out <- function(resultados_in, resultados_out) {
  # Crear una lista de dataframes, uno por cada año, con valores de VRS y CRS
  lista_resultados_combinados <- lapply(unique(names(resultados_in)), function(anio) {
    # Seleccionar los datos de las iteraciones de VRS
    df_vrs_input <- resultados_in[[anio]]$data %>%
      select(IdEstablecimiento, vrs) %>%
      rename(vrs_input = vrs)
    
    df_vrs_output <- resultados_out[[anio]]$data %>%
      select(IdEstablecimiento, vrs) %>%
      rename(vrs_output = vrs)
    
    # Unir los dataframes de VRS por IdEstablecimiento
    df_vrs_combinado <- df_vrs_input %>%
      full_join(df_vrs_output, by = "IdEstablecimiento") %>%
      mutate(
        vrs_input = ifelse(is.na(vrs_input), "NO APLICA", vrs_input),
        vrs_output = ifelse(is.na(vrs_output), "NO APLICA", vrs_output),
      )
    
    # Seleccionar los datos de las iteraciones de CRS
    df_crs_input <- resultados_in[[anio]]$data %>%
      select(IdEstablecimiento, crs) %>%
      rename(crs_input = crs)
    
    df_crs_output <- resultados_out[[anio]]$data %>%
      select(IdEstablecimiento, crs) %>%
      rename(crs_output = crs)
    
    
    # Unir los dataframes de CRS por IdEstablecimiento
    df_crs_combinado <- df_crs_input %>%
      full_join(df_crs_output, by = "IdEstablecimiento") %>%
      mutate(
        crs_input = ifelse(is.na(crs_input), "NO APLICA", crs_input),
        crs_output = ifelse(is.na(crs_output), "NO APLICA", crs_output)
      )
    
    # Unir los resultados de VRS y CRS en un solo dataframe por IdEstablecimiento
    df_combinado <- df_vrs_combinado %>%
      full_join(df_crs_combinado, by = "IdEstablecimiento")
    
    return(df_combinado)
  })
  
  # Nombrar la lista con los años para identificación
  names(lista_resultados_combinados) <- unique(names(resultados_in))
  
  return(lista_resultados_combinados)
}

# ==============================================
#  RESUMEN DE EFICIENCIA
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
#  SENSIBILIDAD VS ATIPICOS
# ==============================================
# Función para procesar datos y calcular correlaciones
procesar_datos <- function(resultados_in, resultados_out, resultados_in_cut_vrs, resultados_in_cut_crs, 
                           resultados_out_cut_vrs, resultados_out_cut_crs) {
  
  # Crear función interna para generar dataframes base
  generar_dataframe_base <- function(resultados, key) {
    df <- data.frame(ID = resultados[["original"]][["2014"]][["data"]][["IdEstablecimiento"]])
    for (year in names(resultados[["original"]])) {
      df[[year]] <- resultados[["original"]][[year]][["data"]][[key]]
    }
    return(df)
  }
  
  # Crear dataframes para cada caso
  in_vrs_df <- generar_dataframe_base(resultados_in, "vrs")
  in_crs_df <- generar_dataframe_base(resultados_in, "crs")
  out_vrs_df <- generar_dataframe_base(resultados_out, "vrs")
  out_crs_df <- generar_dataframe_base(resultados_out, "crs")
  
  in_vrs_por_anio_cut <- generar_dataframe_base(resultados_in_cut_vrs, "vrs")
  in_crs_por_anio_cut <- generar_dataframe_base(resultados_in_cut_crs, "crs")
  out_vrs_por_anio_cut <- generar_dataframe_base(resultados_out_cut_vrs, "vrs")
  out_crs_por_anio_cut <- generar_dataframe_base(resultados_out_cut_crs, "crs")
  
  # Calcular las correlaciones
  correlaciones_in_vrs <- calcular_correlaciones(in_vrs_df, in_vrs_por_anio_cut)
  correlaciones_in_crs <- calcular_correlaciones(in_crs_df, in_crs_por_anio_cut)
  correlaciones_out_vrs <- calcular_correlaciones(out_vrs_df, out_vrs_por_anio_cut)
  correlaciones_out_crs <- calcular_correlaciones(out_crs_df, out_crs_por_anio_cut)
  
  # Retornar las correlaciones en una lista
  return(list(
    correlaciones_in_vrs = correlaciones_in_vrs,
    correlaciones_in_crs = correlaciones_in_crs,
    correlaciones_out_vrs = correlaciones_out_vrs,
    correlaciones_out_crs = correlaciones_out_crs
  ))
}




# ==============================================
#  RANDOM FOREST
# ==============================================
analize_rf <- function(year, resultados_in, n_top, trees, tipo, orientacion){

  #print(head(df_vrs))
  df_w_vrs <- df %>%
    filter(idEstablecimiento %in% df_vrs$idEstablecimiento)
  
  # Combinar los DataFrames
  df_merged <- merge(df_w_vrs, df_vrs, by = "idEstablecimiento", all.x = TRUE)
  
  # Eliminar columnas completamente NA
  df_merged <- df_merged[, colSums(is.na(df_merged)) < nrow(df_merged)]
  #print(colnames(df_merged))

  # Calcular correlaciones
  correlaciones <- cor(df_merged[,-1])["vrs", ]
  correlaciones <- correlaciones[!names(correlaciones) %in% "vrs"]
  correlaciones_ordenadas <- sort(abs(correlaciones), decreasing = TRUE)
  
  
  # Seleccionar las top_n variables más correlacionadas
  
  top_vars <- head(correlaciones_ordenadas, top_n)
  
  return(correlaciones_ordenadas)
  
}




analize_rf <- function(year, resultados_in, n_top,tipo ){
  #year <- 2014
  print("---------------------------")
  print("---------------------------")
  print(paste0("AÑO: ", year, "- RETORNO: ", tipo))
  
  data_path <- paste0("data/", year, "/", year, "_consolidated_data.csv")
  
  #print(data_path)
  # Leer los datos consolidados
  datos_consolidados <- read.table(data_path, sep = ";", header = TRUE)
  df <- datos_consolidados
  
  # Convertir columnas a enteros
  df[colnames(datos_consolidados)] <- lapply(df[colnames(datos_consolidados)], as.integer)
  
  # Filtrar los resultados de VRS
  df_vrs <- resultados_in[["original"]][[as.character(year)]][["data"]][, c("IdEstablecimiento", tipo)] %>% 
    rename("idEstablecimiento" = "IdEstablecimiento")


  df_w_vrs <- df %>%
    filter(idEstablecimiento %in% df_vrs$idEstablecimiento)
  
  # Combinar los DataFrames
  df_merged_original <- merge(df_w_vrs, df_vrs, by = "idEstablecimiento", all.x = TRUE)
  df_merged_clean <- df_merged_original[, colSums(is.na(df_merged_original)) == 0]
  
  
  correlaciones <- cor(df_merged_clean[,-1])[tipo, ]
  correlaciones <- correlaciones[!names(correlaciones) %in% tipo]
  correlaciones_ordenadas <- sort(abs(correlaciones), decreasing = TRUE)
  
  top_correlacion <- head(correlaciones_ordenadas, n=n_top)
  top_variables <- names(top_correlacion)
  
  
  columnas_a_incluir <- c(tipo, top_variables)
  
  # Crear el DataFrame con las variables seleccionadas
  df_top <- df_merged_clean[, columnas_a_incluir]
  
  
  # PROBANDO RANDOM FOREST
  
  set.seed(123)  # Para reproducibilidad
  library(caret)
  trainIndex <- createDataPartition(df_top[[tipo]], p = 0.7, list = FALSE)
  
  trainData <- df_top[trainIndex, ]
  testData <- df_top[-trainIndex, ]
  
  control <- trainControl(method = "cv", number = 10)  # 10-fold CV
  
  library(randomForest)
  formula <- as.formula(paste(tipo, "~ ."))
  
  cat(paste0("\n", year, "-", orientacion, " ", tipo))
  # Ajustar el modelo de Random Forest
  modelo_rf <- randomForest(formula, 
                            data = trainData, 
                            importance = TRUE, 
                            trControl = control, 
                            ntree = 700,
                            do.trace = 100 )
  
  cat("\n")
  # Ver el modelo ajustado
  #print(modelo_rf)
  
  
  #plot(modelo_rf$err.rate[, 1], 
  #     type = "l",
  #     xlab = "Número de árboles",
  #     ylab = "Error OOB")
  
  
  # Predicciones en el conjunto de prueba
  predicciones <- predict(modelo_rf, newdata = testData)
  
  # Evaluar el rendimiento
  library(Metrics)
  r2 <- R2(predicciones, testData[[tipo]])
  rmse <- rmse(predicciones, testData[[tipo]])
  cat("R²:", r2, "\nRMSE:", rmse)
  
  # Importancia de las variables
  importancia <- importance(modelo_rf)
  
  importancia_IncNodePurity <- importancia[order(-importancia[, "IncNodePurity"]), ] # Ordenar en orden descendente
  top_IncNodePurity <- head(importancia_IncNodePurity[, "IncNodePurity"], 50)
  
  importancia_IncMSE <- importancia[order(-importancia[, "%IncMSE"]), ] # Ordenar en orden descendente
  top_IncMSE <- head(importancia_IncMSE[, "%IncMSE"], 50)
  #print(importancia)
  
  # Graficar la importancia
  #varImpPlot(modelo_rf)
  
  print("---------------------------")
  print("---------------------------")
  
  return(list(importancia = importancia,
              top_IncMSE = top_IncMSE,
              top_IncNodePurity = top_IncNodePurity,
              modelo = modelo_rf))
  
  # O si prefieres ordenar por IncNodePurity 
  #importancia_ordenada <- importancia_df[order(importancia_df$IncNodePurity, decreasing = TRUE), ] 
  
  # Para mostrar los resultados con nombres de variables 
  importancia_ordenada$Variables <- rownames(importancia_ordenada) 
  
  # Graficar la importancia
  # varImpPlot(modelo_rf)
  # title(main = paste0("Importancia de las Variables - Modelo Random Forest - Año ", year, " ", tipo ))
  
  return(head(importancia_ordenada,50)) 
  
}



procesar_importancia <- function(random_forest, anios_pre_pandemia, anios_pandemia) {
  # Crear listas para almacenar resultados
  resultados_IncNodePurity <- list()
  resultados_IncMSE <- list()
  
  # Iterar sobre cada método
  for (metodo in names(random_forest)) {
    
    # Crear dataframes vacíos para almacenar resultados
    df_IncNodePurity <- data.frame(Variable = character(), stringsAsFactors = FALSE)
    df_IncMSE <- data.frame(Variable = character(), stringsAsFactors = FALSE)
    
    # Recopilar datos por año
    for (anio in names(random_forest[[metodo]])) {
      # Obtener importancia
      importancia <- random_forest[[metodo]][[anio]]$importancia
      
      # --- IncNodePurity ---
      temp_IncNodePurity <- data.frame(
        Variable = rownames(importancia),
        Valor = importancia[, "IncNodePurity"],  # Seleccionar columna
        Año = anio
      )
      df_IncNodePurity <- rbind(df_IncNodePurity, temp_IncNodePurity)
      
      # --- %IncMSE ---
      temp_IncMSE <- data.frame(
        Variable = rownames(importancia),
        Valor = importancia[, "%IncMSE"],  # Seleccionar columna
        Año = anio
      )
      df_IncMSE <- rbind(df_IncMSE, temp_IncMSE)
    }
    
    # --- Procesar IncNodePurity ---
    pivot_IncNodePurity <- reshape(df_IncNodePurity, 
                                   idvar = "Variable", 
                                   timevar = "Año", 
                                   direction = "wide")
    colnames(pivot_IncNodePurity) <- gsub("Valor\\.", "", colnames(pivot_IncNodePurity))
    
    # Calcular frecuencia y promedios
    pivot_IncNodePurity$Frecuencia <- table(df_IncNodePurity$Variable)[pivot_IncNodePurity$Variable]
    pivot_IncNodePurity$Promedio_Pre_Pandemia <- rowMeans(pivot_IncNodePurity[, anios_pre_pandemia], na.rm = TRUE)
    pivot_IncNodePurity$Promedio_Pandemia <- rowMeans(pivot_IncNodePurity[, anios_pandemia], na.rm = TRUE)
    
    # Calcular frecuencias separadas
    pivot_IncNodePurity$Frecuencia_Pre_Pandemia <- rowSums(!is.na(pivot_IncNodePurity[, anios_pre_pandemia]))
    pivot_IncNodePurity$Frecuencia_Pandemia <- rowSums(!is.na(pivot_IncNodePurity[, anios_pandemia]))
    
    # Ordenar y seleccionar las 50 más importantes
    pivot_IncNodePurity <- pivot_IncNodePurity[order(-pivot_IncNodePurity$Promedio_Pre_Pandemia), ]
    resultados_IncNodePurity[[metodo]] <- head(pivot_IncNodePurity, 50)
    
    # --- Procesar %IncMSE ---
    pivot_IncMSE <- reshape(df_IncMSE, 
                            idvar = "Variable", 
                            timevar = "Año", 
                            direction = "wide")
    colnames(pivot_IncMSE) <- gsub("Valor\\.", "", colnames(pivot_IncMSE))
    
    # Calcular frecuencia y promedios
    pivot_IncMSE$Frecuencia <- table(df_IncMSE$Variable)[pivot_IncMSE$Variable]
    pivot_IncMSE$Promedio_Pre_Pandemia <- rowMeans(pivot_IncMSE[, anios_pre_pandemia], na.rm = TRUE)
    pivot_IncMSE$Promedio_Pandemia <- rowMeans(pivot_IncMSE[, anios_pandemia], na.rm = TRUE)
    
    # Calcular frecuencias separadas
    pivot_IncMSE$Frecuencia_Pre_Pandemia <- rowSums(!is.na(pivot_IncMSE[, anios_pre_pandemia]))
    pivot_IncMSE$Frecuencia_Pandemia <- rowSums(!is.na(pivot_IncMSE[, anios_pandemia]))
    
    # Ordenar y seleccionar las 50 más importantes
    pivot_IncMSE <- pivot_IncMSE[order(-pivot_IncMSE$Promedio_Pre_Pandemia), ]
    resultados_IncMSE[[metodo]] <- head(pivot_IncMSE, 50)
  }
  
  # Retornar las dos listas
  return(list(IncNodePurity = resultados_IncNodePurity, IncMSE = resultados_IncMSE))
}


reemplazar_nulos <- function(df) {
  df[is.na(df) | df == ""] <- "-" # Reemplaza NA o valores vacíos
  return(df)
}
