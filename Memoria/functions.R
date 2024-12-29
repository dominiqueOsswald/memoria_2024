library(Benchmarking)
library(gridExtra)
library(corrplot)
library(censReg)
library(readxl)
library(purrr)
library(tidyr)
library(dplyr)
library(deaR)



### GUARDAR DATAFRAMES ###
guardar_dataframes <- function(dataframes, hoja_nombre, columna) {
  # Extraer el valor especificado (vrs o crs) de cada dataframe por año
  resultados <- lapply(names(dataframes), function(anio) {
    df <- dataframes[[anio]]
    # Seleccionar columna especificada junto con IdEstablecimiento
    df_seleccionado <- reemplazar_nulos(df[, c("IdEstablecimiento", columna)])
    # Renombrar la columna con el año correspondiente
    colnames(df_seleccionado) <- c("IdEstablecimiento", anio)
    return(df_seleccionado)
  })
  
  # Unir los resultados por columna (IdEstablecimiento como fila)
  df_final <- Reduce(function(x, y) merge(x, y, by = "IdEstablecimiento", all = TRUE), resultados)
  
  # Añadir la hoja
  addWorksheet(wb, hoja_nombre)
  writeData(wb, hoja_nombre, df_final)
}


# ******************************* #
# Eliminación de datos atípicos y recalculo DEA
calcular_corte <- function(datos, vector_outliers) {
  lapply(datos, function(df) df %>% filter(!(IdEstablecimiento %in% vector_outliers)))
}

resultados_corte <- function(resultados, tipo) {
  resultados_iteracion(calcular_corte(datos, resultados[[paste0("vector_outliers_", tipo, "_vrs")]]), tipo)
}

malmquist <- function(tipo, orientacion) {
  calcular_malmquist(datos, tipo, orientacion)
}


# ******************************* #


aplicar_sensibilidad <- function(datos, resultados, umbral, orientacion, retorno, mayor) {
  mapply(function(data, resultado, anio) {
    # Mostrar el nombre del año en pantalla
    print(paste("Aplicando sensibilidad para el año:", anio))
    
    # Ejecutar la función principal
    sensibilidad_parametro_general(data, resultado, mayor, umbral, orientacion, retorno)
  },
  datos, resultados, names(datos), SIMPLIFY = FALSE) # Pasar los nombres de los datos como argumento
}

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

# -------------------------------------------- #
#  CONSOLIDACIÓN DE DATOS
# -------------------------------------------- #
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

# -------------------------------------------- #
#  APLICACIÓN DE ANÁLISIS ENVOLVENTE DE DATOS
#  PARA UN AÑO EN ESPECÍFICO
# -------------------------------------------- #
analisis_dea_general <- function(data, orientation) {
  # --- Preparar inputs y outputs
  model <- make_deadata(data, ni=3, no="IdEstablecimiento", dmus=3, inputs=8:10, outputs=5:7)
  
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
  
  # Ordenar dataframes según diferentes columnas
  #eficiencia_vrs_data <- eficiencia_df[order(-eficiencia_df$vrs), ]
  #eficiencia_crs_data <- eficiencia_df[order(-eficiencia_df$crs), ]
  #eficiencia_escala_data <- eficiencia_df[order(eficiencia_df$escala), ]
  
  # Retornar los dataframes ordenados como una lista
  
  return(list(data = eficiencia_df, 
              dea_vrs = resultado_dea_vrs,
              dea_crs = resultado_dea_crs))
  
  #return(list(data = eficiencia_df)) 
  #            vrs = eficiencia_vrs_data, 
  #            crs = eficiencia_crs_data, 
  #            esc = eficiencia_escala_data))
}

# -------------------------------------------- #
#  ELIMINACIÓN DE DATOS SEGÚN VALOR ENTREGADO Y SEGUN 
#  SU TIPO DE DEA (VRS O CRS)
# -------------------------------------------- #
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
  # head(data_set)
  # Aplicar el análisis DEA
  resultados_in  <- analisis_dea_general(data_set, orientacion)
  
  return (resultados = resultados_in)
}

# -------------------------------------------- #
#  CÁLCULO DE INDICE MALMQUIST
# -------------------------------------------- #
calcular_malmquist <- function(datos, tipo, orientacion) {
  
  # Inicializar listas y vectores para almacenar inputs, outputs, ID y TIME
  input_data <- list()
  output_data <- list()
  ID <- vector()
  TIME <- vector()
  print(1)
  # Iterar sobre cada año
  for (year in names(datos)) {
    # Acceder a la lista 'data' dentro de cada año
    data_year <- datos[[year]]
    
    # Extraer los inputs y outputs usando posiciones de columnas
    inputs <- as.matrix(data_year[, 8:10])  # Columnas 8 a 10 como inputs
    outputs <- as.matrix(data_year[, 5:7])  # Columnas 5 a 7 como outputs
    
    # Agregar los datos de este año a las listas
    input_data[[year]] <- inputs
    output_data[[year]] <- outputs
    
    # Crear vectores de ID y tiempo
    ID <- c(ID, data_year$IdEstablecimiento)  # Suponiendo que IdEstablecimiento es el ID de cada DMU
    TIME <- c(TIME, rep(year, nrow(data_year)))
  }
  print(2)
  # Verificar si hay combinaciones duplicadas de ID y TIME
  temp_df <- data.frame(ID = ID, TIME = TIME)
  duplicated_rows <- temp_df[duplicated(temp_df), ]
  
  if (nrow(duplicated_rows) > 0) {
    # Eliminar duplicados en ID y TIME
    temp_df <- temp_df[!duplicated(temp_df), ]
    input_data <- input_data[!duplicated(temp_df), ]
    output_data <- output_data[!duplicated(temp_df), ]
  } 
  
  print(3)
  
  # Convertir listas a matrices para el análisis sin duplicados
  input_matrix <- do.call(rbind, input_data)
  output_matrix <- do.call(rbind, output_data)
  
  print("--------------")
  print(head(ID))
  print("--------------")
  print(head(TIME))
  print("--------------")
  # Realizar el análisis Malmquist
  malmquist_index <- Benchmarking::malmquist(X = input_matrix, Y = output_matrix, ID = ID, TIME = TIME, 
                               RTS = tipo, ORIENTATION = orientacion)
  
  
  # Se generan dataframe que entreguen la info de indice, cambio de eficiencia y tecnologico
  resultados_df <- data.frame(
    ID = malmquist_index$id,
    Año = malmquist_index$time,             # Periodo o año
    MalmquistIndex = malmquist_index$m,     # Índice Malmquist total
    Effch = malmquist_index$ec,          # Cambio en eficiencia
    Techch = malmquist_index$tc        # Cambio tecnológico
  )
  print(4)
  # Se construye cada uno de los dataframe
  malmquist_df <- resultados_df %>%
    select(ID, Año, MalmquistIndex) %>%
    pivot_wider(names_from = Año, values_from = MalmquistIndex)
  
  effch_df <- resultados_df %>%
    select(ID, Año, Effch) %>%
    pivot_wider(names_from = Año, values_from = Effch)
  
  # Tercer dataframe solo con Techch
  techch_df <- resultados_df %>%
    select(ID, Año, Techch) %>%
    pivot_wider(names_from = Año, values_from = Techch)
  
  # Crear el dataframe de resultados
  efficiency_df <- data.frame(
    ID = malmquist_index$id,
    TIME = malmquist_index$time,
    Efficiency = malmquist_index$e11
  )
  
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

# -------------------------------------------- #
#  
# -------------------------------------------- #
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

# -------------------------------------------- #
#  
# -------------------------------------------- #
resultados_iteracion <- function(datos, orientacion){
  anios <- c("2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022","2023")
  original <-  sapply(datos, function(data) analisis_dea_general(data, orientacion), simplify = FALSE)
  #aplicar_analisis_dea(datos, orientacion)
  if (orientacion == "io"){
      iteracion_1_vrs <- aplicar_sensibilidad(datos, lapply(original, `[[`, "data"), 0.99, orientacion, "vrs", FALSE)
      iteracion_2_vrs <- aplicar_sensibilidad(datos, lapply(iteracion_1_vrs, `[[`, "data"), 0.99, orientacion, "vrs", FALSE)
      iteracion_1_crs <- aplicar_sensibilidad(datos, lapply(original, `[[`, "data"), 0.99, orientacion, "crs", FALSE)
      iteracion_2_crs <- aplicar_sensibilidad(datos, lapply(iteracion_1_crs, `[[`, "data"), 0.99, orientacion, "crs", FALSE)
      
    
  }else{
    
    iteracion_1_vrs <- aplicar_sensibilidad(datos, lapply(original, `[[`, "data"), 1, orientacion, "vrs", FALSE)
    iteracion_2_vrs <- aplicar_sensibilidad(datos, lapply(iteracion_1_vrs, `[[`, "data"), 1, orientacion, "vrs", FALSE)
    iteracion_1_crs <- aplicar_sensibilidad(datos, lapply(original, `[[`, "data"), 1, orientacion, "crs", FALSE)
    iteracion_2_crs <- aplicar_sensibilidad(datos, lapply(iteracion_1_crs, `[[`, "data"), 1, orientacion, "crs", FALSE)

    #print(iteracion_1_vrs[[year]][["data"]]$vrs)
    
        
    #print(iteracion_1_vrs[[year]][["data"]]$vrs)
  }
  resultados_combinados <- combinar_resultados_iteraciones(original, iteracion_1_vrs, iteracion_2_vrs, iteracion_1_crs, iteracion_2_crs)
  resultados_correlacion <- calcular_correlaciones_all(resultados_combinados)
  
  
  # Crear una lista vacía para almacenar los valores atípicos por año
  lista_outliers_vrs <- list()
  # Crear un vector vacío para almacenar todos los valores atípicos sin duplicados
  vector_outliers_vrs <- c()
  
  lista_outliers_crs <- list()
  # Crear un vector vacío para almacenar todos los valores atípicos sin duplicados
  vector_outliers_crs <- c()
  
  
  # Especificar los años que quieres iterar
  

  for (anio in anios) {
    
    # Generar y almacenar los valores atípicos de VRS
    outliers_vrs <- boxplot.stats(original[[anio]][["data"]]$vrs)$out
    
    # Filtrar el dataframe para obtener los IDs de los valores atípicos
    ids_outliers_vrs <- original[[anio]][["data"]] %>%
      filter(vrs %in% outliers_vrs) %>%
      select(IdEstablecimiento, vrs)
    
    # Añadir los valores atípicos del año actual a la lista, con el nombre del año
    lista_outliers_vrs[[anio]] <- ids_outliers_vrs
    
    # Añadir los IDs al vector de valores atípicos, asegurando que no se repitan
    vector_outliers_vrs <- unique(c(vector_outliers_vrs, ids_outliers_vrs$IdEstablecimiento))
    
    # Generar y almacenar los valores atípicos de CRS
    outliers_crs <- boxplot.stats(original[[anio]][["data"]]$crs)$out
    
    # Filtrar el dataframe para obtener los IDs de los valores atípicos
    ids_outliers_crs <- original[[anio]][["data"]] %>%
      filter(crs %in% outliers_crs) %>%
      select(IdEstablecimiento, crs)
    
    # Añadir los valores atípicos del año actual a la lista, con el nombre del año
    lista_outliers_crs[[anio]] <- ids_outliers_crs
    
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

# -------------------------------------------- #
#  función para calcular la correlación entre VRS y CRS para cada año
# -------------------------------------------- #
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

# -------------------------------------------- #
#  
# -------------------------------------------- #
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

# -------------------------------------------- #
#  
# -------------------------------------------- #
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


# -------------------------------------------- #
#  sensibilidad vs atipicos
# -------------------------------------------- #
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



analyze_tobit_model <- function(resultados_in, year, top_n = 50) {
  data_path <- paste0("data/", year, "/", year, "_consolidated_data.csv")
  print(data_path)
  # Leer los datos consolidados
  datos_consolidados <- read.table(data_path, sep = ";", header = TRUE)
  df <- datos_consolidados
  
  left_cens <- 0
  right_cens <- 1

  # Convertir columnas a enteros
  df[colnames(datos_consolidados)] <- lapply(df[colnames(datos_consolidados)], as.integer)
  
  # Filtrar los resultados de VRS
  df_vrs <- resultados_in[["original"]][[as.character(year)]][["data"]][, c("IdEstablecimiento", "vrs")] %>% 
    rename("idEstablecimiento" = "IdEstablecimiento")

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
  
}






