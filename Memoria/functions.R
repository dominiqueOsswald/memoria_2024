setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("auxiliar.R")

library(randomForest)
library(Benchmarking)
library(matrixStats)
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



# ===================================================
# CONSOLIDACIÓN DE DATOS
# ===================================================
consolidar_datos_por_anio <- function(anio) {
  
  #browser()
  # Establecer directorio de trabajo
  #setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  
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
    escala = round(eficiencia_crs / eficiencia_vrs, 3),
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
  columna <- ifelse(tipo == "vrs", "vrs", ifelse(tipo == "crs", "crs", "escala"))
  
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
malmquist <- function(datos, tipo, orientacion) {
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
  
  malmquist_df <- malmquist_index(malmquist_df)
  
  return(list(eficiencia = efficiency_df, 
              index = malmquist_df,
              tech = techch_df,
              eff = effch_df))
}

# ==============================================
#  COMBINACIÓN DE RESULTADO DE ITERACIONES
# ==============================================
combinar_resultados_iteraciones <- function(resultados_in, resultados_in_2_vrs, resultados_in_3_vrs, resultados_in_2_crs, resultados_in_3_crs,resultados_in_2_esc, resultados_in_3_esc) {
  browser()
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
    
    
    df_esc_1 <- resultados_in[[anio]]$data %>%
      select(IdEstablecimiento, escala) %>%
      rename(esc_iteracion_1 = escala)
    
    df_esc_2 <- resultados_in_2_esc[[anio]]$data %>%
      select(IdEstablecimiento, escala) %>%
      rename(esc_iteracion_2 = escala)
    
    df_esc_3 <- resultados_in_3_esc[[anio]]$data %>%
      select(IdEstablecimiento, escala) %>%
      rename(esc_iteracion_3 = escala)
    
    # Unir los dataframes de CRS por IdEstablecimiento
    df_esc_combinado <- df_esc_1 %>%
      full_join(df_esc_2, by = "IdEstablecimiento") %>%
      full_join(df_esc_3, by = "IdEstablecimiento") %>%
      mutate(
        esc_iteracion_1 = ifelse(is.na(esc_iteracion_1), "NO APLICA", esc_iteracion_1),
        esc_iteracion_2 = ifelse(is.na(esc_iteracion_2), "NO APLICA", esc_iteracion_2),
        esc_iteracion_3 = ifelse(is.na(esc_iteracion_3), "NO APLICA", esc_iteracion_3)
      )
    
    
    
    
    # Unir los resultados de VRS y CRS en un solo dataframe por IdEstablecimiento
    df_combinado <- df_vrs_combinado %>%
      full_join(df_crs_combinado, by = "IdEstablecimiento") %>%
      full_join(df_esc_combinado, by = "IdEstablecimiento")
    
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
    print("UNO")
    iteracion_1_vrs <- aplicar_sensibilidad(datos, lapply(original, `[[`, "data"), 0.99, orientacion, "vrs", FALSE)
    print("DOS")
    iteracion_2_vrs <- aplicar_sensibilidad(datos, lapply(iteracion_1_vrs, `[[`, "data"), 0.99, orientacion, "vrs", FALSE)
    print("TRES")
    iteracion_1_crs <- aplicar_sensibilidad(datos, lapply(original, `[[`, "data"), 0.99, orientacion, "crs", FALSE)
    print("CUATRO")
    iteracion_2_crs <- aplicar_sensibilidad(datos, lapply(iteracion_1_crs, `[[`, "data"), 0.99, orientacion, "crs", FALSE)
    print("CINCO")
    iteracion_1_esc <- aplicar_sensibilidad(datos, lapply(original, `[[`, "data"), 0.99, orientacion, "esc", FALSE)
    print("SEIS")
    iteracion_2_esc <- aplicar_sensibilidad(datos, lapply(iteracion_1_esc, `[[`, "data"), 0.99, orientacion, "esc", FALSE)
    
    
  }else{
    browser()
    print("UNO")
    iteracion_1_vrs <- aplicar_sensibilidad(datos, lapply(original, `[[`, "data"), 1, orientacion, "vrs", FALSE)
    print("DOS")
    iteracion_2_vrs <- aplicar_sensibilidad(datos, lapply(iteracion_1_vrs, `[[`, "data"), 1, orientacion, "vrs", FALSE)
    print("TRES")
    iteracion_1_crs <- aplicar_sensibilidad(datos, lapply(original, `[[`, "data"), 1, orientacion, "crs", FALSE)
    print("CUATRO")
    iteracion_2_crs <- aplicar_sensibilidad(datos, lapply(iteracion_1_crs, `[[`, "data"), 1, orientacion, "crs", FALSE)
    print("CINCO")
    iteracion_1_esc <- aplicar_sensibilidad(datos, lapply(original, `[[`, "data"), 1, orientacion, "esc", FALSE)
    print("SEIS")
    iteracion_2_esc <- aplicar_sensibilidad(datos, lapply(iteracion_1_esc, `[[`, "data"), 1, orientacion, "esc", FALSE)
  }
  
  
  
  return(list(
    original =  original,
    iteracion_1_vrs = iteracion_1_vrs,
    iteracion_2_vrs = iteracion_2_vrs,
    iteracion_1_crs = iteracion_1_crs,
    iteracion_2_crs = iteracion_2_crs,
    
    iteracion_1_esc = iteracion_1_esc,
    iteracion_2_esc = iteracion_2_esc
    
  ))
  
}





old_resultados_iteracion <- function(datos, orientacion){
  anios <- c("2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022","2023")
  original <-  sapply(datos, function(data) analisis_dea_general(data, orientacion), simplify = FALSE)
  

  #aplicar_analisis_dea(datos, orientacion)
  if (orientacion == "io"){
      print("UNO")
      iteracion_1_vrs <- aplicar_sensibilidad(datos, lapply(original, `[[`, "data"), 0.99, orientacion, "vrs", FALSE)
      print("DOS")
      iteracion_2_vrs <- aplicar_sensibilidad(datos, lapply(iteracion_1_vrs, `[[`, "data"), 0.99, orientacion, "vrs", FALSE)
      print("TRES")
      iteracion_1_crs <- aplicar_sensibilidad(datos, lapply(original, `[[`, "data"), 0.99, orientacion, "crs", FALSE)
      print("CUATRO")
      iteracion_2_crs <- aplicar_sensibilidad(datos, lapply(iteracion_1_crs, `[[`, "data"), 0.99, orientacion, "crs", FALSE)
      print("CINCO")
      iteracion_1_esc <- aplicar_sensibilidad(datos, lapply(original, `[[`, "data"), 0.99, orientacion, "esc", FALSE)
      print("SEIS")
      iteracion_2_esc <- aplicar_sensibilidad(datos, lapply(iteracion_1_esc, `[[`, "data"), 0.99, orientacion, "esc", FALSE)
    
      
  }else{
    browser()
    print("UNO")
    iteracion_1_vrs <- aplicar_sensibilidad(datos, lapply(original, `[[`, "data"), 1, orientacion, "vrs", FALSE)
    print("DOS")
    iteracion_2_vrs <- aplicar_sensibilidad(datos, lapply(iteracion_1_vrs, `[[`, "data"), 1, orientacion, "vrs", FALSE)
    print("TRES")
    iteracion_1_crs <- aplicar_sensibilidad(datos, lapply(original, `[[`, "data"), 1, orientacion, "crs", FALSE)
    print("CUATRO")
    iteracion_2_crs <- aplicar_sensibilidad(datos, lapply(iteracion_1_crs, `[[`, "data"), 1, orientacion, "crs", FALSE)
    print("CINCO")
    iteracion_1_esc <- aplicar_sensibilidad(datos, lapply(original, `[[`, "data"), 1, orientacion, "esc", FALSE)
    print("SEIS")
    iteracion_2_esc <- aplicar_sensibilidad(datos, lapply(iteracion_1_esc, `[[`, "data"), 1, orientacion, "esc", FALSE)
  }
  
  print("2")
  resultados_combinados <- combinar_resultados_iteraciones(original, iteracion_1_vrs, iteracion_2_vrs, iteracion_1_crs, iteracion_2_crs, iteracion_1_esc, iteracion_2_esc)
  #resultados_combinados_out <- resultados_combinaciones
  save(resultados_combinados,file="RESULTADOS_COMBINADOS.RData")
  
  print("22")
  resultados_correlacion <- calcular_correlaciones_all(resultados_combinados)
  
  print("222")
  lista_outliers_vrs <- list()
  print("2")
  vector_outliers_vrs <- c()
  print("22")
  lista_outliers_crs <- list()
  print("222")
  vector_outliers_crs <- c()
  print("2")
  lista_outliers_esc <- list()
  print("22")
  vector_outliers_esc <- c()
  
  print("222")
  # Especificar los años que quieres iterar
  for (anio in anios) {
    # ----------------- #
    print("3")
    # Generar y almacenar los valores atípicos de VRS
    outliers_vrs <- boxplot.stats(original[[anio]][["data"]]$vrs)$out
    print("4")
    ids_outliers_vrs <- original[[anio]][["data"]] %>%
      filter(vrs %in% outliers_vrs) %>%
      select(IdEstablecimiento, vrs)
    print("5")
    lista_outliers_vrs[[anio]] <- ids_outliers_vrs
    print("6")
    vector_outliers_vrs <- unique(c(vector_outliers_vrs, ids_outliers_vrs$IdEstablecimiento))
    print("7")
    # ----------------- #
    # Generar y almacenar los valores atípicos de CRS
    outliers_crs <- boxplot.stats(original[[anio]][["data"]]$crs)$out
    print("8")
    ids_outliers_crs <- original[[anio]][["data"]] %>%
      filter(crs %in% outliers_crs) %>%
      select(IdEstablecimiento, crs)
    print("9")
    lista_outliers_crs[[anio]] <- ids_outliers_crs
    print("10")
    vector_outliers_crs <- unique(c(vector_outliers_crs, ids_outliers_crs$IdEstablecimiento))
    print("11")
    # ----------------- #
    # Generar y almacenar los valores atípicos de escala
    outliers_esc <- boxplot.stats(original[[anio]][["data"]]$escala)$out
    print("12")
    ids_outliers_esc <- original[[anio]][["data"]] %>%
      filter(escala %in% outliers_esc) %>%
      select(IdEstablecimiento, escala)
    print("13")
    lista_outliers_esc[[anio]] <- ids_outliers_esc
    print("14")
    vector_outliers_esc <- unique(c(vector_outliers_esc, ids_outliers_esc$IdEstablecimiento))
  }

  list(
    original =  original,
    iteracion_1_vrs = iteracion_1_vrs,
    iteracion_2_vrs = iteracion_2_vrs,
    iteracion_1_crs = iteracion_1_crs,
    iteracion_2_crs = iteracion_2_crs,
    
    iteracion_1_esc = iteracion_1_esc,
    iteracion_2_esc = iteracion_2_esc,
    
    resultados_combinados = resultados_combinados,
    resultados_correlacion = resultados_correlacion,
    
    lista_outliers_vrs = lista_outliers_vrs,
    vector_outliers_vrs = vector_outliers_vrs,
    lista_outliers_crs = lista_outliers_crs,
    vector_outliers_crs = vector_outliers_crs,
    lista_outliers_esc = lista_outliers_esc,
    vector_outliers_esc = vector_outliers_esc
  )
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
        vrs_input = ifelse(is.na(vrs_input), NA, as.numeric(vrs_input)),
        vrs_output = ifelse(is.na(vrs_output), NA, as.numeric(vrs_output))
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
        crs_input = ifelse(is.na(crs_input), NA, as.numeric(crs_input)),
        crs_output = ifelse(is.na(crs_output), NA, as.numeric(crs_output))
      )
    
    # Seleccionar los datos de las iteraciones de CRS
    df_esc_input <- resultados_in[[anio]]$data %>%
      select(IdEstablecimiento, escala) %>%
      rename(esc_input = escala)
    
    df_esc_output <- resultados_out[[anio]]$data %>%
      select(IdEstablecimiento, escala) %>%
      rename(esc_output = escala)
    
    # Unir los dataframes de CRS por IdEstablecimiento
    df_esc_combinado <- df_esc_input %>%
      full_join(df_esc_output, by = "IdEstablecimiento") %>%
      mutate(
        esc_input = ifelse(is.na(esc_input), NA, as.numeric(esc_input)),
        esc_output = ifelse(is.na(esc_output), NA, as.numeric(esc_output))
      )
    
    # Unir los resultados de VRS y CRS en un solo dataframe por IdEstablecimiento
    df_combinado <- df_vrs_combinado %>%
      full_join(df_crs_combinado, by = "IdEstablecimiento") %>%
      full_join(df_esc_combinado, by = "IdEstablecimiento")
    
    return(df_combinado)
  })
  
  # Nombrar la lista con los años para identificación
  names(lista_resultados_combinados) <- unique(names(resultados_in))
  
  return(lista_resultados_combinados)
}

# ==============================================
#  RANDOM FOREST
# ==============================================
analize_rf <- function(year, resultados_in, n_top,tipo, orientacion ){
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
              modelo = modelo_rf,
              correlaciones = head(top_correlacion,50)))
  
  # O si prefieres ordenar por IncNodePurity 
  #importancia_ordenada <- importancia_df[order(importancia_df$IncNodePurity, decreasing = TRUE), ] 
  
  # Para mostrar los resultados con nombres de variables 
  # importancia_ordenada$Variables <- rownames(importancia_ordenada) 
  
  # Graficar la importancia
  # varImpPlot(modelo_rf)
  # title(main = paste0("Importancia de las Variables - Modelo Random Forest - Año ", year, " ", tipo ))
  
  # return(head(importancia_ordenada,50)) 
  
}

# ==============================================
#  DETERMINANTES IMPORTANCIA 2
# ==============================================
determinantes_importancia_2 <- function(random_forest, anios_pre_pandemia, anios_pandemia) {
  # Crear listas para almacenar resultados
  resultados_IncNodePurity <- list()
  resultados_IncMSE <- list()
  
  # Iterar sobre cada método
  for (metodo in names(random_forest)) {
    
    # Crear dataframes vacíos para almacenar resultados
    df <- data.frame(Variable = character(), stringsAsFactors = FALSE)

    # Recopilar datos por año
    for (anio in names(random_forest[[metodo]])) {
      # Obtener importancia
      importancia <- random_forest[[metodo]][[anio]]$importancia
      
      
      temp <- data.frame(
        Variable = rownames(importancia),
        Valor = importancia[, c("IncNodePurity","%IncMSE")],  # Seleccionar columna
        Año = anio
      )

      df <- rbind(df, temp)
    
    }
    
    # --- Procesar IncNodePurity ---

    
    # --- Procesar %IncMSE ---
    pivot <- reshape(df, 
                            idvar = "Variable", 
                            timevar = "Año", 
                            direction = "wide")
    colnames(pivot) <- gsub("Valor\\.", "", colnames(pivot))
    
    
  }
  
  # Retornar las dos listas
  #return(list(Importancia = df))
  return(list(Importancia = pivot))
}


# ==============================================
#  DETERMINANTES IMPORTANCIA
# ==============================================
original_determinantes_importancia <- function(random_forest, anios_pre_pandemia, anios_pandemia) {
  # Crear listas para almacenar resultados
  resultados_IncNodePurity <- list()
  resultados_IncMSE <- list()
  resultados_corr <- list()
  
  resultados_pre_IncNodePurity <- list()
  resultados_pre_IncMSE <- list()
  resultados_pre_corr <- list()
  
  resultados_post_IncNodePurity <- list()
  resultados_post_IncMSE <- list()
  resultados_post_corr <- list()
  
  # Iterar sobre cada método
  for (metodo in names(random_forest)) {
    
    # Crear dataframes vacíos para almacenar resultados
    df_IncNodePurity <- data.frame(Variable = character(), stringsAsFactors = FALSE)
    df_IncMSE <- data.frame(Variable = character(), stringsAsFactors = FALSE)
    df_corr <- data.frame(Variable = character(), stringsAsFactors = FALSE)
    
    # Recopilar datos por año
    for (anio in names(random_forest[[metodo]])) {
      # Obtener importancia
      importancia <- random_forest[[metodo]][[anio]]$importancia
      
      
      # --- %IncMSE ---
      temp_corr <- data.frame(
        Variable = "Correlacion",
        Valor = random_forest[[metodo]][[anio]]$correlacion,  # Seleccionar columna
        Año = anio
      )
      df_corr <- rbind(df_corr, temp_corr)
      
      
      # --- %IncMSE ---
      temp_IncMSE <- data.frame(
        Variable = rownames(importancia),
        Valor = importancia[, "%IncMSE"],  # Seleccionar columna
        Año = anio
      )
      df_IncMSE <- rbind(df_IncMSE, temp_IncMSE)

      
      # --- IncNodePurity ---
      temp_IncNodePurity <- data.frame(
        Variable = rownames(importancia),
        Valor = importancia[, "IncNodePurity"],  # Seleccionar columna
        Año = anio
      )
      df_IncNodePurity <- rbind(df_IncNodePurity, temp_IncNodePurity)
    }
    
    
    # --- Procesar Correlacion ---
    pivot_corr <- reshape(df_IncMSE, 
                            idvar = "Variable", 
                            timevar = "Año", 
                            direction = "wide")
    colnames(pivot_corr) <- gsub("Valor\\.", "", colnames(pivot_corr))
    
    # Calcular frecuencia y promedios
    pivot_corr$Frecuencia <- table(df_IncMSE$Variable)[pivot_corr$Variable]
    pivot_corr$Promedio <- rowMeans(pivot_corr[, c(2:11)], na.rm = TRUE)
    submat <- as.matrix(pivot_corr[, 2:11])
    pivot_corr$Varianza <- rowVars(submat, na.rm = TRUE)
    pivot_corr$Promedio_Pre_Pandemia <- rowMeans(pivot_corr[, anios_pre_pandemia], na.rm = TRUE)
    pivot_corr$Promedio_Pandemia <- rowMeans(pivot_corr[, anios_pandemia], na.rm = TRUE)
    
    # Calcular frecuencias separadas
    pivot_corr$Frecuencia_Pre_Pandemia <- rowSums(!is.na(pivot_corr[, anios_pre_pandemia]))
    pivot_corr$Frecuencia_Pandemia <- rowSums(!is.na(pivot_corr[, anios_pandemia]))
    
    # Ordenar y seleccionar las 50 más importantes
    pivot_corr <- pivot_corr[order(-pivot_corr$Promedio,-pivot_corr$Frecuencia), ]
    resultados_corr[[metodo]] <- head(pivot_corr, 50)
    
    pivot_pre_corr <- pivot_corr[order(-pivot_corr$Promedio_Pre_Pandemia,-pivot_corr$Frecuencia_Pre_Pandemia), ]
    pivot_post_corr <- pivot_corr[order(-pivot_corr$Promedio_Pandemia,-pivot_corr$Frecuencia_Pandemia), ]
    
    resultados_pre_corr[[metodo]] <- head(pivot_pre_corr, 50)
    resultados_post_corr[[metodo]] <- head(pivot_post_corr, 50)
    
    
    # --- Procesar %IncMSE ---
    pivot_IncMSE <- reshape(df_IncMSE, 
                            idvar = "Variable", 
                            timevar = "Año", 
                            direction = "wide")
    colnames(pivot_IncMSE) <- gsub("Valor\\.", "", colnames(pivot_IncMSE))
    
    # Calcular frecuencia y promedios
    pivot_IncMSE$Frecuencia <- table(df_IncMSE$Variable)[pivot_IncMSE$Variable]
    pivot_IncMSE$Promedio <- rowMeans(pivot_IncMSE[, c(2:11)], na.rm = TRUE)
    submat <- as.matrix(pivot_IncMSE[, 2:11])
    pivot_IncMSE$Varianza <- rowVars(submat, na.rm = TRUE)
    pivot_IncMSE$Promedio_Pre_Pandemia <- rowMeans(pivot_IncMSE[, anios_pre_pandemia], na.rm = TRUE)
    pivot_IncMSE$Promedio_Pandemia <- rowMeans(pivot_IncMSE[, anios_pandemia], na.rm = TRUE)
    
    # Calcular frecuencias separadas
    pivot_IncMSE$Frecuencia_Pre_Pandemia <- rowSums(!is.na(pivot_IncMSE[, anios_pre_pandemia]))
    pivot_IncMSE$Frecuencia_Pandemia <- rowSums(!is.na(pivot_IncMSE[, anios_pandemia]))
    
    # Ordenar y seleccionar las 50 más importantes
    pivot_IncMSE <- pivot_IncMSE[order(-pivot_IncMSE$Promedio,-pivot_IncMSE$Frecuencia), ]
    #pivot_IncMSE <- pivot_IncMSE[order(-pivot_IncMSE$Promedio, pivot_IncMSE$Varianza), ]
    resultados_IncMSE[[metodo]] <- head(pivot_IncMSE, 50)
    
    pivot_pre_IncMSE <- pivot_IncMSE[order(-pivot_IncMSE$Promedio_Pre_Pandemia,-pivot_IncMSE$Frecuencia_Pre_Pandemia), ]
    pivot_post_IncMSE <- pivot_IncMSE[order(-pivot_IncMSE$Promedio_Pandemia,-pivot_IncMSE$Frecuencia_Pandemia), ]
    
    resultados_pre_IncMSE[[metodo]] <- head(pivot_pre_IncMSE, 50)
    resultados_post_IncMSE[[metodo]] <- head(pivot_post_IncMSE, 50)    
    
    # --- Procesar IncNodePurity ---
    pivot_IncNodePurity <- reshape(df_IncNodePurity, 
                                   idvar = "Variable", 
                                   timevar = "Año", 
                                   direction = "wide")
    colnames(pivot_IncNodePurity) <- gsub("Valor\\.", "", colnames(pivot_IncNodePurity))
    
    # Calcular frecuencia y promedios
    pivot_IncNodePurity$Frecuencia <- table(df_IncNodePurity$Variable)[pivot_IncNodePurity$Variable]
    pivot_IncNodePurity$Promedio <- rowMeans(pivot_IncNodePurity[, c(2:11)], na.rm = TRUE)
    pivot_IncNodePurity$Promedio_Pre_Pandemia <- rowMeans(pivot_IncNodePurity[, anios_pre_pandemia], na.rm = TRUE)
    pivot_IncNodePurity$Promedio_Pandemia <- rowMeans(pivot_IncNodePurity[, anios_pandemia], na.rm = TRUE)
    
    # Calcular frecuencias separadas
    pivot_IncNodePurity$Frecuencia_Pre_Pandemia <- rowSums(!is.na(pivot_IncNodePurity[, anios_pre_pandemia]))
    pivot_IncNodePurity$Frecuencia_Pandemia <- rowSums(!is.na(pivot_IncNodePurity[, anios_pandemia]))
    
    pivot_pre_IncNodePurity <- pivot_IncNodePurity
    pivot_post_IncNodePurity <- pivot_IncNodePurity
    
    
    # 1) Sacar las 50 primeras variables de IncMSE
    top50_IncMSE <- pivot_IncMSE$Variable[1:50]
    top50_pre_IncMSE <- pivot_pre_IncMSE$Variable[1:50]
    top50_post_IncMSE <- pivot_post_IncMSE$Variable[1:50]
    
    # 2) Filtrar pivot_IncNodePurity sólo con esas 50
    #    y reordenar para que aparezcan en el mismo orden
    pivot_IncNodePurity <- pivot_IncNodePurity[pivot_IncNodePurity$Variable %in% top50_IncMSE, ]
    orden_np <- match(top50_IncMSE, pivot_IncNodePurity$Variable)
    pivot_IncNodePurity <- pivot_IncNodePurity[orden_np, ]
    
    # 3) Finalmente, asignar pivot_IncNodePurity a tu objeto de resultados
    resultados_IncNodePurity[[metodo]] <- pivot_IncNodePurity    
    
    # -----
    pivot_pre_IncNodePurity <- pivot_pre_IncNodePurity[pivot_pre_IncNodePurity$Variable %in% top50_pre_IncMSE, ]
    orden_np <- match(top50_pre_IncMSE, pivot_pre_IncNodePurity$Variable)
    pivot_pre_IncNodePurity <- pivot_pre_IncNodePurity[orden_np, ]
    
    # 3) Finalmente, asignar pivot_IncNodePurity a tu objeto de resultados
    resultados_pre_IncNodePurity[[metodo]] <- pivot_pre_IncNodePurity 
    
    # -----
    pivot_post_IncNodePurity <- pivot_post_IncNodePurity[pivot_post_IncNodePurity$Variable %in% top50_post_IncMSE, ]
    orden_np <- match(top50_post_IncMSE, pivot_post_IncNodePurity$Variable)
    pivot_post_IncNodePurity <- pivot_post_IncNodePurity[orden_np, ]
    
    # 3) Finalmente, asignar pivot_IncNodePurity a tu objeto de resultados
    resultados_post_IncNodePurity[[metodo]] <- pivot_post_IncNodePurity 
    
    
  }
  
  # Retornar las dos listas
  return(list(IncNodePurity = resultados_IncNodePurity, 
              IncMSE = resultados_IncMSE, 
              IncNodePurity_pre = resultados_pre_IncNodePurity, 
              IncNodePurity_post = resultados_post_IncNodePurity,
              IncMSE_pre = resultados_pre_IncMSE, 
              IncMSE_post = resultados_post_IncMSE,
              Corr = resultados_corr,
              Corr_Pre = resultados_pre_corr,
              Corr_Post = resultados_post_corr))
}

determinantes_importancia <- function(random_forest, anios_pre_pandemia, anios_pandemia) {
  library(dplyr)
  
  # Crear listas para almacenar resultados
  resultados_IncNodePurity <- list()
  resultados_IncMSE <- list()
  resultados_corr <- list()
  
  resultados_pre_IncNodePurity <- list()
  resultados_pre_IncMSE <- list()
  resultados_pre_corr <- list()
  
  resultados_post_IncNodePurity <- list()
  resultados_post_IncMSE <- list()
  resultados_post_corr <- list()
  
  # Iterar sobre cada método
  for (metodo in names(random_forest)) {
    
    # Crear dataframes vacíos para almacenar resultados
    df_IncMSE <- data.frame(Variable = character(), stringsAsFactors = FALSE)
    df_IncNodePurity <- data.frame(Variable = character(), stringsAsFactors = FALSE)
    df_corr <- data.frame(Variable = character(), stringsAsFactors = FALSE)
    
    # Recopilar datos por año
    for (anio in names(random_forest[[metodo]])) {
      # Obtener importancia
      importancia <- random_forest[[metodo]][[anio]]$importancia
      
      # --- %IncMSE ---
      temp_IncMSE <- data.frame(
        Variable = rownames(importancia),
        Valor = importancia[, "%IncMSE"],  # Seleccionar columna
        Año = anio
      )
      df_IncMSE <- rbind(df_IncMSE, temp_IncMSE)
      
      # --- IncNodePurity ---
      temp_IncNodePurity <- data.frame(
        Variable = rownames(importancia),
        Valor = importancia[, "IncNodePurity"],  # Seleccionar columna
        Año = anio
      )
      df_IncNodePurity <- rbind(df_IncNodePurity, temp_IncNodePurity)
      
      # --- Correlación ---
      temp_corr <- data.frame(
        Variable = "Correlacion",
        Valor = random_forest[[metodo]][[anio]]$correlaciones,  
        Año = anio
      )
      df_corr <- rbind(df_corr, temp_corr)
    }
    
    # --- Procesar %IncMSE ---
    pivot_IncMSE <- reshape(df_IncMSE, idvar = "Variable", timevar = "Año", direction = "wide")
    colnames(pivot_IncMSE) <- gsub("Valor\\.", "", colnames(pivot_IncMSE))
    
    # Calcular frecuencia y estadísticos
    pivot_IncMSE$Frecuencia <- table(df_IncMSE$Variable)[pivot_IncMSE$Variable]
    pivot_IncMSE$Promedio <- rowMeans(pivot_IncMSE[, -1], na.rm = TRUE)
    
    # Calcular la varianza específica por período
    pivot_IncMSE$Varianza_Pre_Pandemia <- apply(pivot_IncMSE[, anios_pre_pandemia], 1, var, na.rm = TRUE)
    pivot_IncMSE$Varianza_Pandemia <- apply(pivot_IncMSE[, anios_pandemia], 1, var, na.rm = TRUE)
    
    # Calcular los promedios por período
    pivot_IncMSE$Promedio_Pre_Pandemia <- rowMeans(pivot_IncMSE[, anios_pre_pandemia], na.rm = TRUE)
    pivot_IncMSE$Promedio_Pandemia <- rowMeans(pivot_IncMSE[, anios_pandemia], na.rm = TRUE)
    
    # Calcular frecuencias separadas
    pivot_IncMSE$Frecuencia_Pre_Pandemia <- rowSums(!is.na(pivot_IncMSE[, anios_pre_pandemia]))
    pivot_IncMSE$Frecuencia_Pandemia <- rowSums(!is.na(pivot_IncMSE[, anios_pandemia]))
    
    # Filtrar eliminando las variables con varianza alta **global**
    threshold_varianza <- quantile(pivot_IncMSE$Varianza_Pre_Pandemia, 0.75, na.rm = TRUE)
    pivot_IncMSE <- pivot_IncMSE %>% filter(Varianza_Pre_Pandemia <= threshold_varianza)
    
    # Seleccionar las 50 mejores variables basadas en %IncMSE global
    pivot_IncMSE <- pivot_IncMSE %>% arrange(desc(Promedio), desc(Frecuencia))
    top50_IncMSE <- head(pivot_IncMSE$Variable, 50)
    resultados_IncMSE[[metodo]] <- head(pivot_IncMSE, 50)
    
    # --- Filtrar y ordenar para cada período ---
    pivot_pre_IncMSE <- pivot_IncMSE %>%
      filter(Varianza_Pre_Pandemia <= quantile(pivot_IncMSE$Varianza_Pre_Pandemia, 0.75, na.rm = TRUE)) %>%
      arrange(desc(Promedio_Pre_Pandemia), desc(Frecuencia_Pre_Pandemia))
    
    pivot_post_IncMSE <- pivot_IncMSE %>%
      filter(Varianza_Pandemia <= quantile(pivot_IncMSE$Varianza_Pandemia, 0.75, na.rm = TRUE)) %>%
      arrange(desc(Promedio_Pandemia), desc(Frecuencia_Pandemia))
    
    resultados_pre_IncMSE[[metodo]] <- head(pivot_pre_IncMSE, 50)
    resultados_post_IncMSE[[metodo]] <- head(pivot_post_IncMSE, 50)
    
    # --- Filtrar IncNodePurity con las variables seleccionadas ---
    pivot_IncNodePurity <- reshape(df_IncNodePurity, idvar = "Variable", timevar = "Año", direction = "wide")
    colnames(pivot_IncNodePurity) <- gsub("Valor\\.", "", colnames(pivot_IncNodePurity))
    
    pivot_IncNodePurity <- pivot_IncNodePurity %>% filter(Variable %in% top50_IncMSE)
    resultados_IncNodePurity[[metodo]] <- pivot_IncNodePurity
    
    # Filtrar pre y post pandemia para IncNodePurity
    pivot_pre_IncNodePurity <- pivot_IncNodePurity %>% filter(Variable %in% resultados_pre_IncMSE[[metodo]]$Variable)
    pivot_post_IncNodePurity <- pivot_IncNodePurity %>% filter(Variable %in% resultados_post_IncMSE[[metodo]]$Variable)
    
    resultados_pre_IncNodePurity[[metodo]] <- pivot_pre_IncNodePurity
    resultados_post_IncNodePurity[[metodo]] <- pivot_post_IncNodePurity
    
    # --- Filtrar Correlación con las variables seleccionadas ---
    pivot_corr <- reshape(df_corr, idvar = "Variable", timevar = "Año", direction = "wide")
    colnames(pivot_corr) <- gsub("Valor\\.", "", colnames(pivot_corr))
    
    pivot_corr <- pivot_corr %>% filter(Variable %in% top50_IncMSE)
    resultados_corr[[metodo]] <- pivot_corr
    
    # Filtrar pre y post pandemia para correlación
    pivot_pre_corr <- pivot_corr %>% filter(Variable %in% resultados_pre_IncMSE[[metodo]]$Variable)
    pivot_post_corr <- pivot_corr %>% filter(Variable %in% resultados_post_IncMSE[[metodo]]$Variable)
    
    resultados_pre_corr[[metodo]] <- pivot_pre_corr
    resultados_post_corr[[metodo]] <- pivot_post_corr
  }
  
  return(list(
    IncMSE = resultados_IncMSE, 
    IncNodePurity = resultados_IncNodePurity, 
    Corr = resultados_corr,
    IncMSE_Pre = resultados_pre_IncMSE, 
    IncMSE_Post = resultados_post_IncMSE,
    IncNodePurity_Pre = resultados_pre_IncNodePurity,
    IncNodePurity_Post = resultados_post_IncNodePurity,
    Corr_Pre = resultados_pre_corr,
    Corr_Post = resultados_post_corr
  ))
}



# ==============================================
#  FILTRO DE DATOS SEGUN DATOS ATIPICOS DE ITERACIONES AL CONJUNTO ORIGINAL
# ==============================================
datos_filtrados_atipicos <- function(datos, resultados) {
  # Filtrar datos
  datos_filtrados_vrs_io <- filtrar_datos(datos, resultados[["io"]][["vector_outliers_vrs"]])
  datos_filtrados_crs_io <- filtrar_datos(datos, resultados[["io"]][["vector_outliers_crs"]])
  datos_filtrados_vrs_oo <- filtrar_datos(datos, resultados[["oo"]][["vector_outliers_vrs"]])
  datos_filtrados_crs_oo <- filtrar_datos(datos, resultados[["oo"]][["vector_outliers_crs"]])
  datos_filtrados_esc_io <- filtrar_datos(datos, resultados[["io"]][["vector_outliers_esc"]])
  datos_filtrados_esc_oo <- filtrar_datos(datos, resultados[["oo"]][["vector_outliers_esc"]])
  
  return (list(
    vrs_io = datos_filtrados_vrs_io,
    crs_io = datos_filtrados_crs_io,
    vrs_oo = datos_filtrados_vrs_oo,
    crs_oo = datos_filtrados_crs_oo,
    esc_io = datos_filtrados_esc_io,
    esc_oo = datos_filtrados_esc_oo
    
  ))
}

# ==============================================
#  GUARDAR DATOS DE EFICIENCIA Y DETERMINANTES
# ==============================================
guardar_resultados <- function(dataframes, resultados_IncNodePurity,resultados_pre_IncNodePurity,resultados_post_IncNodePurity, resultados_IncMSE,resultados_pre_IncMSE, resultados_post_IncMSE,malmquist_vrs,malmquist_crs,archivo_salida, prefijo) {
  # Instalar y cargar el paquete necesario
  if (!require(openxlsx)) install.packages("openxlsx")
  library(openxlsx)
  
  # Función para reemplazar valores vacíos, NULL o NA por "-"
  reemplazar_nulos <- function(df) {
    df[is.na(df) | df == ""] <- "-" # Reemplaza NA o valores vacíos
    df <- df[order(-df$Frecuencia), ]
    return(df)
  }
  
  # Crear un workbook
  wb <- createWorkbook()
  
  # Guardar resultados VRS y CRS
  vrs <- guardar_dataframe_por_columna(dataframes, "vrs")
  crs <- guardar_dataframe_por_columna(dataframes, "crs")
  escala <- guardar_dataframe_por_columna(dataframes, "escala")
  
  
  
  # Añadir la hoja
  addWorksheet(wb, "VRS")
  writeData(wb, "VRS", vrs)
  
  addWorksheet(wb, "CRS")
  writeData(wb, "CRS", crs)
  
  addWorksheet(wb, "ESCALA")
  writeData(wb, "ESCALA", escala)
  
  addWorksheet(wb, "MALMQUIST VRS")
  writeData(wb, "MALMQUIST VRS", malmquist_vrs)
  
  addWorksheet(wb, "MALMQUIST CRS")
  writeData(wb, "MALMQUIST CRS", malmquist_crs)
  

  
  ### GUARDAR IMPORTANCIA ###
  # Correlacion
  addWorksheet(wb, paste0("Determinantes correlaciones VRS"))
  writeData(wb, paste0("Determinantes IncNodePurity VRS"), 
            reemplazar_nulos(resultados_IncNodePurity[[paste0(prefijo, "_vrs")]]))
  
  
  
  
  # IncNodePurity
  addWorksheet(wb, paste0("Determinantes IncNodePurity VRS"))
  writeData(wb, paste0("Determinantes IncNodePurity VRS"), 
            reemplazar_nulos(resultados_IncNodePurity[[paste0(prefijo, "_vrs")]]))
  
  addWorksheet(wb, paste0("Determinantes IncNodePurity CRS"))
  writeData(wb, paste0("Determinantes IncNodePurity CRS"), 
            reemplazar_nulos(resultados_IncNodePurity[[paste0(prefijo, "_crs")]]))
  
  # PRE PANDEMIA #
  addWorksheet(wb, paste0("Det PRE IncNodePurity VRS"))
  writeData(wb, paste0("Det PRE IncNodePurity VRS"), 
            reemplazar_nulos(resultados_pre_IncNodePurity[[paste0(prefijo, "_vrs")]]))
  
  addWorksheet(wb, paste0("Det PRE IncNodePurity CRS"))
  writeData(wb, paste0("Det PRE IncNodePurity CRS"), 
            reemplazar_nulos(resultados_pre_IncNodePurity[[paste0(prefijo, "_crs")]]))
  
  # POST #
  addWorksheet(wb, paste0("Det POST IncNodePurity VRS"))
  writeData(wb, paste0("Det POST IncNodePurity VRS"), 
            reemplazar_nulos(resultados_post_IncNodePurity[[paste0(prefijo, "_vrs")]]))
  
  addWorksheet(wb, paste0("Det POST IncNodePurity CRS"))
  writeData(wb, paste0("Det POST IncNodePurity CRS"), 
            reemplazar_nulos(resultados_post_IncNodePurity[[paste0(prefijo, "_crs")]]))
  
  
  # %IncMSE
  addWorksheet(wb, paste0("Determinantes IncMSE VRS"))
  writeData(wb, paste0("Determinantes IncMSE VRS"), 
            reemplazar_nulos(resultados_IncMSE[[paste0(prefijo, "_vrs")]]))
  
  addWorksheet(wb, paste0("Determinantes IncMSE CRS"))
  writeData(wb, paste0("Determinantes IncMSE CRS"), 
            reemplazar_nulos(resultados_IncMSE[[paste0(prefijo, "_crs")]]))
  
  # PRE PANDEMIA #
  addWorksheet(wb, paste0("Det PRE IncMSE VRS"))
  writeData(wb, paste0("Det PRE IncMSE VRS"), 
            reemplazar_nulos(resultados_pre_IncMSE[[paste0(prefijo, "_vrs")]]))
  
  addWorksheet(wb, paste0("Det PRE IncMSE CRS"))
  writeData(wb, paste0("Det PRE IncMSE CRS"), 
            reemplazar_nulos(resultados_pre_IncMSE[[paste0(prefijo, "_crs")]]))

  # POST PANDEMIA #
  addWorksheet(wb, paste0("Det POST IncMSE VRS"))
  writeData(wb, paste0("Det POST IncMSE VRS"), 
            reemplazar_nulos(resultados_post_IncMSE[[paste0(prefijo, "_vrs")]]))
  
  addWorksheet(wb, paste0("Det POST IncMSE CRS"))
  writeData(wb, paste0("Det POST IncMSE CRS"), 
            reemplazar_nulos(resultados_post_IncMSE[[paste0(prefijo, "_crs")]]))
  
  # Guardar el archivo
  saveWorkbook(wb, archivo_salida, overwrite = TRUE)
  cat("Archivo guardado como:", archivo_salida, "\n")
}








