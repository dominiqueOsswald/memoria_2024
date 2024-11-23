library(readxl)
library(corrplot)
library(deaR)
library(dplyr)
library(Benchmarking)
library(tidyr)
library(corrplot)
library(gridExtra)
library(purrr)

aplicar_sensibilidad <- function(datos, resultados, umbral, orientacion, retorno, mayor) {
  mapply(function(data, resultado) sensibilidad_parametro_general(data, resultado, mayor, umbral, orientacion, retorno),
         datos, resultados, SIMPLIFY = FALSE)
}

normalize_min_max <- function(x) {
  (x - min(x)) / (max(x) - min(x))
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
  path_estadisticas <- "data/Consolidado estadísticas hospitalarias 2014-2021.xlsx"
  
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


  consultas <- list("idEstablecimiento", "X07020130", "X07020230", "X07020330", "X07020400", 
                    "X07020500", "X07020600", "X07020700", "X07020800", "X07020900", "X07024970", 
                    "X07021000", "X07021100", "X07021230", "X07021300", "X07021400", "X07024980", 
                    "X07021531", "X07021600", "X07021700", "X07021800", "X07021900", "X07022000", 
                    "X07022130", "X07022131", "X07022200", "X07022330", "X07024990", "X07022400", 
                    "X07022500", "X07022631", "X07022700", "X07022800", "X07022900", "X07023000", 
                    "X07023100", "X07023200", "X07023400", "X070251000", "X07030100", "X07023600", 
                    "X070251100", "X07023700", "X07023800", "X07023900", "X07024000", "X07024200", 
                    "X07024900", "X07024915", "X07024925", "X07024935", "X07024920", "X07024816", 
                    "X07024607", "X07024817", "X07024809", "X07024705", "X07024506", "X07024930", 
                    "X07024940", "X070251200", "X070251300", "X070251400", "X07030100A", "X07030200", 
                    "X07030300", "X07030400", "X07024950", "X07024960", "X07024814", "X07024815", 
                    "X07030500", "X07030600", "X070251500", "X070251600", "X070251700", "X070251800", 
                    "X070251900", "X070252000", "X070252100", "X070252200", "X070252300", "X070252400", 
                    "X070252500", "X070252600", "X07030700", "X07030800", "X07030900", "X07031000", 
                    "X07025300", "X07025310", "X07025320", "X07025330", "X07025340", "X07031100", 
                    "X07031200", "X07025350", "X07031200A", "X07031300", "X07031400", "X07031500", 
                    "X07031600", "X07031700", "X07031800", "X07031900", "X07032000", "X07032100", 
                    "X07032200", "X07032300", "X07032400", "X07032500", "X07032600", "X07032700", 
                    "X07032800", "X07032900", "X07033000", "X07033100", "X07033200", "X07033300", 
                    "X07033400", "X07033500", "X07033600", "X07033700", "X07033800", "X07033900", 
                    "X07034000", "X07034100", "X07034200", "X07034300", "X07034400", "X07034500", 
                    "X07034600", "X07034700", "X07034800", "X07034900", "X07035000", "X07035100", 
                    "X07035200", "X07035300")
  
  

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
  quirofano <- list("idEstablecimiento", "X21220100", "X21220200", "X21220700", "X21220600", "X21400300", "X21220900",
                    "X21400500","X21400600","X21800810")
  quirofano_data <- subset(datos_consolidados, select = unlist(quirofano))
  
  # Reemplazar comas por puntos y convertir a numérico
  quirofano_data <- quirofano_data %>%
    mutate(across(-idEstablecimiento, ~ as.numeric(gsub(",", ".", .))))

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
  
  # Verificar si hay combinaciones duplicadas de ID y TIME
  temp_df <- data.frame(ID = ID, TIME = TIME)
  duplicated_rows <- temp_df[duplicated(temp_df), ]
  
  if (nrow(duplicated_rows) > 0) {
    message("Se encontraron combinaciones de ID y TIME duplicadas:")
    print(duplicated_rows)
    
    # Eliminar duplicados en ID y TIME
    temp_df <- temp_df[!duplicated(temp_df), ]
    input_data <- input_data[!duplicated(temp_df), ]
    output_data <- output_data[!duplicated(temp_df), ]
  } else {
    message("No hay duplicados.")
  }
  
  # Convertir listas a matrices para el análisis sin duplicados
  input_matrix <- do.call(rbind, input_data)
  output_matrix <- do.call(rbind, output_data)
  
  # Realizar el análisis Malmquist
  malmquist_index <- malmquist(X = input_matrix, Y = output_matrix, ID = ID, TIME = TIME, 
                               RTS = tipo, ORIENTATION = orientacion)
  
  
  # Se generan dataframe que entreguen la info de indice, cambio de eficiencia y tecnologico
  resultados_df <- data.frame(
    ID = malmquist_index$id,
    Año = malmquist_index$time,             # Periodo o año
    MalmquistIndex = malmquist_index$m,     # Índice Malmquist total
    Effch = malmquist_index$ec,          # Cambio en eficiencia
    Techch = malmquist_index$tc        # Cambio tecnológico
  )
  
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
  
  # Se elimina la columna que no tienen ningun valor
  malmquist_df <- Filter(function(x) !all(is.na(x)),malmquist_df)
  effch_df <- Filter(function(x) !all(is.na(x)),effch_df)
  techch_df <- Filter(function(x) !all(is.na(x)), techch_df)
  
  # Ordenar el dataframe por ID y TIME
  efficiency_df <- efficiency_df[order(efficiency_df$ID, efficiency_df$TIME), ]
  
  # Transformar el dataframe en formato ancho (wide) con 'ID' como fila y 'TIME' como columnas
  efficiency_wide <- pivot_wider(efficiency_df, names_from = TIME, values_from = Efficiency)
  
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
  
  original <-  sapply(datos, function(data) analisis_dea_general(data, orientacion), simplify = FALSE)
  print("1")
  #aplicar_analisis_dea(datos, orientacion)
  if (orientacion == "io"){
      iteracion_1_vrs <- aplicar_sensibilidad(datos, lapply(original, `[[`, "data"), 0.99, orientacion, "vrs", FALSE)
      iteracion_2_vrs <- aplicar_sensibilidad(datos, lapply(iteracion_1_vrs, `[[`, "data"), 0.99, orientacion, "vrs", FALSE)
      iteracion_1_crs <- aplicar_sensibilidad(datos, lapply(original, `[[`, "data"), 0.99, orientacion, "crs", FALSE)
      iteracion_2_crs <- aplicar_sensibilidad(datos, lapply(iteracion_1_crs, `[[`, "data"), 0.99, orientacion, "crs", FALSE)
      
    
  }else{
    
    
    iteracion_1_vrs <- aplicar_sensibilidad(datos, lapply(original, `[[`, "data"), 1, orientacion, "vrs", TRUE)
    iteracion_2_vrs <- aplicar_sensibilidad(datos, lapply(iteracion_1_vrs, `[[`, "data"), 1, orientacion, "vrs", TRUE)
    iteracion_1_crs <- aplicar_sensibilidad(datos, lapply(original, `[[`, "data"), 1, orientacion, "crs", TRUE)
    iteracion_2_crs <- aplicar_sensibilidad(datos, lapply(iteracion_1_crs, `[[`, "data"), 1, orientacion, "crs", TRUE)

    #print(iteracion_1_vrs[[year]][["data"]]$vrs)
    
        
    # Normalizando los datos
    for (year in names(original)) {
      # Normalizar VRS
      original[[year]][["data"]]$vrs <- normalize_min_max(original[[year]][["data"]]$vrs)
      iteracion_1_vrs[[year]][["data"]]$vrs <- normalize_min_max(iteracion_1_vrs[[year]][["data"]]$vrs)
      iteracion_2_vrs[[year]][["data"]]$vrs <- normalize_min_max(iteracion_2_vrs[[year]][["data"]]$vrs)
      
      
      
      # Normalizar CRS
      original[[year]][["data"]]$crs <- normalize_min_max(original[[year]][["data"]]$crs)
      iteracion_1_crs[[year]][["data"]]$crs <- normalize_min_max(iteracion_1_crs[[year]][["data"]]$crs)
      iteracion_2_crs[[year]][["data"]]$crs <- normalize_min_max(iteracion_2_crs[[year]][["data"]]$crs)
      
    }
    print("2")
    #print(iteracion_1_vrs[[year]][["data"]]$vrs)
  }
  resultados_combinados <- combinar_resultados_iteraciones(original, iteracion_1_vrs, iteracion_2_vrs, iteracion_1_crs, iteracion_2_crs)
  print("3")
  resultados_correlacion <- calcular_y_graficar_correlaciones(resultados_combinados, anios)
  
  
  print("4")
  # Crear una lista vacía para almacenar los valores atípicos por año
  lista_outliers_vrs <- list()
  # Crear un vector vacío para almacenar todos los valores atípicos sin duplicados
  vector_outliers_vrs <- c()
  
  lista_outliers_crs <- list()
  # Crear un vector vacío para almacenar todos los valores atípicos sin duplicados
  vector_outliers_crs <- c()
  
  
  # Especificar los años que quieres iterar
  anios <- c("2014", "2015", "2016", "2017", "2018", "2019", "2020")
  print("5")
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
  print("6")
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


