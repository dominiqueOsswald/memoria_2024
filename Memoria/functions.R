library(readxl)
library(corrplot)
library(deaR)
library(dplyr)


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
  
  return(all)
}

analisis_dea_general <- function(data, orientation) {
  # Preparar inputs y outputs
  model <- make_deadata(data, ni=3, no="IdEstablecimiento", dmus=3, inputs=8:10, outputs=5:7)
  
  # Aplicar DEA con la orientación y RTS (VRS y CRS)
  resultado_dea_vrs <- model_basic(model, orientation=orientation, rts="vrs", dmu_eval = 1:nrow(data), dmu_ref = 1:nrow(data)) 
  resultado_dea_crs <- model_basic(model, orientation=orientation, rts="crs", dmu_eval = 1:nrow(data), dmu_ref = 1:nrow(data)) 
  
  # Calcular eficiencias
  eficiencia_vrs <- efficiencies(resultado_dea_vrs)
  eficiencia_crs <- efficiencies(resultado_dea_crs)
  
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
  eficiencia_vrs_data <- eficiencia_df[order(-eficiencia_df$vrs), ]
  eficiencia_crs_data <- eficiencia_df[order(-eficiencia_df$crs), ]
  eficiencia_escala_data <- eficiencia_df[order(eficiencia_df$escala), ]
  
  # Retornar los dataframes ordenados como una lista
  return(list(data = eficiencia_df, 
              vrs = eficiencia_vrs_data, 
              crs = eficiencia_crs_data, 
              esc = eficiencia_escala_data))
}

sensibilidad_parametro_general <- function(data, data_original, mayor, valor, orientacion, tipo) {
  #data <- datos[["2014"]]
  #tipo <- "vrs"
  #data_original <- resultados_out[["2014"]]$data
  #mayor <- FALSE
  #valor <- 0.99
  #orientacion <- "oo"
  
  #sensibilidad_parametro_general(datos[["2014"]], resultados_in[["2014"]], FALSE, 0.99, "io", "vrs")
  # head(data)
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
  
  # Combinar los dataframes por IdEstablecimiento, manteniendo solo las columnas de interés
  #resultados_combinados <- merge(
  #  data_filtrada[, c("IdEstablecimiento", columna)],
  #  resultados_in[[tipo]][, c("IdEstablecimiento", columna)],
  #  by = "IdEstablecimiento",
  # suffixes = c("_1", "_2")
  #)
  
  # Calcular la correlación entre las dos columnas
  #correlacion <- cor(resultados_combinados[[paste0(columna, "_1")]], resultados_combinados[[paste0(columna, "_2")]], use = "pairwise.complete.obs")
  
  # Calcular los rankings
  #resultados_combinados[[paste0("ranking_", columna, "_1")]] <- rank(-resultados_combinados[[paste0(columna, "_1")]])  # Invertir para mayor a menor
  #resultados_combinados[[paste0("ranking_", columna, "_2")]] <- rank(-resultados_combinados[[paste0(columna, "_2")]])
  
  # Verificar si los rankings coinciden
  #resultados_combinados$ranking_coincide <- resultados_combinados[[paste0("ranking_", columna, "_1")]] == resultados_combinados[[paste0("ranking_", columna, "_2")]]
  #resultados_combinados$diff_ranking <- resultados_combinados[[paste0("ranking_", columna, "_1")]] - resultados_combinados[[paste0("ranking_", columna, "_2")]]
  
  #print(correlacion)
  
  # Retornar los resultados
  #return (list(correlacion = correlacion, resultados = resultados_in))
  return (resultados = resultados_in)
}

# Función para combinar resultados de VRS y CRS en una lista de dataframes por año
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

aplicar_analisis_dea <- function(datos, metodo) {
  sapply(datos, function(data) analisis_dea_general(data, metodo), simplify = FALSE)
}

# Función para aplicar la sensibilidad del parámetro general
aplicar_sensibilidad <- function(datos, resultados, umbral, orientacion, retorno, mayor) {
  mapply(function(data, resultado) sensibilidad_parametro_general(data, resultado, mayor, umbral, orientacion, retorno),
         datos, resultados, SIMPLIFY = FALSE)
}






comparativa <- function(resultados_2014_in, resultados_2015_in, resultados_2016_in, resultados_2017_in, resultados_2018_in, resultados_2019_in) {
  
  resultados_2014_in_df <- data.frame(IdEstablecimiento = resultados_2014_in$IdEstablecimiento, vrs = resultados_2014_in$vrs)
  resultados_2015_in_df <- data.frame(IdEstablecimiento = resultados_2015_in$IdEstablecimiento, vrs = resultados_2015_in$vrs)
  resultados_2016_in_df <- data.frame(IdEstablecimiento = resultados_2016_in$IdEstablecimiento, vrs = resultados_2016_in$vrs)
  resultados_2017_in_df <- data.frame(IdEstablecimiento = resultados_2017_in$IdEstablecimiento, vrs = resultados_2017_in$vrs)
  resultados_2018_in_df <- data.frame(IdEstablecimiento = resultados_2018_in$IdEstablecimiento, vrs = resultados_2018_in$vrs)
  resultados_2019_in_df <- data.frame(IdEstablecimiento = resultados_2019_in$IdEstablecimiento, vrs = resultados_2019_in$vrs)
  
  # Luego, aplicar reduce y full_join para combinar todos los dataframes por 'IdEstablecimiento'
  resultados_combinados <- reduce(list(resultados_2014_in_df, resultados_2015_in_df, resultados_2016_in_df, 
                                       resultados_2017_in_df, resultados_2018_in_df, 
                                       resultados_2019_in_df), 
                                  function(x, y) full_join(x, y, by = "IdEstablecimiento"))
  
  
  # Renombrar las columnas para indicar el año
  colnames(resultados_combinados)[-1] <- c("vrs_2014", "vrs_2015", "vrs_2016", "vrs_2017", "vrs_2018", "vrs_2019")
  
  # Visualizar los resultados combinados
  #print(resultados_combinados)
  
  resultados_combinados <- resultados_combinados %>%
    mutate(
      diff_2014_2015 = vrs_2015 - vrs_2014,
      diff_2015_2016 = vrs_2016 - vrs_2015,
      diff_2016_2017 = vrs_2017 - vrs_2016,
      diff_2017_2018 = vrs_2018 - vrs_2017,
      diff_2018_2019 = vrs_2019 - vrs_2018
    )

  # Calcular el promedio de las diferencias por fila, ignorando NA si existen
  resultados_combinados <- resultados_combinados %>%
    mutate(
      avg_diff = rowMeans(select(., diff_2014_2015, diff_2015_2016, diff_2016_2017, diff_2017_2018, diff_2018_2019), na.rm = TRUE)
    )
  
  # Visualizar los resultados
  # print(resultados_combinados)
  
  return(resultados_combinados) 
  
  
}
