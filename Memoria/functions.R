library(readxl)
library(dplyr)
library(Benchmarking)
library(corrplot)

#anio <- 2014
#path_hospitales <- paste0("data/", anio, "/", anio, "_hospitals.csv")
#path_datos_consolidados <- paste0("data/", anio, "/", anio, "_consolidated_data.csv")
#hospitales <- read.csv(path_hospitales) %>% rename("IdEstablecimiento" = "hospital_id")
#path_estadisticas <- "data/Consolidado estadísticas hospitalarias 2014-2021.xlsx"

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

analisis_dea_in <- function(data) {

  # Preparar inputs y outputs
  input_dea <- as.data.frame(data[c(8,9,10)])
  output_dea <- as.data.frame(data[c(5,6,7)])

  # Aplicar DEA orientado a los inputs
  resultado_dea_in_vrs <- dea(X = input_dea, Y = output_dea, RTS = "vrs", ORIENTATION = "in")
  resultado_dea_in_crs <- dea(X = input_dea, Y = output_dea, RTS = "crs", ORIENTATION = "in")

  # Calcular eficiencias
  eficiencia_vrs <- resultado_dea_in_vrs$eff
  eficiencia_crs <- resultado_dea_in_crs$eff

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
  
  # ------------------------------------------------------------------- #
  # Ordenar dataframes según diferentes columnas
  eficiencia_vrs_data <- eficiencia_df[order(-eficiencia_df$vrs), ]
  eficiencia_crs_data <- eficiencia_df[order(-eficiencia_df$crs), ]
  eficiencia_escala_data <- eficiencia_df[order(eficiencia_df$escala), ]
  # ------------------------------------------------------------------- #

  # Clasificar eficiencia VRS
  clasificacion_vrs <- cut(eficiencia_vrs, 
                           breaks = c(-Inf, 0.5, 0.75, 1, Inf), 
                           labels = c("Menor que 0.5", "Entre 0.5 y 0.75", "Entre 0.75 y 1", "Valor 1"),
                           right = FALSE)
  
  # Clasificar eficiencia CRS
  clasificacion_crs <- cut(eficiencia_crs, 
                           breaks = c(-Inf, 0.5, 0.75, 1, Inf), 
                           labels = c("Menor que 0.5", "Entre 0.5 y 0.75", "Entre 0.75 y 1", "Valor 1"),
                           right = FALSE)
  
  # Calcular frecuencia y porcentaje para VRS
  frecuencia_vrs_clasificada <- table(clasificacion_vrs)
  porcentaje_vrs_clasificada <- prop.table(frecuencia_vrs_clasificada) * 100
  
  # Calcular frecuencia y porcentaje para CRS
  frecuencia_crs_clasificada <- table(clasificacion_crs)
  porcentaje_crs_clasificada <- prop.table(frecuencia_crs_clasificada) * 100
  
  # Mostrar resultados
  # cat("Clasificación de eficiencia en VRS:\n")
  # print(frecuencia_vrs_clasificada)
  # cat("----------------------------------\n")
  # cat("Porcentaje de eficiencia en VRS:\n")
  # print(porcentaje_vrs_clasificada)
  
  # cat("----------------------------------\n")
  # cat("----------------------------------\n")
  
  # cat("\nClasificación de eficiencia en CRS:\n")
  # print(frecuencia_crs_clasificada)
  # cat("----------------------------------\n")
  # cat("Porcentaje de eficiencia en CRS:\n")
  # print(porcentaje_crs_clasificada)
  
  # Retornar los dataframes ordenados como una lista
  return(list(data= eficiencia_df, 
              vrs = eficiencia_vrs_data, 
              crs = eficiencia_crs_data, 
              esc = eficiencia_escala_data))
}

analisis_dea_out <- function(data) {
  
  # Preparar inputs y outputs
  input_dea_2019 <- as.data.frame(data[c(8,9,10)])
  output_dea_2019 <- as.data.frame(data[c(5,6,7)])
  
  # Aplicar DEA orientado a los inputs
  resultado_dea_2019_in_vrs <- dea(X = input_dea_2019, Y = output_dea_2019, RTS = "vrs", ORIENTATION = "out")
  resultado_dea_2019_in_crs <- dea(X = input_dea_2019, Y = output_dea_2019, RTS = "crs", ORIENTATION = "out")
  
  # Calcular eficiencias
  eficiencia_vrs <- resultado_dea_2019_in_vrs$eff
  eficiencia_crs <- resultado_dea_2019_in_crs$eff
  
  # Crear dataframe con eficiencias y retorno a escala
  eficiencia_df <- data.frame(
    IdEstablecimiento = data$IdEstablecimiento,
    Nombre = data$'Nombre Establecimiento',
    Region = data$'Region',
    vrs = round(eficiencia_vrs, 3),
    crs = round(eficiencia_crs, 3),
    escala = round(eficiencia_vrs / eficiencia_crs, 3)
  )
  
  # ------------------------------------------------------------------- #
  # Ordenar dataframes según diferentes columnas
  #eficiencia_vrs_data <- eficiencia_df[order(-eficiencia_df$vrs), ]
  #eficiencia_crs_data <- eficiencia_df[order(-eficiencia_df$crs), ]
  #eficiencia_escala_data <- eficiencia_df[order(eficiencia_df$escala), ]
  # ------------------------------------------------------------------- #
  
  # Clasificar eficiencia VRS
  clasificacion_vrs <- cut(eficiencia_vrs, 
                           breaks = c(-Inf, 0.5, 1, 1.5, Inf), 
                           labels = c("Menor que 0.5", "Entre 0.5 y 1", "Entre 1 y 1.5", "Mayor que 1.5"),
                           right = FALSE)
  
  # Clasificar eficiencia CRS
  clasificacion_crs <- cut(eficiencia_crs, 
                           breaks = c(-Inf, 0.5, 1, 1.5, Inf),
                           labels = c("Menor que 0.5", "Entre 0.5 y 1", "Entre 1 y 1.5", "Mayor que 1.5"),
                           right = FALSE)
  
  # Calcular frecuencia y porcentaje para VRS
  frecuencia_vrs_clasificada <- table(clasificacion_vrs)
  porcentaje_vrs_clasificada <- prop.table(frecuencia_vrs_clasificada) * 100
  
  # Calcular frecuencia y porcentaje para CRS
  frecuencia_crs_clasificada <- table(clasificacion_crs)
  porcentaje_crs_clasificada <- prop.table(frecuencia_crs_clasificada) * 100
  
  # Mostrar resultados
  cat("Clasificación de eficiencia en VRS:\n")
  print(frecuencia_vrs_clasificada)
  cat("----------------------------------\n")
  cat("Porcentaje de eficiencia en VRS:\n")
  print(porcentaje_vrs_clasificada)
  
  cat("----------------------------------\n")
  cat("----------------------------------\n")
  
  cat("\nClasificación de eficiencia en CRS:\n")
  print(frecuencia_crs_clasificada)
  cat("----------------------------------\n")
  cat("Porcentaje de eficiencia en CRS:\n")
  print(porcentaje_crs_clasificada)
  
  # Retornar los dataframes ordenados como una lista
  return(eficiencia_df)
}

analisis_dea_graph <- function(data) {
  
  # Preparar inputs y outputs
  input_dea_2019 <- as.data.frame(data[c(8,9,10)])
  output_dea_2019 <- as.data.frame(data[c(5,6,7)])
  
  # Aplicar DEA orientado a los inputs
  resultado_dea_2019_in_vrs <- dea(X = input_dea_2019, Y = output_dea_2019, RTS = "vrs", ORIENTATION = "graph")
  resultado_dea_2019_in_crs <- dea(X = input_dea_2019, Y = output_dea_2019, RTS = "crs", ORIENTATION = "graph")
  
  # Calcular eficiencias
  eficiencia_vrs <- resultado_dea_2019_in_vrs$eff
  eficiencia_crs <- resultado_dea_2019_in_crs$eff
  
  # Crear dataframe con eficiencias y retorno a escala
  eficiencia_df <- data.frame(
    IdEstablecimiento = data$IdEstablecimiento,
    Nombre = data$'Nombre Establecimiento',
    Region = data$'Region',
    vrs = round(eficiencia_vrs, 3),
    crs = round(eficiencia_crs, 3),
    escala = round(eficiencia_vrs / eficiencia_crs, 3)
  )
  
  # ------------------------------------------------------------------- #
  # Ordenar dataframes según diferentes columnas
  #eficiencia_vrs_data <- eficiencia_df[order(-eficiencia_df$vrs), ]
  #eficiencia_crs_data <- eficiencia_df[order(-eficiencia_df$crs), ]
  #eficiencia_escala_data <- eficiencia_df[order(eficiencia_df$escala), ]
  # ------------------------------------------------------------------- #
  
  # Clasificar eficiencia VRS
  clasificacion_vrs <- cut(eficiencia_vrs, 
                           breaks = c(-Inf, 0.5, 1, 1.5, Inf), 
                           labels = c("Menor que 0.5", "Entre 0.5 y 1", "Entre 1 y 1.5", "Mayor que 1.5"),
                           right = FALSE)
  
  # Clasificar eficiencia CRS
  clasificacion_crs <- cut(eficiencia_crs, 
                           breaks = c(-Inf, 0.5, 1, 1.5, Inf),
                           labels = c("Menor que 0.5", "Entre 0.5 y 1", "Entre 1 y 1.5", "Mayor que 1.5"),
                           right = FALSE)
  
  # Calcular frecuencia y porcentaje para VRS
  frecuencia_vrs_clasificada <- table(clasificacion_vrs)
  porcentaje_vrs_clasificada <- prop.table(frecuencia_vrs_clasificada) * 100
  
  # Calcular frecuencia y porcentaje para CRS
  frecuencia_crs_clasificada <- table(clasificacion_crs)
  porcentaje_crs_clasificada <- prop.table(frecuencia_crs_clasificada) * 100
  
  # Mostrar resultados
  cat("Clasificación de eficiencia en VRS:\n")
  print(frecuencia_vrs_clasificada)
  cat("----------------------------------\n")
  cat("Porcentaje de eficiencia en VRS:\n")
  print(porcentaje_vrs_clasificada)
  
  cat("----------------------------------\n")
  cat("----------------------------------\n")
  
  cat("\nClasificación de eficiencia en CRS:\n")
  print(frecuencia_crs_clasificada)
  cat("----------------------------------\n")
  cat("Porcentaje de eficiencia en CRS:\n")
  print(porcentaje_crs_clasificada)
  
  # Retornar los dataframes ordenados como una lista
  return(eficiencia_df)
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
