library(readxl)
library(dplyr)
library(Benchmarking)
library(corrplot)


consolidar_datos_por_anio <- function(anio) {
  # Establecer directorio de trabajo
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  
  # Definir rutas de archivos utilizando el año como variable
  path_hospitales <- paste0("data/", anio, "/", anio, "_hospitals.csv")
  print("-")
  path_predicciones_grd <- paste0("data/", anio, "/", anio, "_prediciones_grd.txt")
  print("-")
  path_datos_consolidados <- paste0("data/", anio, "/", anio, "_consolidated_data.csv")
  print("-")
  path_financiero <- paste0("data/", anio, "/", anio, "_financial_data.csv")
  print("-")
  path_estadisticas <- "data/Consolidado estadísticas hospitalarias 2014-2021.xlsx"
  print("1")
  # Cargar datos
  hospitales <- read.csv(path_hospitales) %>% rename("IdEstablecimiento" = "hospital_id")
  predicciones_grd <- read.csv(path_predicciones_grd, sep=",")
  datos_consolidados <- read.table(path_datos_consolidados, sep=";", header=TRUE)
  financiero <- read.csv(path_financiero) %>% 
    select(hospital_id, X21_value, X22_value) %>% rename("IdEstablecimiento" = "hospital_id")
  financiero$X21_value <- as.numeric(financiero$X21_value)
  financiero$X22_value <- as.numeric(financiero$X22_value)
  print("2")
  estadisticas <- read_excel(path_estadisticas, sheet = 6, skip = 2)  %>% 
    rename("IdEstablecimiento" = "Cód. Estab.", "Region" = "Nombre SS/SEREMI") %>%
    filter(`Nombre Nivel Cuidado` == "Datos Establecimiento") %>% 
    select(-"Cód. Nivel Cuidado", -"Cód. SS/SEREMI", -"Nombre Nivel Cuidado") %>%  
    semi_join(predicciones_grd, by = "IdEstablecimiento") %>%
    select(1:5)
  print("3")  
  # Procesar estadísticas
  dias_cama_disponibles <- estadisticas %>% 
    filter(Glosa == "Dias Cama Disponibles") %>%  
    select(1:5) %>% rename("dias_cama_disponible" = "Acum") %>% select(-Glosa)
  
  egresos <- estadisticas %>% 
    filter(Glosa == "Numero de Egresos") %>%  
    select(1:5) %>% rename("egresos" = "Acum") %>% select(-Glosa)
  print("4")
  # Definir consultas
  consultas <- list("idEstablecimiento", "X07020130", "X07020230", "X07020330", "X07020331", 
                    "X07020332", "X07024219", "X07020500", "X07020501", "X07020600", 
                    "X07020601", "X07020700", "X07020800", "X07020801", "X07020900", 
                    "X07020901", "X07021000", "X07021001", "X07021100", "X07021101", 
                    "X07021230", "X07021300", "X07021301", "X07022000", "X07022001", 
                    "X07021531", "X07022132", "X07022133", "X07022134", "X07021700", 
                    "X07021800", "X07021801", "X07021900", "X07022130", "X07022142", 
                    "X07022143", "X07022144", "X07022135", "X07022136", "X07022137", 
                    "X07022700", "X07022800", "X07022900", "X07021701", "X07023100", 
                    "X07023200", "X07023201", "X07023202", "X07023203", "X07023700", 
                    "X07023701", "X07023702", "X07023703", "X07024000", "X07024001", 
                    "X07024200", "X07030500", "X07024201", "X07024202", "X07030501", 
                    "X07030502")
  
  print("5")
  # Seleccionar y convertir columnas válidas
  columnas_validas <- intersect(unlist(consultas), colnames(datos_consolidados))
  print("5")
  str(columnas_validas)
  str(datos_consolidados)
  consultas_data <- subset(datos_consolidados, select = columnas_validas)
  print("5")
  # Identificar columnas tipo character
  cols_char <- sapply(consultas_data, is.character)
  
  # Convertir columnas character a numeric
  consultas_data[, cols_char] <- lapply(consultas_data[, cols_char], function(x) as.numeric(x))
  print("6")
  # Crear suma total de consultas
  consultas_data$sumaTotal <- rowSums(consultas_data[, -which(names(consultas_data) == "idEstablecimiento")], na.rm = TRUE)
  
  consultas <- data.frame(idEstablecimiento = consultas_data$idEstablecimiento, 
                          Consultas = consultas_data$sumaTotal) %>%
    rename("IdEstablecimiento" = "idEstablecimiento") %>%
    inner_join(predicciones_grd, by = "IdEstablecimiento") %>%
    select(IdEstablecimiento, Consultas)
  print("7")
  # Definir quirofano
  quirofano <- list("idEstablecimiento", "X21220100", "X21220200", "X21220700", "X21220600")
  quirofano_data <- subset(datos_consolidados, select = unlist(quirofano))
  
  # Reemplazar comas por puntos y convertir a numérico
  quirofano_data <- quirofano_data %>%
    mutate(across(-idEstablecimiento, ~ as.numeric(gsub(",", ".", .))))
  print("8")
  # Crear suma total de quirofano
  quirofano_data$sumaTotal <- rowSums(select(quirofano_data, -idEstablecimiento), na.rm = TRUE)
  
  quirofano <- data.frame(idEstablecimiento = quirofano_data$idEstablecimiento, 
                          Quirofano = quirofano_data$sumaTotal) %>%
    rename("IdEstablecimiento" = "idEstablecimiento") %>%
    inner_join(predicciones_grd, by = "IdEstablecimiento") %>%
    select(IdEstablecimiento, Quirofano)
  print("9")
  # Procesar egresos y predicciones GRD
  intermediate_df <- egresos %>%
    inner_join(predicciones_grd, by = "IdEstablecimiento") %>%
    mutate(Egresos.GRD = Prediction * egresos) %>%
    select("Region", IdEstablecimiento, "Nombre Establecimiento", Egresos.GRD)
  
  # Combinar datos financieros y días cama disponibles
  input <- left_join(financiero, dias_cama_disponibles %>% 
                       select(IdEstablecimiento, dias_cama_disponible), by = "IdEstablecimiento")
  print("10")
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
  # eficiencia_vrs_data <- eficiencia_df[order(-eficiencia_df$vrs), ]
  # eficiencia_crs_data <- eficiencia_df[order(-eficiencia_df$crs), ]
  # eficiencia_escala_data <- eficiencia_df[order(eficiencia_df$escala), ]
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
  return(eficiencia_df)
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


comparativa <- function(resultados_2014_in, resultados_2015_in, resultados_2017_in, resultados_2018_in, resultados_2019_in) {
  
  resultados_2014_in_df <- data.frame(IdEstablecimiento = resultados_2014_in$IdEstablecimiento, vrs = resultados_2014_in$vrs)
  resultados_2015_in_df <- data.frame(IdEstablecimiento = resultados_2015_in$IdEstablecimiento, vrs = resultados_2015_in$vrs)
  resultados_2017_in_df <- data.frame(IdEstablecimiento = resultados_2017_in$IdEstablecimiento, vrs = resultados_2017_in$vrs)
  resultados_2018_in_df <- data.frame(IdEstablecimiento = resultados_2018_in$IdEstablecimiento, vrs = resultados_2018_in$vrs)
  resultados_2019_in_df <- data.frame(IdEstablecimiento = resultados_2019_in$IdEstablecimiento, vrs = resultados_2019_in$vrs)
  
  # Luego, aplicar reduce y full_join para combinar todos los dataframes por 'IdEstablecimiento'
  resultados_combinados <- reduce(list(resultados_2014_in_df, resultados_2015_in_df, 
                                       resultados_2017_in_df, resultados_2018_in_df, 
                                       resultados_2019_in_df), 
                                  function(x, y) full_join(x, y, by = "IdEstablecimiento"))
  
  # Renombrar las columnas para indicar el año
  colnames(resultados_combinados)[-1] <- c("vrs_2014", "vrs_2015", "vrs_2017", "vrs_2018", "vrs_2019")
  
  # Visualizar los resultados combinados
  print(resultados_combinados)
  
  resultados_combinados <- resultados_combinados %>%
    mutate(
      diff_2014_2015 = vrs_2015 - vrs_2014,
      diff_2015_2017 = vrs_2017 - vrs_2015,
      diff_2017_2018 = vrs_2018 - vrs_2017,
      diff_2018_2019 = vrs_2019 - vrs_2018
    )
  
  # Calcular el promedio de las diferencias por fila, ignorando NA si existen
  resultados_combinados <- resultados_combinados %>%
    mutate(
      avg_diff = rowMeans(select(., diff_2014_2015, diff_2015_2017, diff_2017_2018, diff_2018_2019), na.rm = TRUE)
    )
  
  # Visualizar los resultados
  print(resultados_combinados)
  
  
}