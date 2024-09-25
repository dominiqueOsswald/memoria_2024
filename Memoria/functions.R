library(readxl)
library(dplyr)
library(Benchmarking)
library(corrplot)


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
  
  #setwd("..")
  estadisticas <- read_excel(path_estadisticas, sheet = 6, skip = 2)  %>% 
    rename("IdEstablecimiento" = "Cód. Estab.", "Region" = "Nombre SS/SEREMI") %>%
    filter(`Nombre Nivel Cuidado` == "Datos Establecimiento") %>% 
    select(-"Cód. Nivel Cuidado", -"Cód. SS/SEREMI", -"Nombre Nivel Cuidado") %>%  
    semi_join(predicciones_grd, by = "IdEstablecimiento") %>%
    select(1:5)
  
  dias_cama_disponibles <- estadisticas %>% filter(Glosa == "Dias Cama Disponibles") %>%  
    select(1:5) %>% rename("dias_cama_disponible" = "Acum") %>% select(-Glosa)
  
  egresos <- estadisticas %>% filter(Glosa == "Numero de Egresos") %>%  
    select(1:5) %>% rename("egresos" = "Acum") %>% select(-Glosa)
  
  consultas <- list("idEstablecimiento","X07020130","X07020230","X07020330","X07020331","X07020332","X07024219",
                    "X07020500","X07020501","X07020600","X07020601","X07020700","X07020800","X07020801",
                    "X07020900","X07020901","X07021000","X07021001","X07021100","X07021101","X07021230",
                    "X07021300","X07021301","X07022000","X07022001","X07021531","X07022132","X07022133",
                    "X07022134","X07021700","X07021800","X07021801","X07021900","X07022130","X07022142",
                    "X07022143","X07022144","X07022135","X07022136","X07022137","X07022700","X07022800",
                    "X07022900","X07021701","X07023100","X07023200","X07023201","X07023202","X07023203",
                    "X07023700","X07023701","X07023702","X07023703","X07024000","X07024001","X07024200",
                    "X07030500","X07024201","X07024202","X07030501","X07030502")
  consultas_data <- subset(datos_consolidados, select = unlist(consultas))
  consultas_data$sumaTotal <- rowSums(consultas_data[ , -which(names(consultas_data) == "idEstablecimiento")], na.rm = TRUE)
  
  consultas <- data.frame(idEstablecimiento = consultas_data$idEstablecimiento, 
                          Consultas = consultas_data$sumaTotal) %>%
    rename("IdEstablecimiento" = "idEstablecimiento")
  
  consultas <- consultas %>% inner_join(predicciones_grd, by = "IdEstablecimiento") %>% 
    mutate(Consultas.GRD = Prediction * Consultas) %>% 
    select(IdEstablecimiento, Consultas.GRD)
  
  dias_cama_ocupadas <- estadisticas %>% filter(Glosa == "Dias Cama Ocupados") %>%  
    select(1:5) %>% rename("dias_cama_ocupados" = "Acum") %>% select(-Glosa)
  
  dias_cama_ocupadas <- dias_cama_ocupadas %>% 
    inner_join(predicciones_grd, by = "IdEstablecimiento") %>% 
    mutate(dias_cama_ocupados.GRD = Prediction * dias_cama_ocupados) %>% 
    select(IdEstablecimiento, dias_cama_ocupados.GRD)
  
  # Unir egresos y predicciones GRD
  intermediate_df <- egresos %>%
    inner_join(predicciones_grd, by = "IdEstablecimiento") %>%
    mutate(Egresos.GRD = Prediction * egresos) %>%
    select("Region", IdEstablecimiento, "Nombre Establecimiento", Egresos.GRD)
  
  input <- left_join(financiero, dias_cama_disponibles %>% 
                       select(IdEstablecimiento, dias_cama_disponible), by = "IdEstablecimiento")
  
  output <- intermediate_df %>%
    left_join(consultas, by = "IdEstablecimiento") %>%
    left_join(dias_cama_ocupadas, by = "IdEstablecimiento")
  
  all <- inner_join(output, input, by = "IdEstablecimiento") %>%
    left_join(hospitales %>% select(IdEstablecimiento, region_id), by = "IdEstablecimiento") %>%
    relocate(region_id, .after = Region)
  
  return(all)
}

analisis_dea <- function(data) {
  
  # Preparar inputs y outputs
  input_dea_2019 <- as.data.frame(data[c(8,9,10)])
  output_dea_2019 <- as.data.frame(data[c(5,6,7)])
  
  # Aplicar DEA orientado a los inputs
  resultado_dea_2019_in_vrs <- dea(X = input_dea_2019, Y = output_dea_2019, RTS = "vrs", ORIENTATION = "in")
  resultado_dea_2019_in_crs <- dea(X = input_dea_2019, Y = output_dea_2019, RTS = "crs", ORIENTATION = "in")
  
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
  
  # Ordenar dataframes según diferentes columnas
  eficiencia_vrs_data <- eficiencia_df[order(-eficiencia_df$vrs), ]
  eficiencia_crs_data <- eficiencia_df[order(-eficiencia_df$crs), ]
  eficiencia_escala_data <- eficiencia_df[order(eficiencia_df$escala), ]
  
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
  return(list(
    eficiencia_df = eficiencia_df,
    eficiencia_vrs_data = eficiencia_vrs_data,
    eficiencia_crs_data = eficiencia_crs_data,
    eficiencia_escala_data = eficiencia_escala_data
    
  ))
}
