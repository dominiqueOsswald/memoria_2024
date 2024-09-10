library(readxl)
library(dplyr)
library(Benchmarking)
library(corrplot)


analisis_dea <- function(all_2019) {
  
  # Preparar inputs y outputs
  input_dea_2019 <- as.data.frame(all_2019[c(7,8,9)])
  output_dea_2019 <- as.data.frame(all_2019[c(4,5,6)])
  
  # Aplicar DEA orientado a los inputs
  resultado_dea_2019_in_vrs <- dea(X = input_dea_2019, Y = output_dea_2019, RTS = "vrs", ORIENTATION = "in")
  resultado_dea_2019_in_crs <- dea(X = input_dea_2019, Y = output_dea_2019, RTS = "crs", ORIENTATION = "in")
  
  # Calcular eficiencias
  eficiencia_vrs <- resultado_dea_2019_in_vrs$eff
  eficiencia_crs <- resultado_dea_2019_in_crs$eff
  
  # Crear dataframe con eficiencias y retorno a escala
  eficiencia_df <- data.frame(
    ID = all_2019$IdEstablecimiento,
    Nombre = all_2019$'Nombre Establecimiento',
    Region = all_2019$'Nombre SS/SEREMI',
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