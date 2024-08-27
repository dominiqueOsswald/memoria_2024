library(readxl)
library(dplyr)
library(Benchmarking)
library(corrplot)


analisis_dea <- function(all_2019) {
  
  # Preparar inputs y outputs
  input_dea_2019 <- as.data.frame(all_2019[c(6,7,8)])
  output_dea_2019 <- as.data.frame(all_2019[c(4,5)])
  
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



setwd("/home/domi/Escritorio/Seminario/Taller")
#getwd()

#Hospitales
hospitales <- read.csv("hospital_region.csv") 

#Predicciones GRD
predicciones_grd_2019 <- read.csv("Predicion GRD/prediciones_grd_2019.txt", sep="," )

#Datos consolidados
data <- read.table("data2019.csv", sep=";", header=TRUE)

#Estadísticas de establecimientos 2019
datos_2019 <- read_excel("Consolidado estadísticas hospitalarias 2014-2021.xlsx", sheet = 6, skip = 2)  %>% rename("IdEstablecimiento" = "Cód. Estab.") %>% filter(`Nombre Nivel Cuidado` == "Datos Establecimiento") %>% select(-"Cód. Nivel Cuidado", -"Cód. SS/SEREMI", -"Nombre Nivel Cuidado") %>%  semi_join(predicciones_grd_2019, by = "IdEstablecimiento")
datos_2019 <- datos_2019[1:5]


# ----- INPUTS -----
#DATOS FINANCIEROS - sub 21 GASTOS DE PERSONAL ; sub 22 GASTOS DE BIENES Y SERVICIOS
financiero_2019 <- read.csv("financial_data_2019.csv") %>% select(hospital_id, X21_value, X22_value) %>% rename("IdEstablecimiento" = "hospital_id")
#CAMAS
dias_cama_ocupadas_2019 <-  datos_2019 %>% filter(Glosa == "Dias Cama Ocupados") %>%  select(1:5) %>% rename("dias_cama_ocupados" = "Acum") %>% select(-Glosa)


#----- OUTPUT -----
#EGRESOS
egresos_2019 <-  datos_2019 %>% filter(Glosa == "Numero de Egresos") %>%  select(1:5) %>% rename("egresos" = "Acum") %>% select(-Glosa) 

# Consultas médicas
consultas <- list("idEstablecimiento","X03020101","X03020201","X03020301","X03020402","X03020403","X03020401","X03040210","X03040220","X04040100","X04025010","X04025020","X04025025","X04040427","X03020501")
consultas_data_2019 <- subset(data, select = unlist(consultas))
consultas_data_2019$sumaTotal <- rowSums(consultas_data_2019[ , -which(names(consultas_data_2019) == "idEstablecimiento")], na.rm = TRUE)

consultas_2019 <- data.frame(idEstablecimiento = consultas_data_2019$idEstablecimiento, Consultas = consultas_data_2019$sumaTotal) %>% rename("IdEstablecimiento" = "idEstablecimiento")

# Multiplicando el GRD a la cantidad de consultas realizadas
consultas_2019 <- consultas_2019 %>% inner_join(predicciones_grd_2019, by = "IdEstablecimiento") %>% mutate(Consultas.GRD = Prediction * Consultas) %>% select(IdEstablecimiento, Consultas.GRD)

# -------------------
#INPUTS
input_2019 <- left_join(financiero_2019, dias_cama_ocupadas_2019 %>% select(IdEstablecimiento,dias_cama_ocupados), by = "IdEstablecimiento")
#OUTPUTS
output_2019 <- left_join(egresos_2019 %>% inner_join(predicciones_grd_2019, by = "IdEstablecimiento") %>% mutate(Egresos.GRD = Prediction * egresos) %>% select("Nombre SS/SEREMI",IdEstablecimiento, "Nombre Establecimiento", Egresos.GRD), consultas_2019, by = "IdEstablecimiento")

#output_2019 <- egresos_2019 %>% inner_join(predicciones_grd_2019, by = "IdEstablecimiento") %>% mutate(Egresos.GRD = Prediction * egresos) %>% select("Nombre SS/SEREMI",IdEstablecimiento, "Nombre Establecimiento", Egresos.GRD)

#VARIABLES
all_2019 <- inner_join(output_2019, input_2019, by = "IdEstablecimiento")  %>% left_join(hospitales %>% select(hospital_id,region), by = c("IdEstablecimiento" = "hospital_id"))

#  ------------------------------------------------------------------
# Todos los hospitales, aplicando multiplicación de GRD em ambos parametros de salida
resultados <- analisis_dea(all_2019)

#  ------------------------------------------------------------------

all_2019_2 <- all_2019 %>%  filter(Consultas.GRD != 0)

# Mostrar el dataframe resultante
print(all_2019_2)

# Todos los hospitales, aplicando multiplicación de GRD em ambos parametros de salida pero eliminando los los hospitales que tienen valor 0 en consultas
resultados_2 <- analisis_dea(all_2019_2)



# -----------------------------------------
# Dataframe que combina los resultados de ambos resultados

resultados_2019_vrs <- inner_join(
  resultados$eficiencia_vrs_data %>% select(ID, Nombre, Region, vrs_1 = vrs),
  resultados_2$eficiencia_vrs_data %>% select(ID, vrs_2 = vrs),
  by = "ID"
)




#eficiencia_df <- resultados$eficiencia_df
#eficiencia_vrs_data <- resultados$eficiencia_vrs_data
#eficiencia_crs_data <- resultados$eficiencia_crs_data
#eficiencia_escala_data <- resultados$eficiencia_escala_data


#print(eficiencia_df)
#print(eficiencia_vrs_data)
#print(eficiencia_crs_data)
#print(eficiencia_escala_data)

