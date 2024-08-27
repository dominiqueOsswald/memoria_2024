library(readxl)
library(dplyr)
library(Benchmarking)
library(corrplot)

setwd("/home/domi/Escritorio/Seminario/Taller")
#getwd()

hospitales <- read.csv("hospital_region.csv") 

#PREDICCIONES GRD
predicciones_grd_2019 <- read.csv("Predicion GRD/prediciones_grd_2019.txt", sep="," )


#DATA CONSOLIDADA 2019

consultas <- list("idEstablecimiento","X03020101","X03020201","X03020301","X03020402","X03020403","X03020401","X03040210","X03040220","X04040100","X04025010","X04025020","X04025025","X04040427","X03020501")
data <- read.table("data2019.csv", sep=";", header=TRUE)

nuevo_data <- subset(data, select = unlist(consultas))

nuevo_data$sumaTotal <- rowSums(nuevo_data[ , -which(names(nuevo_data) == "idEstablecimiento")], na.rm = TRUE)


# Calcular la suma de los valores de cada fila, excluyendo la columna idEstablecimiento
suma_total <- rowSums(nuevo_data[ , -which(names(nuevo_data) == "idEstablecimiento")], na.rm = TRUE)




#data_consultas <- data  
#left_join(financiero_2019, dias_cama_ocupadas_2019 %>% select(IdEstablecimiento,dias_cama_ocupados), by = "IdEstablecimiento")

#ESTADISTICAS DE ESTABLECIMIENTOS
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
consultas_2019 <- data.frame(idEstablecimiento = nuevo_data$idEstablecimiento, Consultas = suma_total) %>% rename("IdEstablecimiento" = "idEstablecimiento")

# -------------------
#INPUTS
input_2019 <- left_join(financiero_2019, dias_cama_ocupadas_2019 %>% select(IdEstablecimiento,dias_cama_ocupados), by = "IdEstablecimiento")
#OUTPUTS
output_2019 <- left_join(egresos_2019 %>% inner_join(predicciones_grd_2019, by = "IdEstablecimiento") %>% mutate(Egresos.GRD = Prediction * egresos) %>% select("Nombre SS/SEREMI",IdEstablecimiento, "Nombre Establecimiento", Egresos.GRD), consultas_2019, by = "IdEstablecimiento")

#output_2019 <- egresos_2019 %>% inner_join(predicciones_grd_2019, by = "IdEstablecimiento") %>% mutate(Egresos.GRD = Prediction * egresos) %>% select("Nombre SS/SEREMI",IdEstablecimiento, "Nombre Establecimiento", Egresos.GRD)

#VARIABLES
all_2019 <- inner_join(output_2019, input_2019, by = "IdEstablecimiento")  %>% left_join(hospitales %>% select(hospital_id,region), by = c("IdEstablecimiento" = "hospital_id"))


# ------> DEA <------
input_dea_2019 <- as.data.frame(all_2019[c(6,7,8)])
output_dea_2019 <- as.data.frame(all_2019[c(4,5)])

# Aplicando DEA
# Orientado a los inputs 
resultado_dea_2019_in_vrs <- dea(X = input_dea_2019, Y = output_dea_2019, RTS = "vrs", ORIENTATION = "in")
resultado_dea_2019_in_crs <- dea(X = input_dea_2019, Y = output_dea_2019, RTS = "crs", ORIENTATION = "in")


# Orientado a las salidas
#resultado_dea_2019_out_vrs <- dea(X = input_dea_2019, Y = output_dea_2019, RTS = "vrs", ORIENTATION = "out")
#resultado_dea_2019_out_crs <- dea(X = input_dea_2019, Y = output_dea_2019, RTS = "crs", ORIENTATION = "out")

eficiencia_vrs <- resultado_dea_2019_in_vrs$eff
eficiencia_crs <- resultado_dea_2019_in_crs$eff

# Crear un dataframe con las dos eficiencias
eficiencia_df <- data.frame(
  ID = all_2019$IdEstablecimiento,
  Nombre = all_2019$'Nombre Establecimiento',
  Region = all_2019$'Nombre SS/SEREMI',
  vrs = resultado_dea_2019_in_vrs$eff,
  crs = resultado_dea_2019_in_crs$eff
)

# Mostrar el dataframe resultante
print(eficiencia_df)


# Ordenar el dataframe en orden decreciente según la columna 'vrs'
eficiencia_vrs_data <- eficiencia_df[order(-eficiencia_df$vrs), ]

# Mostrar el dataframe ordenado
print(eficiencia_vrs_data)


# Ordenar el dataframe en orden decreciente según la columna 'crs'
eficiencia_crs_data <- eficiencia_df[order(-eficiencia_df$crs), ]

# Mostrar el dataframe ordenado
print(eficiencia_crs_data)



# Clasificar eficiencia VRS en los rangos especificados
clasificacion_vrs <- cut(eficiencia_vrs, 
                         breaks = c(-Inf, 0.5, 0.75, 1, Inf), 
                         labels = c("Menor que 0.5", "Entre 0.5 y 0.75", "Entre 0.75 y 1", "Valor 1"),
                         right = FALSE)


# Clasificar eficiencia CRS en los rangos especificados
clasificacion_crs <- cut(eficiencia_crs, 
                         breaks = c(-Inf, 0.5, 0.75, 1, Inf), 
                         labels = c("Menor que 0.5", "Entre 0.5 y 0.75", "Entre 0.75 y 1", "Valor 1"),
                         right = FALSE)



# Calcular la frecuencia y el porcentaje para cada categoría en VRS
frecuencia_vrs_clasificada <- table(clasificacion_vrs)
porcentaje_vrs_clasificada <- prop.table(frecuencia_vrs_clasificada) * 100

# Calcular la frecuencia y el porcentaje para cada categoría en CRS
frecuencia_crs_clasificada <- table(clasificacion_crs)
porcentaje_crs_clasificada <- prop.table(frecuencia_crs_clasificada) * 100


# Mostrar resultados para VRS
print("Clasificación de eficiencia en VRS:")
print(frecuencia_vrs_clasificada)
print("Porcentaje de eficiencia en VRS:")
print(porcentaje_vrs_clasificada)

# Mostrar resultados para CRS
print("Clasificación de eficiencia en CRS:")
print(frecuencia_crs_clasificada)
print("Porcentaje de eficiencia en CRS:")
print(porcentaje_crs_clasificada)