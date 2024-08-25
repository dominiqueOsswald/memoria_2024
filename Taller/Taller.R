library(readxl)
library(dplyr)
library(Benchmarking)
library(corrplot)

setwd("/home/domi/Escritorio/Seminario/Taller")
getwd()

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

print(eficiencia_vrs)

eficiencia_crs <- resultado_dea_2019_in_crs$eff

efficiency_df_in_vrs_id <- data.frame(
  ID = all_2019$IdEstablecimiento,
  Nombre = all_2019$'Nombre Establecimiento',
  Region = all_2019$'Nombre SS/SEREMI',
  VRS = eficiencia_vrs
)

efficiency_df_in_crs_id <- data.frame(
  ID = all_2019$IdEstablecimiento,
  CRS = eficiencia_crs
)


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



# Crear un dataframe con los IDs y su clasificación en VRS y CRS
clasificacion_establecimientos <- data.frame(
  ID = all_2019$IdEstablecimiento,
  Nombre = all_2019$'Nombre Establecimiento',
  Region = all_2019$'Nombre SS/SEREMI',
  Clasificacion_VRS = clasificacion_vrs,
  Clasificacion_CRS = clasificacion_crs
)


# Listado de establecimientos con eficiencia en el rango "Valor 1" en VRS
establecimientos_vrs_valor1 <- subset(clasificacion_establecimientos, Clasificacion_VRS == "Valor 1")

# Listado de establecimientos con eficiencia en el rango "Entre 0.75 y 1" en VRS
establecimientos_vrs_075_1 <- subset(clasificacion_establecimientos, Clasificacion_VRS == "Entre 0.75 y 1")

# Listado de establecimientos con eficiencia en el rango "Entre 0.5 y 0.75" en VRS
establecimientos_vrs_05_075 <- subset(clasificacion_establecimientos, Clasificacion_VRS == "Entre 0.5 y 0.75")

# Listado de establecimientos con eficiencia en el rango "Menor que 0.5" en VRS
establecimientos_vrs_menor_05 <- subset(clasificacion_establecimientos, Clasificacion_VRS == "Menor que 0.5")


# Visualizar establecimientos con eficiencia Valor 1 en VRS
print("Establecimientos con eficiencia Valor 1 en VRS:")
print(establecimientos_vrs_valor1)

# Visualizar establecimientos con eficiencia entre 0.75 y 1 en VRS
print("Establecimientos con eficiencia entre 0.75 y 1 en VRS:")
print(establecimientos_vrs_075_1)

# Visualizar establecimientos con eficiencia entre 0.5 y 0.75 en VRS
print("Establecimientos con eficiencia entre 0.5 y 0.75 en VRS:")
print(establecimientos_vrs_05_075)

# Visualizar establecimientos con eficiencia menor que 0.5 en VRS
print("Establecimientos con eficiencia menor que 0.5 en VRS:")
print(establecimientos_vrs_menor_05)









# Ordenar por eficiencia VRS
tabla_vrs <- efficiency_df_in_vrs_id[order(efficiency_df_in_vrs_id$VRS, decreasing = TRUE), ]

# Ordenar por eficiencia CRS
tabla_crs <- efficiency_df_in_crs_id[order(efficiency_df_in_crs_id$CRS, decreasing = TRUE), ]


# Mostrar la tabla completa ordenada por VRS
print(tabla_vrs)

# Mostrar la tabla completa ordenada por CRS
print(tabla_crs)







# Crear un dataframe vacío para almacenar los IDs que coinciden
ids_coincidentes <- data.frame(
  ID = character(),  # Inicializa un dataframe vacío con columna de IDs
  Eficiencia_VRS = numeric(),
  Eficiencia_CRS = numeric(),
  stringsAsFactors = FALSE
)

# Recorrer la tabla de VRS y buscar coincidencias en CRS
for (i in 1:nrow(tabla_vrs)) {
  # Buscar el ID actual en la tabla CRS
  id_actual <- tabla_vrs$ID[i]
  
  # Encontrar el índice del ID en la tabla CRS
  posicion_crs <- which(tabla_crs$ID == id_actual)
  
  # Si el ID se encuentra en la tabla CRS
  if (length(posicion_crs) > 0) {
    # Comparar los valores de eficiencia en VRS y CRS
    if (tabla_vrs$VRS[i] == tabla_crs$CRS[posicion_crs]) {
      # Si coinciden, almacenar el ID y las eficiencias en el nuevo dataframe
      ids_coincidentes <- rbind(ids_coincidentes, data.frame(
        ID = id_actual,
        Eficiencia_VRS = tabla_vrs$VRS[i],
        Eficiencia_CRS = tabla_crs$CRS[posicion_crs]
      ))
    }
  }
}

# Mostrar el resultado
print(ids_coincidentes)



















efficiency_df_in <- data.frame(
  VRS = eficiencia_vrs,
  CRS = eficiencia_crs
)

# Visualizar el resultado
head(efficiency_df_in)
head(efficiency_df_in_names)


# Calcular la matriz de correlación
correlation_matrix <- cor(efficiency_df_in)

# Mostrar la matriz de correlación
print(correlation_matrix)

#En términos prácticos, una correlación de 0.5920349 sugiere que hay una relación moderada entre las dos variables que estás analizando, pero también implica que existen otros factores que podrían estar influyendo en las variaciones de las variables. La relación no es lo suficientemente fuerte como para afirmar que las variables están muy estrechamente ligadas.
