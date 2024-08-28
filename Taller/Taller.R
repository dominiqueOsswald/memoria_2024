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
dias_cama_disponibles_2019 <-  datos_2019 %>% filter(Glosa == "Dias Cama Disponibles") %>%  select(1:5) %>% rename("dias_cama_disponible" = "Acum") %>% select(-Glosa)


#----- OUTPUT -----
#EGRESOS
egresos_2019 <-  datos_2019 %>% filter(Glosa == "Numero de Egresos") %>%  select(1:5) %>% rename("egresos" = "Acum") %>% select(-Glosa) 

# Consultas médicas
#consultas_A04 <- list("idEstablecimiento","X03020101","X03020201","X03020301","X03020402","X03020403","X03020401","X03040210","X03040220","X04040100","X04025010","X04025020","X04025025","X04040427","X03020501")
#consultas_A04 <- c("X03020702", "X04025030", "X04025040", "X04040200", "X04025050", "X04050100", "X03020604", "X04050110", "X04050120", "X03020908", "X03030250", "X03030270", "X03030280", "X03020806")

consultas <- list("idEstablecimiento","X07020130","X07020230","X07020330","X07020331","X07020332","X07024219","X07020500","X07020501","X07020600","X07020601","X07020700","X07020800","X07020801","X07020900","X07020901","X07021000","X07021001","X07021100","X07021101","X07021230","X07021300","X07021301","X07022000","X07022001","X07021531","X07022132","X07022133","X07022134","X07021700","X07021800","X07021801","X07021900","X07022130","X07022142","X07022143","X07022144","X07022135","X07022136","X07022137","X07022700","X07022800","X07022900","X07021701","X07023100","X07023200","X07023201","X07023202","X07023203","X07023700","X07023701","X07023702","X07023703","X07024000","X07024001","X07024200","X07030500","X07024201","X07024202","X07030501","X07030502")
consultas_data_2019 <- subset(data, select = unlist(consultas))
consultas_data_2019$sumaTotal <- rowSums(consultas_data_2019[ , -which(names(consultas_data_2019) == "idEstablecimiento")], na.rm = TRUE)
# consultas odontologicas "09400082","09400081","09230300","09400084"


consultas_2019 <- data.frame(idEstablecimiento = consultas_data_2019$idEstablecimiento, Consultas = consultas_data_2019$sumaTotal) %>% rename("IdEstablecimiento" = "idEstablecimiento")


# Multiplicando el GRD a la cantidad de consultas realizadas
consultas_2019 <- consultas_2019 %>% inner_join(predicciones_grd_2019, by = "IdEstablecimiento") %>% mutate(Consultas.GRD = Prediction * Consultas) %>% select(IdEstablecimiento, Consultas.GRD)


dias_cama_ocupadas_2019 <-  datos_2019 %>% filter(Glosa == "Dias Cama Ocupados") %>%  select(1:5) %>% rename("dias_cama_ocupados" = "Acum") %>% select(-Glosa)

# Multiplicando el GRD a la cantidad de consultas realizadas
dias_cama_ocupadas_2019 <- dias_cama_ocupadas_2019 %>% inner_join(predicciones_grd_2019, by = "IdEstablecimiento") %>% mutate(dias_cama_ocupados.GRD = Prediction * dias_cama_ocupados) %>% select(IdEstablecimiento, dias_cama_ocupados.GRD)


# -------------------
#INPUTS
input_2019 <- left_join(financiero_2019, dias_cama_disponibles_2019 %>% select(IdEstablecimiento,dias_cama_disponible), by = "IdEstablecimiento")
#OUTPUTS
#output_2019 <- left_join(egresos_2019 %>% inner_join(predicciones_grd_2019, by = "IdEstablecimiento") %>% mutate(Egresos.GRD = Prediction * egresos) %>% select("Nombre SS/SEREMI",IdEstablecimiento, "Nombre Establecimiento", Egresos.GRD), consultas_2019, dias_cama_ocupadas_2019, by = "IdEstablecimiento")
# Primero unir egresos_2019 y predicciones_grd_2019
intermediate_df <- egresos_2019 %>%
  inner_join(predicciones_grd_2019, by = "IdEstablecimiento") %>%
  mutate(Egresos.GRD = Prediction * egresos) %>%
  select("Nombre SS/SEREMI", IdEstablecimiento, "Nombre Establecimiento", Egresos.GRD)

# Luego hacer un left_join con consultas_2019
output_2019 <- intermediate_df %>%
  left_join(consultas_2019, by = "IdEstablecimiento") %>%
  left_join(dias_cama_ocupadas_2019, by = "IdEstablecimiento")

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


# Resumen estadístico de las columnas vrs_1 y vrs_2
summary(resultados_2019_vrs[c("vrs_1", "vrs_2")])


# Calcular la correlación entre vrs_1 y vrs_2
correlacion <- cor(resultados_2019_vrs$vrs_1, resultados_2019_vrs$vrs_2)

# Mostrar la correlación
print(correlacion)

# Cargar el paquete ggplot2 para la visualización
library(ggplot2)

# Crear un gráfico de dispersión
ggplot(resultados_2019_vrs, aes(x = vrs_1, y = vrs_2)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue", se = FALSE) +  # Línea de regresión lineal
  labs(title = "Correlación entre VRS_1 y VRS_2",
       x = "VRS 1",
       y = "VRS 2") +
  theme_minimal()





# Resultados que presenten una diferencia menor a 0,2 -> Para la lista de consultas_1
# Resultados que presenten una diferencia menor a 0,05 -> Para la lista de consultas
resultados_2019_vrs_filtrado <- resultados_2019_vrs %>%
  filter(abs(vrs_1 - vrs_2) <= 0.09)

all_2019_3 <- all_2019 %>%
  semi_join(resultados_2019_vrs_filtrado, by = c("IdEstablecimiento" = "ID"))


resultados_3 <- analisis_dea(all_2019_3)

# Segundo join con el tercer dataframe
resultados_2019_vrs_2 <- inner_join(
  resultados_2019_vrs,
  resultados_3$eficiencia_vrs_data %>% select(ID, vrs_3 = vrs),
  by = "ID"
)

# Calcular la matriz de correlación entre vrs_1, vrs_2 y vrs_3
correlacion <- cor(resultados_2019_vrs_2[, c("vrs_1", "vrs_2", "vrs_3")])

# Mostrar la matriz de correlación
print(correlacion)


# Convertir la matriz de correlación a un data.frame largo manualmente
#correlacion_long <- data.frame(
#  Var1 = rep(rownames(correlacion), times = ncol(correlacion)),
#  Var2 = rep(colnames(correlacion), each = nrow(correlacion)),
#  value = as.vector(correlacion)
#)

# Cargar ggplot2 para la visualización
#library(ggplot2)

# Crear el mapa de calor
#ggplot(data = correlacion_long, aes(Var1, Var2, fill = value)) +
#  geom_tile() +
#  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
#                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
#                       name = "Correlación") +
#  theme_minimal() +
#  labs(title = "Matriz de Correlación entre VRS_1, VRS_2 y VRS_3",
#       x = "Variables",
#       y = "Variables")




# ------ 
# Ya que la correlación de los datos es muy cercana a 1, se trabajará utilizando el dataframe con todos los datos

eficiencia_vrs_data_2019 <- resultados$eficiencia_vrs_data 
print(eficiencia_vrs_data_2019)
analisis_dea(all_2019)
