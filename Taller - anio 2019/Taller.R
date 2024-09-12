library(readxl)
library(dplyr)
library(Benchmarking)
library(corrplot)


setwd("/home/domi/Escritorio/Memoria/Taller - anio 2019")
source("funciones.R")
source("data.R")


# ----- INPUTS -----
#DATOS FINANCIEROS - sub 21 GASTOS DE PERSONAL ; sub 22 GASTOS DE BIENES Y SERVICIOS
#CAMAS
input_2019 <- left_join(financiero_2019, dias_cama_disponibles_2019 %>% select(IdEstablecimiento,dias_cama_disponible), by = "IdEstablecimiento")

# ----- OUTPUTS -----
# EGRESOS
# CONSULTAS
# DIAS DE CAMAS OCUPADAS
output_2019 <- intermediate_df %>%
  left_join(consultas_2019, by = "IdEstablecimiento") %>%
  left_join(dias_cama_ocupadas_2019, by = "IdEstablecimiento")


#VARIABLES
all_2019 <- inner_join(output_2019, input_2019, by = "IdEstablecimiento")  %>% left_join(hospitales %>% select(hospital_id,region), by = c("IdEstablecimiento" = "hospital_id"))

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





