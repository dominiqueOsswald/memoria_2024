library(readxl)
library(dplyr)
library(Benchmarking)
library(corrplot)


setwd("/home/domi/Escritorio/Memoria/Taller - anio 2019")
source("funciones.R")
source("data.R")
source("graphics.R")


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
resultados_geo <- resultados$eficiencia_vrs_data %>%
  left_join(hospitales, by = "IdEstablecimiento")

graficar_hospitales_vrs(resultados_geo)
graficar_hospitales_crs(resultados_geo)

graficar_hospitales_vrs_rm(resultados_geo)





















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




# ------ 
# Ya que la correlación de los datos es muy cercana a 1, se trabajará utilizando el dataframe con todos los datos

eficiencia_vrs_data_2019 <- resultados$eficiencia_vrs_data 
print(eficiencia_vrs_data_2019)
analisis_dea(all_2019)





