library(readxl)
library(dplyr)


setwd("/home/domi/Escritorio/Memoria/Taller - anio 2019")


#Código, región y nombre de todos los hospitales
hospitales <- read.csv("hospitals.csv")  %>% 
  rename("IdEstablecimiento" = "hospital_id")

#Predicciones GRD de todos los hospitales
predicciones_grd_2019 <- read.csv("Predicion GRD/prediciones_grd_2019.txt", sep="," )

#Datos específicos de todos los hospitales (todas las variables)
datos_consolidados_2019 <- read.table("data2019.csv", sep=";", header=TRUE)

#Estadísticas de hospitales 2019
datos_2019 <- read_excel("Consolidado estadísticas hospitalarias 2014-2021.xlsx", sheet = 6, skip = 2)  %>% 
  rename("IdEstablecimiento" = "Cód. Estab.") %>%
  rename("Region" = "Nombre SS/SEREMI") %>%
  filter(`Nombre Nivel Cuidado` == "Datos Establecimiento") %>% 
  select(-"Cód. Nivel Cuidado", -"Cód. SS/SEREMI", -"Nombre Nivel Cuidado") %>%  
  semi_join(predicciones_grd_2019, by = "IdEstablecimiento")

datos_2019 <- datos_2019[1:5]


# INPUTS 
#DATOS FINANCIEROS - sub 21 GASTOS DE PERSONAL ; sub 22 GASTOS DE BIENES Y SERVICIOS
financiero_2019 <- read.csv("financial_data_2019.csv") %>% select(hospital_id, X21_value, X22_value) %>% rename("IdEstablecimiento" = "hospital_id")

#CAMAS
dias_cama_disponibles_2019 <-  datos_2019 %>% filter(Glosa == "Dias Cama Disponibles") %>%  select(1:5) %>% rename("dias_cama_disponible" = "Acum") %>% select(-Glosa)

#----- OUTPUT -----
#EGRESOS
egresos_2019 <-  datos_2019 %>% filter(Glosa == "Numero de Egresos") %>%  select(1:5) %>% rename("egresos" = "Acum") %>% select(-Glosa) 

# Consultas médicas

consultas <- list("idEstablecimiento","X07020130","X07020230","X07020330","X07020331","X07020332","X07024219","X07020500","X07020501","X07020600","X07020601","X07020700","X07020800","X07020801","X07020900","X07020901","X07021000","X07021001","X07021100","X07021101","X07021230","X07021300","X07021301","X07022000","X07022001","X07021531","X07022132","X07022133","X07022134","X07021700","X07021800","X07021801","X07021900","X07022130","X07022142","X07022143","X07022144","X07022135","X07022136","X07022137","X07022700","X07022800","X07022900","X07021701","X07023100","X07023200","X07023201","X07023202","X07023203","X07023700","X07023701","X07023702","X07023703","X07024000","X07024001","X07024200","X07030500","X07024201","X07024202","X07030501","X07030502")
consultas_data_2019 <- subset(datos_consolidados_2019, select = unlist(consultas))
consultas_data_2019$sumaTotal <- rowSums(consultas_data_2019[ , -which(names(consultas_data_2019) == "idEstablecimiento")], na.rm = TRUE)
# consultas odontologicas "09400082","09400081","09230300","09400084"

# Consultas médicas
#consultas_A04 <- list("idEstablecimiento","X03020101","X03020201","X03020301","X03020402","X03020403","X03020401","X03040210","X03040220","X04040100","X04025010","X04025020","X04025025","X04040427","X03020501")
#consultas_A04 <- c("X03020702", "X04025030", "X04025040", "X04040200", "X04025050", "X04050100", "X03020604", "X04050110", "X04050120", "X03020908", "X03030250", "X03030270", "X03030280", "X03020806")




consultas_2019 <- data.frame(idEstablecimiento = consultas_data_2019$idEstablecimiento, Consultas = consultas_data_2019$sumaTotal) %>% rename("IdEstablecimiento" = "idEstablecimiento")


# Multiplicando el GRD a la cantidad de consultas realizadas
consultas_2019 <- consultas_2019 %>% inner_join(predicciones_grd_2019, by = "IdEstablecimiento") %>% mutate(Consultas.GRD = Prediction * Consultas) %>% select(IdEstablecimiento, Consultas.GRD)


dias_cama_ocupadas_2019 <-  datos_2019 %>% filter(Glosa == "Dias Cama Ocupados") %>%  select(1:5) %>% rename("dias_cama_ocupados" = "Acum") %>% select(-Glosa)

# Multiplicando el GRD a la cantidad de consultas realizadas
dias_cama_ocupadas_2019 <- dias_cama_ocupadas_2019 %>% inner_join(predicciones_grd_2019, by = "IdEstablecimiento") %>% mutate(dias_cama_ocupados.GRD = Prediction * dias_cama_ocupados) %>% select(IdEstablecimiento, dias_cama_ocupados.GRD)


# -------------------

#OUTPUTS
#output_2019 <- left_join(egresos_2019 %>% inner_join(predicciones_grd_2019, by = "IdEstablecimiento") %>% mutate(Egresos.GRD = Prediction * egresos) %>% select("Nombre SS/SEREMI",IdEstablecimiento, "Nombre Establecimiento", Egresos.GRD), consultas_2019, dias_cama_ocupadas_2019, by = "IdEstablecimiento")
# Primero unir egresos_2019 y predicciones_grd_2019
intermediate_df <- egresos_2019 %>%
  inner_join(predicciones_grd_2019, by = "IdEstablecimiento") %>%
  mutate(Egresos.GRD = Prediction * egresos) %>%
  select("Region", IdEstablecimiento, "Nombre Establecimiento", Egresos.GRD)
