
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("main_orientacion.R")


# ==============================================
#    PRUEBAS ESTADISTICAS
# ==============================================


# ==============================================
#    VARIABLES
# ==============================================

# Lista de años disponibles
anios <- names(datos)

# Lista de columnas de interés
columnas_interes <- c("Egresos.GRD", "Consultas", "Quirofano", "X21_value", 
                      "X22_value", "dias_cama_disponible")

# TODOS los hospitales
ef_tec <- guardar_dataframe_por_columna(resultados_usar[[orientacion]], retorno)
ef_tec <- ef_tec[,-c(2,3)]

# Variables de interés
variables_interes <- c("X21_value", "X22_value")
# ==============================================
#    REVISAR LA DIFERENCIA ENTRE VARIABLES
# ==============================================



# ---- ALMACENANDO LAS VARIABLES DE ENTRADAS DEL MODELO

# Crear un listado de dataframes por cada columna
dataframes_por_columna <- lapply(columnas_interes, function(col) {
  df <- do.call(cbind, lapply(anios, function(anio) datos[[anio]][[col]]))
  colnames(df) <- anios
  df <- as.data.frame(df)
  return(df)
})

# Asignar nombres a los dataframes generados
names(dataframes_por_columna) <- columnas_interes




# ==============================================
#    EFICIENCIA
# ==============================================



normalidad_EF <- verificar_normalidad(ef_tec,c(2:11))
print(normalidad_EF)

# NO SON NORMALES LOS DATOS

all_ef <- analisis_eficiencia_tecnica(hospitals_long_ef)



path_hospitales_complejidades <- paste0("data/hospitales.csv")
hospitales_complejidades <- read.csv(path_hospitales_complejidades, sep=";" ) %>% rename("IdEstablecimiento" = "hospital_id")

for (year in 2014:2023) {
  resultados_usar[[orientacion]]$original[[as.character(year)]]$data <- resultados_usar[[orientacion]]$original[[as.character(year)]]$data %>%
    left_join(hospitales_complejidades %>% select(IdEstablecimiento, complejidad), 
              by = "IdEstablecimiento")
}


# ANÁLISIS DE EFICIENCIA A HOSPITALES
ef_tec_complejidades <- guardar_dataframe_por_columna(resultados_usar[[orientacion]], retorno)


# ALTA COMPLEJIDAD

ef_tec_alta <-  ef_tec_complejidades %>% filter(.data[["complejidad"]] == "Alta")


ef_tec_alta_long <- ef_tec_alta[,-c(2,3)]%>%
  pivot_longer(cols = -c(IdEstablecimiento), names_to = "Año", values_to = "Valor")


alta_ef <- analisis_eficiencia_tecnica(ef_tec_alta_long)


# MEDIANA COMPLEJIDAD

ef_tec_mediana <-  ef_tec_complejidades %>% filter(.data[["complejidad"]] == "Mediana")


ef_tec_mediana_long <- ef_tec_mediana[,-c(2,3)]%>%
  pivot_longer(cols = -c(IdEstablecimiento), names_to = "Año", values_to = "Valor")


mediana_ef <- analisis_eficiencia_tecnica(ef_tec_mediana_long)


# BAJA COMPLEJIDAD

ef_tec_baja <-  ef_tec_complejidades %>% filter(.data[["complejidad"]] == "Baja")


ef_tec_baja_long <- ef_tec_baja[,-c(2,3)]%>%
  pivot_longer(cols = -c(IdEstablecimiento), names_to = "Año", values_to = "Valor")


baja_ef <- analisis_eficiencia_tecnica(ef_tec_baja_long)









#######################################
#######################################
# GRAFICA COMPARATIVA DE COMPLEJIDADES

# Convertir a formato largo
df_long <- ef_tec_complejidades %>%
  pivot_longer(cols = starts_with("20"),  # Seleccionar columnas de los años
               names_to = "Año", 
               values_to = "Valor")

medianas <- df_long %>%
  group_by(Año, complejidad) %>%
  summarise(Mediana = median(Valor, na.rm = TRUE), .groups = "drop")

# Definir el ancho de desplazamiento
dodge_width <- 0.87  # Ajustar el desplazamiento para alinear mejor

# Definir colores personalizados para cada nivel de complejidad
colores_violin <- c("Alta" = brewer.pal(11, "RdYlGn")[9],   # Verde para Alta
                    "Mediana" = brewer.pal(11, "RdYlGn")[5], # Amarillo para Mediana
                    "Baja" = brewer.pal(11, "RdYlGn")[3])   # Rojo para Baja

# Crear el gráfico de violín con medianas alineadas y colores por complejidad
grafica_alta_med_baj <- ggplot(df_long, aes(x = factor(Año), y = Valor, fill = complejidad)) +
  geom_violin(trim = FALSE, alpha = 0.6, aes(color = complejidad, fill = complejidad), 
              position = position_dodge(dodge_width), width = 1.5) +  # Aumentar el ancho de los violines
  # Puntos de mediana en su violín respectivo
  geom_line(data = medianas, 
            aes(x = as.numeric(factor(Año)) + (as.numeric(factor(complejidad)) - 2) * 0.3, 
                y = Mediana, group = complejidad, color = complejidad), 
            size = 0.8, linetype = "dashed") +  # Línea de medianas punteada con color por complejidad
  geom_point(data = medianas, 
             aes(x = as.numeric(factor(Año)) + (as.numeric(factor(complejidad)) - 2) * 0.3, 
                 y = Mediana, color = complejidad), 
             size = 2) +  # Puntos en la línea de medianas
  labs(title = "Distribución de valores según Complejidad Hospitalaria",
       x = "Año", y = "Valor", 
       fill = "Complejidad Hospitalaria",  # Nombre de la leyenda para los violines
       color = "Complejidad Hospitalaria") +  # Nombre de la leyenda para las líneas
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.margin = unit(c(2, 2, 2, 2), "cm"),
    panel.spacing.x = unit(1.5, "lines")  # Aumentar el espacio entre los años
  ) +
  scale_fill_manual(values = colores_violin) +  # Asignar colores a los violines según complejidad
  scale_color_manual(values = colores_violin)   # Asignar colores a las líneas de medianas

print(grafica_alta_med_baj)
ggsave(paste0("distribucion_por_complejidad.jpg"), plot = grafica_alta_med_baj, width = 13, height = 5, dpi = 300)

# ==============================================
#    MALMQUIST
# ==============================================




mal_tec <- malmquist_indices[["index"]][,c(1:10)]

normalidad_malm <- verificar_normalidad(mal_tec,c(2:9))
print(normalidad_malm)






# REVISAR SI HAY DIFERENCIAS EN LAS MEDIANAS
df_long_malm <- mal_tec %>%        # Identificador del DMU
  pivot_longer(
    cols = starts_with("20"),          # Ajustar si tus columnas se llaman "2014", "2015", etc.
    names_to = "year",
    values_to = "index"
  ) %>%
  mutate(
    year = as.factor(year)  # o as.numeric si prefieres, pero factor para la prueba
  )

df_long_malm <- df_long_malm %>% rename(IdEstablecimiento = ID)

df_long_malm <- df_long_malm %>% rename(Año = year)
df_long_malm <- df_long_malm %>% rename(Valor = index)



# Aplicar la prueba de Kruskal-Wallis
resultado_kruskal_malm <- kruskal.test(Valor ~ Año, data = df_long_malm)

# Mostrar el resultado
print(resultado_kruskal_malm)

# RESULTADO:
# Kruskal-Wallis chi-squared = 454.43, df = 8, p-value < 2.2e-16



posthoc_wilcoxon_mal <- df_long_malm %>%
  pairwise_wilcox_test(Valor ~ Año, paired = TRUE, p.adjust.method = "holm")

# Mostrar resultados
print(n=50,posthoc_wilcoxon_mal)



posthoc_wilcoxon_mal_less <- df_long_malm %>%
  pairwise_wilcox_test(Valor ~ Año, paired = TRUE, p.adjust.method = "holm", alternative="less")

# Mostrar resultados
print(n=50,posthoc_wilcoxon_mal_less)






# ==============================================
#    DETERMINANTES
# ==============================================


# REVISAR SI HAY SIGNIFICANCIA POR AÑO EN DETERMINANTES:


df_incmse <- resultados_importancia[["df_incmse_10"]]
df_incmse_all <- df_incmse
df_incmse_pre <- df_incmse[,c(1:6)]
df_incmse_post <- df_incmse[,c(1,7:10)]


df_long_all_comp <- df_incmse_all %>% pivot_longer(-Variable, names_to = "Año", values_to = "Valor")
df_long_all_comp <- na.omit(df_long_all_comp)

# Calcular la mediana de los valores en el periodo "pre" (columnas 2 a 7)
df_median_pre <- df_incmse_pre %>%
  mutate(Mediana_Pre = apply(df_incmse_pre[, -1], 1, median, na.rm = TRUE)) %>%
  select(Variable, Mediana_Pre)  # Seleccionar solo Variable y la mediana

# Calcular la mediana de los valores en el periodo "post" (columnas 8 a 11)
df_median_post <- df_incmse_post %>%
  mutate(Mediana_Post = apply(df_incmse_post[, -1], 1, median, na.rm = TRUE)) %>%
  select(Variable, Mediana_Post)  # Seleccionar solo Variable y la mediana


# Unir ambos dataframes en un único dataframe con las medianas
df_median <- left_join(df_median_pre, df_median_post, by = "Variable")
df_median <- calcular_estadisticas(df_median) %>% filter(Frecuencia > 1) 
df_median <- df_median[,-c(3,4)]

df_incmse_all_est <- calcular_estadisticas(df_incmse_all) %>% filter(Frecuencia > 1)
df_incmse_all <- df_incmse_all_est[,c(1:11)]

df_incmse_pre_est <- calcular_estadisticas(df_incmse_pre) %>% filter(Frecuencia > 1) 
df_incmse_pre <- df_incmse_pre_est[,c(1:7)]

df_incmse_post_est <- calcular_estadisticas(df_incmse_post) %>% filter(Frecuencia > 1)
df_incmse_post <- df_incmse_post_est[,c(1:5)]




# Convertir a formato largo (tidy)
df_long_all <- df_incmse_all %>% pivot_longer(-Variable, names_to = "Año", values_to = "Valor")
df_long_all <- na.omit(df_long_all)

df_long_pre <- df_incmse_pre %>% pivot_longer(-Variable, names_to = "Año", values_to = "Valor")
df_long_pre <- na.omit(df_long_pre)

df_long_post <- df_incmse_post %>% pivot_longer(-Variable, names_to = "Año", values_to = "Valor")
df_long_post <- na.omit(df_long_post)

df_long_pre_post <- df_median %>% pivot_longer(-Variable, names_to = "Periodo", values_to = "Valor")
df_long_pre_post <- na.omit(df_long_pre_post)


# Aplicar Kruskal-Wallis para cada fila (variable)
kruskal_results_all <- df_long_all %>%
  group_by(Variable) %>%
  summarise(
    p_value = kruskal.test(Valor ~ Año)$p.value
  )


# Aplicar Kruskal-Wallis para cada fila (variable)
kruskal_results_pre <- df_long_pre %>%
  group_by(Variable) %>%
  summarise(
    p_value = kruskal.test(Valor ~ Año)$p.value
  )


# Aplicar Kruskal-Wallis para cada fila (variable)
kruskal_results_post <- df_long_post %>%
  group_by(Variable) %>%
  summarise(
    p_value = kruskal.test(Valor ~ Año)$p.value
  )

# Aplicar Kruskal-Wallis para cada fila (variable)
kruskal_results_pre_post <- df_long_pre_post %>%
  group_by(Variable) %>%
  summarise(
    p_value = kruskal.test(Valor ~ Periodo)$p.value
  )



print(n=100,kruskal_results_all)
print(n=100,kruskal_results_pre)
print(n=100,kruskal_results_post)
print(n=100,kruskal_results_pre_post)


df_long_kruskal_all <- df_long_all %>% filter(Variable %in% kruskal_results_all$Variable)
df_long_kruskal_pre <- df_long_pre %>% filter(Variable %in% kruskal_results_pre$Variable)
df_long_kruskal_post <- df_long_post %>% filter(Variable %in% kruskal_results_post$Variable)

# Gráfico de evolución de las variables analizadas










grafica <- ggplot(df_long_all_comp, aes(x = Año, y = Variable, fill = Valor)) +
  geom_tile() +
  theme_bw() +  # Cambiar a theme_bw() para fondo blanco
  labs(title = "Matriz de valores por variable y año",
       x = "Año", y = "Variable", fill = "Valor") +
  scale_fill_gradientn(
    colors = brewer.pal(11, "RdYlGn"),  # Paleta de colores RdYlGn
    limits = range(df_long_all_comp$Valor, na.rm = TRUE)  # Escala de valores automática
  ) +
  theme(
    panel.background = element_blank(),  # Fondo del panel en blanco
    plot.background = element_blank(),   # Fondo del gráfico en blanco
    panel.grid.major = element_blank(),  # Eliminar líneas de la cuadrícula principales
    panel.grid.minor = element_blank()   # Eliminar líneas de la cuadrícula secundarias
  )


ggsave(paste0("determinantes.jpg"), plot = grafica, width = 15, height = 20, dpi = 500)



grafica <- ggplot(df_long_kruskal_all, aes(x = Año, y = Variable, fill = Valor)) +
  geom_tile() +
  theme_minimal() +
  labs(title = "Matriz de valores por variable y año",
       x = "Año", y = "Variable", fill = "Valor") +
  scale_fill_gradientn(
    colors = brewer.pal(11, "RdYlGn"),  # Paleta de colores RdYlGn
    limits = range(df_long_kruskal_all$Valor, na.rm = TRUE)  # Escala de valores automática
  )



ggsave(paste0("determinantes","_","todos",".jpg"), plot = grafica, width = 10, height = 8, dpi = 300)



grafica <- ggplot(df_long_kruskal_pre, aes(x = Año, y = Variable, fill = Valor)) +
  geom_tile() +
  theme_minimal() +
  labs(title = "Matriz de valores por variable y año",
       x = "Año", y = "Variable", fill = "Valor") +
  scale_fill_gradientn(
    colors = brewer.pal(11, "RdYlGn"),  # Paleta de colores RdYlGn
    limits = range(df_long_kruskal_pre$Valor, na.rm = TRUE)  # Escala de valores automática
  )



ggsave(paste0("determinantes","_","pre",".jpg"), plot = grafica, width = 10, height = 8, dpi = 300)





grafica <- ggplot(df_long_kruskal_post, aes(x = Año, y = Variable, fill = Valor)) +
  geom_tile() +
  theme_minimal() +
  labs(title = "Matriz de valores por variable y año",
       x = "Año", y = "Variable", fill = "Valor") +
  scale_fill_gradientn(
    colors = brewer.pal(11, "RdYlGn"),  # Paleta de colores RdYlGn
    limits = range(df_long_kruskal_post$Valor, na.rm = TRUE)  # Escala de valores automática
  )



ggsave(paste0("determinantes","_","post",".jpg"), plot = grafica, width = 10, height = 8, dpi = 300)




df_incmse_est <- resultados_importancia[["df_incmse_est_10"]]



df_incmse_est <- df_incmse_est %>%
  left_join(kruskal_results_all, by = "Variable") %>%
  rename(P_value_all = p_value)  # Cambia el nombre de la columna

df_incmse_est <- df_incmse_est %>%
  left_join(kruskal_results_pre, by = "Variable") %>%
  rename(P_value_pre = p_value)  # Cambia el nombre de la columna

df_incmse_est <- df_incmse_est %>%
  left_join(kruskal_results_post, by = "Variable") %>%
  rename(P_value_post = p_value)  # Cambia el nombre de la columna



























# Almacenar resultados en excel
guardar_resultados(
  dataframes = resultados_usar[[orientacion]],
  retorno,
  df_incmse_est,
  malmquist = malmquist_indices$index,
  carpeta=paste0("results/",columna2),
  archivo_salida = "RESULTADOS.xlsx",
  prefijo = orientacion
)



# Gráfico de evolución de las 10 variables
#ggplot(df_long_10, aes(x = Año, y = Valor, group = Variable, color = Variable)) +
#  geom_line() + 
#  geom_point() + 
#  theme_minimal() +
#  labs(title = "Evolución de las primeras 10 variables (2014-2023)",
#       x = "Año", y = "Valor de la Variable")

