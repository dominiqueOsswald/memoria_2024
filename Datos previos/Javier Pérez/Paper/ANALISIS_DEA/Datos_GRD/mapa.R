library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(plotly)
library(chilemapas)


data_hospitales = read.csv(file = "Hospitales.csv",sep=";",header=T)
grd_2014 = read.csv("prediciones_grd_2020.txt",sep=",",header=T)


# Cargar el mapa de Chile usando rnaturalearth
chile_map = ne_countries(scale = "medium", country = "Chile", returnclass = "sf")

# Coordenadas de Santiago
hospitales = data.frame(lon = data_hospitales$LON, lat=data_hospitales$LAT)
hospitales = st_as_sf(hospitales, coords = c("lon", "lat"), crs = 4326)



#==============================================
# Hospitales en Chile
#==============================================
tamano_hospitales = rep(2,length(data_hospitales$ID))
tamano_hospitales[which(data_hospitales$COM=="Mediana Complejidad")] = 4
tamano_hospitales[which(data_hospitales$COM=="Alta Complejidad")] = 6
grd_2014$OriginalGRD = as.numeric(replace(grd_2014$OriginalGRD, grd_2014$OriginalGRD == "-", NA))
hospitales$GRD_real = grd_2014$OriginalGRD
hospitales$GRD_pred = grd_2014$Prediction
hospitales$tamano_hospitales=tamano_hospitales
hospitales$REGIONES_ID = data_hospitales$REGIONES_ID

g=ggplot(data = chile_map) +
  geom_sf(fill = "gray") + 
  geom_sf(data = hospitales, aes(size = as.factor(tamano_hospitales), fill = GRD_pred), shape = 21,alpha=1) +
  coord_sf(xlim = c(-76, -66), ylim = c(-56, -17), expand = FALSE) +
  scale_fill_gradientn(colors = c("aquamarine4", "gold", "firebrick4"),
                       values = scales::rescale(c(0.5, 1.05, 1.6)),
                       na.value = "black",
                       limits = c(0.5, 1.6)) +
  theme_minimal() +
  theme(text = element_text(size = 18),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.title.x = element_blank(), 
        axis.title.y = element_blank(), axis.text.x = element_blank(), 
        axis.text.y = element_blank()) + labs(fill = "CMI index", size = "Hospital Complexity (MINSAL)") +
  scale_size_manual(values = c(2, 4, 6),
                    breaks = c(2, 4, 6),
                    labels = c("Low complexity", "Medium complexity", "High complexity"))
plot(g)

 
# Cargar el mapa de Chile usando rnaturalearth
 mapa_santiago = mapa_comunas %>% 
   filter((codigo_region == "13")) 
 
 hospitales = hospitales %>% filter((REGIONES_ID==13))
 
 
 # Ajustar los límites para un zoom muy cercano a Santiago
 xlim_santiago = c(-72, -69.5)
 ylim_santiago = c(-34.5, -32.5)
 
 g1 = ggplot(data = mapa_santiago) +
   geom_sf(fill = "gray", aes(geometry = geometry)) +
   geom_sf(data = hospitales, aes(size = as.factor(tamano_hospitales), fill = GRD_pred), shape = 21, alpha = 1) +
   coord_sf(xlim = xlim_santiago, ylim = ylim_santiago, expand = FALSE) +
   scale_fill_gradientn(colors = c("aquamarine4", "gold", "firebrick4"),
                        values = scales::rescale(c(0.5, 1.0, 1.6)),
                        na.value = "black",
                        limits = c(0.5, 1.6)) +
   theme_minimal() +
   theme(text = element_text(size = 18),
         panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
         panel.background = element_blank(),
         axis.title.x = element_blank(),
         axis.title.y = element_blank(),
         axis.text.x = element_blank(),
         axis.text.y = element_blank(),
         legend.position = "right",
         legend.text = element_text(size = 16),
         legend.title = element_text(size = 16),
         plot.margin = unit(c(0,0,0,0), "cm")) +  # Ajusta los márgenes del gráfico
   labs(fill = "CMI index", size = "Hospital Complexity (MINSAL)") +
   scale_size_manual(values = c(2, 4, 6),
                     breaks = c(2, 4, 6),
                     labels = c("Low complexity", "Medium complexity", "High complexity")) +
   guides(size = guide_legend(nrow = 3, title.position = "top"))

 
 plot(g1)
 
 
 
 #==============================================
 # Box plot según complejidad
 #==============================================
 data_hospitales$GRD_real =  grd_2014$OriginalGRD
 data_hospitales$GRD_pred =  grd_2014$Prediction
 
 # Reemplazar y ordenar los niveles de complejidad
 data_hospitales <- data_hospitales %>%
   mutate(COM = factor(COM,
                       levels = c("Baja Complejidad", "Mediana Complejidad", "Alta Complejidad"),
                       labels = c("Low", "Medium", "High")))
 
 # Crear el boxplot con el orden especificado
 g2 <- ggplot(data_hospitales, aes(x = COM, y = GRD_pred, fill = COM)) +
   geom_boxplot(alpha = 0.4, outlier.shape = 19, outlier.size = 3) + # Aumenta el tamaño de los outliers
   scale_fill_manual(values = c("Low" = "lightyellow1", "Medium" = "sandybrown", "High" = "tomato3")) +
   labs(x = "Hospital Complexity (MINSAL)", y = "CMI", title = "") +
   theme_minimal(base_size = 14) +
   theme(panel.background = element_rect(fill = "white", colour = "white"),
         axis.line = element_line(colour = "black"),
         axis.text.x = element_text(angle = 90, hjust = 1, size = 16, margin = margin(t = 10)), # Añade espacio arriba de las etiquetas del eje x
         axis.text.y = element_text(size = 16),
         axis.title.x = element_text(size = 16, margin = margin(t = 20)), # Añade espacio entre el título del eje x y las etiquetas
         axis.title.y = element_text(size = 16),
         plot.background = element_rect(fill = "white", colour = "white"),
         legend.position = "none") +  # Oculta la leyenda
   coord_cartesian(ylim = c(0.5, 2))
 
 plot(g2)
 
 # library("PMCMR")
 # library("PMCMRplus") 
 # 
 # kwAllPairsDunnTest(GRD_real~as.factor(COM),data = data_hospitales,
 #                    p.adjust.method="holm")
 # 
 tmp=data_hospitales[which(!is.na(data_hospitales$GRD_real)),]
 table(tmp$COM)
 table(data_hospitales$COM)
 
 
 
 data_hospitales = read.csv(file = "Hospitales.csv",sep=";",header=T)
 grd_2014 = read.csv("prediciones_grd_2014.txt",sep=",",header=T)
 grd_2014$COM = data_hospitales$COM
 grd_2015 = read.csv("prediciones_grd_2015.txt",sep=",",header=T)
 grd_2015$COM = data_hospitales$COM
 grd_2016 = read.csv("prediciones_grd_2016.txt",sep=",",header=T)
 grd_2016$COM = data_hospitales$COM
 grd_2017 = read.csv("prediciones_grd_2017.txt",sep=",",header=T)
 grd_2017$COM = data_hospitales$COM
 grd_2018 = read.csv("prediciones_grd_2018.txt",sep=",",header=T)
 grd_2018$COM = data_hospitales$COM
 grd_2019 = read.csv("prediciones_grd_2019.txt",sep=",",header=T)
 grd_2019$COM = data_hospitales$COM
 grd_2020 = read.csv("prediciones_grd_2020.txt",sep=",",header=T)
 grd_2020$COM = data_hospitales$COM
 
grd_2014$año = rep("2014",nrow(grd_2014))
grd_2015$año = rep("2015",nrow(grd_2015))
grd_2016$año = rep("2016",nrow(grd_2016))
grd_2017$año = rep("2017",nrow(grd_2017))
grd_2018$año = rep("2018",nrow(grd_2018))
grd_2019$año = rep("2019",nrow(grd_2019))
grd_2020$año = rep("2020",nrow(grd_2020))

datos_grd = rbind(grd_2014,grd_2015,grd_2016,grd_2017,grd_2018,grd_2019,grd_2020)
datos_grd_low = datos_grd[which(datos_grd$COM=="Baja Complejidad"),]
datos_grd_mid= datos_grd[which(datos_grd$COM=="Mediana Complejidad"),]
datos_grd_hig = datos_grd[which(datos_grd$COM=="Alta Complejidad"),]

datos_grd_low$OriginalGRD = as.numeric(datos_grd_low$OriginalGRD)
datos_grd_mid$OriginalGRD = as.numeric(datos_grd_mid$OriginalGRD)
datos_grd_hig$OriginalGRD = as.numeric(datos_grd_hig$OriginalGRD)



g2 = ggplot(datos_grd_low, aes(x = año, y = Prediction)) +
  geom_violin(alpha = 0.4, outlier.shape = 19, outlier.size = 3, fill = "lightyellow") + # Establece el color verde para todas las barras
  labs(x = "Years", y = "CMI", title = "") +
  theme_minimal(base_size = 14) +
  theme(panel.background = element_rect(fill = "white", colour = "white"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 90, hjust = 1, size = 16, margin = margin(t = 10)), # Añade espacio arriba de las etiquetas del eje x
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 16, margin = margin(t = 20)), # Añade espacio entre el título del eje x y las etiquetas
        axis.title.y = element_text(size = 16),
        plot.background = element_rect(fill = "white", colour = "white"),
        legend.position = "none") +  # Oculta la leyenda
  coord_cartesian(ylim = c(0.5, 2))

plot(g2)


g3 = ggplot(datos_grd_mid, aes(x = año, y = Prediction)) +
  geom_violin(alpha = 0.4, outlier.shape = 19, outlier.size = 3, fill = "sandybrown") + # Establece el color verde para todas las barras
  labs(x = "Years", y = "CMI", title = "") +
  theme_minimal(base_size = 14) +
  theme(panel.background = element_rect(fill = "white", colour = "white"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 90, hjust = 1, size = 16, margin = margin(t = 10)), # Añade espacio arriba de las etiquetas del eje x
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 16, margin = margin(t = 20)), # Añade espacio entre el título del eje x y las etiquetas
        axis.title.y = element_text(size = 16),
        plot.background = element_rect(fill = "white", colour = "white"),
        legend.position = "none") +  # Oculta la leyenda
  coord_cartesian(ylim = c(0.5, 2))

plot(g3)

g4 = ggplot(datos_grd_hig, aes(x = año, y = Prediction)) +
  geom_violin(alpha = 0.4, outlier.shape = 19, outlier.size = 3, fill = "tomato3") + # Establece el color verde para todas las barras
  labs(x = "Years", y = "CMI", title = "") +
  theme_minimal(base_size = 14) +
  theme(panel.background = element_rect(fill = "white", colour = "white"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 90, hjust = 1, size = 16, margin = margin(t = 10)), # Añade espacio arriba de las etiquetas del eje x
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 16, margin = margin(t = 20)), # Añade espacio entre el título del eje x y las etiquetas
        axis.title.y = element_text(size = 16),
        plot.background = element_rect(fill = "white", colour = "white"),
        legend.position = "none") +  # Oculta la leyenda
  coord_cartesian(ylim = c(0.5, 2))

plot(g4)


g5 = ggplot(datos_grd_low, aes(x = año, y = OriginalGRD)) +
  geom_violin(alpha = 0.4, outlier.shape = 19, outlier.size = 3, fill = "lightyellow") + # Establece el color verde para todas las barras
  labs(x = "Years", y = "CMI", title = "") +
  theme_minimal(base_size = 14) +
  theme(panel.background = element_rect(fill = "white", colour = "white"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 90, hjust = 1, size = 16, margin = margin(t = 10)), # Añade espacio arriba de las etiquetas del eje x
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 16, margin = margin(t = 20)), # Añade espacio entre el título del eje x y las etiquetas
        axis.title.y = element_text(size = 16),
        plot.background = element_rect(fill = "white", colour = "white"),
        legend.position = "none") +  # Oculta la leyenda
  coord_cartesian(ylim = c(0.5, 2))

plot(g5)


g6 = ggplot(datos_grd_mid, aes(x = año, y = OriginalGRD)) +
  geom_violin(alpha = 0.4, outlier.shape = 19, outlier.size = 3, fill = "sandybrown") + # Establece el color verde para todas las barras
  labs(x = "Years", y = "CMI", title = "") +
  theme_minimal(base_size = 14) +
  theme(panel.background = element_rect(fill = "white", colour = "white"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 90, hjust = 1, size = 16, margin = margin(t = 10)), # Añade espacio arriba de las etiquetas del eje x
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 16, margin = margin(t = 20)), # Añade espacio entre el título del eje x y las etiquetas
        axis.title.y = element_text(size = 16),
        plot.background = element_rect(fill = "white", colour = "white"),
        legend.position = "none") +  # Oculta la leyenda
  coord_cartesian(ylim = c(0.5, 2))

plot(g6)


g7 = ggplot(datos_grd_hig, aes(x = año, y = OriginalGRD)) +
  geom_violin(alpha = 0.4, outlier.shape = 19, outlier.size = 3, fill = "tomato3") + # Establece el color verde para todas las barras
  labs(x = "Years", y = "CMI", title = "") +
  theme_minimal(base_size = 14) +
  theme(panel.background = element_rect(fill = "white", colour = "white"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 90, hjust = 1, size = 16, margin = margin(t = 10)), # Añade espacio arriba de las etiquetas del eje x
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 16, margin = margin(t = 20)), # Añade espacio entre el título del eje x y las etiquetas
        axis.title.y = element_text(size = 16),
        plot.background = element_rect(fill = "white", colour = "white"),
        legend.position = "none") +  # Oculta la leyenda
  coord_cartesian(ylim = c(0.5, 2))


plot(g7)

ANO = 2018
datos_grd_low_ANO = datos_grd_low %>% filter(año==ANO)
idx = which(!is.na(datos_grd_low_ANO$OriginalGRD))
coefcor_low=cor(datos_grd_low_ANO$OriginalGRD[idx],datos_grd_low_ANO$Prediction[idx])

datos_grd_mid_ANO = datos_grd_mid %>% filter(año==ANO)
idx = which(!is.na(datos_grd_mid_ANO$OriginalGRD))
coefcor_mid=cor(datos_grd_mid_ANO$OriginalGRD[idx],datos_grd_mid_ANO$Prediction[idx])

datos_grd_hig_ANO = datos_grd_hig %>% filter(año==ANO)
idx = which(!is.na(datos_grd_hig_ANO$OriginalGRD))
coefcor_hig=cor(datos_grd_hig_ANO$OriginalGRD[idx],datos_grd_hig_ANO$Prediction[idx])


print(round(coefcor_low,3))
print(round(coefcor_mid,3))
print(round(coefcor_hig,3))
