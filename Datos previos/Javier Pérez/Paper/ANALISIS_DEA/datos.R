
#-----------------------------
#Análisis DEA
#----------------------------
datos_DEA=read.csv("DEA.csv",header=T, sep=";")
lista_modelos = unique(datos_DEA$MODEL)


M_list = NULL
Geo_list = NULL
DRG_list = NULL


for (index_models in lista_modelos)
{
  print(index_models)
  
  datos_filtrados = datos_DEA[which(datos_DEA$MODEL==index_models),]
  
  #Input
  idx_max = which(datos_filtrados$te_input==max(datos_filtrados$te_input))
  
  if (datos_filtrados$S_MODEL[1]=="M")
  { M_list = c(M_list,datos_filtrados$hospital_id[idx_max])}
  
  
  if (datos_filtrados$S_MODEL[1]=="Geo")
  { Geo_list = c(Geo_list,datos_filtrados$hospital_id[idx_max])}
  
  
  if (datos_filtrados$S_MODEL[1]=="DRG")
  { DRG_list = c(DRG_list,datos_filtrados$hospital_id[idx_max])}
  
  #Output
  idx_max = which(datos_filtrados$te_output==max(datos_filtrados$te_output))
  
  
  if (datos_filtrados$S_MODEL[1]=="M")
  { M_list = c(M_list,datos_filtrados$hospital_id[idx_max])}
  
  
  if (datos_filtrados$S_MODEL[1]=="Geo")
  { Geo_list = c(Geo_list,datos_filtrados$hospital_id[idx_max])}
  
  
  if (datos_filtrados$S_MODEL[1]=="DRG")
  { DRG_list = c(DRG_list,datos_filtrados$hospital_id[idx_max])}
  

  #Directional
  idx_max = which(datos_filtrados$te_directional==max(datos_filtrados$te_directional))
  
  print(sort(datos_filtrados$hospital_id[idx_max]))
  print("--------------")
  
  if (datos_filtrados$S_MODEL[1]=="M")
  { M_list = c(M_list,datos_filtrados$hospital_id[idx_max])}
  
  
  if (datos_filtrados$S_MODEL[1]=="Geo")
  { Geo_list = c(Geo_list,datos_filtrados$hospital_id[idx_max])}
  
  
  if (datos_filtrados$S_MODEL[1]=="DRG")
  { DRG_list = c(DRG_list,datos_filtrados$hospital_id[idx_max])}
  
}

# 
# #-----------------------------
# #Análisis Malquism
# #----------------------------
# 
# datos_Malq = read.csv("MAL.csv",header=T,sep=";")
# lista_modelos = unique(datos_Malq$TIPO)
# 
# M_list = NULL
# Geo_list = NULL
# DRG_list = NULL
# 
# 
# for (index_models in lista_modelos)
# {
#   datos_filtrados = datos_Malq[which(datos_Malq$TIPO==index_models),]
#   
#   for (a in 4:9)
#   {
#     
#     idx_max = which(datos_filtrados[,a]==max(datos_filtrados[,a]))
#     
#     if (datos_filtrados$S_MODEL[1]=="M")
#     { M_list = c(M_list,datos_filtrados$H[idx_max])}
#     
#     
#     if (datos_filtrados$S_MODEL[1]=="Geo")
#     { Geo_list = c(Geo_list,datos_filtrados$H[idx_max])}
#     
#     
#     if (datos_filtrados$S_MODEL[1]=="DRG")
#     { DRG_list = c(DRG_list,datos_filtrados$H[idx_max])}
#     
#     
#   }
#   
# }
# 
# #================================
# #CALCULAR EFICIENCIA TÉCNICA DEA
# #================================
# hospitales = unique(datos_DEA$hospital_id)
# index = 0
# prom_M_I=NULL
# sd_M_I=NULL
# prom_M_O=NULL
# sd_M_O=NULL
# for (a in hospitales)
# {
#   
#   index = index+1
#   
#   # Ministerio" 
#   datos_filtrados = datos_DEA[which(datos_DEA$S_MODEL=="M"),]
#   idx_hospital = which(datos_filtrados$hospital_id==a)
#     #Input
#   prom_M_I[index]= mean(datos_filtrados$te_input[idx_hospital])
#   sd_M_I[index]= sd(datos_filtrados$te_input[idx_hospital])
#     #Output
#   prom_M_O[index]= mean(datos_filtrados$te_output[idx_hospital])
#   sd_M_O[index]= sd(datos_filtrados$te_output[idx_hospital])
# }
# 
# salida_M_IO=data.frame(hospitales,prom_M_I,sd_M_I,prom_M_O,sd_M_O)
# 
# 
# #GEo
# index = 0
# prom_M_I=NULL
# sd_M_I=NULL
# prom_M_O=NULL
# sd_M_O=NULL
# for (a in hospitales)
# {
#   
#   index = index+1
#   
#   # Geo" 
#   datos_filtrados = datos_DEA[which(datos_DEA$S_MODEL=="Geo"),]
#   idx_hospital = which(datos_filtrados$hospital_id==a)
#   #Input
#   prom_M_I[index]= mean(datos_filtrados$te_input[idx_hospital])
#   sd_M_I[index]= sd(datos_filtrados$te_input[idx_hospital])
#   #Output
#   prom_M_O[index]= mean(datos_filtrados$te_output[idx_hospital])
#   sd_M_O[index]= sd(datos_filtrados$te_output[idx_hospital])
# }
# 
# salida_Geo_IO=data.frame(hospitales,prom_M_I,sd_M_I,prom_M_O,sd_M_O)
# 
# 
# 
# #DRG
# index = 0
# prom_M_I=NULL
# sd_M_I=NULL
# prom_M_O=NULL
# sd_M_O=NULL
# for (a in hospitales)
# {
#   
#   index = index+1
#   
#   # GRD" 
#   datos_filtrados = datos_DEA[which(datos_DEA$S_MODEL=="DRG"),]
#   idx_hospital = which(datos_filtrados$hospital_id==a)
#   #Input
#   prom_M_I[index]= mean(datos_filtrados$te_input[idx_hospital])
#   sd_M_I[index]= sd(datos_filtrados$te_input[idx_hospital])
#   #Output
#   prom_M_O[index]= mean(datos_filtrados$te_output[idx_hospital])
#   sd_M_O[index]= sd(datos_filtrados$te_output[idx_hospital])
# }
# 
# salida_DRG_IO=data.frame(hospitales,prom_M_I,sd_M_I,prom_M_O,sd_M_O)
# 
# 
