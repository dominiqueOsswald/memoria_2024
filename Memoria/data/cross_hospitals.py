import pandas as pd

anio = "2022"
# Leer el primer archivo CSV (Hospitales con IDs y nombres)
df1 = pd.read_csv(anio + "/1_" + anio + "_hospitals.csv", header=None, names=['hospital_fns_id', 'hospital_name'])

# Leer el segundo archivo CSV (Hospitales con más información, incluyendo latitud, longitud, etc.)
df2 = pd.read_csv('2019/2019_hospitals.csv')

# Convertir la columna `hospital_fns_id` de ambos DataFrames a string para evitar errores de tipo
df1['hospital_fns_id'] = df1['hospital_fns_id'].astype(str)
df2['hospital_fns_id'] = df2['hospital_fns_id'].astype(str)

# Unir los dataframes por la columna `hospital_fns_id` de ambos archivos
# El `how='left'` asegura que todos los registros de df1 permanezcan y se añada información de df2 solo cuando hay coincidencias
df_merged = pd.merge(df1, df2, on='hospital_fns_id', how='left')

# Ver las columnas resultantes del merge para asegurar que todo esté en orden
print(df_merged.columns)

# Mostrar el resultado completo del merge (con todas las columnas)
print(df_merged)

# Guardar el archivo con la información combinada
df_merged.to_csv("hospitales_completados_"+ anio +".csv", index=False)