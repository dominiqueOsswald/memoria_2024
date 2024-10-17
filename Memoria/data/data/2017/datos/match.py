import pandas as pd

anio = "2017"
# Leer el primer archivo CSV (Hospitales con IDs y más información)
df1 = pd.read_csv("hospitales_completados_" + anio + ".csv")

# Leer el segundo archivo CSV (Código y nombre)
df2 = pd.read_csv("resultado_" + anio + ".csv")

# Rellenar valores NaN con un valor apropiado (por ejemplo, 0) y convertir a int
df1['hospital_id'] = pd.to_numeric(df1['hospital_id'], errors='coerce').fillna(0).astype(int)
df2['codigo'] = pd.to_numeric(df2['codigo'], errors='coerce').fillna(0).astype(int)

# Hacer el merge en base a la columna 'hospital_id' de df1 y 'codigo' de df2
df_merged = pd.merge(df1, df2, left_on='hospital_id', right_on='codigo', how='left')

# Ver las columnas resultantes del merge para asegurar que todo esté en orden
print(df_merged.columns)

# Mostrar el resultado completo del merge (con todas las columnas)
print(df_merged)

# Guardar el archivo con la información combinada
df_merged.to_csv("hospitales_completados_"+ anio +"_2.csv", index=False)

