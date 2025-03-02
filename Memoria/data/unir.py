import pandas as pd

anio_num = 2023
anio = str(anio_num)

# Cargar el archivo de datos transformados
archivo_transformado = anio + ".csv"
df_transformado = pd.read_csv(archivo_transformado, sep=";", encoding="utf-8")

# Cargar el otro archivo que contiene la columna "Cód. Estab."
archivo_base = anio + "_consolidated_data.csv"  # Cambia esto con el nombre de tu archivo
df_base = pd.read_csv(archivo_base, sep=";", encoding="utf-8")

# Verificar la estructura de los datos cargados
print("Primeros registros de df_transformado:")
print(df_transformado.head())

print("Primeros registros de df_base:")
print(df_base.head())

# Asegurar que "Cód. Estab." esté en el mismo formato en ambos archivos
df_transformado["idEstablecimiento"] = df_transformado["idEstablecimiento"].astype(str)
df_base["idEstablecimiento"] = df_base["idEstablecimiento"].astype(str)

# Unir los dos DataFrames en base a "Cód. Estab."
df_final = df_base.merge(df_transformado, on="idEstablecimiento", how="left")


df_final.to_csv("datos_unidos_"+anio+".csv", sep=";", encoding="utf-8", index=False)
# Guardar el resultado en un nuevo archivo Excel
#df_final.to_excel("datos_unidos.xlsx", index=False, encoding="utf-8")

print("Archivo 'datos_unidos.csv' generado correctamente.")
