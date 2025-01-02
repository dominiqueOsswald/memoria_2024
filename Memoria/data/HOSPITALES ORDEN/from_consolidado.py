import pandas as pd

# Cargar el archivo Excel
archivo_excel = 'Consolidado estadísticas hospitalarias 2014-2021.xlsx'   # Reemplaza con el nombre de tu archivo
hoja = '2022'   # Reemplaza con el nombre de la hoja que estás trabajando

# Cargar los datos del archivo, limitando las columnas a las primeras 4
df = pd.read_excel(archivo_excel, sheet_name=hoja, usecols=[0, 1, 2, 3])

df.iloc[:, 0] = pd.to_numeric(df.iloc[:, 0], errors='coerce')

df_filtrado = df[(df.iloc[:, 0] > 0) & (~df.iloc[:, 3].str.contains("Datos Servicio Salud", case=False, na=False))]

# Eliminar filas duplicadas
df_sin_duplicados = df_filtrado.drop_duplicates()
df_final = df_sin_duplicados.drop(df.columns[[0, 1]], axis=1)

# Guardar los resultados en un archivo CSV
archivo_csv = "resultado_" + hoja+ ".csv"  # Nombre del archivo CSV de salida
df_final.to_csv(archivo_csv, index=False)


print(f"Archivo CSV guardado como {archivo_csv}")