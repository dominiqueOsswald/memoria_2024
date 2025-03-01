import pandas as pd

# Ruta del archivo Excel
archivo_excel = "rem20.xlsx"  # Cambia esto con la ruta de tu archivo

# Nombre de la hoja específica que quieres leer
hoja_especifica = "2023"  # Cambia esto con el nombre real de la hoja

# Cargar la hoja específica del archivo Excel
df = pd.read_excel(archivo_excel, sheet_name=hoja_especifica)

# Ver los primeros registros para entender la estructura
print(df.head())


# Asignar nombres a las columnas si es necesario
df.columns = ["Cód. Estab.", "Nombre Nivel Cuidado", "Glosa", "Acum"]

# Crear un nuevo DataFrame con la estructura deseada
df_pivot = df.pivot(index="Cód. Estab.", columns=["Nombre Nivel Cuidado", "Glosa"], values="Acum")

# Renombrar la columna combinada
df_pivot.columns = [".".join(col).replace(" ", ".") for col in df_pivot.columns]

# Guardar el nuevo archivo en formato CSV con tabulaciones
df_pivot.to_csv("datos_transformados.csv", sep=";", encoding="utf-8")

print("Archivo 'datos_transformados.csv' generado correctamente.")
