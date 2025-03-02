import pandas as pd
import re

# Ruta del archivo Excel
archivo_excel = "rem20.xlsx"  # Asegúrate de que este es el nombre correcto del archivo
anio = 2023
# Nombre de la hoja específica que quieres leer
hoja_especifica = str(anio)  # Nombre exacto de la hoja

# Cargar la hoja específica del archivo Excel
df = pd.read_excel(archivo_excel, sheet_name=hoja_especifica)

# Ver los primeros registros para entender la estructura
print(df.head())

# Asignar nombres a las columnas si es necesario
df.columns = ["Cód. Estab.", "Nombre Nivel Cuidado", "Glosa", "Acum"]

# Función para limpiar la columna "Nombre Nivel Cuidado"
def limpiar_texto(texto):
    if isinstance(texto, str):  # Verificar si es un string
        return re.sub(r'^\d+\s*-\s*', '', texto)  # Eliminar números iniciales y guion
    return texto

# Aplicar la función a la columna "Nombre Nivel Cuidado"
df["Nombre Nivel Cuidado"] = df["Nombre Nivel Cuidado"].apply(limpiar_texto)

# Crear un nuevo DataFrame con la estructura deseada
df_pivot = df.pivot(index="Cód. Estab.", columns=["Nombre Nivel Cuidado", "Glosa"], values="Acum")

# Renombrar la columna combinada
df_pivot.columns = [".".join(col).replace(" ", ".") for col in df_pivot.columns]

# Guardar el nuevo archivo en formato CSV con tabulaciones
df_pivot.to_csv(str(anio)+".csv", sep=";", encoding="utf-8")

print("Archivo generado correctamente.")
