import pandas as pd

# Lee la hoja de Excel
archivo_excel = "RESULTADOS OUTPUT.xlsx"  # Reemplaza con tu archivo
hoja = "EFICIENCIA_ANOS"  # Especifica el nombre de la hoja o su índice (por ejemplo, 0 para la primera hoja)

# Lee la hoja específica del Excel
df = pd.read_excel(archivo_excel, sheet_name=hoja)

# Asegura que la primera columna sea un entero
df.iloc[:, 0] = df.iloc[:, 0].astype(int)

# Convierte el DataFrame en formato de tabla LaTeX
def dataframe_a_latex(df):
    latex_tabla = []
    for _, row in df.iterrows():
        latex_fila = " & ".join(map(str, row)) + " \\\\"
        latex_tabla.append(latex_fila)
    return "\n".join(latex_tabla)

# Genera la tabla
tabla_latex = dataframe_a_latex(df)

# Guarda la tabla en un archivo de texto o imprime directamente
with open("tabla_latex.txt", "w", encoding="utf-8") as f:
    f.write(tabla_latex)

print("Tabla generada con éxito.")
print(tabla_latex)  # Opcional, para ver la tabla en consola