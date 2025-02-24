setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("functions.R")
source("graphics.R")

# ==============================================
#  CÁLCULO DEA
# ==============================================

# CORRELACION DE VALORES ORIGINALES PARA TODAS LAS COMBINACIONES EN TODOS LOS AÑOS
correlaciones_eficiencia_grafica(correlacion_todos_metodos[["correlaciones_lista"]], "ambos", c("VRS Input", "VRS Output", "CRS Input", "CRS Output", "ESC Input", "ESC Output"))


#  ELIMINACIÓN DE DATOS ATÍPICOS
# EN GENERAL
correlaciones_eficiencia_grafica(correlacion_todos_metodos_atipicos[["vrs_oo"]][["original_vs_sin_atipicos"]][["oo"]][["promedio_correlacion"]], "ambos", c("VRS original", "VRS sin atipicos", "CRS original", "CRS sin atipicos"), "Comparación Original vs sin atipicos - Orientación Outputs VRS")

correlaciones_promedio <- list(
  "vrs_oo_Entrada" = correlacion_todos_metodos_atipicos[["vrs_oo"]][["original_vs_sin_atipicos"]][["io"]][["promedio_correlacion"]],
  "vrs_oo_Salida" = correlacion_todos_metodos_atipicos[["vrs_oo"]][["original_vs_sin_atipicos"]][["oo"]][["promedio_correlacion"]],
  "vrs_io_Entrada" = correlacion_todos_metodos_atipicos[["vrs_io"]][["original_vs_sin_atipicos"]][["io"]][["promedio_correlacion"]],
  "vrs_io_Salida" = correlacion_todos_metodos_atipicos[["vrs_io"]][["original_vs_sin_atipicos"]][["oo"]][["promedio_correlacion"]],
  "crs_oo_Entrada" = correlacion_todos_metodos_atipicos[["crs_oo"]][["original_vs_sin_atipicos"]][["io"]][["promedio_correlacion"]],
  "crs_oo_Salida" = correlacion_todos_metodos_atipicos[["crs_oo"]][["original_vs_sin_atipicos"]][["oo"]][["promedio_correlacion"]],
  "crs_io_Entrada" = correlacion_todos_metodos_atipicos[["crs_io"]][["original_vs_sin_atipicos"]][["io"]][["promedio_correlacion"]],
  "crs_io_Salida" = correlacion_todos_metodos_atipicos[["crs_io"]][["original_vs_sin_atipicos"]][["oo"]][["promedio_correlacion"]]
)

correlaciones_single_eficiencia_grafica(correlaciones_promedio,"ambos", c("VRS original", "VRS sin atipicos", "CRS original", "CRS sin atipicos"), "Comparación Original vs sin atipicos - Orientación Outputs VRS")


# REVISION DE SENSIBILIDAD ANTE ATIPICOS AÑO POR AÑO
correlaciones_eficiencia_grafica(correlacion_todos_metodos_atipicos[["vrs_oo"]][["original_vs_sin_atipicos"]][["io"]][["correlaciones_lista"]], "ambos", c("VRS original", "VRS sin atipicos", "CRS original", "CRS sin atipicos"), "Comparación Original vs sin atipicos - Orientación Outputs VRS")
correlaciones_eficiencia_grafica(correlacion_todos_metodos_atipicos[["vrs_io"]][["original_vs_sin_atipicos"]][["io"]][["correlaciones_lista"]], "ambos", c("VRS original", "VRS sin atipicos", "CRS original", "CRS sin atipicos"), "Comparación Original vs sin atipicos - Orientación Inputs VRS")

correlaciones_eficiencia_grafica(correlacion_todos_metodos_atipicos[["crs_oo"]][["original_vs_sin_atipicos"]][["io"]][["correlaciones_lista"]], "ambos", c("VRS original", "VRS sin atipicos", "CRS original", "CRS sin atipicos"), "Comparación Original vs sin atipicos - Orientación Outputs CRS")
correlaciones_eficiencia_grafica(correlacion_todos_metodos_atipicos[["crs_io"]][["original_vs_sin_atipicos"]][["io"]][["correlaciones_lista"]], "ambos", c("VRS original", "VRS sin atipicos", "CRS original", "CRS sin atipicos"), "Comparación Original vs sin atipicos - Orientación Inputs CRS")



correlaciones_eficiencia_grafica(correlacion_todos_metodos_atipicos[["vrs_oo"]][["original_vs_sin_atipicos"]][["oo"]][["correlaciones_lista"]], "ambos", c("VRS original", "VRS sin atipicos", "CRS original", "CRS sin atipicos"), "Comparación Original vs sin atipicos - Orientación Outputs VRS")
correlaciones_eficiencia_grafica(correlacion_todos_metodos_atipicos[["vrs_io"]][["original_vs_sin_atipicos"]][["oo"]][["correlaciones_lista"]], "ambos", c("VRS original", "VRS sin atipicos", "CRS original", "CRS sin atipicos"), "Comparación Original vs sin atipicos - Orientación Inputs VRS")

correlaciones_eficiencia_grafica(correlacion_todos_metodos_atipicos[["crs_oo"]][["original_vs_sin_atipicos"]][["oo"]][["correlaciones_lista"]], "ambos", c("VRS original", "VRS sin atipicos", "CRS original", "CRS sin atipicos"), "Comparación Original vs sin atipicos - Orientación Outputs CRS")
correlaciones_eficiencia_grafica(correlacion_todos_metodos_atipicos[["crs_io"]][["original_vs_sin_atipicos"]][["oo"]][["correlaciones_lista"]], "ambos", c("VRS original", "VRS sin atipicos", "CRS original", "CRS sin atipicos"), "Comparación Original vs sin atipicos - Orientación Inputs CRS")







# GRAFICA DE SENSIBILIDAD ELIMINACION DE ATIPICOS X METODO
correlaciones_eficiencia_grafica(resultados_sin_atipicos[["vrs_io"]][["io"]][["resultados_correlacion"]][["correlaciones_lista"]], "io", c("VRS iteracion 1", "VRS iteracion 2", "VRS iteracion 3", "CRS iteracion 1", "CRS iteracion 2",  "CRS iteracion 3"), "Sensibilidad por eliminación de DMU eficientes - Sin datos atipicos VRS Input")
correlaciones_eficiencia_grafica(resultados_sin_atipicos[["vrs_io"]][["oo"]][["resultados_correlacion"]][["correlaciones_lista"]], "oo", c("VRS iteracion 1", "VRS iteracion 2", "VRS iteracion 3", "CRS iteracion 1", "CRS iteracion 2",  "CRS iteracion 3"), "Sensibilidad por eliminación de DMU eficientes - Sin datos atipicos VRS Input")

correlaciones_eficiencia_grafica(resultados_sin_atipicos[["crs_io"]][["io"]][["resultados_correlacion"]][["correlaciones_lista"]], "io", c("VRS iteracion 1", "VRS iteracion 2", "VRS iteracion 3", "CRS iteracion 1", "CRS iteracion 2",  "CRS iteracion 3"), "Sensibilidad por eliminación de DMU eficientes - Sin datos atipicos CRS Input")
correlaciones_eficiencia_grafica(resultados_sin_atipicos[["crs_io"]][["oo"]][["resultados_correlacion"]][["correlaciones_lista"]], "oo", c("VRS iteracion 1", "VRS iteracion 2", "VRS iteracion 3", "CRS iteracion 1", "CRS iteracion 2",  "CRS iteracion 3"), "Sensibilidad por eliminación de DMU eficientes - Sin datos atipicos CRS Input" )

correlaciones_eficiencia_grafica(resultados_sin_atipicos[["vrs_oo"]][["io"]][["resultados_correlacion"]][["correlaciones_lista"]], "io", c("VRS iteracion 1", "VRS iteracion 2", "VRS iteracion 3", "CRS iteracion 1", "CRS iteracion 2",  "CRS iteracion 3"), "Sensibilidad por eliminación de DMU eficientes - Sin datos atipicos VRS Output")
correlaciones_eficiencia_grafica(resultados_sin_atipicos[["vrs_oo"]][["oo"]][["resultados_correlacion"]][["correlaciones_lista"]], "oo", c("VRS iteracion 1", "VRS iteracion 2", "VRS iteracion 3", "CRS iteracion 1", "CRS iteracion 2",  "CRS iteracion 3"), "Sensibilidad por eliminación de DMU eficientes - Sin datos atipicos VRS Output")

correlaciones_eficiencia_grafica(resultados_sin_atipicos[["crs_oo"]][["io"]][["resultados_correlacion"]][["correlaciones_lista"]], "io", c("VRS iteracion 1", "VRS iteracion 2", "VRS iteracion 3", "CRS iteracion 1", "CRS iteracion 2",  "CRS iteracion 3"), "Sensibilidad por eliminación de DMU eficientes - Sin datos atipicos CRS Output")
correlaciones_eficiencia_grafica(resultados_sin_atipicos[["crs_oo"]][["oo"]][["resultados_correlacion"]][["correlaciones_lista"]], "oo", c("VRS iteracion 1", "VRS iteracion 2", "VRS iteracion 3", "CRS iteracion 1", "CRS iteracion 2",  "CRS iteracion 3"), "Sensibilidad por eliminación de DMU eficientes - Sin datos atipicos CRS Output")
