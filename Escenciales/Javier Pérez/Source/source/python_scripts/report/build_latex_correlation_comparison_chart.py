""" Module to build the Latex table of E.T models """

# External imports
import sys
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

# Sys instructions
sys.path.append("..")

# Internal imports
from common import config, helpers

def row_name_dictionary(name):
    mapping = {
        'te_input_crs': 'CRS-I',
        'te_input_vrs': 'VRS-I',
        'te_input_nirs': 'NIRS-I',
        'te_input_region_crs': 'Geo. CRS-I',
        'te_input_region_vrs': 'Geo. VRS-I',
        'te_input_region_nirs': 'Geo. NIRS-I',
        'te_input_grd_crs': 'GRD CRS-I',
        'te_input_grd_vrs': 'GRD VRS-I',
        'te_input_grd_nirs': 'GRD NIRS-I',
        'te_output_crs': 'CRS-O',
        'te_output_vrs': 'VRS-O',
        'te_output_nirs': 'NIRS-O',
        'te_output_region_crs': 'Geo. CRS-O',
        'te_output_region_vrs': 'Geo. VRS-O',
        'te_output_region_nirs': 'Geo. NIRS-O',
        'te_output_grd_crs': 'GRD CRS-O',
        'te_output_grd_vrs': 'GRD VRS-O',
        'te_output_grd_nirs': 'GRD NIRS-O',
        'te_directional_crs': 'CRS-D',
        'te_directional_vrs': 'VRS-D',
        'te_directional_nirs': 'NIRS-D',
        'te_directional_region_crs': 'Geo. CRS-D',
        'te_directional_region_vrs': 'Geo. VRS-D',
        'te_directional_region_nirs': 'Geo. NIRS-D',
        'te_directional_grd_crs': 'GRD CRS-D',
        'te_directional_grd_vrs': 'GRD VRS-D',
        'te_directional_grd_nirs': 'GRD NIRS-D' 
    }
    return mapping.get(name, name)

def correlation_name_dictionary(name):
    mapping = {
        'pearson': 'Matriz distancia correlación Pearson',
        'euclidean': 'Matriz distancia Euclideana'
    }
    return mapping.get(name, name)

def build_correlation_rows(option, type):
    correlation_dataframe = helpers.dataframe_read_csv_file(path=f'{PATH}/{YEAR}.csv')
    correlation_dataframe = correlation_dataframe.astype(str)
    correlation_dataframe = correlation_dataframe[option[1]]
    if type == 'MINSAL':
        correlation_dataframe = correlation_dataframe.iloc[:9]
    elif type == 'GEOGRÁFICA':
        correlation_dataframe = correlation_dataframe.iloc[9:18]
    else:
        print('Error')
    correlation_dataframe['et_names'] = option[0]


    for col in option[1]:
        if col != 'et_names':
            correlation_dataframe[col] = correlation_dataframe[col].astype(str).apply(lambda x: f'{float(x):.2f}')

    et_names_col = correlation_dataframe.pop('et_names')
    correlation_dataframe.insert(0, 'et_names', et_names_col)
    correlation_dataframe = correlation_dataframe.apply(join_row, axis=1)
    return correlation_dataframe

def join_row(row):
    join_values =  ' & '.join(row)
    values = join_values.split(' & ')
    first_value = values[0]
    new_name = row_name_dictionary(first_value)
    output_string = row.replace(first_value, new_name)
    return ' & '.join(output_string)

# Body
YEAR = sys.argv[1]
CORRELATION = sys.argv[2].lower()

PATH = f'../../../results/{CORRELATION}_correlation'

DATA_COLUMNS = [
    'te_input_crs', 'te_input_vrs', 'te_input_nirs',
    'te_output_crs', 'te_output_vrs', 'te_output_nirs',
    'te_directional_crs', 'te_directional_vrs', 'te_directional_nirs',
    'te_input_region_crs', 'te_input_region_vrs', 'te_input_region_nirs',
    'te_output_region_crs', 'te_output_region_vrs', 'te_output_region_nirs',
    'te_directional_region_crs', 'te_directional_region_vrs', 'te_directional_region_nirs',
    'te_input_grd_crs', 'te_input_grd_vrs', 'te_input_grd_nirs',
    'te_output_grd_crs', 'te_output_grd_vrs', 'te_output_grd_nirs',
    'te_directional_grd_crs', 'te_directional_grd_vrs', 'te_directional_grd_nirs'
]

COMPARISON_OPTIONS_MINSAL = [
    ['te_input_crs', 'te_input_vrs', 'te_input_nirs',
    'te_output_crs', 'te_output_vrs', 'te_output_nirs',
    'te_directional_crs', 'te_directional_vrs', 'te_directional_nirs'],
    ['te_input_grd_crs', 'te_input_grd_vrs', 'te_input_grd_nirs',
    'te_output_grd_crs', 'te_output_grd_vrs', 'te_output_grd_nirs',
    'te_directional_grd_crs', 'te_directional_grd_vrs', 'te_directional_grd_nirs']
]
COMPARISON_OPTIONS_GEO = [
    ['te_input_region_crs', 'te_input_region_vrs', 'te_input_region_nirs',
    'te_output_region_crs', 'te_output_region_vrs', 'te_output_region_nirs',
    'te_directional_region_crs', 'te_directional_region_vrs', 'te_directional_region_nirs'],
    ['te_input_grd_crs', 'te_input_grd_vrs', 'te_input_grd_nirs',
    'te_output_grd_crs', 'te_output_grd_vrs', 'te_output_grd_nirs',
    'te_directional_grd_crs', 'te_directional_grd_vrs', 'te_directional_grd_nirs']
]

minsal_correlation = build_correlation_rows(COMPARISON_OPTIONS_MINSAL, 'MINSAL')
geo_correlation = build_correlation_rows(COMPARISON_OPTIONS_MINSAL, 'GEOGRÁFICA')

splitted_data_minsal = [s.split(' & ') for s in minsal_correlation.values.tolist()]
diagonal_elements_minsal = []
i = 0
while i < len(splitted_data_minsal):
    diagonal_elements_minsal.append(splitted_data_minsal[i][i + 1])
    i += 1

splitted_data_geo = [s.split(' & ') for s in geo_correlation.values.tolist()]
diagonal_elements_geo = []
i = 0
while i < len(splitted_data_geo):
    diagonal_elements_geo.append(splitted_data_geo[i][i + 1])
    i += 1

# Imprimir la lista de elementos actuales más uno
diagonal_elements_minsal = [float(item) for item in diagonal_elements_minsal]
diagonal_elements_geo = [float(item) for item in diagonal_elements_geo]

labels = ["GRD CRS-I", "GRD VRS-I", "GRD NIRS-I", "GRD CRS-O", "GRD VRS-O", "GRD NIRS-O", "GRD CRS-D", "GRD VRS-D", "GRD NIRS-D"]

# Configuración de la gráfica
x = np.arange(len(labels))  # Ubicaciones de las etiquetas
width = 0.35  # Ancho de las barras

fig, ax = plt.subplots(figsize=(10, 6))
bars1 = ax.bar(x - width/2, diagonal_elements_minsal, width, label='Clasificación MINSAL', color='#277A7A')
bars2 = ax.bar(x + width/2, diagonal_elements_geo, width, label='Clasificación GEOGRÁFICA', color='#FF9900')

# Etiquetas, título y leyenda
ax.set_xlabel('Modelos eficiencia técnica - GRD')
ax.set_ylabel('Valor correlación')
ax.set_title(f'{correlation_name_dictionary(CORRELATION)} - {YEAR}')
ax.set_xticks(x)
ax.set_xticklabels(labels, rotation=45, ha='right')
ax.set_ylim(0, 1)
ax.legend()

fig.tight_layout()

file_name = '_'.join(correlation_name_dictionary(CORRELATION).split(' ')).lower()
file_path = f"../../../results/report/images/{file_name} - {YEAR}.png"
plt.savefig(file_path)
