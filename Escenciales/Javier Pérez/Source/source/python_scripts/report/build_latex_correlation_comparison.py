""" Module to build the Latex table of E.T models """

# External imports
import sys
import pandas as pd

# Sys instructions
sys.path.append("..")

# Internal imports
from common import config, helpers

def build_correlation_table():
    header, footer = section_kind()
    joined_rows = build_correlation_rows()
    result = '\\\\ \\hline \n'.join(joined_rows)

    print(header  + result + ' \\\\ \\hline \n' + footer + '\n')

def section_kind():
    if CORRELATION == 'pearson':
        etiqueta = 'Correlación'
        label = 'correlation'
    elif CORRELATION == 'euclidean':
        etiqueta = 'Distancia'
        label = 'distance'
    else:
        print('Error')

    header =    r"""\begin{table}[H]
    \centering
    \scalebox{0.55}{
        \begin{tabular}{c|*{18}{c|}}
        joined_values
    """

    header = header.replace("joined_values",'& ' + ' & '.join([row_name_dictionary(key) for key in COMPARISON_OPTIONS[1]]) + ' \\\\ \\hline')

    footer = r"""       \end{{tabular}}
    }}
    \caption{{Matriz {etiqueta} {TYPE} con GRD - {YEAR}}}
    \label{{tab:{label}_{TYPE}_{YEAR}}}
\end{{table}}""".format(YEAR=YEAR, TYPE=TYPE, etiqueta=etiqueta, label=label)

    return header, footer

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

def build_correlation_rows():
    correlation_dataframe = helpers.dataframe_read_csv_file(path=f'{PATH}/{YEAR}.csv')
    correlation_dataframe = correlation_dataframe.astype(str)
    correlation_dataframe = correlation_dataframe[COMPARISON_OPTIONS[1]]
    if TYPE == 'MINSAL':
        correlation_dataframe = correlation_dataframe.iloc[:9]
    elif TYPE == 'GEOGRÁFICA':
        correlation_dataframe = correlation_dataframe.iloc[9:18]
    else:
        print('Error')
    correlation_dataframe['et_names'] = COMPARISON_OPTIONS[0]


    for col in COMPARISON_OPTIONS[1]:
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
TYPE = sys.argv[2]
CORRELATION = sys.argv[3].lower()

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

if TYPE == 'MINSAL':
    COMPARISON_OPTIONS = [
        ['te_input_crs', 'te_input_vrs', 'te_input_nirs',
        'te_output_crs', 'te_output_vrs', 'te_output_nirs',
        'te_directional_crs', 'te_directional_vrs', 'te_directional_nirs'],
        ['te_input_grd_crs', 'te_input_grd_vrs', 'te_input_grd_nirs',
        'te_output_grd_crs', 'te_output_grd_vrs', 'te_output_grd_nirs',
        'te_directional_grd_crs', 'te_directional_grd_vrs', 'te_directional_grd_nirs']
    ]
elif TYPE == 'GEOGRÁFICA':
    COMPARISON_OPTIONS = [
        ['te_input_region_crs', 'te_input_region_vrs', 'te_input_region_nirs',
        'te_output_region_crs', 'te_output_region_vrs', 'te_output_region_nirs',
        'te_directional_region_crs', 'te_directional_region_vrs', 'te_directional_region_nirs'],
        ['te_input_grd_crs', 'te_input_grd_vrs', 'te_input_grd_nirs',
        'te_output_grd_crs', 'te_output_grd_vrs', 'te_output_grd_nirs',
        'te_directional_grd_crs', 'te_directional_grd_vrs', 'te_directional_grd_nirs']
    ]
else:
    print('Error in type')

build_correlation_table()
