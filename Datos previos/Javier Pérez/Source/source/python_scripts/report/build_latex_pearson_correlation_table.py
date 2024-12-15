""" Module to build the Latex table of E.T models """

# External imports
import sys
import warnings
import pandas as pd

# Instructions
sys.path.append("..")
warnings.filterwarnings('ignore')

# Internal imports
from common import config, helpers

def section_kind(table_number):
    num_columns = 9  # Number of columns in each table
    column_width = "0.5cm"  # Adjust the width as needed

    headers = [
        ['CRS-I', 'VRS-I', 'NIRS-I', 'CRS-O', 'VRS-O', 'NIRS-O', 'CRS-D', 'VRS-D', 'NIRS-D'],
        ['Geo. CRS-I', 'Geo. VRS-I', 'Geo. NIRS-I', 'Geo. CRS-O', 'Geo. VRS-O', 'Geo. NIRS-O', 'Geo. CRS-D', 'Geo. VRS-D', 'Geo. NIRS-D'],
        ['GRD CRS-I', 'GRD VRS-I', 'GRD NIRS-I', 'GRD CRS-O', 'GRD VRS-O', 'GRD NIRS-O', 'GRD CRS-D', 'GRD VRS-D', 'GRD NIRS-D']
    ]

    header = r"""\begin{table}[H]
    \centering
    \scalebox{0.625}{
             \begin{tabular}{|c|""" + '|'.join(['p{' + column_width + '}' for _ in range(num_columns)]) + '|}\n'
    header += '                & ' + ' & '.join(['\\rotatebox{90}{{' + h + '}}' for h in headers[table_number - 1]]) + ' \\\\ \\hline\n'

    footer = r"""\end{{tabular}}
    }}
    \caption{{Valores distancia correlación Pearson - Eficiencia técnica {YEAR} ({table_number}/3)}}
\end{{table}}""".format(YEAR=YEAR, table_number=table_number)

    return header, footer

def build_correlation_tables():
    correlation_dataframe = helpers.dataframe_read_csv_file(path=f'{PATH}/{YEAR}.csv')
    correlation_dataframe = correlation_dataframe.astype(str)
    correlation_dataframe['et_names'] = correlation_dataframe.columns.to_list()

    section = f"\\subsection{{Matriz distancia correlación Pearson {YEAR} }}"
    print(section + '\n')

    for table_number in range(1, 4):
        columns_for_table = DATA_COLUMNS[1 + (table_number - 1) * 9 : 1 + table_number * 9]
        selected_columns = ['et_names'] + columns_for_table
        table_dataframe = correlation_dataframe[selected_columns]
        for col in columns_for_table:
            table_dataframe[col] = table_dataframe[col].astype(str).apply(lambda x: f'{float(x):.2f}')
        
        table_rows = table_dataframe.apply(join_row, axis=1)
        result = ' \\\\ \\hline \n'.join(table_rows)
        
        
        header, footer = section_kind(table_number)
        
        
        print(header + result + ' \\\\ \\hline \n' + footer + '\n')

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
    correlation_dataframe['et_names'] = correlation_dataframe.columns.to_list()

    correlation_dataframe = correlation_dataframe[DATA_COLUMNS]
    for col in DATA_COLUMNS:
        if col != 'et_names':
            correlation_dataframe[col] = correlation_dataframe[col].astype(str).apply(lambda x: f'{float(x):.2f}')
    print('\n')

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
PATH = '../../../results/pearson_correlation'
YEAR = sys.argv[1]
DATA_COLUMNS = [
    'et_names',
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

build_correlation_tables()
