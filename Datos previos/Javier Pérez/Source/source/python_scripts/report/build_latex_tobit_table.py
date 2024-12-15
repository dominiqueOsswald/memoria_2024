""" Module to build the Latex table of tobit models"""

# External imports
import sys
import pandas as pd

# Sys instructions
sys.path.append("..")

# Internal imports
from common import config, helpers

# Internal functions
def build_tobit_table(kind, data_frame):
    data_frame = data_frame.astype(str)
    section = f"\\subsection{{Modelo Tobit Año {YEAR} - {kind}}}"

    header_input, footer_input = section_kind(kind, 'Input')
    header_output, footer_output = section_kind(kind, 'Output')
    header_directional, footer_directional = section_kind(kind,  'Directional')

    data_frame_input = data_frame[['Unnamed: 0', 'input_crs', 'input_vrs', 'input_nirs']]
    data_frame_output = data_frame[['Unnamed: 0', 'output_crs', 'output_vrs', 'output_nirs']]
    data_frame_directional = data_frame[['Unnamed: 0', 'directional_crs', 'directional_vrs', 'directional_nirs']]

    joined_rows_input = data_frame_input.apply(join_row, axis=1)
    joined_rows_output = data_frame_output.apply(join_row, axis=1)
    joined_rows_directional = data_frame_directional.apply(join_row, axis=1)

    result_input = ' \\\\ \\hline \n'.join(joined_rows_input)
    result_output = ' \\\\ \\hline \n'.join(joined_rows_output)
    result_directional = ' \\\\ \\hline \n'.join(joined_rows_directional)

    print(section + '\n')
    print(header_input + '\n' + result_input + ' \\\\ \\hline \n' + footer_input + '\n')
    print(header_output + '\n' + result_output + ' \\\\ \\hline \n' + footer_output + '\n')
    print(header_directional + '\n' + result_directional + ' \\\\ \\hline \n' + footer_directional)

def section_kind(kind, orientation):

    header =  r"""\begin{table}[H]
    \centering
    \scalebox{0.75}{
        \begin{tabular}{|c|c|c|c|c|}
            \hline
            \multirow{2}{*}{Variable} & \multicolumn{3}{c|}{orientation} \\ \cline{2-4} 
            & \multicolumn{1}{c|}{CRS} & \multicolumn{1}{c|}{VRS} & NIRS \\ \hline
    """

    header = header.replace("orientation", orientation)

    footer = r"""       \end{{tabular}}
    }}
    \caption{{Modelo TOBIT año {YEAR} - {kind} ({orientation}). \\ Fuente: elaboración propia.}}
\end{{table}}""".format(YEAR=YEAR, kind=kind, orientation=orientation)

    return header, footer

def row_name_dictionary(name):
    mapping = {
        'X21_value_normalized': 'Subtítulo 21',
        'X22_value_normalized': 'Subtítulo 22',
        'hospital_discharge_normalized': 'Egreso Hospitalario',
        'diagnostic_tests_normalized': 'Exámenes de Diagnóstico',
        'surgical_interventions_normalized': 'Intervenciones Quirúrgicas',
        'medical_consultations_normalized': 'Consultas Médicas'
    }
    return mapping.get(name, name)

def merge_columns(data_frame, column_name, scope):
    data_frame[scope[0]] = data_frame[scope[0]].apply(lambda x: f'{float(x):.3f}')
    data_frame[scope[1]] = data_frame[scope[1]].apply(lambda x: '<0.001' if float(x) < 0.001 else f'{float(x):.3f}')

    data_frame[column_name] = data_frame.apply(
        lambda row: f"{row[scope[0]]} ({row[scope[1]]} {row[scope[2]]})", axis=1
    )

    return data_frame

def join_row(row):
    join_values =  ' & '.join(row)
    values = join_values.split(' & ')
    first_value = values[0]
    new_name = row_name_dictionary(first_value)
    output_string = row.replace(first_value, new_name)
    return ' & '.join(output_string)

# Body
PATH = '../../../results/tobit'
YEAR = sys.argv[1]

# Gather all dataframes info
hospital_dataframe = helpers.dataframe_read_csv_file(path=f'../{config.HOSPITAL_DICTIONARY_PATH}')
tobit_model_minsal = helpers.dataframe_read_csv_file(path=f'{PATH}/{YEAR}.csv')
tobit_model_grd = helpers.dataframe_read_csv_file(path=f'{PATH}/{YEAR}_grd.csv')
tobit_model_region = helpers.dataframe_read_csv_file(path=f'{PATH}/{YEAR}_region.csv')

column_groups = [
    ['input_coefficient_crs', 'input_P_value_crs', 'input_significance_crs'],
    ['input_coefficient_vrs', 'input_P_value_vrs', 'input_significance_vrs'],
    ['input_coefficient_nirs', 'input_P_value_nirs', 'input_significance_nirs'],
    ['output_coefficient_crs', 'output_P_value_crs', 'output_significance_crs'],
    ['output_coefficient_vrs', 'output_P_value_vrs', 'output_significance_vrs'],
    ['output_coefficient_nirs', 'output_P_value_nirs', 'output_significance_nirs'],
    ['directional_coefficient_crs', 'directional_P_value_crs', 'directional_significance_crs'],
    ['directional_coefficient_vrs', 'directional_P_value_vrs', 'directional_significance_vrs'],
    ['directional_coefficient_nirs', 'directional_P_value_nirs', 'directional_significance_nirs'],
]

final_columns = [
                 'Unnamed: 0',
                 'input_crs', 'input_vrs', 'input_nirs',
                 'output_crs', 'output_vrs', 'output_nirs',
                 'directional_crs', 'directional_vrs', 'directional_nirs']


for group in column_groups:
    splitted_column = group[0].split("_")
    identifier = f'{splitted_column[0]}_{splitted_column[2]}'

    tobit_model_minsal = merge_columns(tobit_model_minsal, identifier, group)
    tobit_model_grd = merge_columns(tobit_model_grd, identifier, group)
    tobit_model_region = merge_columns(tobit_model_region, identifier, group)


# build_tobit_table('Minsal', tobit_model_minsal[final_columns])
# build_tobit_table('GRD', tobit_model_grd[final_columns])
# build_tobit_table('Geográfica', tobit_model_region[final_columns])

p_value_input_columns = ['Unnamed: 0'] + [col for col in tobit_model_minsal.columns if 'input_P_value' in col]
p_value_output_columns = ['Unnamed: 0'] + [col for col in tobit_model_minsal.columns if 'output_P_value' in col]
p_value_directional_columns = ['Unnamed: 0'] + [col for col in tobit_model_minsal.columns if 'directional_P_value' in col]


for model in [tobit_model_minsal, tobit_model_grd, tobit_model_region]:
    for columns in [p_value_input_columns, p_value_output_columns, p_value_directional_columns]:

        aux_dataframe = pd.DataFrame()
        for col in columns:
            aux_dataframe[col] = pd.to_numeric(model[col], errors='coerce')

        most_significant_values = aux_dataframe[columns].min().to_list()

        for idx in [1, 2, 3]:
            df = model[columns]
            column_name = df.columns[idx]
            filtered_df = df.loc[df[column_name] == f'{float(most_significant_values[idx]):.3f}']
            unnamed_value = filtered_df['Unnamed: 0'].iloc[0]
            print(row_name_dictionary(unnamed_value))
