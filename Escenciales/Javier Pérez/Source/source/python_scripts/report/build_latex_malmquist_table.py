""" Module to build the Latex table of Malmquist models """

# External imports
import sys
import pandas as pd

# Sys instructions
sys.path.append("..")

# Internal imports
from common import config, helpers

def build_malmquist_table(df, h_df, kind, orientation):
    header_1, footer_1 = section_kind(kind, orientation, '1')
    header_2, footer_2 = section_kind(kind, orientation, '2')
    rows = build_malmquist_rows(df, h_df)

    print(header_1 + '\n' + '\n'.join(rows[FIRST_RANGE]) + '\n' + footer_1 + '\n')
    print(header_2 + '\n' + '\n'.join(rows[SECOND_RANGE]) + '\n' + footer_2 + '\n')


def section_kind(kind, orientation, step):
    header =    r"""\begin{table}[H]
    \centering
    \scalebox{0.75}{
        \begin{tabular}{|c|c|c|c|c|c|c|}
            \hline
            Nombre & 2015 & 2016 & 2017 & 2017 & 2019 & 2020  \\ \hline"""

    footer = r"""       \end{{tabular}}
    }}
    \caption{{Resultado Malmquist-DEA - {kind}-{orientation} ({step}/2)}}
\end{{table}}""".format(kind=kind,orientation=orientation, step=step)

    return header, footer

def row_name_dictionary(name):
    mapping = {
        'in': 'Input',
        'out': 'Output'
    }
    return mapping.get(name, name)

def build_malmquist_rows(df, h_df):
    row_list = []

    for element in df.values.tolist():
        formatted_elements = (h_df.loc[h_df['hospital_id'] == int(str(element[0])), 'hospital_alternative_name'].tolist())
        for index, val in enumerate(element[1:], start=1):
            formatted_val = f'{float(val):.3f}' if index != 0 else str(val)  # Ajuste aquí
            formatted_elements.append(formatted_val)
        row_list.append('\t    ' + ' & '.join(formatted_elements) + '  \\\\ \\hline')

    return row_list

FIRST_RANGE = slice(0, 27)
SECOND_RANGE = slice(27, 54)
PATH = '../../../results/dea_malmquist_grd'
hospital_dataframe = helpers.dataframe_read_csv_file(path=f'../{config.HOSPITAL_DICTIONARY_PATH}')
hospital_dataframe = helpers.data_frame_sort_values(data_frame=hospital_dataframe, key='hospital_id')

for orientation in ["in", "out"]:
    titleize_orientation = row_name_dictionary(orientation)
    section = f"\\subsection{{Malmquist-DEA {titleize_orientation}}}"
    print(section)
    for return_type in ["crs", "vrs", "nirs"]:
        malmquist_dataframe = helpers.dataframe_read_csv_file(path=f'{PATH}/malmquist_{orientation}_{return_type}.csv')
        malmquist_dataframe = helpers.data_frame_sort_values(data_frame=malmquist_dataframe, key='hospital_id')

        malmquist_dataframe = malmquist_dataframe.astype(str)
        build_malmquist_table(malmquist_dataframe, hospital_dataframe, return_type.upper(), titleize_orientation)

