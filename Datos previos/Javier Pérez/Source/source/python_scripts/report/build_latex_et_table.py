""" Module to build the Latex table of E.T models """

# External imports
import sys
import pandas as pd

# Sys instructions
sys.path.append("..")

# Internal imports
from common import config, helpers

# Internal functions
def build_et_table(kind):
    section = f"\\subsection{{Eficiencia técnica Año {YEAR} - {kind}}}"

    header_input_1, footer_input_1 = section_kind(kind, '1')
    header_input_2, footer_input_2 = section_kind(kind, '2')

    rows, rows_scope = build_et_rows(kind)

    print(section + '\n')
    print(header_input_1 + '\n' + '\n'.join(rows[rows_scope[0]]) + '\n' + footer_input_1 + '\n')
    print(header_input_2 + '\n' + '\n'.join(rows[rows_scope[1]]) + '\n' + footer_input_2 + '\n')

def build_et_rows(kind):
    if kind == 'GRD':
        indentifier = '_grd'
        first_range = slice(0, 27)
        second_range = slice(27, 54)
    elif kind == 'Geográfica':
        indentifier = '_region'
        first_range = slice(0, 24)
        second_range = slice(24, 49)
    else:
        indentifier = ''
        first_range = slice(0, 27)
        second_range = slice(27, 54)
    
    # Get dataframes
    hospital_dataframe = helpers.dataframe_read_csv_file(path=f'../{config.HOSPITAL_DICTIONARY_PATH}')
    et_dataframe_input = helpers.dataframe_read_csv_file(path=f'{PATH}/te_input{indentifier}.csv')
    et_dataframe_output = helpers.dataframe_read_csv_file(path=f'{PATH}/te_output{indentifier}.csv')
    et_dataframe_directional = helpers.dataframe_read_csv_file(path=f'{PATH}/te_directional{indentifier}.csv')

    # Order dataframes
    hospital_dataframe = helpers.data_frame_sort_values(hospital_dataframe, 'hospital_id')
    et_dataframe_input = helpers.data_frame_sort_values(et_dataframe_input, 'hospital_id')
    et_dataframe_output = helpers.data_frame_sort_values(et_dataframe_output, 'hospital_id')
    et_dataframe_directional = helpers.data_frame_sort_values(et_dataframe_directional, 'hospital_id')

    # Create new Dataframe
    merged_df =  pd.DataFrame()
    merged_df['hospital_name'] = et_dataframe_input['hospital_name']

    merged_df['te_input_crs'] = et_dataframe_input[f'{YEAR}_te_crs']
    merged_df['te_input_vrs'] = et_dataframe_input[f'{YEAR}_te_vrs']
    merged_df['te_input_nirs'] = et_dataframe_input[f'{YEAR}_te_nirs']

    merged_df['te_output_crs'] = et_dataframe_output[f'{YEAR}_te_crs']
    merged_df['te_output_vrs'] = et_dataframe_output[f'{YEAR}_te_vrs']
    merged_df['te_output_nirs'] = et_dataframe_output[f'{YEAR}_te_nirs']

    merged_df['te_directional_crs'] = et_dataframe_directional[f'{YEAR}_te_crs']
    merged_df['te_directional_vrs'] = et_dataframe_directional[f'{YEAR}_te_vrs']
    merged_df['te_directional_nirs'] = et_dataframe_directional[f'{YEAR}_te_nirs']

    merged_df['merged'] = merged_df.apply(lambda row: 
        f"\t {row['hospital_name']} & "
        f"{row['te_input_crs']:.2f} & {row['te_input_vrs']:.2f} & {row['te_input_nirs']:.2f} & "
        f"{row['te_output_crs']:.2f} & {row['te_output_vrs']:.2f} & {row['te_output_nirs']:.2f} & "
        f"{row['te_directional_crs']:.2f} & {row['te_directional_vrs']:.2f} & {row['te_directional_nirs']:.2f} \\\ \\hline",
        axis=1)

    return merged_df['merged'].tolist(), [first_range, second_range]

def section_kind(kind, step):
    header =    r"""\begin{table}[H]
    \centering
    \scalebox{0.75}{
        \begin{tabular}{|c|lllllllll|}
            \hline
            \multirow{2}{*}{Nombre} & \multicolumn{3}{c|}{Input} & \multicolumn{3}{c|}{Output} & \multicolumn{3}{c|}{Directional} \\ \cline{2-10}
            & \multicolumn{1}{l|}{CRS} & \multicolumn{1}{l|}{VRS} & \multicolumn{1}{l|}{NIRS}
            & \multicolumn{1}{l|}{CRS} & \multicolumn{1}{l|}{VRS} & \multicolumn{1}{l|}{NIRS}
            & \multicolumn{1}{l|}{CRS} & \multicolumn{1}{l|}{VRS} & \multicolumn{1}{l|}{NIRS} \\ \hline
    """

    footer = r"""       \end{{tabular}}
    }}
    \caption{{Eficiencia técnica año {YEAR} - {kind} ({step}/2)}}
\end{{table}}""".format(YEAR=YEAR, kind=kind, step=step)

    return header, footer

# Body
PATH = '../../../results/report'
YEAR = sys.argv[1]


# print(TABLE_HEADER + '\n'.join(latex_list[first_range]) + '\n' + TABLE_FOOTER)
# print('\n')
# print(TABLE_HEADER + '\n'.join(latex_list[second_range]) + '\n' + TABLE_FOOTER)

for element in ['Minsal', 'GRD', 'Geográfica']:
    build_et_table(element)
