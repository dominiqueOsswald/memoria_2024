""" Module to build the Latex table of tobit models"""

# External imports
import sys
import pandas as pd

# Sys instructions
sys.path.append("..")

# Internal imports
from common import config, helpers

# Internal functions
def check_dominance(df, year, orientation):
    df[f'dominance_{year}'] = '-'
    
    if orientation == 'directional':
        # Marca las filas con los valores de eficiencia más altos
        # Esto genera una frontera relativa
        max_value = df[f'te_{orientation}'].max()
        df.loc[df[f'te_{orientation}'] == max_value, f'dominance_{year}'] = 'X'
    else:
        df.loc[df[f'te_{orientation}'] == 1, f'dominance_{year}'] = 'X'
    
    return df

def inner_section_kind(model_orientation, return_type):
    header = r"""\begin{table}[H]
    \centering
    \scalebox{0.6}{
        \begin{tabular}{|c|c|c|c|c|c|c|c|c|c|}
            \hline
            Nombre & 2014 & 2015 & 2016 & 2017 & 2018 & 2019 & 2020 & Total \\ \hline
    """

    footer = r"""       \end{{tabular}}
    }}
    \caption{{Dominancia hospitales - Modelo {model_orientation} {return_type} }}
    \end{{table}}""".format(model_orientation=model_orientation.title(), return_type=return_type.upper())

    return header, footer

def build_table(df):
    section = f"\\subsection{{Dominancia hospitales - Modelo {model_orientation.title()} {return_type.upper()} }}"
    header, footer = inner_section_kind(model_orientation, return_type)
    print(section + '\n')
    print(header + '\n'.join(build_rows(df)) + '\n' + footer)

def build_rows(df):
    df['merged'] = df.apply(lambda row: f"\t    {row['hospital_name']} & "
                                        f"{row['dominance_2014']} & {row['dominance_2015']} & {row['dominance_2016']} & "
                                        f"{row['dominance_2017']} & {row['dominance_2018']} & {row['dominance_2019']} & "
                                        f"{row['dominance_2020']} & {total_dominance(row)} \\\ \\hline",
                                        axis=1)
    return df['merged'].tolist()

def total_dominance(collection):
    acc = 0
    for element in config.YEARS_RANGE:
        if collection[f'dominance_{element}'] == 'X':
            acc += 1
    return acc

# Body
years = config.YEARS_RANGE
model_orientations = ['input', 'output', 'directional']
return_types = ['crs', 'vrs', 'nirs']
hospital_dataframe = helpers.dataframe_read_csv_file(path=f'../{config.HOSPITAL_DICTIONARY_PATH}')
dominance_dataframe = pd.DataFrame()
classification_type = sys.argv[1]

if classification_type == 'MINSAL':
    results_path = 'dea_te'
elif classification_type == 'REGION':
    results_path = 'dea_te_region'
elif classification_type == 'GRD':
    results_path = 'dea_te_grd'

print(f"\\section{{Dominancia hospitales - Clasificación {classification_type}}}\n")

for model_orientation in model_orientations:
    for return_type in return_types:
        for year in years:
            actual_year_df = helpers.dataframe_read_csv_file(path=f'../../../results/{results_path}/{year}_{return_type}.csv')
            actual_year_df = helpers.data_frame_sort_values(data_frame=actual_year_df, key='hospital_id')
            actual_year_df = helpers.dataframe_filter_columns(data_frame=actual_year_df,
                                                            columns=['hospital_id', f'te_{model_orientation}'])
            if year == '2014':
                dominance_dataframe['hospital_id'] = actual_year_df['hospital_id']

            actual_year_df = check_dominance(actual_year_df, year, model_orientation)
            dominance_dataframe[f'dominance_{year}'] = actual_year_df[f'dominance_{year}']

        for index, row in dominance_dataframe.iterrows():
            hospital_id = row['hospital_id']
            matching_row = hospital_dataframe[hospital_dataframe['hospital_id'] == hospital_id]
            dominance_dataframe.at[index, 'hospital_name'] = matching_row['hospital_alternative_name'].values[0] if not matching_row.empty else ''

        build_table(dominance_dataframe)
