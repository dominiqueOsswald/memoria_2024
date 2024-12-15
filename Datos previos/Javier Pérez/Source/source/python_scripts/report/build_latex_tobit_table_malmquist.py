""" Module to build the Latex table of malmquist tobit models"""

# External imports
import sys
import pandas as pd
from collections import Counter

# Sys instructions
sys.path.append("..")

# Internal imports
from common import config, helpers

# Internal functions

def merge_columns(data_frame, scope):
    data_frame[scope[0]] = data_frame[scope[0]].apply(lambda x: f'{float(x):.3f}')
    data_frame[scope[1]] = data_frame[scope[1]].apply(lambda x: '<0.001' if float(x) < 0.001 else f'{float(x):.3f}')

    data_frame['merged'] = data_frame.apply(
        lambda row: f"{row[scope[0]]} ({row[scope[1]]} {row[scope[2]]})", axis=1
    )

    return data_frame

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

# Body

orientations = ['in', 'out']
model_type = ['crs', 'vrs', 'nirs']
years = config.YEARS_RANGE
tobit_columns = ['coefficients','p_values','significance_levels']

most_significants = []

for orientation in orientations:
    for model in model_type:
        for year in years[1:]:
            path = f'../../../results/dea_malmquist/tobit_results_{year}_{orientation}_{model}.csv'
            data_frame = pd.read_csv(path)
            data_frame = merge_columns(data_frame, tobit_columns)

            most_significant_values = data_frame[tobit_columns].min().to_list()

            data_frame_filtered = data_frame.loc[data_frame['p_values'] == f'{float(most_significant_values[1]):.3f}']
            most_significants.append(row_name_dictionary(data_frame_filtered['Unnamed: 0'].iloc[0]))


counter = Counter(most_significants)
print(counter)