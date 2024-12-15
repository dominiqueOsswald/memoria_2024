""" Module to retrieve REM raw_data of hospitals """

# External imports
import pandas as pd
import sys

# Internal imports
from common import config, helpers

# Internal functions
def rem_path(year, rem_id):
    """ Builds the paths to retrieve rem file
    """
    return f'../../raw_data/REM/{year}/{rem_id}.csv'


def formatted_dataframe(output):
    """ Deletes duplicated columns from dataframe
    """
    unique_columns = []
    for col in output.columns:
        if col not in unique_columns:

            unique_columns.append(col)
    return output[unique_columns].iloc[:, 2:]

# Constants
REM_IDS = ['A04', 'B17', '20']
REM_PROCEDURE_IDS = [config.MEDICAL_CONSULTATIONS_IDS,
                     config.SURGICAL_INTERVENTIONS_IDS + config.DIAGNOSTIC_TEST_IDS,
                     ['Datos Establecimiento-Numero de Egresos']]
TOTAL_COLUMN = 'Col-1'

# Body

hospital_dataframe = helpers.dataframe_read_csv_file(path=config.HOSPITAL_DICTIONARY_PATH)
helpers.dataframe_rewrite_index(data_frame=hospital_dataframe, new_index='hospital_id')
hospital_ids = helpers.dataframe_index_to_list(data_frame=hospital_dataframe)

for year in config.YEARS_RANGE:
    REMA04_path = rem_path(year, REM_IDS[0])
    REMB17_path = rem_path(year, REM_IDS[1])
    REM20_path = rem_path(year, REM_IDS[2])
    REMA04_df = helpers.dataframe_read_csv_file(path=REMA04_path)
    REMB17_df = helpers.dataframe_read_csv_file(path=REMB17_path)
    REM20_df = helpers.dataframe_read_csv_file(path=REM20_path)
    GRD_df = helpers.dataframe_read_csv_file(path='../../data/grd_weight.csv')

    #A04
    REMA04_df = helpers.dataframe_filter_rows(data_frame=REMA04_df,
                                            column_name='hospital_id',
                                            rows=hospital_ids)
    REMA04_df = helpers.dataframe_filter_columns(data_frame=REMA04_df,
                                                columns=['hospital_id'] + [element + '-Col1' for element in REM_PROCEDURE_IDS[0]])

    #B17
    REMB17_df = helpers.dataframe_filter_rows(data_frame=REMB17_df,
                                            column_name='hospital_id',
                                            rows=hospital_ids)
    REMB17_df = helpers.dataframe_filter_columns(data_frame=REMB17_df,
                                                columns=['hospital_id'] + [element + '-Col1' for element in REM_PROCEDURE_IDS[1]])

    #20
    REM20_df = helpers.dataframe_filter_rows(data_frame=REM20_df,
                                            column_name='hospital_id',
                                            rows=hospital_ids)
    REM20_df = helpers.dataframe_filter_columns(data_frame=REM20_df,
                                                columns=['hospital_id'] + REM_PROCEDURE_IDS[2])

    #GRD
    GRD_df = helpers.dataframe_filter_rows(data_frame=GRD_df,
                                            column_name='hospital_id',
                                            rows=hospital_ids)
    GRD_df = helpers.dataframe_filter_columns(data_frame=GRD_df,
                                              columns=['hospital_id', str(year)])

    REMA04_df = helpers.data_frame_sort_values(data_frame=REMA04_df, key='hospital_id')
    REMB17_df = helpers.data_frame_sort_values(data_frame=REMB17_df, key='hospital_id')
    REM20_df = helpers.data_frame_sort_values(data_frame=REM20_df, key='hospital_id')
    GRD_df = helpers.data_frame_sort_values(data_frame=GRD_df, key='hospital_id')

    output_df =  pd.concat([REMA04_df, REMB17_df, REM20_df], axis=1)
    output_df.rename(columns={ col: col.split('-')[0] for col in output_df.columns if col.endswith('-Col1') }, inplace=True)
    output_df = output_df.rename(columns={ 'Datos Establecimiento-Numero de Egresos': 'Datos Establecimiento Numero de Egresos' })
    output_df = formatted_dataframe(output_df)

    output_df[f'GRD{year}'] = GRD_df[str(year)]

    helpers.dataframe_write_csv_file(data_frame=output_df,
                                    path=f'../../raw_data/discharge_data/{year}.csv')

