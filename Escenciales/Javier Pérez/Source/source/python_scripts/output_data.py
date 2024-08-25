""" Module to retrieve discharge data of hospitals """

# Internal imports
from common import config, helpers

# Internal functions
def consolidate_discharge_data(discharge_df, actual_year):
    """ Returns a dataframe with a new column with the
    data consolidated of hospital discharge
    """
    discharge_df['medical_consultations'] = discharge_df[config.MEDICAL_CONSULTATIONS_IDS].astype(float).sum(axis=1)
    discharge_df['surgical_interventions'] = discharge_df[config.SURGICAL_INTERVENTIONS_IDS].astype(float).sum(axis=1)
    discharge_df['diagnostic_tests'] = discharge_df[config.DIAGNOSTIC_TEST_IDS].astype(float).sum(axis=1)
    discharge_df[f'GRD{actual_year}'] = discharge_df[f'GRD{actual_year}'].astype(float)
    discharge_df.rename(columns = {'Datos Establecimiento Numero de Egresos': 'hospital_discharge'}, inplace = True)
    discharge_df.rename(columns = {f'GRD{actual_year}': 'GRD'}, inplace = True)
    return discharge_df

def swap_discharge_dataframe_columns(discharge_df):
    """ Swaps last 2 columns of the dataframe for
    better order
    """
    columns = discharge_df.columns.tolist()
    columns[-1], columns[-2], columns[-3], columns[-4] = (columns[-4], columns[-3],
                                                          columns[-2], columns[-1])
    return discharge_df[columns]

# Body

hospital_dataframe = helpers.dataframe_read_csv_file(path=config.HOSPITAL_DICTIONARY_PATH)
years = config.YEARS_RANGE

print("\033[92mRetrieving discharge data...\033[0m")

for year in years:
    raw_discharge_data_path = f'../../raw_data/discharge_data/{year}.csv'

    columns_to_filter = (config.HOSPITAL_ID + config.MEDICAL_CONSULTATIONS_IDS +
                         config.SURGICAL_INTERVENTIONS_IDS + config.DIAGNOSTIC_TEST_IDS +
                         config.DISCHARGE_NUMBER + [f'GRD{year}'])

    output_columns = ['hospital_id',
                      'hospital_discharge',
                      'diagnostic_tests',
                      'surgical_interventions',
                      'medical_consultations',
                      'GRD']

    helpers.dataframe_rewrite_index(data_frame=hospital_dataframe, new_index='hospital_id')
    discharge_data_df = helpers.dataframe_read_csv_file(path=raw_discharge_data_path)
    hospital_ids = helpers.dataframe_index_to_list(data_frame=hospital_dataframe)

    discharge_data_df = helpers.dataframe_filter_rows(data_frame=discharge_data_df,
                                                      column_name='hospital_id',
                                                      rows=hospital_ids)
    discharge_data_df = helpers.dataframe_filter_columns(data_frame=discharge_data_df,
                                                         columns=columns_to_filter)

    discharge_data_df = consolidate_discharge_data(discharge_data_df, year)
    discharge_data_df = helpers.dataframe_filter_columns(data_frame=discharge_data_df,
                                                         columns=output_columns)

    helpers.dataframe_write_csv_file(data_frame=discharge_data_df,
                                     path=f'../../data/output/{year}.csv')
    helpers.dataframe_reset_index(data_frame=hospital_dataframe)

    print(f"\t\033[93mYear: {year} done!\033[93m")

print("\033[92mDischarge data retrieved!\033[0m")
