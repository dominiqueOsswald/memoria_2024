""" Module to build the Latex table of tobit models"""

# External imports
import sys
import pandas as pd

# Sys instructions
sys.path.append("..")

# Internal imports
from common import config, helpers

# Internal functions
def check_dominance(df, period, orientation, model):
    """
    Marks the dominant rows in the DataFrame based on the given year, orientation, and model.

    Parameters:
    - df (pandas.DataFrame): The DataFrame to be modified.
    - period (str): The period for which dominance is being checked.
    - orientation (str): The orientation of dominance ('directional' or 'non-directional').
    - model (str): The model used for dominance calculation.

    Returns:
    - df (pandas.DataFrame): The modified DataFrame with dominance marked.
    """
    df[f'dominance_{period}_{orientation}_{model}'] = '-'  
    if orientation == 'directional':
        max_value = df[f'te_{orientation}'].max()
        df.loc[df[f'te_{orientation}'] == max_value, f'dominance_{period}_{orientation}_{model}'] = 'X'
    else:
        df.loc[df[f'te_{orientation}'] == 1, f'dominance_{period}_{orientation}_{model}'] = 'X'
    return df

# Body
years = config.YEARS_RANGE
classifications = ['', '_grd', '_region']
hospital_dataframe = helpers.dataframe_read_csv_file(path=f'../{config.HOSPITAL_DICTIONARY_PATH}')

for classification in classifications:
    dominance_dataframe = pd.DataFrame()
    if classification == '':
        translated_classification = 'MINSAL'
    elif classification == '_grd':
        translated_classification = 'GRD'
    else:
        translated_classification = 'REGION'

    for year in years:
        crs_model_path = f'../../../results/dea_te{classification}/{year}_crs.csv'
        vrs_model_path = f'../../../results/dea_te{classification}/{year}_vrs.csv'
        nirs_model_path = f'../../../results/dea_te{classification}/{year}_nirs.csv'

        hospital_dataframe = helpers.data_frame_sort_values(data_frame=hospital_dataframe,
                                                            key='hospital_id')

        actual_year_crs_df = helpers.dataframe_read_csv_file(path=crs_model_path)
        actual_year_crs_df = helpers.data_frame_sort_values(data_frame=actual_year_crs_df,
                                                            key='hospital_id')
        actual_year_crs_df = helpers.dataframe_filter_columns(data_frame=actual_year_crs_df,
                                                              columns=['hospital_id', 'te_input', 'te_output', 'te_directional'])
        
        actual_year_vrs_df = helpers.dataframe_read_csv_file(path=vrs_model_path)
        actual_year_vrs_df = helpers.data_frame_sort_values(data_frame=actual_year_vrs_df,
                                                            key='hospital_id')
        actual_year_vrs_df = helpers.dataframe_filter_columns(data_frame=actual_year_vrs_df,
                                                            columns=['hospital_id', 'te_input', 'te_output', 'te_directional'])
        
        actual_year_nirs_df = helpers.dataframe_read_csv_file(path=nirs_model_path)
        actual_year_nirs_df = helpers.data_frame_sort_values(data_frame=actual_year_nirs_df,
                                                             key='hospital_id')
        actual_year_nirs_df = helpers.dataframe_filter_columns(data_frame=actual_year_nirs_df,
                                                               columns=['hospital_id', 'te_input', 'te_output', 'te_directional'])

        if year == '2014':
            dominance_dataframe['hospital_id'] = actual_year_crs_df['hospital_id']
            dominance_dataframe['hospital_alternative_name'] = hospital_dataframe['hospital_alternative_name']

        actual_year_crs_df = check_dominance(actual_year_crs_df, year, 'input', 'crs')
        actual_year_crs_df = check_dominance(actual_year_crs_df, year, 'input', 'vrs')
        actual_year_crs_df = check_dominance(actual_year_crs_df, year, 'input', 'nirs')
        dominance_dataframe[f'dominance_{year}_input_crs'] = actual_year_crs_df[f'dominance_{year}_input_crs']
        dominance_dataframe[f'dominance_{year}_input_vrs'] = actual_year_crs_df[f'dominance_{year}_input_vrs']
        dominance_dataframe[f'dominance_{year}_input_nirs'] = actual_year_crs_df[f'dominance_{year}_input_nirs']

        actual_year_vrs_df = check_dominance(actual_year_vrs_df, year, 'output', 'crs')
        actual_year_vrs_df = check_dominance(actual_year_vrs_df, year, 'output', 'vrs')
        actual_year_vrs_df = check_dominance(actual_year_vrs_df, year, 'output', 'nirs')
        dominance_dataframe[f'dominance_{year}_output_crs'] = actual_year_vrs_df[f'dominance_{year}_output_crs']
        dominance_dataframe[f'dominance_{year}_output_vrs'] = actual_year_vrs_df[f'dominance_{year}_output_vrs']
        dominance_dataframe[f'dominance_{year}_output_nirs'] = actual_year_vrs_df[f'dominance_{year}_output_nirs']

        actual_year_nirs_df = check_dominance(actual_year_nirs_df, year, 'directional', 'crs')
        actual_year_nirs_df = check_dominance(actual_year_nirs_df, year, 'directional', 'vrs')
        actual_year_nirs_df = check_dominance(actual_year_nirs_df, year, 'directional', 'nirs')
        dominance_dataframe[f'dominance_{year}_directional_crs'] = actual_year_nirs_df[f'dominance_{year}_directional_crs']
        dominance_dataframe[f'dominance_{year}_directional_vrs'] = actual_year_nirs_df[f'dominance_{year}_directional_vrs']
        dominance_dataframe[f'dominance_{year}_directional_nirs'] = actual_year_nirs_df[f'dominance_{year}_directional_nirs']

    dominance_dataframe['input_crs'] = dominance_dataframe[['dominance_2014_input_crs', 'dominance_2015_input_crs', 'dominance_2016_input_crs', 'dominance_2017_input_crs', 'dominance_2018_input_crs', 'dominance_2019_input_crs', 'dominance_2020_input_crs']].sum(axis=1)
    dominance_dataframe['input_vrs'] = dominance_dataframe[['dominance_2014_input_vrs', 'dominance_2015_input_vrs', 'dominance_2016_input_vrs', 'dominance_2017_input_vrs', 'dominance_2018_input_vrs', 'dominance_2019_input_vrs', 'dominance_2020_input_vrs']].sum(axis=1)
    dominance_dataframe['input_nirs'] = dominance_dataframe[['dominance_2014_input_nirs', 'dominance_2015_input_nirs', 'dominance_2016_input_nirs', 'dominance_2017_input_nirs', 'dominance_2018_input_nirs', 'dominance_2019_input_nirs', 'dominance_2020_input_nirs']].sum(axis=1)
    dominance_dataframe['output_crs'] = dominance_dataframe[['dominance_2014_output_crs', 'dominance_2015_output_crs', 'dominance_2016_output_crs', 'dominance_2017_output_crs', 'dominance_2018_output_crs', 'dominance_2019_output_crs', 'dominance_2020_output_crs']].sum(axis=1)
    dominance_dataframe['output_vrs'] = dominance_dataframe[['dominance_2014_output_vrs', 'dominance_2015_output_vrs', 'dominance_2016_output_vrs', 'dominance_2017_output_vrs', 'dominance_2018_output_vrs', 'dominance_2019_output_vrs', 'dominance_2020_output_vrs']].sum(axis=1)
    dominance_dataframe['output_nirs'] = dominance_dataframe[['dominance_2014_output_nirs', 'dominance_2015_output_nirs', 'dominance_2016_output_nirs', 'dominance_2017_output_nirs', 'dominance_2018_output_nirs', 'dominance_2019_output_nirs', 'dominance_2020_output_nirs']].sum(axis=1)
    dominance_dataframe['directional_crs'] = dominance_dataframe[['dominance_2014_directional_crs', 'dominance_2015_directional_crs', 'dominance_2016_directional_crs', 'dominance_2017_directional_crs', 'dominance_2018_directional_crs', 'dominance_2019_directional_crs', 'dominance_2020_directional_crs']].sum(axis=1)
    dominance_dataframe['directional_vrs'] = dominance_dataframe[['dominance_2014_directional_vrs', 'dominance_2015_directional_vrs', 'dominance_2016_directional_vrs', 'dominance_2017_directional_vrs', 'dominance_2018_directional_vrs', 'dominance_2019_directional_vrs', 'dominance_2020_directional_vrs']].sum(axis=1)
    dominance_dataframe['directional_nirs'] = dominance_dataframe[['dominance_2014_directional_nirs', 'dominance_2015_directional_nirs', 'dominance_2016_directional_nirs', 'dominance_2017_directional_nirs', 'dominance_2018_directional_nirs', 'dominance_2019_directional_nirs', 'dominance_2020_directional_nirs']].sum(axis=1)
    dominance_dataframe['directional_crs'] = dominance_dataframe[['dominance_2014_directional_crs', 'dominance_2015_directional_crs', 'dominance_2016_directional_crs', 'dominance_2017_directional_crs', 'dominance_2018_directional_crs', 'dominance_2019_directional_crs', 'dominance_2020_directional_crs']].sum(axis=1)
    dominance_dataframe['directional_vrs'] = dominance_dataframe[['dominance_2014_directional_vrs', 'dominance_2015_directional_vrs', 'dominance_2016_directional_vrs', 'dominance_2017_directional_vrs', 'dominance_2018_directional_vrs', 'dominance_2019_directional_vrs', 'dominance_2020_directional_vrs']].sum(axis=1)
    dominance_dataframe['directional_nirs'] = dominance_dataframe[['dominance_2014_directional_nirs', 'dominance_2015_directional_nirs', 'dominance_2016_directional_nirs', 'dominance_2017_directional_nirs', 'dominance_2018_directional_nirs', 'dominance_2019_directional_nirs', 'dominance_2020_directional_nirs']].sum(axis=1)
    dominance_dataframe['total_dominance'] = dominance_dataframe[['input_crs', 'input_vrs', 'input_nirs', 'output_crs', 'output_vrs', 'output_nirs', 'directional_crs', 'directional_vrs', 'directional_nirs']].apply(lambda x: x.str.count('X').sum(), axis=1)

    dominance_dataframe['input_crs'] = dominance_dataframe['input_crs'].astype(str).apply(lambda x: 'X' if 'X' in x else '')
    dominance_dataframe['input_vrs'] = dominance_dataframe['input_vrs'].astype(str).apply(lambda x: 'X' if 'X' in x else '')
    dominance_dataframe['input_nirs'] = dominance_dataframe['input_nirs'].astype(str).apply(lambda x: 'X' if 'X' in x else '')
    dominance_dataframe['output_crs'] = dominance_dataframe['output_crs'].astype(str).apply(lambda x: 'X' if 'X' in x else '')
    dominance_dataframe['output_vrs'] = dominance_dataframe['output_vrs'].astype(str).apply(lambda x: 'X' if 'X' in x else '')
    dominance_dataframe['output_nirs'] = dominance_dataframe['output_nirs'].astype(str).apply(lambda x: 'X' if 'X' in x else '')
    dominance_dataframe['directional_crs'] = dominance_dataframe['directional_crs'].astype(str).apply(lambda x: 'X' if 'X' in x else '')
    dominance_dataframe['directional_vrs'] = dominance_dataframe['directional_vrs'].astype(str).apply(lambda x: 'X' if 'X' in x else '')
    dominance_dataframe['directional_nirs'] = dominance_dataframe['directional_nirs'].astype(str).apply(lambda x: 'X' if 'X' in x else '')

    dominance_dataframe = dominance_dataframe.sort_values(by='total_dominance', ascending=False)
    dominance_dataframe = dominance_dataframe[['hospital_id', 'hospital_alternative_name', 'input_crs', 'input_vrs', 'input_nirs', 'output_crs', 'output_vrs', 'output_nirs', 'directional_crs', 'directional_vrs', 'directional_nirs', 'total_dominance']]
    
    print(dominance_dataframe)

    helpers.dataframe_write_csv_file(data_frame=dominance_dataframe.head(10),
                                    path=f'../../../results/hospital_summary_eficency/{translated_classification}.csv' )