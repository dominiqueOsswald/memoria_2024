""" Aux module """

# External imports
import sys
import pandas as pd

# Sys instructions
sys.path.append("..")

# Internal imports
from common import config, helpers

# Body
PATH = '../../../results/dea_malmquist'

####################################

hospital_dataframe = helpers.dataframe_read_csv_file(path=f'../{config.HOSPITAL_DICTIONARY_PATH}')

dea_malmquist_input_crs = helpers.dataframe_read_csv_file(path=f'{PATH}/malmquist_in_crs.csv')
dea_malmquist_input_vrs = helpers.dataframe_read_csv_file(path=f'{PATH}/malmquist_in_vrs.csv')
dea_malmquist_input_nirs = helpers.dataframe_read_csv_file(path=f'{PATH}/malmquist_in_nirs.csv')

dea_malmquist_input_crs_grd = helpers.dataframe_read_csv_file(path=f'{PATH}_grd/malmquist_in_crs.csv')
dea_malmquist_input_vrs_grd = helpers.dataframe_read_csv_file(path=f'{PATH}_grd/malmquist_in_vrs.csv')
dea_malmquist_input_nirs_grd = helpers.dataframe_read_csv_file(path=f'{PATH}_grd/malmquist_in_nirs.csv')

dea_malmquist_input_crs_region = helpers.dataframe_read_csv_file(path=f'{PATH}_region/malmquist_in_crs.csv')
dea_malmquist_input_vrs_region = helpers.dataframe_read_csv_file(path=f'{PATH}_region/malmquist_in_vrs.csv')
dea_malmquist_input_nirs_region = helpers.dataframe_read_csv_file(path=f'{PATH}_region/malmquist_in_nirs.csv')

####################################

dea_malmquist_output_crs = helpers.dataframe_read_csv_file(path=f'{PATH}/malmquist_out_crs.csv')
dea_malmquist_output_vrs = helpers.dataframe_read_csv_file(path=f'{PATH}/malmquist_out_vrs.csv')
dea_malmquist_output_nirs = helpers.dataframe_read_csv_file(path=f'{PATH}/malmquist_out_nirs.csv')

dea_malmquist_output_crs_grd = helpers.dataframe_read_csv_file(path=f'{PATH}_grd/malmquist_out_crs.csv')
dea_malmquist_output_vrs_grd = helpers.dataframe_read_csv_file(path=f'{PATH}_grd/malmquist_out_vrs.csv')
dea_malmquist_output_nirs_grd = helpers.dataframe_read_csv_file(path=f'{PATH}_grd/malmquist_out_nirs.csv')

dea_malmquist_output_crs_region = helpers.dataframe_read_csv_file(path=f'{PATH}_region/malmquist_out_crs.csv')
dea_malmquist_output_vrs_region = helpers.dataframe_read_csv_file(path=f'{PATH}_region/malmquist_out_vrs.csv')
dea_malmquist_output_nirs_region = helpers.dataframe_read_csv_file(path=f'{PATH}_region/malmquist_out_nirs.csv')

####################################

hospital_dataframe = helpers.data_frame_sort_values(hospital_dataframe, 'hospital_id')

dea_malmquist_input_crs = helpers.data_frame_sort_values(dea_malmquist_input_crs, 'hospital_id')
dea_malmquist_input_vrs = helpers.data_frame_sort_values(dea_malmquist_input_vrs, 'hospital_id')
dea_malmquist_input_nirs = helpers.data_frame_sort_values(dea_malmquist_input_nirs, 'hospital_id')

dea_malmquist_input_crs_grd = helpers.data_frame_sort_values(dea_malmquist_input_crs_grd, 'hospital_id')
dea_malmquist_input_vrs_grd = helpers.data_frame_sort_values(dea_malmquist_input_vrs_grd, 'hospital_id')
dea_malmquist_input_nirs_grd = helpers.data_frame_sort_values(dea_malmquist_input_nirs_grd, 'hospital_id')

dea_malmquist_input_crs_region = helpers.data_frame_sort_values(dea_malmquist_input_crs_region, 'hospital_id')
dea_malmquist_input_vrs_region = helpers.data_frame_sort_values(dea_malmquist_input_vrs_region, 'hospital_id')
dea_malmquist_input_nirs_region = helpers.data_frame_sort_values(dea_malmquist_input_nirs_region, 'hospital_id')

####################################

dea_malmquist_output_crs = helpers.data_frame_sort_values(dea_malmquist_output_crs, 'hospital_id')
dea_malmquist_output_vrs = helpers.data_frame_sort_values(dea_malmquist_output_vrs, 'hospital_id')
dea_malmquist_output_nirs = helpers.data_frame_sort_values(dea_malmquist_output_nirs, 'hospital_id')

dea_malmquist_output_crs_grd = helpers.data_frame_sort_values(dea_malmquist_output_crs_grd, 'hospital_id')
dea_malmquist_output_vrs_grd = helpers.data_frame_sort_values(dea_malmquist_output_vrs_grd, 'hospital_id')
dea_malmquist_output_nirs_grd = helpers.data_frame_sort_values(dea_malmquist_output_nirs_grd, 'hospital_id')

dea_malmquist_output_crs_region = helpers.data_frame_sort_values(dea_malmquist_output_crs_region, 'hospital_id')
dea_malmquist_output_vrs_region = helpers.data_frame_sort_values(dea_malmquist_output_vrs_region, 'hospital_id')
dea_malmquist_output_nirs_region = helpers.data_frame_sort_values(dea_malmquist_output_nirs_region, 'hospital_id')

####################################

years = config.YEARS_RANGE
merged_df = pd.DataFrame()
for year in years[1:]:

    merged_df['hospital_id'] = hospital_dataframe['hospital_id']
    merged_df['hospital_name'] = hospital_dataframe['hospital_alternative_name']

    merged_df['te_input_crs'] = dea_malmquist_input_crs[f'{year}']
    merged_df['te_input_vrs'] = dea_malmquist_input_vrs[f'{year}']
    merged_df['te_input_nirs'] = dea_malmquist_input_nirs[f'{year}']

    merged_df['te_output_crs'] = dea_malmquist_output_crs[f'{year}']
    merged_df['te_output_vrs'] = dea_malmquist_output_vrs[f'{year}']
    merged_df['te_output_nirs'] = dea_malmquist_output_nirs[f'{year}']

    merged_df['te_input_crs_grd'] = dea_malmquist_input_crs_grd[f'{year}']
    merged_df['te_input_vrs_grd'] = dea_malmquist_input_vrs_grd[f'{year}']
    merged_df['te_input_nirs_grd'] = dea_malmquist_input_nirs_grd[f'{year}']

    merged_df['te_output_crs_grd'] = dea_malmquist_output_crs_grd[f'{year}']
    merged_df['te_output_vrs_grd'] = dea_malmquist_output_vrs_grd[f'{year}']
    merged_df['te_output_nirs_grd'] = dea_malmquist_output_nirs_grd[f'{year}']

    merged_df['te_input_crs_region'] = dea_malmquist_input_crs_region[f'{year}']
    merged_df['te_input_vrs_region'] = dea_malmquist_input_vrs_region[f'{year}']
    merged_df['te_input_nirs_region'] = dea_malmquist_input_nirs_region[f'{year}']

    merged_df['te_output_crs_region'] = dea_malmquist_output_crs_region[f'{year}']
    merged_df['te_output_vrs_region'] = dea_malmquist_output_vrs_region[f'{year}']
    merged_df['te_output_nirs_region'] = dea_malmquist_output_nirs_region[f'{year}']

    helpers.dataframe_write_csv_file(data_frame=merged_df,
                                 path=f'../../../data/malmquist_correlation/{year}.csv' )

