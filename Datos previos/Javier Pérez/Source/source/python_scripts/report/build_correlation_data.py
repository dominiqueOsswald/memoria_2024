""" Aux module """

# External imports
import sys
import pandas as pd

# Sys instructions
sys.path.append("..")

# Internal imports
from common import config, helpers

# Body
YEAR = sys.argv[1]
PATH = '../../../results/report'

####################################

hospital_dataframe = helpers.dataframe_read_csv_file(path=f'../{config.HOSPITAL_DICTIONARY_PATH}')

et_dataframe_input = helpers.dataframe_read_csv_file(path=f'{PATH}/te_input.csv')
et_dataframe_output = helpers.dataframe_read_csv_file(path=f'{PATH}/te_output.csv')
et_dataframe_directional = helpers.dataframe_read_csv_file(path=f'{PATH}/te_directional.csv')

et_dataframe_input_grd = helpers.dataframe_read_csv_file(path=f'{PATH}/te_input_grd.csv')
et_dataframe_output_grd = helpers.dataframe_read_csv_file(path=f'{PATH}/te_output_grd.csv')
et_dataframe_directional_grd = helpers.dataframe_read_csv_file(path=f'{PATH}/te_directional_grd.csv')

et_dataframe_input_region = helpers.dataframe_read_csv_file(path=f'{PATH}/te_input_region.csv')
et_dataframe_output_region = helpers.dataframe_read_csv_file(path=f'{PATH}/te_output_region.csv')
et_dataframe_directional_region = helpers.dataframe_read_csv_file(path=f'{PATH}/te_directional_region.csv')

####################################

hospital_dataframe = helpers.data_frame_sort_values(hospital_dataframe, 'hospital_id')

et_dataframe_input = helpers.data_frame_sort_values(et_dataframe_input, 'hospital_id')
et_dataframe_output = helpers.data_frame_sort_values(et_dataframe_output, 'hospital_id')
et_dataframe_directional = helpers.data_frame_sort_values(et_dataframe_directional, 'hospital_id')

et_dataframe_input_grd = helpers.data_frame_sort_values(et_dataframe_input_grd, 'hospital_id')
et_dataframe_output_grd = helpers.data_frame_sort_values(et_dataframe_output_grd, 'hospital_id')
et_dataframe_directional_grd = helpers.data_frame_sort_values(et_dataframe_directional_grd, 'hospital_id')

et_dataframe_input_region = helpers.data_frame_sort_values(et_dataframe_input_region, 'hospital_id')
et_dataframe_output_region = helpers.data_frame_sort_values(et_dataframe_output_region, 'hospital_id')
et_dataframe_directional_region = helpers.data_frame_sort_values(et_dataframe_directional_region, 'hospital_id')

# Create new Dataframe
merged_df =  pd.DataFrame()

merged_df['hospital_id'] = hospital_dataframe['hospital_id']
merged_df['hospital_name'] = hospital_dataframe['hospital_alternative_name']

merged_df['te_input_crs'] = et_dataframe_input[f'{YEAR}_te_crs']
merged_df['te_input_vrs'] = et_dataframe_input[f'{YEAR}_te_vrs']
merged_df['te_input_nirs'] = et_dataframe_input[f'{YEAR}_te_nirs']

merged_df['te_output_crs'] = et_dataframe_output[f'{YEAR}_te_crs']
merged_df['te_output_vrs'] = et_dataframe_output[f'{YEAR}_te_vrs']
merged_df['te_output_nirs'] = et_dataframe_output[f'{YEAR}_te_nirs']

merged_df['te_directional_crs'] = et_dataframe_directional[f'{YEAR}_te_crs']
merged_df['te_directional_vrs'] = et_dataframe_directional[f'{YEAR}_te_vrs']
merged_df['te_directional_nirs'] = et_dataframe_directional[f'{YEAR}_te_nirs']

merged_df['te_input_region_crs'] = et_dataframe_input_region[f'{YEAR}_te_crs']
merged_df['te_input_region_vrs'] = et_dataframe_input_region[f'{YEAR}_te_vrs']
merged_df['te_input_region_nirs'] = et_dataframe_input_region[f'{YEAR}_te_nirs']

merged_df['te_output_region_crs'] = et_dataframe_output_region[f'{YEAR}_te_crs']
merged_df['te_output_region_vrs'] = et_dataframe_output_region[f'{YEAR}_te_vrs']
merged_df['te_output_region_nirs'] = et_dataframe_output_region[f'{YEAR}_te_nirs']

merged_df['te_directional_region_crs'] = et_dataframe_directional_region[f'{YEAR}_te_crs']
merged_df['te_directional_region_vrs'] = et_dataframe_directional_region[f'{YEAR}_te_vrs']
merged_df['te_directional_region_nirs'] = et_dataframe_directional_region[f'{YEAR}_te_nirs']

merged_df['te_input_grd_crs'] = et_dataframe_input_grd[f'{YEAR}_te_crs']
merged_df['te_input_grd_vrs'] = et_dataframe_input_grd[f'{YEAR}_te_vrs']
merged_df['te_input_grd_nirs'] = et_dataframe_input_grd[f'{YEAR}_te_nirs']

merged_df['te_output_grd_crs'] = et_dataframe_output_grd[f'{YEAR}_te_crs']
merged_df['te_output_grd_vrs'] = et_dataframe_output_grd[f'{YEAR}_te_vrs']
merged_df['te_output_grd_nirs'] = et_dataframe_output_grd[f'{YEAR}_te_nirs']

merged_df['te_directional_grd_crs'] = et_dataframe_directional_grd[f'{YEAR}_te_crs']
merged_df['te_directional_grd_vrs'] = et_dataframe_directional_grd[f'{YEAR}_te_vrs']
merged_df['te_directional_grd_nirs'] = et_dataframe_directional_grd[f'{YEAR}_te_nirs']

helpers.dataframe_write_csv_file(data_frame=merged_df,
                                 path=f'../../../data/correlation/{YEAR}.csv' )
