""" Aux module """

# External imports
import sys
import pandas as pd

# Sys instructions
sys.path.append("..")

# Internal imports
from common import config, helpers

# Body
TYPE = sys.argv[1]

if sys.argv[2] == 'GRD':
    IDENTIFIER = '_grd'
elif sys.argv[2] == 'REGION':
    IDENTIFIER = '_region'
else:
    IDENTIFIER = ''

PATH = f'../../../results/dea_te{IDENTIFIER}'

# Get dataframes
hospital_dataframe = helpers.dataframe_read_csv_file(path=f'../{config.HOSPITAL_DICTIONARY_PATH}')
et_dataframe_2014_crs = helpers.dataframe_read_csv_file(path=f'{PATH}/2014_crs.csv')
et_dataframe_2015_crs = helpers.dataframe_read_csv_file(path=f'{PATH}/2015_crs.csv')
et_dataframe_2016_crs = helpers.dataframe_read_csv_file(path=f'{PATH}/2016_crs.csv')
et_dataframe_2017_crs = helpers.dataframe_read_csv_file(path=f'{PATH}/2017_crs.csv')
et_dataframe_2018_crs = helpers.dataframe_read_csv_file(path=f'{PATH}/2018_crs.csv')
et_dataframe_2019_crs = helpers.dataframe_read_csv_file(path=f'{PATH}/2019_crs.csv')
et_dataframe_2020_crs = helpers.dataframe_read_csv_file(path=f'{PATH}/2020_crs.csv')

et_dataframe_2014_vrs = helpers.dataframe_read_csv_file(path=f'{PATH}/2014_vrs.csv')
et_dataframe_2015_vrs = helpers.dataframe_read_csv_file(path=f'{PATH}/2015_vrs.csv')
et_dataframe_2016_vrs = helpers.dataframe_read_csv_file(path=f'{PATH}/2016_vrs.csv')
et_dataframe_2017_vrs = helpers.dataframe_read_csv_file(path=f'{PATH}/2017_vrs.csv')
et_dataframe_2018_vrs = helpers.dataframe_read_csv_file(path=f'{PATH}/2018_vrs.csv')
et_dataframe_2019_vrs = helpers.dataframe_read_csv_file(path=f'{PATH}/2019_vrs.csv')
et_dataframe_2020_vrs = helpers.dataframe_read_csv_file(path=f'{PATH}/2020_vrs.csv')

et_dataframe_2014_nirs = helpers.dataframe_read_csv_file(path=f'{PATH}/2014_nirs.csv')
et_dataframe_2015_nirs = helpers.dataframe_read_csv_file(path=f'{PATH}/2015_nirs.csv')
et_dataframe_2016_nirs = helpers.dataframe_read_csv_file(path=f'{PATH}/2016_nirs.csv')
et_dataframe_2017_nirs = helpers.dataframe_read_csv_file(path=f'{PATH}/2017_nirs.csv')
et_dataframe_2018_nirs = helpers.dataframe_read_csv_file(path=f'{PATH}/2018_nirs.csv')
et_dataframe_2019_nirs = helpers.dataframe_read_csv_file(path=f'{PATH}/2019_nirs.csv')
et_dataframe_2020_nirs = helpers.dataframe_read_csv_file(path=f'{PATH}/2020_nirs.csv')


# Order dataframes

hospital_dataframe = helpers.data_frame_sort_values(hospital_dataframe, 'hospital_id')
et_dataframe_2014_crs = helpers.data_frame_sort_values(et_dataframe_2014_crs, 'hospital_id')
et_dataframe_2015_crs = helpers.data_frame_sort_values(et_dataframe_2015_crs, 'hospital_id')
et_dataframe_2016_crs = helpers.data_frame_sort_values(et_dataframe_2016_crs, 'hospital_id')
et_dataframe_2017_crs = helpers.data_frame_sort_values(et_dataframe_2017_crs, 'hospital_id')
et_dataframe_2018_crs = helpers.data_frame_sort_values(et_dataframe_2018_crs, 'hospital_id')
et_dataframe_2019_crs = helpers.data_frame_sort_values(et_dataframe_2019_crs, 'hospital_id')
et_dataframe_2020_crs = helpers.data_frame_sort_values(et_dataframe_2020_crs, 'hospital_id')

et_dataframe_2014_vrs = helpers.data_frame_sort_values(et_dataframe_2014_vrs, 'hospital_id')
et_dataframe_2015_vrs = helpers.data_frame_sort_values(et_dataframe_2015_vrs, 'hospital_id')
et_dataframe_2016_vrs = helpers.data_frame_sort_values(et_dataframe_2016_vrs, 'hospital_id')
et_dataframe_2017_vrs = helpers.data_frame_sort_values(et_dataframe_2017_vrs, 'hospital_id')
et_dataframe_2018_vrs = helpers.data_frame_sort_values(et_dataframe_2018_vrs, 'hospital_id')
et_dataframe_2019_vrs = helpers.data_frame_sort_values(et_dataframe_2019_vrs, 'hospital_id')
et_dataframe_2020_vrs = helpers.data_frame_sort_values(et_dataframe_2020_vrs, 'hospital_id')

et_dataframe_2014_nirs = helpers.data_frame_sort_values(et_dataframe_2014_nirs, 'hospital_id')
et_dataframe_2015_nirs = helpers.data_frame_sort_values(et_dataframe_2015_nirs, 'hospital_id')
et_dataframe_2016_nirs = helpers.data_frame_sort_values(et_dataframe_2016_nirs, 'hospital_id')
et_dataframe_2017_nirs = helpers.data_frame_sort_values(et_dataframe_2017_nirs, 'hospital_id')
et_dataframe_2018_nirs = helpers.data_frame_sort_values(et_dataframe_2018_nirs, 'hospital_id')
et_dataframe_2019_nirs = helpers.data_frame_sort_values(et_dataframe_2019_nirs, 'hospital_id')
et_dataframe_2020_nirs = helpers.data_frame_sort_values(et_dataframe_2020_nirs, 'hospital_id')



# Create new Dataframe
merged_df =  pd.DataFrame()
orientation = f'te_{TYPE}'

merged_df['hospital_id'] = hospital_dataframe['hospital_id']
merged_df['hospital_name'] = hospital_dataframe['hospital_alternative_name']

merged_df['2014_te_crs'] = et_dataframe_2014_crs[orientation]
merged_df['2014_te_vrs'] = et_dataframe_2014_vrs[orientation]
merged_df['2014_te_nirs'] = et_dataframe_2014_nirs[orientation]

merged_df['2015_te_crs'] = et_dataframe_2015_crs[orientation]
merged_df['2015_te_vrs'] = et_dataframe_2015_vrs[orientation]
merged_df['2015_te_nirs'] = et_dataframe_2015_nirs[orientation]

merged_df['2016_te_crs'] = et_dataframe_2016_crs[orientation]
merged_df['2016_te_vrs'] = et_dataframe_2016_vrs[orientation]
merged_df['2016_te_nirs'] = et_dataframe_2016_nirs[orientation]

merged_df['2017_te_crs'] = et_dataframe_2017_crs[orientation]
merged_df['2017_te_vrs'] = et_dataframe_2017_vrs[orientation]
merged_df['2017_te_nirs'] = et_dataframe_2017_nirs[orientation]

merged_df['2018_te_crs'] = et_dataframe_2018_crs[orientation]
merged_df['2018_te_vrs'] = et_dataframe_2018_vrs[orientation]
merged_df['2018_te_nirs'] = et_dataframe_2018_nirs[orientation]

merged_df['2019_te_crs'] = et_dataframe_2019_crs[orientation]
merged_df['2019_te_vrs'] = et_dataframe_2019_vrs[orientation]
merged_df['2019_te_nirs'] = et_dataframe_2019_nirs[orientation]

merged_df['2020_te_crs'] = et_dataframe_2020_crs[orientation]
merged_df['2020_te_vrs'] = et_dataframe_2020_vrs[orientation]
merged_df['2020_te_nirs'] = et_dataframe_2020_nirs[orientation]


helpers.dataframe_write_csv_file(data_frame=merged_df,
                                 path=f'../../../results/report/te_{TYPE}{IDENTIFIER}.csv' )
