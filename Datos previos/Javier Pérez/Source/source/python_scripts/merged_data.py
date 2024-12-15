""" Module to build the merged data of hospitals """

# Internal imports
from common import config, helpers

# Body
years = config.YEARS_RANGE

print("\033[92mBuilding merged data...\033[0m")

for year in years:
    financial_df = helpers.dataframe_read_csv_file(path=f'../../data/input/{year}.csv')
    discharge_df = helpers.dataframe_read_csv_file(path=f'../../data/output/{year}.csv')

    financial_df = helpers.data_frame_sort_values(data_frame=financial_df, key='hospital_id')
    discharge_df = helpers.data_frame_sort_values(data_frame=discharge_df, key='hospital_id')

    columns_to_filter = ['hospital_id', 'hospital_fns_id', 'region',
                         '21_value', '22_value']
    columns_to_drop = ['hospital_id']

    merged_df = helpers.merge_data_frames(financial_df, discharge_df,
                                          columns_to_filter, columns_to_drop)

    helpers.dataframe_write_csv_file(data_frame=merged_df,
                                     path=f'../../data/merged/{year}.csv')

    print(f"\t\033[93mYear: {year} done!\033[93m")

print("\033[92mMerged data builded!\033[0m")
