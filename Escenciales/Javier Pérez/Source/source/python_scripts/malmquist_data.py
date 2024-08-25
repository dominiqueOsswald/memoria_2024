""" Module to build gather all the data per hospital """

# External imports
import pandas as pd

# Internal imports
from common import config, helpers

# Body

years = config.YEARS_RANGE

hospital_dataframe = helpers.dataframe_read_csv_file(path=config.HOSPITAL_DICTIONARY_PATH)
helpers.dataframe_rewrite_index(data_frame=hospital_dataframe, new_index='hospital_id')
hospital_ids = helpers.dataframe_index_to_list(data_frame=hospital_dataframe)

print("\n\033[92mBuilding malmquist data...\033[0m")

output_df = pd.DataFrame()
for hospital_id in hospital_ids:
    for year in years:
        merged_df = helpers.dataframe_read_csv_file(path=f'../../data/normalized/{year}.csv')
        merged_df = helpers.data_frame_sort_values(data_frame=merged_df, key='hospital_id')
        hospital_df = helpers.dataframe_filter_rows(data_frame=merged_df,
                                                    column_name='hospital_id',
                                                    rows=[hospital_id])
        input_df = pd.DataFrame()
        input_df['21_value_normalized'] = hospital_df['21_value_normalized']
        input_df['22_value_normalized'] = hospital_df['22_value_normalized']

        hospital_ID_df = pd.DataFrame()
        hospital_ID_df['DMUs'] = hospital_df['hospital_id']
        hospital_ID_df['region'] = hospital_df['region']
        hospital_ID_df['GRD'] = hospital_df['GRD']
        normalized_columns = hospital_df.filter(like='_normalized').iloc[:, 2:]
        year_df = pd.DataFrame().assign(period=[year])

        partial_output_df = pd.concat([ hospital_ID_df, year_df, input_df, normalized_columns], axis=1)
        if len(partial_output_df) > 1:
            partial_output_df = partial_output_df.iloc[0].combine_first(partial_output_df.iloc[1]).to_frame().T

        output_df = pd.concat([output_df, partial_output_df], axis=0)

        print(f"\t\033[93mHospital {hospital_id} in year {year} done!\033[93m")

output_df['DMUs'] = output_df['DMUs'].astype(int)
output_df['region'] = output_df['region'].astype(int)
output_df = helpers.data_frame_sort_values(data_frame=output_df, key='period')

helpers.dataframe_write_csv_file(data_frame=output_df,
                                     path='../../data/malmquist/malmquist.csv')


print("\033[92mMalmquist data builded!\033[0m")
