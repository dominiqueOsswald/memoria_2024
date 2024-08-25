""" Module to build the normalized data of hospitals """


# Internal imports
from common import config, helpers

# Internal functions
def get_min_max(dfs, columns):
    """ Given a list of dataframes, gets the maximun
    and minimun value of specified columns
    """
    max_values = {}
    min_values = {}

    for data_frame in dfs:
        for col in columns:
            if col in max_values:
                max_values[col] = max(max_values[col], data_frame[col].max())
                min_values[col] = min(min_values[col], data_frame[col].min())
            else:
                max_values[col] = data_frame[col].max()
                min_values[col] = data_frame[col].min()

    return max_values, min_values

def normalize_df(data_frame, max_values, min_values):
    """ Normalizes dataframe
    """
    for col in config.COLUMNS_TO_NORMALIZE:
        new_col_name = f"{col}_normalized"
        data_frame[new_col_name] = (data_frame[col] - min_values[col]) / (max_values[col] - min_values[col])
    return data_frame

def adjust_input(data_frame, actual_year):
    """ Adjusts numeric currency input values
    based on 'Time Value of Money'
    """
    target_year = 2020
    pow_number = target_year - int(actual_year)
    data_frame['21_value'] = data_frame['21_value'].apply(lambda x: x * (pow(1.07, pow_number)))
    data_frame['22_value'] = data_frame['22_value'].apply(lambda x: x * (pow(1.07, pow_number)))
    return data_frame

# Body
years = config.YEARS_RANGE

print("\033[92mBuilding merged data...\033[0m")

hospital_dfs = []

for year in years:
    hospital_dfs.append(helpers.dataframe_read_csv_file(path=f'../../data/merged/{year}.csv'))

max_columns, min_columns = get_min_max(hospital_dfs, config.COLUMNS_TO_NORMALIZE)

for idx, df in enumerate(hospital_dfs):
    adjusted_df = adjust_input(df, years[idx])
    normalized_df = normalize_df(df, max_columns, min_columns)
    normalized_df = normalized_df.round(2)
    grd_column = normalized_df.pop('GRD')
    normalized_df['GRD'] = grd_column

    helpers.dataframe_write_csv_file(data_frame=normalized_df,
                                     path=f'../../data/normalized/{years[idx]}.csv')

    print(f"\t\033[93mYear: {years[idx]} done!\033[93m")

print("\033[92mMerged data builded!\033[0m")
