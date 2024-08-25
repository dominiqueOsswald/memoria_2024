""" File with functions to use in the entire project """

# External imports
import pandas as pd
import numpy as np

def dataframe_read_csv_file(path):
    """ Returns a dataframe based on the
    csv file readed
    """
    df = pd.read_csv(path) # Read the CSV file into a DF
    return df

def dataframe_write_csv_file(data_frame, path):
    """ Writes dataframe of the corresponding year as
    csv file
    """
    data_frame.to_csv(path, index=False)
    return True

def dataframe_rewrite_index(data_frame, new_index):
    """ Changes index of dataframe based on new_index
    parameter
    """
    return data_frame.set_index(new_index, inplace=True)

def dataframe_reset_index(data_frame):
    """ Reset index of given dataframe
    """
    return data_frame.reset_index(inplace=True)

def dataframe_index_to_list(data_frame):
    """ Retrieve as a list the index of the
    given dataframe
    """
    return data_frame.index.tolist()

def dataframe_filter_rows(data_frame, column_name, rows):
    """ Returns a dataframe with filtered rows given an
    column values
    """
    return data_frame[data_frame[column_name].isin(rows)]

def dataframe_filter_columns(data_frame, columns):
    """ Returns a dataframe filtered by columns
    """
    filtered_df = pd.DataFrame()

    for column in columns:
        try:
            filtered_df[column] = data_frame[column]
        except KeyError:
            filtered_df[column] = np.nan

    return filtered_df

def dataframe_delete_columns(data_frame, columns):
    """ Returns a dataframe without the specified columns
    """
    return data_frame.drop(columns=columns, axis=1)

def data_frame_sort_values(data_frame, key):
    """ Returns a dataframe sorted by the key
    passed as parameter
    """
    return data_frame.sort_values(by=key).reset_index(drop=True)

def merge_data_frames(left_df, righ_df, filter_columns, drop_columns):
    """ Merges two dataframes by desired columns
    """

    filtered_left_df = left_df[filter_columns]
    righ_df.drop(columns=drop_columns, inplace=True)

    return pd.concat([filtered_left_df, righ_df], axis=1)

def list_flatten(list):
    """ Flatten the given list in 1 dimention
    """
    return [item for row in list for item in row] 