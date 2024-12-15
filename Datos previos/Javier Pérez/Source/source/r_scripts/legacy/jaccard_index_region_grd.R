# Functions

# Calculate Jaccard Index function
calculate_jaccard <- function(df1, df2) {
  intersect_count <- length(intersect(df1, df2))
  union_count <- length(union(df1, df2))
  
  jaccard_index <- intersect_count / union_count
  return(jaccard_index)
}

# Body

# Load all data frames for input Jaccard index obtention

# Global
start_year <- 2014
end_year <- 2020
regions <- c(3, 4, 5, 6, 7, 8, 9, 10, 13, 16)

df_input_list <- list()
df_output_list <- list()
df_directional_list <- list()

for (region in regions) {
  
  current_region_input_data <- list()
  current_region_output_data <- list()
  current_region_dir_data <- list()
  
  for (year in start_year:end_year) {
    read_file_path <- sprintf("../../results/dea_te_region_grd/%d_%d.csv", year, region)
    identifier <- paste0(year, "_", region)
    
    # Read the CSV file
    data <- read.csv(read_file_path, header = TRUE)
    
    # Filtered data frames
    filtered_df <- data[, (ncol(data) - 2):ncol(data)]
    # Specific data frames
    # Input
    input_df <- filtered_df[1]
    current_region_input_data[as.character(year)] <- input_df
    # Output
    output_df <- filtered_df[2]
    current_region_output_data[as.character(year)] <- output_df
    # Directional
    directional_df <- filtered_df[3]
    current_region_dir_data[as.character(year)] <- directional_df
  }
  
  df_input_list[[as.character(region)]] <- current_region_input_data
  df_output_list[[as.character(region)]] <- current_region_output_data
  df_directional_list[[as.character(region)]] <- current_region_dir_data
}

for(region in names(df_input_list)) {
  write_input_file_path <- sprintf("../../results/jaccard_index_region_grd/jaccard_te_input_%s.csv", region)
  # Calculate Jaccard Index for each pair of data frames
  actual_df <- df_input_list[[region]]
  jaccard_indexes_input <- matrix(NA,
                                  nrow = length(actual_df),
                                  ncol = length(actual_df))
  
  for (i in 1:length(actual_df)) {
    for (j in 1:length(actual_df)) {
      jaccard_indexes_input[i, j] <- calculate_jaccard(actual_df[[i]],
                                                       actual_df[[j]])
    }
  }
  
  new_column_names <- c("2014", "2015", "2016", "2017",
                        "2018", "2019", "2020", "2021")
  
  colnames(jaccard_indexes_input) <- new_column_names
  
  write.csv(jaccard_indexes_input,
            file = write_input_file_path,
            row.names = FALSE)
}

for(region in names(df_output_list)) {
  write_output_file_path <- sprintf("../../results/jaccard_index_region_grd/jaccard_te_output_%s.csv", region)
  # Calculate Jaccard Index for each pair of data frames
  actual_df <- df_output_list[[region]]
  jaccard_indexes_output <- matrix(NA,
                                  nrow = length(actual_df),
                                  ncol = length(actual_df))
  
  for (i in 1:length(actual_df)) {
    for (j in 1:length(actual_df)) {
      jaccard_indexes_output[i, j] <- calculate_jaccard(actual_df[[i]],
                                                       actual_df[[j]])
    }
  }
  
  new_column_names <- c("2014", "2015", "2016", "2017",
                        "2018", "2019", "2020", "2021")
  
  colnames(jaccard_indexes_output) <- new_column_names
  
  write.csv(jaccard_indexes_output,
            file = write_output_file_path,
            row.names = FALSE)
}

for(region in names(df_directional_list)) {
  write_dir_file_path <- sprintf("../../results/jaccard_index_region_grd/jaccard_te_dir_%s.csv", region)
  # Calculate Jaccard Index for each pair of data frames
  actual_df <- df_directional_list[[region]]
  jaccard_indexes_dir <- matrix(NA,
                                  nrow = length(actual_df),
                                  ncol = length(actual_df))
  
  for (i in 1:length(actual_df)) {
    for (j in 1:length(actual_df)) {
      jaccard_indexes_dir[i, j] <- calculate_jaccard(actual_df[[i]],
                                                       actual_df[[j]])
    }
  }
  
  new_column_names <- c("2014", "2015", "2016", "2017",
                        "2018", "2019", "2020", "2021")
  
  colnames(jaccard_indexes_dir) <- new_column_names
  
  write.csv(jaccard_indexes_dir,
            file = write_dir_file_path,
            row.names = FALSE)
}

