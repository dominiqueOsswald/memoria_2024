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
df_input_list <- list()
df_output_list <- list()
df_directional_list <- list()

for (year in start_year:end_year) {
  read_file_path <- sprintf("../../results/dea_te/%d.csv", year)
  
  # Read the CSV file
  data <- read.csv(read_file_path, header = TRUE)
  
  # Filtered data frames
  filtered_df <- data[, (ncol(data) - 2):ncol(data)]
  # Specific data frames
  # Input
  input_df <- filtered_df[1]
  df_input_list[sprintf("%d", year)] <- input_df
  # Output
  output_df <- filtered_df[2]
  df_output_list[sprintf("%d", year)] <- output_df
  # Directional
  directional_df <- filtered_df[3]
  df_directional_list[sprintf("%d", year)] <- directional_df
}

# Calculate Jaccard Index for each pair of data frames
jaccard_indexes_input <- matrix(NA,
                                nrow = length(df_input_list),
                                ncol = length(df_input_list))
jaccard_indexes_output <- matrix(NA,
                                nrow = length(df_input_list),
                                ncol = length(df_input_list))
jaccard_indexes_dir <- matrix(NA,
                              nrow = length(df_input_list),
                              ncol = length(df_input_list))

for (i in 1:length(df_input_list)) {
  for (j in 1:length(df_input_list)) {
    jaccard_indexes_input[i, j] <- calculate_jaccard(df_input_list[[i]],
                                                     df_input_list[[j]])
    jaccard_indexes_output[i, j] <- calculate_jaccard(df_output_list[[i]],
                                                      df_output_list[[j]])
    jaccard_indexes_dir[i, j] <- calculate_jaccard(df_directional_list[[i]],
                                                   df_directional_list[[j]])
  }
}


new_column_names <- c("2014", "2015", "2016", "2017",
                      "2018", "2019", "2020", "2021")

colnames(jaccard_indexes_input) <- new_column_names
colnames(jaccard_indexes_output) <- new_column_names
colnames(jaccard_indexes_dir) <- new_column_names

write_input_file_path <- "../../results/jaccard_index_te/jaccard_te_input.csv"
write_output_file_path <- "../../results/jaccard_index_te/jaccard_te_output.csv"
write_dir_file_path <- "../../results/jaccard_index_te/jaccard_te_directional.csv"
                                 
write.csv(jaccard_indexes_input,
          file = write_input_file_path,
          row.names = FALSE)

write.csv(jaccard_indexes_output,
          file = write_output_file_path,
          row.names = FALSE)

write.csv(jaccard_indexes_dir,
          file = write_dir_file_path,
          row.names = FALSE)

