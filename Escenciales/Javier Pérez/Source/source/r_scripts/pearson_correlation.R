library(amap)

start_year <- 2014
end_year <- 2020

for (year in start_year:end_year) {
  # Path de lectura
  read_file_path <- sprintf("../../data/correlation/%d.csv",
                            year)
  
  write_file_path <- sprintf("../../results/pearson_correlation/%d.csv", year)
  
  # Se lee el archivo .csv
  eff_data <- read.csv(read_file_path, header = TRUE)
  
  # Se quita la columna hospital_id
  dataframe_subset <- subset(eff_data, select = -c(hospital_id, hospital_name)) 
  dataframe_subset[is.na(dataframe_subset)] <- 1
  
  nombres = colnames(dataframe_subset)
  
  # Se construye la matriz de correlación
  #correlation_matrix <- cor(dataframe_subset)
  
  dataframe_subset = t(as.matrix(dataframe_subset))
  
  correlation_matrix <- round(as.matrix(Dist(dataframe_subset, method = 'pearson')), 3)
  names(correlation_matrix) = rownames(dataframe_subset)
  rownames(correlation_matrix) = rownames(dataframe_subset)
  
  write.csv(correlation_matrix, write_file_path, row.names = FALSE, quote = FALSE) 
}
