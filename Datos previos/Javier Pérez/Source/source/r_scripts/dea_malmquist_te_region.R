# Packages
library(productivity)

return_types <- c("crs", "vrs", "nirs")
orientation_types <- c("in", "out")

# Read the CSV file
read_file_path <- "../../data/malmquist/malmquist.csv"
hospital_data <- read.csv(read_file_path, header = TRUE)

number_counts <- table(hospital_data$region)
available_regions <- names(number_counts[number_counts > 7])
hospital_data <- subset(hospital_data, region %in% available_regions)
hospital_data <- hospital_data[, -which(names(hospital_data) == "region")]
hospital_data <- hospital_data[, -which(names(hospital_data) == "GRD")]

for (orientation_type in orientation_types) {
  for (return_type in return_types) {
    
    write_file_path <- sprintf("../../results/dea_malmquist_region/malmquist_%s_%s.csv",
                              orientation_type,
                              return_type)
    
    
    malmquist <- malm(data = hospital_data, id.var = "DMUs", time.var = "period",
                      orientation=orientation_type, rts = return_type,
                      x.vars = c("X21_value_normalized", "X22_value_normalized"), 
                      y.vars = c("hospital_discharge_normalized",
                                 "diagnostic_tests_normalized",
                                 "surgical_interventions_normalized",
                                 "medical_consultations_normalized"))
    
    # Vector with 324 elements
    malmquist_index <- malmquist[["Changes"]][["malmquist"]]
    
    # Reshape the vector into a dataframe with 6 columns (2015 to 2020) and 55 rows
    num_years <- 6
    chunk_size <- 49
    
    # Create an empty dataframe to store the values
    malmquist_result <- data.frame(matrix(NA, nrow = chunk_size, ncol = num_years))
    
    # Loop through the vector and populate the dataframe
    for (i in 1:num_years) {
      start_index <- (i - 1) * chunk_size + 1
      end_index <- i * chunk_size
      malmquist_result[, i] <- malmquist_index[start_index:end_index]
    }
    
    rownames(malmquist_result) <- hospital_data$DMUs[1:49]
    colnames(malmquist_result) <- c(2015:2020)
    malmquist_result <- round(malmquist_result, 2)
    
    write.csv(malmquist_result,
              write_file_path,
              row.names = TRUE,
              quote = FALSE)
    
    print_text <- sprintf("Malmquimst index calculated with %s and %s... done!",
                          orientation_type,
                          return_type)
    
    print(print_text)
  }
}
