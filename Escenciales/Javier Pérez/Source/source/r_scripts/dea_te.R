# Packages
library(deaR)
library(qcc)
library(ggplot2)
library(rPref)
library(dplyr)
library(igraph)

# Global
start_year <- 2014
end_year <- 2020
return_types <- c("crs")

for (return_type in return_types) {
  for (year in start_year:end_year) {
    read_file_path <- sprintf("../../data/normalized/%d.csv", year)
    write_file_path <- sprintf("../../results/dea_te/%d_%s.csv", year, return_type)
    write_file_grd_path <- sprintf("../../results/dea_te_grd/%d_%s.csv", year, return_type)

    # Read the CSV file
    data <- read.csv(read_file_path, header = TRUE)

    # Filter rows
    normalized_columns <- c(1, 10, 11, 12, 13, 14, 15)
    filtered_data <- data[, normalized_columns]
    filtered_data[is.na(filtered_data)] <- 0.00

    # DEA input
    dea_data <- make_deadata(filtered_data, ni= 2, no= 4, dmus = 1,
                            inputs = 2:3, outputs = 4:7)
    
    dea_input_result <- model_basic(dea_data,
                                    orientation = "io",
                                    rts = return_type, )

    # DEA output
    dea_output_result <- model_basic(dea_data,
                                    orientation = "oo",
                                    rts = return_type, )

    # DEA directional
        dea_dir_result <- model_basic(dea_data,
                                  orientation = "dir",
                                  rts = return_type, )
    # Efficiency output
    te_input <- efficiencies(dea_input_result)
    te_output <- efficiencies(dea_output_result)
    te_directional <- efficiencies(dea_dir_result)

    # Dataframe build
    hospitals_data <- data.frame(te_input)
    hospitals_data$te_input <- round(hospitals_data$te_input, 3)

    hospitals_data$te_output <- data.frame(te_output)$te_output
    hospitals_data$te_output <- round(hospitals_data$te_output, 3)

    hospitals_data$te_directional <- data.frame(te_directional)$te_directional
    hospitals_data$te_directional <- round(hospitals_data$te_directional, 3)

    hospitals_data$grd <- data[["GRD"]]

    hospitals_data <- cbind(hospital_id = rownames(hospitals_data),
                            hospitals_data)

    rownames(hospitals_data) <- 1:nrow(hospitals_data) 
    output_data <- cbind(hospitals_data, filtered_data)

    # Column selection in output_data
    output_data_normal <- output_data[, c("hospital_id",
                                  "X21_value_normalized",
                                  "X22_value_normalized",
                                  "hospital_discharge_normalized",
                                  "diagnostic_tests_normalized",
                                  "surgical_interventions_normalized",
                                  "medical_consultations_normalized",
                                  "grd",
                                  "te_input",
                                  "te_output",
                                  "te_directional")]
    
    # Save to file
    output_data_normal$hospital_id <- gsub("DMU", "", output_data_normal$hospital_id)
    write.csv(output_data_normal, write_file_path, row.names = FALSE, quote = FALSE) 
  }
}
