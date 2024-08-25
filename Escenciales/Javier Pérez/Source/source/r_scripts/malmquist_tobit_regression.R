# Packages
library(productivity)
library(censReg) # Para la regresión Tobit


assign_significance_level <- function(p_value) {
  if (p_value < 0.001) {
    return("***")
  } else if (p_value < 0.01) {
    return("**")
  } else if (p_value < 0.05) {
    return("*")
  } else if (p_value < 0.1) {
    return(".")
  } else {
    return(" ")
  }
}


return_types <- c("crs", "vrs", "nirs")
orientation_types <- c("in", "out")

# Read the CSV file
read_file_path <- "../../data/malmquist/malmquist.csv"
hospital_data <- read.csv(read_file_path, header = TRUE)
hospital_data <- hospital_data[, -which(names(hospital_data) == "region")]
hospital_data <- hospital_data[, -which(names(hospital_data) == "GRD")]

for (orientation_type in orientation_types) {
  for (return_type in return_types) {

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
    chunk_size <- 54
    
    # Create an empty dataframe to store the values
    malmquist_result <- data.frame(matrix(NA, nrow = chunk_size, ncol = num_years))
    
    # Loop through the vector and populate the dataframe
    for (i in 1:num_years) {
      start_index <- (i - 1) * chunk_size + 1
      end_index <- i * chunk_size
      malmquist_result[, i] <- malmquist_index[start_index:end_index]
    }
    
    rownames(malmquist_result) <- hospital_data$DMUs[1:54]
    colnames(malmquist_result) <- c(2015:2020)
    malmquist_result <- round(malmquist_result, 2)

    # Tobit
    for(year in c(2015:2020)) {
      
      tobit_results_path <- sprintf("../../results/dea_malmquist/tobit_results_%d_%s_%s.csv",
                                    year,
                                    orientation_type,
                                    return_type)
      
      dependent_var <- malmquist_result[[as.character(year)]]
      explanatory_vars <- hospital_data[1:54, c("X21_value_normalized", "X22_value_normalized",
                                                "hospital_discharge_normalized",
                                                "diagnostic_tests_normalized",
                                                "surgical_interventions_normalized",
                                                "medical_consultations_normalized")]
      
      regression_data <- data.frame(dependent_var, explanatory_vars)
      
      tobit_model <- censReg(dependent_var ~ (X21_value_normalized +
                                                X22_value_normalized +
                                                hospital_discharge_normalized +
                                                diagnostic_tests_normalized +
                                                surgical_interventions_normalized +
                                                medical_consultations_normalized),
                             data = regression_data,
                             left = 0,
                             right =  1)
      
      tobit_summary <- summary(tobit_model)
      print(tobit_summary)
      
      summary_table <- tobit_summary$estimate
      coefficients <- summary_table[, "Estimate"]
      p_values <- summary_table[, "Pr(> t)"]
      significance_levels <- sapply(p_values, assign_significance_level)
      
      result_df <- data.frame(coefficients, p_values, significance_levels)
      summary_table_filtered <- result_df[!(rownames(result_df) %in% c("(Intercept)", "logSigma")), , drop = FALSE]
      summary_table_filtered$p_value <- sprintf("%.2e",
                                                summary_table_filtered$p_value)
      summary_table_filtered$coefficient <- round(summary_table_filtered$coefficient, 2)
      
      
      write.csv(summary_table_filtered, tobit_results_path, row.names = TRUE, quote = FALSE)
      
      print(sprintf("Tobit regression for %s and %s... done! (YEAR %d)",
                    orientation_type,
                    return_type,
                    year))
    }
  }
}