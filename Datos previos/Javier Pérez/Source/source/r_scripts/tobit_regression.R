library(censReg)

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

# Constantes
start_year <- 2014
end_year <- 2020

for (year in start_year:end_year) {
  year_data <- list()

  # Path de lectura
  read_file_path_1 <- sprintf("../../results/dea_te_grd/%d_crs.csv", year)
  read_file_path_2 <- sprintf("../../results/dea_te_grd/%d_vrs.csv", year)
  read_file_path_3 <- sprintf("../../results/dea_te_grd/%d_nirs.csv", year)
  write_file_path <- sprintf("../../results/tobit/%d_grd.csv", year)
  
  # Leyendo dataframe
  eff_data <- read.csv(read_file_path_1, header = TRUE)
  eff_data[is.na(eff_data)] <- 0.00
  
  # Limpia dataframe
  # Eliminar columnas por nombre
  eff_data_input_crs <- eff_data[, !names(eff_data) %in% c("grd", "te_output", "te_directional")]
  eff_data_output_crs <- eff_data[, !names(eff_data) %in% c("grd", "te_input", "te_directional")]
  eff_data_directional_crs <- eff_data[, !names(eff_data) %in% c("grd", "te_output", "te_input")]
  
  # Crear el modelo de regresión TOBIT
  model_input_crs <- censReg(te_input ~ (X21_value_normalized + X22_value_normalized +
                                           hospital_discharge_normalized + diagnostic_tests_normalized +
                                           surgical_interventions_normalized + medical_consultations_normalized),
                             left = 0, right = max(eff_data_input_crs$te_input),
                             data = eff_data_input_crs)
  
  model_output_crs <- censReg(te_output ~ (X21_value_normalized + X22_value_normalized +
                                             hospital_discharge_normalized + diagnostic_tests_normalized +
                                             surgical_interventions_normalized + medical_consultations_normalized),
                              left = 0, right = max(eff_data_output_crs$te_output),
                              data = eff_data_output_crs)
  
  model_directional_crs <- censReg(te_directional ~ (X21_value_normalized + X22_value_normalized +
                                                  hospital_discharge_normalized + diagnostic_tests_normalized +
                                                  surgical_interventions_normalized + medical_consultations_normalized),
                                   left = 0, max(eff_data_directional_crs$te_directional),
                                   data = eff_data_directional_crs)
  
  ########## INPUT CRS ##########
  summary_info_input_crs <- summary(model_input_crs)
  summary_table <- summary_info_input_crs$estimate
  coefficients <- summary_table[, "Estimate"]
  p_values <- summary_table[, "Pr(> t)"]
  
  # Create significance levels based on p-values
  significance_levels <- sapply(p_values, assign_significance_level)

  result_df_crs <- data.frame(coefficients, p_values, significance_levels)
  names(result_df_crs) <- c("input_coefficient_crs", "input_P_value_crs", "input_significance_crs")
  summary_table_filtered <- result_df_crs[!(rownames(result_df_crs) %in% c("(Intercept)", "logSigma")), , drop = FALSE]
  summary_table_filtered$input_P_value_crs <- sprintf("%.2e",
                                                      summary_table_filtered$input_P_value_crs)
  summary_table_filtered$input_coefficient_crs <- round(summary_table_filtered$input_coefficient_crs, 2)
  year_data[[sprintf('%d_input_crs', year)]] <- summary_table_filtered
  
  
  ########## OUTPUT CRS ##########
  summary_info_output_crs <- summary(model_output_crs)
  summary_table <- summary_info_output_crs$estimate
  coefficients <- summary_table[, "Estimate"]
  p_values <- summary_table[, "Pr(> t)"]
  significance_levels <- sapply(p_values, assign_significance_level)

  result_df_crs <- data.frame(coefficients, p_values, significance_levels)
  names(result_df_crs) <- c("output_coefficient_crs", "output_P_value_crs", "output_significance_crs")
  summary_table_filtered <- result_df_crs[!(rownames(result_df_crs) %in% c("(Intercept)", "logSigma")), , drop = FALSE]
  summary_table_filtered$output_P_value_crs <- sprintf("%.2e",
                                                      summary_table_filtered$output_P_value_crs)
  summary_table_filtered$output_coefficient_crs <- round(summary_table_filtered$output_coefficient_crs, 2)
  year_data[[sprintf('%d_output_crs', year)]] <- summary_table_filtered
  
  ########## DIRECTIONAL CRS ##########
  summary_info_directional_crs <- summary(model_directional_crs)
  summary_table <- summary_info_directional_crs$estimate
  coefficients <- summary_table[, "Estimate"]
  p_values <- summary_table[, "Pr(> t)"]
  significance_levels <- sapply(p_values, assign_significance_level)

  result_df_crs <- data.frame(coefficients, p_values, significance_levels)
  names(result_df_crs) <- c("directional_coefficient_crs", "directional_P_value_crs", "directional_significance_crs")
  summary_table_filtered <- result_df_crs[!(rownames(result_df_crs) %in% c("(Intercept)", "logSigma")), , drop = FALSE]
  summary_table_filtered$directional_P_value_crs <- sprintf("%.2e",
                                                      summary_table_filtered$directional_P_value_crs)
  summary_table_filtered$directional_coefficient_crs <- round(summary_table_filtered$directional_coefficient_crs, 2)
  year_data[[sprintf('%d_directional_crs', year)]] <- summary_table_filtered
  
  ################################################################################
  
  # Leyendo dataframe
  eff_data <- read.csv(read_file_path_2, header = TRUE)
  eff_data[is.na(eff_data)] <- 0.00
  
  # Limpia dataframe
  # Eliminar columnas por nombre
  eff_data_input_vrs <- eff_data[, !names(eff_data) %in% c("grd", "te_output", "te_directional")]
  eff_data_output_vrs <- eff_data[, !names(eff_data) %in% c("grd", "te_input", "te_directional")]
  eff_data_directional_vrs <- eff_data[, !names(eff_data) %in% c("grd", "te_output", "te_input")]
  
  # Crear el modelo de regresión TOBIT
  model_input_vrs <- censReg(te_input ~ (X21_value_normalized + X22_value_normalized +
                                           hospital_discharge_normalized + diagnostic_tests_normalized +
                                           surgical_interventions_normalized + medical_consultations_normalized),
                             left = 0, right = max(eff_data_input_vrs$te_input),
                             data = eff_data_input_vrs)
  
  model_output_vrs <- censReg(te_output ~ (X21_value_normalized + X22_value_normalized +
                                             hospital_discharge_normalized + diagnostic_tests_normalized +
                                             surgical_interventions_normalized + medical_consultations_normalized),
                              left = 0, right = max(eff_data_output_vrs$te_output),
                              data = eff_data_output_vrs)
  
  model_directional_vrs <- censReg(te_directional ~ (X21_value_normalized + X22_value_normalized +
                                                  hospital_discharge_normalized + diagnostic_tests_normalized +
                                                  surgical_interventions_normalized + medical_consultations_normalized),
                                   left = 0, right = max(eff_data_directional_vrs$te_directional),
                                   data = eff_data_directional_vrs)
  
  ########## INPUT CRS ##########
  summary_info_input_vrs <- summary(model_input_vrs)
  summary_table <- summary_info_input_vrs$estimate
  coefficients <- summary_table[, "Estimate"]
  p_values <- summary_table[, "Pr(> t)"]
  significance_levels <- sapply(p_values, assign_significance_level)

  result_df_vrs <- data.frame(coefficients, p_values, significance_levels)
  names(result_df_vrs) <- c("input_coefficient_vrs", "input_P_value_vrs", "input_significance_vrs")
  summary_table_filtered <- result_df_vrs[!(rownames(result_df_vrs) %in% c("(Intercept)", "logSigma")), , drop = FALSE]
  summary_table_filtered$input_P_value_vrs <- sprintf("%.2e",
                                                      summary_table_filtered$input_P_value_vrs)
  summary_table_filtered$input_coefficient_vrs <- round(summary_table_filtered$input_coefficient_vrs, 2)
  year_data[[sprintf('%d_input_vrs', year)]] <- summary_table_filtered
  
  
  ########## OUTPUT CRS ##########
  summary_info_output_vrs <- summary(model_output_vrs)
  summary_table <- summary_info_output_vrs$estimate
  coefficients <- summary_table[, "Estimate"]
  p_values <- summary_table[, "Pr(> t)"]
  significance_levels <- sapply(p_values, assign_significance_level)

  result_df_vrs <- data.frame(coefficients, p_values, significance_levels)
  names(result_df_vrs) <- c("output_coefficient_vrs", "output_P_value_vrs", "output_significance_vrs")
  summary_table_filtered <- result_df_vrs[!(rownames(result_df_vrs) %in% c("(Intercept)", "logSigma")), , drop = FALSE]
  summary_table_filtered$output_P_value_vrs <- sprintf("%.2e",
                                                      summary_table_filtered$output_P_value_vrs)
  summary_table_filtered$output_coefficient_vrs <- round(summary_table_filtered$output_coefficient_vrs, 2)
  year_data[[sprintf('%d_output_vrs', year)]] <- summary_table_filtered
  
  ########## DIRECTIONAL CRS ##########
  summary_info_directional_vrs <- summary(model_directional_vrs)
  summary_table <- summary_info_directional_vrs$estimate
  coefficients <- summary_table[, "Estimate"]
  p_values <- summary_table[, "Pr(> t)"]
  significance_levels <- sapply(p_values, assign_significance_level)

  result_df_vrs <- data.frame(coefficients, p_values, significance_levels)
  names(result_df_vrs) <- c("directional_coefficient_vrs", "directional_P_value_vrs", "directional_significance_vrs")
  summary_table_filtered <- result_df_vrs[!(rownames(result_df_vrs) %in% c("(Intercept)", "logSigma")), , drop = FALSE]
  summary_table_filtered$directional_P_value_vrs <- sprintf("%.2e",
                                                      summary_table_filtered$directional_P_value_vrs)
  summary_table_filtered$directional_coefficient_vrs <- round(summary_table_filtered$directional_coefficient_vrs, 2)
  year_data[[sprintf('%d_directional_vrs', year)]] <- summary_table_filtered
  
  ################################################################################
  
  # Leyendo dataframe
  eff_data <- read.csv(read_file_path_3, header = TRUE)
  eff_data[is.na(eff_data)] <- 0.00
  
  # Limpia dataframe
  # Eliminar columnas por nombre
  eff_data_input_nirs <- eff_data[, !names(eff_data) %in% c("grd", "te_output", "te_directional")]
  eff_data_output_nirs <- eff_data[, !names(eff_data) %in% c("grd", "te_input", "te_directional")]
  eff_data_directional_nirs <- eff_data[, !names(eff_data) %in% c("grd", "te_output", "te_input")]
  
  # Crear el modelo de regresión TOBIT
  model_input_nirs <- censReg(te_input ~ (X21_value_normalized + X22_value_normalized +
                                           hospital_discharge_normalized + diagnostic_tests_normalized +
                                           surgical_interventions_normalized + medical_consultations_normalized),
                             left = 0, right = max(eff_data_input_nirs$te_input),
                             data = eff_data_input_nirs)
  
  model_output_nirs <- censReg(te_output ~ (X21_value_normalized + X22_value_normalized +
                                             hospital_discharge_normalized + diagnostic_tests_normalized +
                                             surgical_interventions_normalized + medical_consultations_normalized),
                              left = 0, right = max(eff_data_output_nirs$te_output),
                              data = eff_data_output_nirs)
  
  model_directional_nirs <- censReg(te_directional ~ (X21_value_normalized + X22_value_normalized +
                                                  hospital_discharge_normalized + diagnostic_tests_normalized +
                                                  surgical_interventions_normalized + medical_consultations_normalized),
                                   left = 0,right = max(eff_data_directional_nirs$te_directional),
                                   data = eff_data_directional_nirs)
  
  ########## INPUT CRS ##########
  summary_info_input_nirs <- summary(model_input_nirs)
  summary_table <- summary_info_input_nirs$estimate
  coefficients <- summary_table[, "Estimate"]
  p_values <- summary_table[, "Pr(> t)"]
  significance_levels <- sapply(p_values, assign_significance_level)

  result_df_nirs <- data.frame(coefficients, p_values, significance_levels)
  names(result_df_nirs) <- c("input_coefficient_nirs", "input_P_value_nirs", "input_significance_nirs")
  summary_table_filtered <- result_df_nirs[!(rownames(result_df_nirs) %in% c("(Intercept)", "logSigma")), , drop = FALSE]
  summary_table_filtered$input_P_value_nirs <- sprintf("%.2e",
                                                      summary_table_filtered$input_P_value_nirs)
  summary_table_filtered$input_coefficient_nirs <- round(summary_table_filtered$input_coefficient_nirs, 2)
  year_data[[sprintf('%d_input_nirs', year)]] <- summary_table_filtered
  
  
  ########## OUTPUT CRS ##########
  summary_info_output_nirs <- summary(model_output_nirs)
  summary_table <- summary_info_output_nirs$estimate
  coefficients <- summary_table[, "Estimate"]
  p_values <- summary_table[, "Pr(> t)"]
  significance_levels <- sapply(p_values, assign_significance_level)

  result_df_nirs <- data.frame(coefficients, p_values, significance_levels)
  names(result_df_nirs) <- c("output_coefficient_nirs", "output_P_value_nirs", "output_significance_nirs")
  summary_table_filtered <- result_df_nirs[!(rownames(result_df_nirs) %in% c("(Intercept)", "logSigma")), , drop = FALSE]
  summary_table_filtered$output_P_value_nirs <- sprintf("%.2e",
                                                      summary_table_filtered$output_P_value_nirs)
  summary_table_filtered$output_coefficient_nirs <- round(summary_table_filtered$output_coefficient_nirs, 2)
  year_data[[sprintf('%d_output_nirs', year)]] <- summary_table_filtered
  
  ########## DIRECTIONAL CRS ##########
  summary_info_directional_nirs <- summary(model_directional_nirs)
  summary_table <- summary_info_directional_nirs$estimate
  coefficients <- summary_table[, "Estimate"]
  p_values <- summary_table[, "Pr(> t)"]
  significance_levels <- sapply(p_values, assign_significance_level)

  result_df_nirs <- data.frame(coefficients, p_values, significance_levels)
  names(result_df_nirs) <- c("directional_coefficient_nirs", "directional_P_value_nirs", "directional_significance_nirs")
  summary_table_filtered <- result_df_nirs[!(rownames(result_df_nirs) %in% c("(Intercept)", "logSigma")), , drop = FALSE]
  summary_table_filtered$directional_P_value_nirs <- sprintf("%.2e",
                                                      summary_table_filtered$directional_P_value_nirs)
  summary_table_filtered$directional_coefficient_nirs <- round(summary_table_filtered$directional_coefficient_nirs, 2)
  year_data[[sprintf('%d_directional_nirs', year)]] <- summary_table_filtered
  
  
  new_order_indices <- c(1, 4, 7, 2, 5, 8, 3, 6, 9)
  year_data <- year_data[new_order_indices]
  appended_df <- Reduce(function(x, y) cbind(x, y), year_data)
  
  write.csv(appended_df, write_file_path, row.names = TRUE, quote = FALSE)
}

