# Clean data by removing NA values
clean_data <- function(data) {
  return(na.omit(data))
}

# Calculate average solar radiation for each month
calculate_avg_solar <- function(data) {
  data %>%
    group_by(Month) %>%
    summarize(Average_Solar_Radiation = mean(Solar.R, na.rm = TRUE))
}

# Calculate correlation between Ozone and Solar Radiation for each month
# Calculate correlation between Ozone and Solar Radiation for each month
calculate_correlations <- function(data) {
  data %>%
    group_by(Month) %>%
    summarize(Correlation = ifelse(n() > 1, cor(Ozone, Solar.R, use = "complete.obs"), NA_real_)) %>%
    pull(Correlation)
}


# Determine shape for plot based on correlation value
# Determine shape for plot based on correlation value
shape_assignment <- function(correlation_value) {
  if (is.na(correlation_value)) {
    return(16)  # Return a default shape for NA values
  } else if (correlation_value > 0.5) {
    return(19)
  } else if (correlation_value > 0) {
    return(17)
  } else {
    return(15)
  }
}


# Extract month name from month number
get_month_name <- function(month_number) {
  month.abb[month_number]
}

# Generate and save plot for a specific month
generate_and_save_plot <- function(data_for_month, correlation_for_month) {
  month_name <- get_month_name(unique(data_for_month$Month)[1])
  shape_for_plot <- shape_assignment(correlation_for_month)
  
  plot_for_month <- ggplot(data_for_month, aes(x = Solar.R, y = Ozone)) +
    geom_point(shape = shape_for_plot) +
    ggtitle(month_name)
  
  print(plot_for_month) # Display plot
  ggsave(filename = paste0("plot_", tolower(month_name), ".png"), plot = plot_for_month)
}

# Plot data by month
plot_data_by_month <- function(data, correlations) {
  unique_months <- unique(data$Month)
  
  for(month in unique_months) {
    data_for_month <- data %>% filter(Month == month)
    correlation_for_month <- correlations[month]
    
    generate_and_save_plot(data_for_month, correlation_for_month)
  }
}

# Save cleaned data
save_cleaned_data <- function(data) {
  readr::write_csv(data, "cleaned_data.csv")
}

analyze_airquality_data <- function(input_data) {
  # Step 1: Clean the data
  cleaned_data <- clean_data(input_data)
  
  # Step 2: Calculate and print average solar radiation
  avg_solar <- calculate_avg_solar(cleaned_data)
  cat("\nAverage Solar Radiation for Each Month:\n")
  print(avg_solar)
  
  # Step 3: Calculate and print correlations
  correlations <- calculate_correlations(cleaned_data)
  cat("\nCorrelation between Ozone and Solar Radiation for Each Month:\n")
  print(correlations)
  
  # Step 4: Plot data by month
  plot_data_by_month(cleaned_data, correlations)
  
  # Step 5: Save cleaned data
  save_cleaned_data(cleaned_data)
}
