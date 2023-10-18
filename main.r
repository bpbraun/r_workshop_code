library(ggplot2)

clean_data <- function(data) {
  return(na.omit(data))
}

calculate_avg_solar <- function(data) {
  return(aggregate(Solar.R ~ Month, data, mean))
}

calculate_correlations <- function(data) {
  return(sapply(unique(data$Month), function(m) {
    cor(data$Ozone[data$Month == m], data$Solar.R[data$Month == m])
  }))
}

shape_assignment <- function(cor_value) {
  if (cor_value > 0.5) {
    return(19)
  } else if (cor_value > 0) {
    return(17)
  } else {
    return(15)
  }
}

plot_data_by_month <- function(data, correlations) {
  for (m in unique(data$Month)) {
    month_name <- month.abb[m]
    shape <- shape_assignment(correlations[month_name])
    g <- ggplot(data[data$Month == m,], aes(x = Solar.R, y = Ozone)) + 
      geom_point(shape = shape) + 
      ggtitle(month_name)
    ggsave(paste0("plot_", tolower(month_name), ".png"), g)
  }
}

save_cleaned_data <- function(data) {
  write.csv(data, "cleaned_data.csv", row.names = FALSE)
}

# Main Execution
data <- airquality
data <- clean_data(data)

avg_solar <- calculate_avg_solar(data)
print(avg_solar)

correlations <- calculate_correlations(data)
print(correlations)

plot_data_by_month(data, correlations)

save_cleaned_data(data)
