# Loading necessary libraries
library(ggplot2)

# Fetching data
data <- airquality

# Data Cleaning - Remove rows with NA
data <- na.omit(data)

# Analysis Part 1: Calculate average Solar Radiation for each month
avg_solar <- aggregate(Solar.R ~ Month, data, mean)
avg_solar

# Analysis Part 2: Correlation between Ozone and Solar Radiation for each month
correlations <- sapply(unique(data$Month), function(m) {
  cor(data$Ozone[data$Month == m], data$Solar.R[data$Month == m])
})

names(correlations) <- c("May", "June", "July", "August", "September")
print(correlations)

# Visualization
shape_assignment <- function(cor_value) {
  if (cor_value > 0.5) {
    return(19)
  } else if (cor_value > 0 & cor_value <= 0.5) {
    return(17)
  } else {
    return(15)
  }
}

# Save Plots
for (m in unique(data$Month)) {
  month_name <- month.abb[m]
  shape <- shape_assignment(correlations[month_name])
  g <- ggplot(data[data$Month == m,], aes(x = Solar.R, y = Ozone)) + 
    geom_point(shape = shape) + 
    ggtitle(month_name)
  ggsave(paste0("plot_", tolower(month_name), ".png"), g)
}

# Save data
write.csv(data, "cleaned_data.csv", row.names = FALSE)
