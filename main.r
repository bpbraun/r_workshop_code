# Loading necessary libraries
library(ggplot2)

# Fetching data
data <- airquality

# Data Cleaning - Remove rows with NA
data <- na.omit(data)

# Analysis Part 1: Calculate average Solar Radiation for each month
avg_solar_May <- 0
avg_solar_June <- 0
avg_solar_July <- 0
avg_solar_August <- 0
avg_solar_September <- 0

for (i in 1:nrow(data)) {
    if (data$Month[i] == 5) {
        avg_solar_May <- avg_solar_May + data$Solar.R[i]
    } else if (data$Month[i] == 6) {
        avg_solar_June <- avg_solar_June + data$Solar.R[i]
    } else if (data$Month[i] == 7) {
        avg_solar_July <- avg_solar_July + data$Solar.R[i]
    } else if (data$Month[i] == 8) {
        avg_solar_August <- avg_solar_August + data$Solar.R[i]
    } else {
        avg_solar_September <- avg_solar_September + data$Solar.R[i]
    }
}

# Calculating average and print to console
avg_solar_May <- avg_solar_May/31
print(paste("Average Solar Radiation for May: ", avg_solar_May))

avg_solar_June <- avg_solar_June/30
print(paste("Average Solar Radiation for June: ", avg_solar_June))

avg_solar_July <- avg_solar_July/31
print(paste("Average Solar Radiation for July: ", avg_solar_July))

avg_solar_August <- avg_solar_August/31
print(paste("Average Solar Radiation for August: ", avg_solar_August))

avg_solar_September <- avg_solar_September/30
print(paste("Average Solar Radiation for September: ", avg_solar_September))

# Analysis Part 2: Correlation between Ozone and Solar Radiation for each month
correlation_May <- cor(data$Ozone[data$Month == 5], data$Solar.R[data$Month == 5])
print(paste("Correlation for May: ", correlation_May))

correlation_June <- cor(data$Ozone[data$Month == 6], data$Solar.R[data$Month == 6])
print(paste("Correlation for June: ", correlation_June))

correlation_July <- cor(data$Ozone[data$Month == 7], data$Solar.R[data$Month == 7])
print(paste("Correlation for July: ", correlation_July))

correlation_August <- cor(data$Ozone[data$Month == 8], data$Solar.R[data$Month == 8])
print(paste("Correlation for August: ", correlation_August))

correlation_September <- cor(data$Ozone[data$Month == 9], data$Solar.R[data$Month == 9])
print(paste("Correlation for September: ", correlation_September))

# Visualization
plot_shapes <- vector("character", length=nrow(data))
for (i in 1:nrow(data)) {
    if (correlation_May > 0.5) {
        plot_shapes[i] <- 19
    } else if (correlation_June > 0.5) {
        plot_shapes[i] <- 17
    } else {
        plot_shapes[i] <- 15
    }
}

# Save Plots
g1 <- ggplot(data[data$Month == 5,], aes(x = Solar.R, y = Ozone)) + geom_point(aes(shape = factor(Month))) + ggtitle("May")
ggsave("plot_may.png", g1)

g2 <- ggplot(data[data$Month == 6,], aes(x = Solar.R, y = Ozone)) + geom_point(aes(shape = factor(Month))) + ggtitle("June")
ggsave("plot_june.png", g2)

g3 <- ggplot(data[data$Month == 7,], aes(x = Solar.R, y = Ozone)) + geom_point(aes(shape = factor(Month))) + ggtitle("July")
ggsave("plot_july.png", g3)

g4 <- ggplot(data[data$Month == 8,], aes(x = Solar.R, y = Ozone)) + geom_point(aes(shape = factor(Month))) + ggtitle("August")
ggsave("plot_august.png", g4)

g5 <- ggplot(data[data$Month == 9,], aes(x = Solar.R, y = Ozone)) + geom_point(aes(shape = factor(Month))) + ggtitle("September")
ggsave("plot_september.png", g5)

# Save data
write.csv(data, "cleaned_data.csv", row.names = FALSE)
