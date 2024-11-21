library(ggplot2)
library(plotly)
weather_data <- data.frame(
  Date = as.Date(c("2023-01-01", "2023-01-02", "2023-01-03", "2023-01-04", "2023-01-05")),
  Temperature = c(10, 12, 8, 15, 14),
  Humidity = c(75, 70, 80, 65, 72),
  WindSpeed = c(15, 12, 18, 20, 16))
cat("Summary of Weather Data:\n")
print(summary(weather_data))

scatter_plot <- ggplot(weather_data, aes(x = WindSpeed, y = Humidity, color = Temperature)) +
  geom_point(size = 4) +
  labs(title = "Relationship Between Wind Speed and Humidity",
       x = "Wind Speed (km/h)", y = "Humidity (%)") +
  theme_minimal()

print(scatter_plot)

cat("\nCorrelation Analysis:\n")
correlation_matrix <- cor(weather_data[, -1])
print(correlation_matrix)

humidity_seq <- seq(min(weather_data$Humidity), max(weather_data$Humidity), length.out = 10)
windspeed_seq <- seq(min(weather_data$WindSpeed), max(weather_data$WindSpeed), length.out = 10)

temperature_surface <- outer(humidity_seq, windspeed_seq, 
                             function(h, w) mean(weather_data$Temperature) + 0.1 * (h - mean(weather_data$Humidity)) + 0.2 * (w - mean(weather_data$WindSpeed)))

surface_plot <- plot_ly(x = ~humidity_seq, y = ~windspeed_seq, z = ~temperature_surface) %>%
  add_surface(colorscale = "Viridis") %>%
  layout(scene = list(
    xaxis = list(title = "Humidity (%)"),
    yaxis = list(title = "Wind Speed (km/h)"),
    zaxis = list(title = "Temperature (°C)")),
    title = "3D Surface Plot: Temperature with Humidity and Wind Speed")

surface_plot

plot_temp_humidity <- plot_ly(weather_data, x = ~Humidity, y = ~Temperature, z = ~WindSpeed, type = "scatter3d", mode = "markers") %>%
  layout(scene = list(
    xaxis = list(title = "Humidity (%)"),
    yaxis = list(title = "Temperature (°C)"),
    zaxis = list(title = "Wind Speed (km/h)")),
    title = "3D Scatter Plot: Temperature vs Humidity vs Wind Speed")

plot_temp_humidity
plot_temp_windspeed <- plot_ly(weather_data, x = ~WindSpeed, y = ~Temperature, z = ~Humidity, type = "scatter3d", mode = "markers") %>%
  layout(scene = list(
    xaxis = list(title = "Wind Speed (km/h)"),
    yaxis = list(title = "Temperature (°C)"),
    zaxis = list(title = "Humidity (%)")),
    title = "3D Scatter Plot: Temperature vs Wind Speed vs Humidity")
plot_temp_windspeed
