library(plotly)
environmental_data <- data.frame(
  Location = c("A", "B", "C", "D", "E"),
  Temperature = c(15, 20, 18, 12, 17),
  Humidity = c(65, 70, 68, 60, 72),
  CO2_Levels = c(400, 450, 420, 380, 430)
)
cat("Summary of Environmental Data:\n")
print(summary(environmental_data))

# Correlation between variables
cat("\nCorrelation Analysis:\n")
cor_temp_co2 <- cor(environmental_data$Temperature, environmental_data$CO2_Levels)
cor_humidity_co2 <- cor(environmental_data$Humidity, environmental_data$CO2_Levels)
print(paste("Temperature vs CO2 Levels Correlation:", round(cor_temp_co2, 2)))
print(paste("Humidity vs CO2 Levels Correlation:", round(cor_humidity_co2, 2)))
scatter_plot <- plot_ly(environmental_data, x = ~Temperature, y = ~Humidity, z = ~CO2_Levels,
                        type = "scatter3d", mode = "markers",
                        marker = list(size = 8, color = ~CO2_Levels, colorscale = "Viridis")) %>%
  layout(scene = list(
    xaxis = list(title = "Temperature (°C)"), 
    yaxis = list(title = "Humidity (%)"),
    zaxis = list(title = "CO2 Levels (ppm)")),
    title = "3D Scatter Plot: Temperature, Humidity, and CO2 Levels")

scatter_plot
cat("\nInterpretation of the 3D scatter plot:\n")
cat("Check if higher CO2 levels align with specific combinations of temperature and humidity.\n")

temp_seq <- seq(min(environmental_data$Temperature), max(environmental_data$Temperature), length.out = 10)
humidity_seq <- seq(min(environmental_data$Humidity), max(environmental_data$Humidity), length.out = 10)

co2_surface <- outer(temp_seq, humidity_seq, 
                     function(t, h) mean(environmental_data$CO2_Levels) + 
                       2 * (t - mean(environmental_data$Temperature)) + 
                       1.5 * (h - mean(environmental_data$Humidity)))

surface_plot <- plot_ly(x = ~temp_seq, y = ~humidity_seq, z = ~co2_surface) %>%
  add_surface(colorscale = "Viridis") %>%
  layout(scene = list(
    xaxis = list(title = "Temperature (°C)"),
    yaxis = list(title = "Humidity (%)"),
    zaxis = list(title = "CO2 Levels (ppm)")),
    title = "3D Surface Plot: CO2 Levels vs Temperature and Humidity")

surface_plot
plot_co2_temp <- plot_ly(environmental_data, x = ~Temperature, y = ~CO2_Levels, z = ~Humidity,
                         type = "scatter3d", mode = "markers",
                         marker = list(size = 8, color = ~Temperature, colorscale = "Blues")) %>%
  layout(scene = list(
    xaxis = list(title = "Temperature (°C)"),
    yaxis = list(title = "CO2 Levels (ppm)"),
    
    