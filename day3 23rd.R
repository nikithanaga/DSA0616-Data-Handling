library(plotly)
survey_data <- data.frame(
  Product = c("A", "B", "C", "D", "E"),
  Price = c(50, 70, 60, 45, 55),
  Rating = c(4.2, 3.8, 4.0, 4.5, 3.9),
  AgeGroup = factor(c("25-35", "35-45", "18-25", "45-55", "25-35")))

cat("Summary of Survey Data:\n")
print(summary(survey_data))
cat("\nCorrelation Analysis (Price vs Rating):\n")
correlation <- cor(survey_data$Price, survey_data$Rating)
print(correlation)

scatter_plot <- plot_ly(survey_data, x = ~Price, y = ~Rating, z = ~as.numeric(AgeGroup),
                        type = "scatter3d", mode = "markers",
                        marker = list(size = 8, color = ~Rating, colorscale = "Viridis")) %>%
  layout(scene = list(
    xaxis = list(title = "Price ($)"),
    yaxis = list(title = "Rating (out of 5)"),
    zaxis = list(title = "Age Group (numeric representation)")),
    title = "3D Scatter Plot: Price, Rating, and Age Group")

scatter_plot
cat("\nInterpretation of the 3D scatter plot:\n")
cat("Observe whether higher-priced products receive better ratings or specific age groups dominate.\n")
age_numeric <- as.numeric(survey_data$AgeGroup)
price_seq <- seq(min(survey_data$Price), max(survey_data$Price), length.out = 10)
age_seq <- seq(min(age_numeric), max(age_numeric), length.out = 10)

rating_surface <- outer(price_seq, age_seq, 
                        function(p, a) mean(survey_data$Rating) + 
                          0.05 * (p - mean(survey_data$Price)) - 
                          0.1 * (a - mean(age_numeric)))

surface_plot <- plot_ly(x = ~price_seq, y = ~age_seq, z = ~rating_surface) %>%
  add_surface(colorscale = "Viridis") %>%
  layout(scene = list(
    xaxis = list(title = "Price ($)"),
    yaxis = list(title = "Age Group (numeric)"),
    zaxis = list(title = "Rating (out of 5)")),
    title = "3D Surface Plot: Ratings vs Price and Age Group")

surface_plot
plot_rating_price <- plot_ly(survey_data, x = ~Price, y = ~Rating, z = ~as.numeric(AgeGroup),
                             type = "scatter3d", mode = "markers",
                             marker = list(size = 8, color = ~Price, colorscale = "Blues")) %>%
  layout(scene = list(
    xaxis = list(title = "Price ($)"),
    yaxis = list(title = "Rating (out of 5)"),
    zaxis = list(title = "Age Group (numeric representation)")),
    title = "3D Scatter Plot: Rating vs Price and Age Group")

plot_rating_price
plot_rating_agegroup <- plot_ly(survey_data, x = ~as.numeric(AgeGroup), y = ~Rating, z = ~Price,
                                type = "scatter3d", mode = "markers",
                                marker = list(size = 8, color = ~Rating, colorscale = "Reds")) %>%
  layout(scene = list(
    xaxis = list(title = "Age Group (numeric representation)"),
    yaxis = list(title = "Rating (out of 5)"),
    zaxis = list(title = "Price ($)")),
    title = "3D Scatter Plot: Rating vs Age Group and Price")

plot_rating_agegroup
