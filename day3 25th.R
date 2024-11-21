library(plotly)
performance_data <- data.frame(
  Student = c("A", "B", "C", "D", "E"),
  MathScore = c(85, 72, 90, 78, 88),
  ScienceScore = c(78, 85, 80, 75, 82),
  Attendance = c(95, 92, 98, 85, 93)
)

cat("Summary of Academic Performance Data:\n")
print(summary(performance_data))
cat("\nCorrelation Analysis:\n")
correlation_math_science <- cor(performance_data$MathScore, performance_data$ScienceScore)
correlation_attendance_science <- cor(performance_data$Attendance, performance_data$ScienceScore)
print(paste("Math vs Science Scores Correlation:", round(correlation_math_science, 2)))
print(paste("Attendance vs Science Scores Correlation:", round(correlation_attendance_science, 2)))

scatter_plot <- plot_ly(performance_data, x = ~MathScore, y = ~ScienceScore, z = ~Attendance,
                        type = "scatter3d", mode = "markers",
                        marker = list(size = 8, color = ~ScienceScore, colorscale = "Viridis")) %>%
  layout(scene = list(
    xaxis = list(title = "Math Score"),
    yaxis = list(title = "Science Score"),
    zaxis = list(title = "Attendance (%)")),
    title = "3D Scatter Plot: Math Score, Science Score, and Attendance")

scatter_plot

cat("\nInterpretation of the 3D scatter plot:\n")
cat("Look for trends where higher attendance or math scores align with better science scores.\n")

math_seq <- seq(min(performance_data$MathScore), max(performance_data$MathScore), length.out = 10)
attendance_seq <- seq(min(performance_data$Attendance), max(performance_data$Attendance), length.out = 10)
science_surface <- outer(math_seq, attendance_seq, 
                         function(m, a) mean(performance_data$ScienceScore) + 
                           0.3 * (m - mean(performance_data$MathScore)) + 
                           0.2 * (a - mean(performance_data$Attendance)))

surface_plot <- plot_ly(x = ~math_seq, y = ~attendance_seq, z = ~science_surface) %>%
  add_surface(colorscale = "Viridis") %>%
  layout(scene = list(
    xaxis = list(title = "Math Score"),
    yaxis = list(title = "Attendance (%)"),
    zaxis = list(title = "Science Score")),
    title = "3D Surface Plot: Science Scores vs Math Scores and Attendance")

surface_plot

plot_science_math <- plot_ly(performance_data, x = ~MathScore, y = ~ScienceScore, z = ~Attendance,
                             type = "scatter3d", mode = "markers",
                             marker = list(size = 8, color = ~MathScore, colorscale = "Blues")) %>%
  layout(scene = list(
    xaxis = list(title = "Math Score"),
    yaxis = list(title = "Science Score"),
    zaxis = list(title = "Attendance (%)")),
    title = "3D Scatter Plot: Science Score vs Math Score and Attendance")

plot_science_math

plot_science_attendance <- plot_ly(performance_data, x = ~Attendance, y = ~ScienceScore, z = ~MathScore,
                                   type = "scatter3d", mode = "markers",
                                   marker = list(size = 8, color = ~Attendance, colorscale = "Reds")) %>%
  layout(scene = list(
    xaxis = list(title = "Attendance (%)"),
    yaxis = list(title = "Science Score"),
    zaxis = list(title = "Math Score")),
    title = "3D Scatter Plot: Science Score vs Attendance and Math Score")

plot_science_attendance
