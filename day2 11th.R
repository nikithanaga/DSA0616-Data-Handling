data <- data.frame(
  Year = 2015:2019,
  Revenue = c(30, 35, 40, 45, 50))
ggplot(data, aes(x = Year, y = Revenue)) +
  geom_area(fill = "skyblue", alpha = 0.5) +
  labs(title = "Yearly Revenue",
       x = "Year",
       y = "Revenue (in million)") +theme_minimal()
