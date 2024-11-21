temperature_data <- data.frame(
  Month = c("Jan", "Feb", "Mar", "Apr", "May"),
  city_a = c(5, 6, 7, 8, 9),
  city_b = c(10, 11, 12, 13, 14),
  city_c = c(15, 16, 17, 18, 19)
)
rownames(temperature_data) <- temperature_data$Month
temperature_data <- temperature_data[, -1]
heatmap(as.matrix(temperature_data), 
        main = "Temperature Heatmap", 
        xlab = "Cities", ylab = "Months", 
        col = heat.colors(256), scale = "column", 
        margins = c(5, 10), 
        cexRow = 1.2,         
        cexCol = 1.2,         
        las = 2)          
