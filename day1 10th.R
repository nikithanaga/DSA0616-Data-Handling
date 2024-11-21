Sales_data <- data.frame(
  Month = c("Jan", "Jan", "Feb", "Feb", "Mar", "Mar"),
  Region = c("East", "West", "East", "West", "East", "West"),
  Sales = c(200, 150, 220, 170, 210, 160)
)
par(mfrow = c(1, 2))  
east_sales <- Sales_data[Sales_data$Region == "East", ]
barplot(east_sales$Sales, names.arg = east_sales$Month, 
        main = "East Region Sales", xlab = "Month", ylab = "Sales", col = "pink")
west_sales <- Sales_data[Sales_data$Region == "West", ]
barplot(west_sales$Sales, names.arg = west_sales$Month, 
        main = "West Region Sales", xlab = "Month", ylab = "Sales", col = "yellow")
