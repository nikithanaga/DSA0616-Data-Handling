Company <- c("A", "B", "C", "D")
Market <- c(20, 30, 25, 25)
percentages <- round(Market / sum(Market) * 100, 1)
pie(Market, labels = paste(Company, percentages, "%"), main = "Market Share by Company", col = c("red", "blue", "green", "yellow"))
