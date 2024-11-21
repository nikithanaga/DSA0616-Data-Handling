library(ggplot2)
library(plotly)
market_data <- data.frame(
  Date = as.Date(c("2023-01-01", "2023-01-02", "2023-01-03", "2023-01-04", "2023-01-05")),
  StockPrice = c(100, 102, 98, 105, 108),
  VolumeTraded = c(2.5, 3.0, 2.2, 2.8, 3.5),
  MarketCap = c(500, 510, 490, 525, 540)
)
cat("Summary of Market Data:\n")
print(summary(market_data))

cat("\nCorrelation Analysis:\n")
correlation_matrix <- cor(market_data[, -1]) # Exclude Date for correlation
print(correlation_matrix)

scatter_plot <- plot_ly(market_data, x = ~VolumeTraded, y = ~MarketCap, z = ~StockPrice,
                        type = "scatter3d", mode = "markers",
                        marker = list(size = 5, color = ~StockPrice, colorscale = "Viridis")) %>%
  layout(scene = list(
    xaxis = list(title = "Volume Traded (millions)"),
    yaxis = list(title = "Market Cap ($)"),
    zaxis = list(title = "Stock Price ($)")),
    title = "3D Scatter Plot: Volume Traded, Market Cap, and Stock Price")

scatter_plot

cat("\nClustering or Outliers:\n")
print("Look for grouping or deviations in the 3D scatter plot.")

stockprice_seq <- seq(min(market_data$StockPrice), max(market_data$StockPrice), length.out = 10)
volume_seq <- seq(min(market_data$VolumeTraded), max(market_data$VolumeTraded), length.out = 10)

marketcap_surface <- outer(stockprice_seq, volume_seq, 
                           function(sp, vt) mean(market_data$MarketCap) + 
                             5 * (sp - mean(market_data$StockPrice)) + 
                             20 * (vt - mean(market_data$VolumeTraded)))

surface_plot <- plot_ly(x = ~stockprice_seq, y = ~volume_seq, z = ~marketcap_surface) %>%
  add_surface(colorscale = "Viridis") %>%
  layout(scene = list(
    xaxis = list(title = "Stock Price ($)"),
    yaxis = list(title = "Volume Traded (millions)"),
    zaxis = list(title = "Market Cap ($)")),
    title = "3D Surface Plot: Market Cap with Stock Price and Volume Traded")

surface_plot
plot_price_volume <- plot_ly(market_data, x = ~VolumeTraded, y = ~StockPrice, z = ~MarketCap,
                             type = "scatter3d", mode = "markers") %>%
  layout(scene = list(
    xaxis = list(title = "Volume Traded (millions)"),
    yaxis = list(title = "Stock Price ($)"),
    zaxis = list(title = "Market Cap ($)")),
    title = "3D Scatter Plot: Stock Price vs Volume Traded vs Market Cap")

plot_price_volume
plot_price_marketcap <- plot_ly(market_data, x = ~MarketCap, y = ~StockPrice, z = ~VolumeTraded,
                                type = "scatter3d", mode = "markers") %>%
  layout(scene = list(
    xaxis = list(title = "Market Cap ($)"),
    yaxis = list(title = "Stock Price ($)"),
    zaxis = list(title = "Volume Traded (millions)")),
    title = "3D Scatter Plot: Stock Price vs Market Cap vs Volume Traded")

plot_price_marketcap
