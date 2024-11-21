product=c("A","B","C","D","E")
Sales=c(300,450,500,350,400)
par(lwd = 2)
barplot(Sales,names.arg=product,main="sales vs product",col="lightblue",border="blue")
