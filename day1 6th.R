Class <- c("A", "A", "A", "A", "B", "B", "B", "B")
Scores <- c(85, 90, 78, 92, 88, 76, 80, 84)
boxplot(Scores~Class, main = "Boxplot of Scores by Class", 
        col = c("lightblue", "lightgreen"))
