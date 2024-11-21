library(vioplot)
Group <- c("X", "X", "X", "X", "Y", "Y", "Y", "Y")
Score <- c(80, 85, 78, 92, 88, 76, 80, 84)
vioplot(Score[Group == "X"], Score[Group == "Y"], 
        names = c("Group X", "Group Y"), 
        col = c("orange", "yellow"),
        main = "Violin Plot of Scores by Group",
        xlab = "Group", ylab = "Score")
