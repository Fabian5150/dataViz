install.packages("ggplot2")
install.packages("plot3D")

library("ggplot2")
library("plot3D")
library(doBy)
# Search interesting dataset
data(package = .packages(all.available = TRUE))

# Visualisations inspired by 'Visualisation3D.Rmd'
doBy::math

data("math", package="doBy")
# "This data frame contains the following columns: mechanics, vectors, algebra, analysis, statistics."
# => Reduce to 3 dimensions: grades in algebra, analysis and statistics
# math <- subset(math, select = -c(1,2))

summary(math)
math$meCat <- math$me %/% 10

# question: How do the three math subjects influence the mechanics grade of a student?
# x, y and z coordinates
x <- math$al
y <- math$an
z <- math$st
# Basic scatter plot
scatter3D(x, y, z,
          #clab = c("algebra", "analysis", "statistics"),
          col = rainbow(100)[as.numeric(cut(math$me, breaks = 100))],
          xlab = "Algebra",
          ylab = "Analysis",
          zlab = "Statistics"
)
