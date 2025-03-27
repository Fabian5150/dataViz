install.packages("ggplot2")
install.packages("plot3D")
install.packages("MASS")

library(MASS)
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

# question: How do the three math subjects influence the mechanics grade of a student?

colors <- colorRampPalette(c("blue", "red"))(10)[as.numeric(cut(math$me, breaks = 10))]

# x, y and z coordinates
x <- math$al
y <- math$an
z <- math$st
# Basic scatter plot
scatter3D(x, y, z,
          clab = c("Mechanics"),
          col = colors,
          xlab = "Algebra",
          ylab = "Analysis",
          zlab = "Statistics",
          bty="b2",
          pch=19,
          ticktype = "detailed",
          type = "b"
)

cor(math)

scatter3D(x, y, z,
          xlab = "Algebra",
          ylab = "Analysis",
          zlab = "Statistics",
          bty="b2",
          colvar = NULL,
          cex=0.5,
          pch=19
)

# linear regression from https://www.sthda.com/english/wiki/impressive-package-for-3d-and-4d-graph-r-software-and-data-visualization?title=impressive-package-for-3d-and-4d-graph-r-software-and-data-visualization
fit <- lm(z ~ x + y)

grid.lines = 26
x.pred <- seq(min(x), max(x), length.out = grid.lines)
y.pred <- seq(min(y), max(y), length.out = grid.lines)
xy <- expand.grid(x = x.pred, y = y.pred)
z.pred <- matrix(predict(fit, newdata = xy), 
                 nrow = grid.lines, ncol = grid.lines)

scatter3D(x, y, z,
          clab = c("Mechanics"),
          col = colors,
          xlab = "Algebra",
          ylab = "Analysis",
          zlab = "Statistics",
          bty="b2",
          pch=19,
          cex=0.5,
          ticktype = "detailed",
          surf = list(x = x.pred, y = y.pred, z = z.pred, alpha=0.75)
)

# Histogram