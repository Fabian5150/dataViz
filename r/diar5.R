install.packages("ggplot2")
install.packages("plot3D")
install.packages("plot3Drgl")
install.packages("MASS")
install.packages("magick")

library(magick)
library(MASS)
library("ggplot2")
library("plot3D")
library("plot3Drgl")
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

# linear regression 
# from furhter resource on canvas: https://www.sthda.com/english/wiki/impressive-package-for-3d-and-4d-graph-r-software-and-data-visualization?title=impressive-package-for-3d-and-4d-graph-r-software-and-data-visualization
fit <- lm(z ~ x + y)

grid.lines = 26
x.pred <- seq(min(x), max(x), length.out = grid.lines)
y.pred <- seq(min(y), max(y), length.out = grid.lines)
xy <- expand.grid(x = x.pred, y = y.pred)
z.pred <- matrix(predict(fit, newdata = xy), 
                 nrow = grid.lines, ncol = grid.lines)

scatter3D(x, y, z,
          #clab = c("Mechanics"),
          #col = colors,
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
x.breaks <- seq(min(math$ve), max(math$ve), length.out = 15)
y.breaks <- seq(min(math$me), max(math$me), length.out = 15)

xy.table <- table(cut(math$ve, x.breaks), cut(math$me, y.breaks))

z <- as.matrix(xy.table)

x.mids <- (head(x.breaks, -1) + tail(x.breaks, -1)) / 2
y.mids <- (head(y.breaks, -1) + tail(y.breaks, -1)) / 2

hist3D(
  x = x.mids,
  y = y.mids,
  z = z,
  scale = TRUE,
  border = "black",
  xlab = "vectors",
  ylab = "mechanics",
  zlab = "frequency",
  ticktype = "detailed"
)
plotrgl()

# Visualisations inspired by 'WorkWithImages.Rmd'
# Remake of map (both images from may of 2024 according to google)
count_colors <- function(image){
  data <- image_data(image) %>%
    apply(2:3, paste, collapse= "") %>% 
    as.vector %>% table() %>%  as.data.frame() %>% 
    setNames(c("col", "freq"))
  data$col <- paste("#",data$col, sep="")
  return(data)
}
plot_hist <- function(data){
  img <- image_graph(500, 500)
  plot <- ggplot2::ggplot(data) + 
    geom_bar(aes(col, freq, fill = I(col)), stat = 'identity') +
    theme(axis.title = element_blank(), axis.text.y = element_blank(),
          axis.ticks.y = element_blank())
  print(plot)
  dev.off()
  img
}

opladen <- image_read("~/gitProjects/uni/dpv/diary/r/assets/opladen-google-earth.png") %>% image_scale('x500')
opladen

quantized <- image_quantize(opladen, 6, colorspace = 'YCbCr')
quantized

orig1 <- image_crop(opladen, '500x500')
hist1 <- image_crop(quantized, '500x500') %>% count_colors %>% plot_hist()
opladen_final <- image_append(c(orig1, hist1))
opladen_final


umea <- image_read("~/gitProjects/uni/dpv/diary/r/assets/umea-google-earth.png") %>% image_scale('x500')
umea

quantized <- image_quantize(umea, 6, colorspace = 'YCbCr')
quantized

orig1 <- image_crop(umea, '500x500')
hist1 <- image_crop(quantized, '500x500') %>% count_colors %>% plot_hist()
umea_final <- image_append(c(orig1, hist1))
umea_final