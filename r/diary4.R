# Enable the r-universe repo
options(repos = c(
  fawda123 = 'https://fawda123.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'))

install.packages("klaR")
install.packages("psych")
install.packages("MASS")
install.packages(c("devtools", "car", "ggpur"))
install.packages("factoextra")
install.packages("ggplot2")
install.packages('ggord')

library("klaR")
library("psych")
library("MASS")
library("factoextra")
library("ggplot2")
library("ggord")

# Let's say focus is to cluster quality? so what features attribute most to the wine quality?

load("/home/fabian/gitProjects/uni/dpv/diary/r/data/wine-1.RData")

summary(wine5)
## raw dataset

# PCA
raw_pca <- princomp(wine5, cor = TRUE) # => uses correlation instead of covariance, because data is not scaled
summary(raw_pca)
raw_pca$loadings

# Using wine quality for class
# Raw PCA visualisation
ggplot(data.frame(raw_pca$scores), aes(x = Comp.1, y = Comp.2, color=paste0(wine5$quality) )) +
  geom_point(size = 1) +
  labs(x = "PCA 1", y = "PCA 2")

ggplot(data.frame(raw_pca$scores), aes(x = Comp.1, y = Comp.2, size = Comp.3, color=paste0(wine5$quality)  )) +
  geom_point(alpha = 0.7) +
  labs(x = "PCA 1", y = "PCA 2", size = "PCA 3")

# PCA with scaled data and quality removed
scaled_wine <- scale(wine5[, 1:4])
scaled_pca <- princomp(scaled_wine)
summary(scaled_pca)
scaled_pca$loadings

ggplot(data.frame(scaled_pca$scores), aes(x = Comp.1, y = Comp.2))+#, color=paste0(wine5$quality)  )) +
  geom_point(size = 1) +
  labs(x = "PCA 1", y = "PCA 2")

# with clustering visualized:
ggplot(data.frame(scaled_pca$scores), aes(x = Comp.1, y = Comp.2, color=paste0(wine5$quality)  )) +
  geom_point(size = 1) +
  stat_ellipse(aes(fill = factor(wine5$quality))) +
  labs(x = "PCA 1", y = "PCA 2")
# actual continous nature of quality becomes visual
# quality somewhat reflected in other data? at least small trend 4 -> 5 -> 6 -> 7 visible
# suggest that quality mostly depended on (take a look at what influences PCA1 negatively)


# Using the quality column as different classes for wine5
# Raw LDA
linear <- lda(quality~. - quality, data = wine5) # uses quality as class and removes it as axis for the lda itself
linear

ggord(linear, paste0(wine5$quality),
      ylim = c(-5, 5),
      xlim=c(-6,8))
# not really pretty, but you can see that better quality wine tends to have a higher LD1 and a lower LD2 value

# Scaled LDA
scaled_wine <-  as.data.frame(scale(wine5))
scaled_linear <- lda(quality~. - quality, data = scaled_wine) # uses quality as class and removes it as axis for the lda itself
scaled_linear

ggord(scaled_linear, paste0(wine5$quality),
      ylim = c(-5, 5),
      xlim=c(-6,8))

# k-means raw
get_color <- function(quality) {
  colors <- rep(NA, length(quality))
  colors[quality == 4] <- "red"
  colors[quality == 5] <- "green"
  colors[quality == 6] <- "blue"
  colors[quality == 7] <- "purple"
  
  return(colors)
}

km <- kmeans(wine5[, 1:4], 4, 100)
fviz_cluster(km, data = wine5[, 1:4],
             geom = "point",
             ellipse.type = "convex",
             ggtheme = theme_bw()
) +
  geom_point(aes(color = get_color(wine5$quality))) +
  scale_color_identity()


print(km$centers)
summary(km)
# k-means scaled

km_scaled <- kmeans(scale(wine5[, 1:4]), 4, 100)
fviz_cluster(km_scaled, data = wine5[, 1:4],
             geom = "point",
             ellipse.type = "convex",
             ggtheme = theme_bw()
) +
  geom_point(aes(color = get_color(wine5$quality))) +
  scale_color_identity()

print(km_scaled$centers)
