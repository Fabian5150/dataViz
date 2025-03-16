# Installiere notwendige Pakete
install.packages("klaR")
install.packages("psych")
install.packages("MASS")
#install.packages("car")
#install.packages("factoextra")
install.packages("ggplot2")

# Enable the r-universe repo
options(repos = c(
  fawda123 = 'https://fawda123.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'))

# Install ggord
install.packages('ggord')

library("klaR")
library("psych")
library("MASS")
#library("car")
#library("factoextra")
library("ggplot2")
library("ggord")

load("/home/fabian/gitProjects/uni/dpv/diary/r/data/wine-1.RData")

summary(wine5)
## raw dataset

# PCA
raw_pca <- princomp(wine5, cor = TRUE) # => uses correlation instead of covariance, because data is not scaled
summary(raw_pca)
raw_pca$loadings
print(raw_pca$scores)

# Using wine quality for class
# Raw PCA visualisation
ggplot(data.frame(raw_pca$scores), aes(x = Comp.1, y = Comp.2)) +
  geom_point(size = 1) +
  labs(x = "PCA 1", y = "PCA 2")

ggplot(data.frame(raw_pca$scores), aes(x = Comp.1, y = Comp.2, size = Comp.3)) +
  geom_point(alpha = 0.7) +
  labs(x = "PCA 1", y = "PCA 2", size = "PCA 3")

# PCA with scaled data
scaled_wine <- scale(wine5)
scaled_pca <- princomp(scaled_wine, cor = FALSE)
summary(scaled_pca)

ggplot(data.frame(scaled_pca$scores), aes(x = Comp.1, y = Comp.2)) +
  geom_point(size = 1) +
  labs(x = "PCA 1", y = "PCA 2")

# Using the quality column as different classes for wine5
# LDA
linear <- lda(quality~. - quality, data = wine5) # uses quality as class and removes it as axis for the lda itself
linear
#hist(linear$LD1)
ggord(linear, wine5$quality,
      ylim = c(-5, 5),
      xlim=c(-6,8))