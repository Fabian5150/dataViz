library(ggplot2)
library(MASS)
# First I wanted to check out the available datasets
data(package="ggplot2")
data(package = .packages(all.available = TRUE))

# This dataset shows the effect of swedish speed limits on accidents
# I found it quite interesting, as the introduction of a speed limit is a hot topic in germany right now
# and learning something from the scandinavians turned out pretty well in the past
MASS::Traffic

data("Traffic", package="MASS")

# Shows that the dataset is not really big. Only 184 observations
# Taken from two years time, 1961-1962 Probably not the best data to make decision for the present...
summary(Traffic)
str(Traffic)

ggplot(Traffic, aes(x = day, y = y)) +
  geom_point(aes(color = limit), size = 1) +
  labs(y = "Accidents", x = "Day")
# Not quite right yet, Renders day n of 1961 and 1962 on the same x position..

# TBD: Jahr irgendwie auf X Achse noch dazu schreiben


# Above not very insightful. Maybe do simple histogram instead:
ggplot(Traffic, aes(x=limit, y=y, fill=limit)) +
  geom_boxplot() +
  labs(x="Speed Limit", y="Accidents")
