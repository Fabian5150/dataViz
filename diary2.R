library(ggplot2)
library(MASS)
# First I wanted to check out the available datasets
data(package="ggplot2")
data(package = .packages(all.available = TRUE))

# This dataset shows the effect of swedish speed limits on accidents
# I found it quite interesting, as the introduction of a speed limit is a hot topic in germany right now
# and learning something from the scandinavians turned out pretty well in the past
# => Research question: Does the introduction of a speed limit reduce the amount of road accidents
MASS::Traffic

data("Traffic", package="MASS")

head(Traffic, 5)
tail(Traffic, 5)

# Shows that the dataset is not really big. Only 184 observations
# Taken from two years time, 1961-1962 Probably not the best data to make decision for the present...
summary(Traffic) # => Values for year & day not very useful...
str(Traffic)

# As the dataset is fairly small I could already see, that there are no missing values
# Nevertheless I tested it:
colSums(is.na(Traffic))

# Checking Variance:
var(Traffic$y)
# => high variance and small data set... Not the best to dra conclusions

# Before looking at the data I thought that there would have been a law for a general speed limit,
# so I expected a nice graph view where you can see the accidents way lower after this
# Now seeing that, the data doesn't look like this
# So I did a Boxplot later, as it serves the distribution of the data better in my opinion

ggplot(Traffic, aes(x = day, y = y)) +
  geom_point(aes(color = limit), size = 1) +
  labs(y = "Accidents", x = "Day")
# Not quite right yet, Renders day n of 1961 and 1962 on the same x position..

# data preparation: Make dates continuous
Traffic$date_cont <- ifelse(Traffic$year == "1961", Traffic$day, Traffic$day + 92)

ggplot(Traffic, aes(x = date_cont, y = y)) +
  geom_point(aes(color = limit), size = 1) +
  labs(y = "Accidents", x = "Day")

# Above not very insightful. Maybe do simple histogram instead:
ggplot(Traffic, aes(x=limit, y=y, fill=limit)) +
  geom_boxplot() +
  labs(x="Speed Limit", y="Accidents")

# Looking at the Boxplot; At which dates where the outliers?
# Maybe take a look at the 10 biggest values
# As the lowest possible number would be zero and the data is not to far from that,
# Negative outliers don't exist in this data

# Checking correlation
# Expecting correlation between 'limit' and 'y'
# (Therefore also between day and y...)
# Probably not the best data set to check for correlation, but let's do it anyway
cor(Traffic[,c(1,2,4)])
pairs(Traffic[,c(2,4)])
# It could've been interesting to see if there is a correlation between certain dates (e.g. Weekends vs. Weekdays) and y,
# But since the days are only numbered and not labeled with date, I can not do it

# Let's make 'limit' numeric, so we can check it's correlation with the amount of accidents
Traffic$limit_num <- ifelse(Traffic$limit == 'no', 0, 1)
cor(Traffic[,c("y","limit_num")])

# Looking for outliers: In Boxplot visible, that positive outliers exist
quantile(Traffic$y)
hist(Traffic$y, xlab="Accidents")

# Try to create Boxplot w/o outliers
# Common approach: Interquartile Range
Q1 <- quantile(Traffic$y, 0.25)
Q3 <- quantile(Traffic$y, 0.75)
IQR <- Q3 - Q1

lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

cat(lower_bound, upper_bound)
# lower_bound < 0 => as guessed, no negative outliers

# Can I count the number of outliers and see whether they occured rather w/ or w/o a speed limit?
outliers <- subset(Traffic, y > upper_bound)
print(outliers)
# => 4 outliers, all of them without speed limit

traffic_clean <- subset(Traffic, y <= upper_bound)
print(traffic_clean)