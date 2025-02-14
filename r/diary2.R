install.packages("dplyr")
install.packages("ggalt")

library(ggplot2)
library(MASS)
library(dplyr)
library(ggalt)
# First I wanted to check out the available datasets
data(package="ggplot2")
data(package = .packages(all.available = TRUE))

# This dataset shows the effect of swedish speed limits on accidents
# I found it quite interesting, as the introduction of a speed limit is a hot topic in germany right now
# and learning something from the scandinavians turned out pretty well in the past
# => Research question: Does the introduction of a speed limit reduce the amount of road accidents
MASS::Traffic

data("Traffic", package="MASS")

head(Traffic, 3)
tail(Traffic, 3)

# Shows that the dataset is not really big. Only 184 observations
# Taken from two years time, 1961-1962 Probably not the best data to make decision for the present...
summary(Traffic) # => Values for year & day not very useful...
str(Traffic)

# As the dataset is fairly small I could already see, that there are no missing values
# Nevertheless I tested it:
colSums(is.na(Traffic))

# Checking Variance:
var(Traffic$y)
# => high variance and small data set... Not the best to draw conclusions from

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

# Above not very insightful. Maybe do simple Boxplot instead:
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
cor(Traffic[,c("year", "day","y","limit_num")])

# Looking for outliers: In Boxplot visible, that positive outliers exist
quantile(Traffic$y)
hist(Traffic$y, xlab="Accidents")

# Try to create Boxplot w/o outliers
# Take a look at the biggest values:
tail(Traffic[order(Traffic$y),], 10)

#Common approach: Interquartile Range
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

# To denote processing of the dataset, I will use indices
traffic_1 <- subset(Traffic, y <= upper_bound)
print(traffic_1)
hist(traffic_1$y, xlab="Accidents")

# However Boxplot shows that 'yes' has two outliers, far away from the rest of the data
# Maybe remove outliers for both categories individually?
traffic_limit <- subset(Traffic, limit == 'yes')
tail(traffic_limit[order(traffic_limit$y),], 10)

remove_outliers <- function(dataset, property){
  Q1 <- quantile(property, 0.25)
  Q3 <- quantile(property, 0.75)
  IQR <- Q3 - Q1
  
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  
  cat(lower_bound, upper_bound, "\n")
  print(subset(dataset, (property > upper_bound | property < lower_bound)))
  
  return (subset(dataset, (property < upper_bound & property > lower_bound)))
}

traffic_no_limit <- subset(Traffic, limit == 'no')

# Dataset with outliers removed for both categories
traffic_2 <- rbind(
  remove_outliers(traffic_limit, traffic_limit$y),
  remove_outliers(traffic_no_limit, traffic_no_limit$y)
)

# Now looking at the Boxplot:
ggplot(traffic_2, aes(x=limit, y=y, fill=limit)) +
  geom_boxplot() +
  labs(x="Speed Limit", y="Accidents")

# Mean removal
mean_y <- mean(traffic_2$y)
cat(mean_y)
# I will only remove the mean of 'y', as the other columns contain categorial data

traffic_3 <- mutate(traffic_2, y = y - mean_y)

# Comparison: Mean removal in groups:
remove_mean <- function(dataset, property){
  mean_prop <- mean(property)

  cat(mean_prop, "\n")
  
  return (mutate(dataset, y = property - mean_prop))
}

ggplot(traffic_4, aes(x=limit, y=y, fill=limit)) +
  geom_boxplot() +
  labs(x="Speed Limit", y="Accidents")

traffic_limit_clean <- remove_outliers(traffic_limit, traffic_limit$y)
traffic_no_limit_clean <- remove_outliers(traffic_no_limit, traffic_no_limit$y)

traffic_4 <- rbind(
  remove_mean(traffic_limit_clean, traffic_limit_clean$y),
  remove_mean(traffic_no_limit_clean, traffic_no_limit_clean$y)
)

ggplot(traffic_4, aes(x=limit, y=y, fill=limit)) +
  geom_boxplot() +
  labs(x="Speed Limit", y="Accidents")

# Scaling
sd_y <- sd(traffic_3$y)
traffic_5 <- mutate(traffic_3, y = y / sd_y)

ggplot(traffic_5, aes(x=limit, y=y, fill=limit)) +
  geom_boxplot() +
  labs(x="Speed Limit", y="Accidents")


# Some more graphs

# Marks difference between limit/no-limit clearer. But showing trend doesn't make much sense
ggplot(traffic_2, aes(x = date_cont, y = y)) +
  geom_smooth(aes(color = limit)) +
  labs(y = "Accidents", x = "Day")

# Shows that mostly a similar amount of accidents occurs, but with no limit the range goes up higher (even with removed outliers)
ggplot(traffic_2, aes(x=limit, y=y, fill=limit)) + 
  geom_violin(trim=FALSE)+
  geom_boxplot(width=0.1, fill="white")+
  labs(title="Amount of daily accidents with and without speed limit",x="Limit", y = "Accidents")

# With centered data:
ggplot(traffic_5, aes(x=limit, y=y, fill=limit)) + 
  geom_violin(trim=FALSE)+
  geom_boxplot(width=0.1, fill="white")+
  labs(title="Amount of daily accidents with and without speed limit",x="Limit", y = "Accidents")

# With original data:
ggplot(Traffic, aes(x=limit, y=y, fill=limit)) + 
  geom_violin(trim=FALSE)+
  geom_boxplot(width=0.1, fill="white")+
  labs(title="Amount of daily accidents with and without speed limit",x="Limit", y = "Accidents")
# => Removing outliers was a good idea


# Clustering
ggplot(traffic_5, aes(date_cont, y, col=limit)) + 
  geom_point(aes(shape=limit), size=2) +
  geom_encircle(data = subset(traffic_5, limit == 'yes'), aes(date_cont, y, col=limit)) +
  geom_encircle(data = subset(traffic_5, limit == 'no'), aes(date_cont, y, col=limit))
# => Due to mean removal, variance of 'no limit' group gets more visible

# Comparing year 1 and year 2
# Because we removed the outliers, we have missing data now
year_1 <- subset(traffic_2, year == 1961)
year_2 <- subset(traffic_2, year == 1962)

ggplot(traffic_2, aes(x = day, y = y)) +
  geom_smooth(aes(color = paste0(year))) +
  #geom_point(aes(color = paste0(year), shape=limit))
  labs(y = "Accidents", x = "Day")
# => smoothing helps a lot to compare both years, distorts reality however

# Clustering year 1 and year 2
ggplot(traffic_2, aes(x = day, y = y)) +
  geom_point(aes(color = paste0(year), shape=limit)) +
  geom_encircle(data = subset(traffic_2, year == 1961), aes(day, y, col=paste0(year))) +
  geom_encircle(data = subset(traffic_2, year == 1962), aes(day, y, col=paste0(year)))
labs(y = "Accidents", x = "Day")

# Difference between limit / not limit?
ggplot(traffic_limit_clean, aes(x = day, y = y)) +
  geom_point(aes(color = paste0(year), shape=limit)) +
  geom_encircle(data = subset(traffic_limit_clean, year == 1961), aes(day, y, col=paste0(year))) +
  geom_encircle(data = subset(traffic_limit_clean, year == 1962), aes(day, y, col=paste0(year)))
labs(y = "Accidents", x = "Day")

ggplot(traffic_no_limit_clean, aes(x = day, y = y)) +
  geom_point(aes(color = paste0(year), shape=limit)) +
  geom_encircle(data = subset(traffic_no_limit_clean, year == 1961), aes(day, y, col=paste0(year))) +
  geom_encircle(data = subset(traffic_no_limit_clean, year == 1962), aes(day, y, col=paste0(year)))
labs(y = "Accidents", x = "Day")
