# Import libraries
library(tidyr)
library(dplyr)
library(ggplot2)
library(cluster)

# Read joined and cleaned data into R
df <- read.csv("data/cleaned_01.csv")

# Get basic info about the data
glimpse(df)

# Check for missing values.
df %>%
  summarise(across(everything(), ~ sum(is.na(.x))))

## Single Variable Plots
# Create a bar plot of the count of rows for each neighborhood value
# sorted by count and color coded.
df %>%
  count(neighborhood) %>%
  arrange(desc(n)) %>%
  ggplot(aes(x = neighborhood, y = n, fill = neighborhood)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Neighborhood", y = "Count")

# Create table of the count of rows for each neighborhood value sorted by count.
df %>%
  count(neighborhood) %>%
  arrange(desc(n))

# Create a boxplot of the soldprice variable by neighborhood and color coded.
df %>%
  ggplot(aes(x = neighborhood, y = soldprice, fill = neighborhood)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Neighborhood", y = "Sold Price")

# Create table of the mean soldprice for each neighborhood value sorted by mean.
df %>%
  group_by(neighborhood) %>%
  summarise(mean_soldprice = mean(soldprice)) %>%
  arrange(desc(mean_soldprice))

# Create a histogram showing the year variable distribution by neighborhood
# and color coded.
df %>%
  ggplot(aes(x = year, fill = neighborhood)) +
  geom_histogram(bins = 20) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Year", y = "Count")

# Create a table showing the min, max, and mean year for each neighborhood
# value sorted by mean.
df %>%
  group_by(neighborhood) %>%
  summarise(min_year = min(year),
            max_year = max(year),
            mean_year = mean(year)) %>%
  arrange(desc(mean_year))

## Multiple Variable Plots
# Create a scatter plot of the soldprice and average of the school ratings
# variables (elementary_rating, middle_rating, and high_rating), color
# coded by neighborhood.
df %>%
  mutate(
    school_rating = (elementary_rating + middle_rating + high_rating) / 3) %>%
  ggplot(aes(x = school_rating, y = soldprice, color = neighborhood)) +
  geom_point(alpha = 0.7) +
  labs(x = "Sold Price", y = "School Rating")

# Create a table showing the average school rating and sold price for each
# neighborhood sorted by average school rating.
df %>%
  mutate(
    school_rating = (elementary_rating + middle_rating + high_rating) / 3) %>%
  group_by(neighborhood) %>%
  summarise(mean_school_rating = mean(school_rating),
            mean_soldprice = mean(soldprice)) %>%
  arrange(desc(mean_school_rating))

# Create a 2d density plot of the year constructed and sold price
# by neighborhood.
df %>%
  ggplot(aes(x = year, y = soldprice)) +
  geom_density2d_filled(alpha = 0.5) +
  geom_density_2d(colour = "black", lwd = 0.5, alpha = 0.7) +
  labs(x = "Year", y = "Sold Price")

## Regression Analysis
# Create and plot a linear regression model of the sold price and the
# average of the school ratings variables (elementary_rating, middle_rating,
# and high_rating).
m <- df %>%
  mutate(
    school_rating = (elementary_rating + middle_rating + high_rating) / 3) %>%
  lm(soldprice ~ school_rating + sqft,.)

# Print model summary
summary(m)

# Get Resisuals
r <- residuals(m)

# Plot Residuals
plot(fitted(m), r, pch = 20, xlab = "Sold Price", ylab = "Residuals")
abline(0, 0)
lines(lowess(fitted(m), r), col = "red")

# Create a KMeans Clustering Model of the sold price
# and the average of the school ratings variables
# (elementary_rating, middle_rating, and high_rating).
k <- df %>%
  select(soldprice) %>%
  kmeans(.,3)

df %>%
  mutate(
    school_rating = (elementary_rating + middle_rating + high_rating) / 3) %>%
  select(where(is.numeric)) %>%
  select(soldprice, year, sqft, school_rating) %>%
  plot(df, col = k$cluster)
  points(k$centers, pch = 19)

## Sensitivity Analysis ## 
# Read joined, cleaned and imputed data into R and rerun analysis to check
# for changes.
df <- read.csv("data/cleaned_02.csv")

# Get basic info about the data
glimpse(df)

# Check for missing values.
df %>%
  summarise(across(everything(), ~ sum(is.na(.x))))

## Single Variable Plots
# Create a bar plot of the count of rows for each neighborhood value
# sorted by count and color coded.
df %>%
  count(neighborhood) %>%
  arrange(desc(n)) %>%
  ggplot(aes(x = neighborhood, y = n, fill = neighborhood)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Neighborhood", y = "Count")

# Create table of the count of rows for each neighborhood value sorted by count.
df %>%
  count(neighborhood) %>%
  arrange(desc(n))

# Create a boxplot of the soldprice variable by neighborhood and color coded.
df %>%
  ggplot(aes(x = neighborhood, y = soldprice, fill = neighborhood)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Neighborhood", y = "Sold Price")

# Create table of the mean soldprice for each neighborhood value sorted by mean.
df %>%
  group_by(neighborhood) %>%
  summarise(mean_soldprice = mean(soldprice)) %>%
  arrange(desc(mean_soldprice))

# Create a histogram showing the year variable distribution by neighborhood
# and color coded.
df %>%
  ggplot(aes(x = year, fill = neighborhood)) +
  geom_histogram(bins = 20) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Year", y = "Count")

# Create a table showing the min, max, and mean year for each neighborhood
# value sorted by mean.
df %>%
  group_by(neighborhood) %>%
  summarise(min_year = min(year),
            max_year = max(year),
            mean_year = mean(year)) %>%
  arrange(desc(mean_year))

## Multiple Variable Plots
# Create a scatter plot of the soldprice and average of the school ratings
# variables (elementary_rating, middle_rating, and high_rating), color
# coded by neighborhood.
df %>%
  mutate(
    school_rating = (elementary_rating + middle_rating + high_rating) / 3) %>%
  ggplot(aes(x = school_rating, y = soldprice, color = neighborhood)) +
  geom_point(alpha = 0.7) +
  labs(x = "Sold Price", y = "School Rating")

# Create a table showing the average school rating and sold price for each
#neighborhood sorted by average school rating.
df %>%
  mutate(
    school_rating = (elementary_rating + middle_rating + high_rating) / 3) %>%
  group_by(neighborhood) %>%
  summarise(mean_school_rating = mean(school_rating),
            mean_soldprice = mean(soldprice)) %>%
  arrange(desc(mean_school_rating))

# Create a 2d density plot of the year constructed and sold price
# by neighborhood.
df %>%
  ggplot(aes(x = year, y = soldprice)) +
  geom_density2d_filled(alpha = 0.5) +
  geom_density_2d(colour = "black", lwd = 0.5, alpha = 0.7) +
  labs(x = "Year", y = "Sold Price")

## Regression Analysis
# Create and plot a linear regression model of the sold price and the
# average of the school ratings variables (elementary_rating, middle_rating,
# and high_rating).
m <- df %>%
  mutate(
    school_rating = (elementary_rating + middle_rating + high_rating) / 3) %>%
  lm(soldprice ~ school_rating + sqft,.)

# Print model summary
summary(m)

# Get Resisuals
r <- residuals(m)

# Plot Residuals
plot(fitted(m), r, pch = 20, xlab = "Sold Price", ylab = "Residuals")
abline(0, 0)
lines(lowess(fitted(m), r), col = "red")

# Create a KMeans Clustering Model of the sold price
# and the average of the school ratings variables
# (elementary_rating, middle_rating, and high_rating).
k <- df %>%
  select(soldprice) %>%
  kmeans(.,3)

df %>%
  mutate(
    school_rating = (elementary_rating + middle_rating + high_rating) / 3) %>%
  select(where(is.numeric)) %>%
  select(soldprice, year, sqft, school_rating) %>%
  plot(df, col = k$cluster)
points(k$centers, pch = 19)