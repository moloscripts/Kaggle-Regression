library(tidyverse)

# load the datasets
recipies <- read.csv('Data/dayOne/epi_r.csv')
bikes <- read.csv('Data/dayOne/nyc-east-river-bicycle-counts.csv')
weather <- read.csv('Data/dayOne/weatherHistory.csv')

# recipies Data 
# Hypotheses - Can I predict if a recipe is a dessert, based on how many calories it has??
# Remove outliers - recipies with more than 10,000 calories
recipies <- recipies%>%
  filter(calories<10000)%>%
  na.omit()  # remove rows with NA values

# Distribution plots
ggplot(recipies, aes(calories)) + geom_histogram()
ggplot(recipies, aes(calories)) + geom_freqpoly()
ggplot(recipies, aes(dessert)) + geom_freqpoly()

ggplot(recipies, aes(calories, dessert)) + geom_point()

# Since the hypothesis is trying to test whether or not a recipe is a dessert, we'll use Logistic Regression. 
# The function for Logistic Regression is Binomial
# Calories is the dependent variable, dessert is independent variable

# Plot & add the regression line
ggplot(recipies, aes(calories, dessert)) + geom_point() + geom_smooth(method = "glm", method.args = list(family = "binomial"))
# From the regression model,as the number of calories increases, it's more likely that the recipe is a dessert
# The grey area is the standard error, we get less certain when the level of calories increase


# Bikes Data
# How strongly do weather conditions affect bike volumes?
# Could the temparature affect the number of volumes in the bridges?
# A change in temparature could lead to a high or low number of bikes in bridges but the number of bikes cannot change the temparature. 
# temparature is independent (x), number of bikes is dependent (y).  

# fetch out only continiuous variables
# bikes_continuous <- select_if(bikes, is.numeric) 

# High Temp
ggplot(bikes, aes(High.Temp...F., Brooklyn.Bridge)) + geom_point() + geom_smooth(method = "glm", method.args = list(family = "gaussian"))
ggplot(bikes, aes(High.Temp...F., Manhattan.Bridge)) + geom_point() + geom_smooth(method = "glm", method.args = list(family = "gaussian"))
ggplot(bikes, aes(High.Temp...F., Williamsburg.Bridge)) + geom_point() + geom_smooth(method = "glm", method.args = list(family = "gaussian"))
ggplot(bikes, aes(High.Temp...F., Queensboro.Bridge)) + geom_point() + geom_smooth(method = "glm", method.args = list(family = "gaussian"))

# Low Temp
ggplot(bikes, aes(Low.Temp...F., Brooklyn.Bridge)) + geom_point() + geom_smooth(method = "glm", method.args = list(family = "gaussian"))
ggplot(bikes, aes(Low.Temp...F., Manhattan.Bridge)) + geom_point() + geom_smooth(method = "glm", method.args = list(family = "gaussian"))
ggplot(bikes, aes(Low.Temp...F., Williamsburg.Bridge)) + geom_point() + geom_smooth(method = "glm", method.args = list(family = "gaussian"))
ggplot(bikes, aes(Low.Temp...F., Queensboro.Bridge)) + geom_point() + geom_smooth(method = "glm", method.args = list(family = "gaussian"))
