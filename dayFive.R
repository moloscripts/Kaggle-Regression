# Using Elastic Net to help us select the correct input variables for multilpe regression

library(tidyverse)
library(glmnet) # implementation of Elastic Net for glms. 
library(car) # AV plots

coders_2016 <- read.csv("Data/dayFive/2016-FCC-New-Coders-Survey-Data.csv")

# create a subset of 2016 data with only the variables of interest. Both inputs & output 
dataSubset <- coders_2016 %>%
  mutate(IsWoman = (Gender == 'female')) %>% # In the column Gender, IsWoman = 1 else, 0. Boolean, if TRUE = 1, else FALSE which is 0.
  select(Age, CommuteTime, HasChildren, AttendedBootcamp, IsWoman, HasDebt, HoursLearning, MonthsProgramming, Income) %>%
  na.omit() # remove non-numeric values

# glmnet receives inputs inform of a matrix, therefore, we'll have to convert all inputs to numeric 
# convert the selected input variables to a matrix
input <- dataSubset %>%
  select(-Income) %>% # Select all variables but the Income. Because Income is the output we're trying to predict.
  as.matrix()

# output as a vector
output <- dataSubset$Income
length(output) # count of the data points

print("Mean of our predicted value")
mean(output)
# When the mean is very far from 0, gaussian & poisson distribution tend to converge.

# In this cases, for cross-validation we'll taking 10 diff sub-samples, train a model on each of the sub-sample then average all those models to get our final model
# cv = cross validation
cv_fit <- cv.glmnet(input, output, family = "gaussian")

# get coefficients & the intercept for the best model
coef(cv_fit, s = "lambda.min")
# The code above generates a sparse matrix
# All the variables but HasDebt are useful in predicting the Income. Coefficient of HasDebt was pushed to is 0. In the console, 0 is represented by a dot. (.)

# convert the coefficients to a non-sparse matrix
# fetch the variables that != 0
coef_matrix <- coefficients(cv_fit, s = "lambda.min") %>%
  as.matrix()
input_variables <- row.names(coef_matrix)[coef_matrix != 0] %>% # Get the row names of the matrix with non-zero's
  setdiff("(Intercept)") # Remove the Intercept, if present

new_variables <- paste(input_variables, collapse = "+") # 'Joining all the variables with a "+"
formular <- paste("Income ~ ", new_variables, sep = " ")
income2016_model <- glm(formular, data = dataSubset, family = ("gaussian"))

# diagonstic plots
graphics.off() 
par(mfrow = c(2,2))
par("mar") 
par(mar=c(1,1,1,1))
plot(income2016_model)

summary(income2016_model)
# from the summary, most important value is HasChildren because of the estimate is quite big and also has a small std. error. Also MonthsProgramming
# the higher the estimate, the higher the input value has in influencing the prediction
avPlots(income2016_model)

  
  
  

