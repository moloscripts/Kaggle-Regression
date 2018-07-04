# Multiple Regression

library(tidyverse)
library(car)

bmi_data <- read.csv("Data/dayFour/ehresp_2014.csv") %>%
  filter(erbmi > 0) # removing errors of the BMI, jus incase. You can never have a BMI of less than 0
# nyc_cenus <- read.csv("Data/dayFour/nyc_census_tracts.csv")


# bmi
bmi_model <- glm(erbmi ~ euexfreq + euwgt + euhgt + ertpreat, data = bmi_data, family = "gaussian")

# plot the model in a 2*2 grid using baseR plot
par(mfrow = c(2,2))
plot(bmi_model)
summary(bmi_model)

# To double check the summary stats of the bmi_model, we'll use partial-regression plots AKA Added Variable (AV) plots
# AV plots Displays the relationship of the output when all other inputs but ONE are on hold. 
# i.e. Changing only one of the inputs and the others being stable. 
# In AV plots, the coefficient is shown using a blue-line. More or less the same as correlation plots

avPlots(bmi_model)
# euwgt show a positive estimate whereas euhgt shows a negative estimate. 







