# Fitting a model to data without underlying any assumptions.
# Checking whether the model will be a good representation of our data

library(tidyverse)
library(boot) # For diagnostic plots

# kaggle DataScience Survey is trying to predict salaries of data scientist based on their age
# Intuituion is that older data scientsists have high salaries due to their experience
kaggle <-  read_csv("Data/dayTwo/multipleChoiceResponses.csv")
stackOverflow <- read_csv("Data/dayTwo/survey_results_public.csv")

# look at rows of where people have compensation days of more than 0 units of currency
# Get Salaries that are > 0
# Remove the punctuation marks present in the column
# Convert to Numeric (count)
has_compensation <- kaggle %>%
  filter(CompensationAmount > 0) %>%
  mutate(CleanedCompensationAmount = str_replace_all(CompensationAmount,"[[:punct:]]", " ")) %>%
  mutate(CleanedCompensationAmount = suppressWarnings(as.numeric(CleanedCompensationAmount)))

# Fitting in the model to predict salary age
# Diagnostic plots generated are all plotting residuals
model <- glm(CleanedCompensationAmount ~  Age, data = has_compensation, family = poisson)
glm.diag.plots(model)

# Change the salaries to > 20,000
# residuals are more or less randomly distributed
above_20 <- has_compensation %>%
  filter(CleanedCompensationAmount < 150000)
model <- glm(CleanedCompensationAmount ~ Age, data = above_20, family = poisson)
glm.diag.plots(model)
ggplot(above_20, aes(Age, CleanedCompensationAmount)) + geom_point() + geom_smooth(method = "glm", method.args = list(family = "poisson"))

# StackOverflow Data
# Predict whether people tend to look for new job opportunities because they’re not satisfied with their careers/ jobs. 

stackOverflow <- stackOverflow[!is.na(stackOverflow$CareerSatisfaction), ]
stackOverflow <- stackOverflow[!is.na(stackOverflow$JobSatisfaction), ]

# Career Statisfaction
model <- glm(HoursPerWeek ~ CareerSatisfaction, data = stackOverflow, family = poisson)
glm.diag.plots(model)
ggplot(stackOverflow, aes(CareerSatisfaction, HoursPerWeek)) + geom_point() + geom_smooth(method = "glm", method.args = list(family = "poisson"))

# Job Satisfaction
model <- glm(HoursPerWeek ~ JobSatisfaction, data = stackOverflow, family = poisson)
glm.diag.plots(model)
ggplot(stackOverflow, aes(JobSatisfaction, HoursPerWeek)) + geom_point() + geom_smooth(method = "glm", method.args = list(family = "poisson"))
