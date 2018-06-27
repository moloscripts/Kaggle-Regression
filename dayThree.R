# Intepreting the models. Also, determining if the input(s) do have strong relationship with the output. 
# Coefficient -  Shows the strength of the relationship between input(s) and output values

library(tidyverse)
library(boot)
hard_drives <- read_csv("Data/dayThree/harddrive.csv", n_max = 100000)

# Hard Drives
# Could we predict how likely a hard drive will fail?
# In the column failure: 1 = hard drive failed, 0 = hard drive didn't fail
model <- glm(failure ~ smart_1_normalized, data = hard_drives, family = "binomial")
glm.diag.plots(model)
summary(model)
ggplot(hard_drives, aes(smart_1_normalized, failure)) + geom_point() + geom_smooth(method = "glm", method.args = list(family = "binomial"))

#cameras dataset
