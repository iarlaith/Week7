# Using statistical methods to examine
# the relationships between variables of interest

?beavers
str(beaver2)
# The beaver dataset contains data on body temp of 4 beavers
# every 10 mins over a day for demo purposes 
# We want to examine the difference in average body temp
# during periods of activity to evaluate whether
# body temperature is affected by activity
# First we need to ensure that data is in correct format
# Activ should be a factor
# Temp is numerical

# use the transform function to
# change activ to a factor

transformed_beaver_data <- transform(beaver2,
                                     activ = factor(activ, labels = c("no", "yes")))

# Selecting the appropriate test
# We need to check whether the data is normally distributed or not
# See notes for more information

library("lattice")
# The histogram uses a 1 sided formula, so we
# dont specify anything on left side of ~
# and on right side we specify which variable is in the histogram
# ie temp.
# After the vertical line we show the factor by which the data
# is split ie "activ"
histogram(~temp | activ, data = transformed_beaver_data)

# Quantile-quantile plot allows us to 
# data is distributed normally
# Compare the quantiles of both samples 
# We use square brackets to select the cases we want

with(transformed_beaver_data,
     qqplot(temp[activ == "yes"],
            temp[activ == "no"], 
            main = "Comparing 2 samples", 
            xlab = "Active temp = yes",
            ylab =  "Active temp = no"))

# Using a QQ plot to check for normality
# qqnorm function plots your sample 
# against a normal distribution
with(transformed_beaver_data, {
  qqnorm(temp[activ == "no"], 
         main = "Inactive")
})

# We can add normailty line 
# to the plot to evaluate normality
# for active period = no
with(transformed_beaver_data, {
  qqnorm(temp[activ == "no"], 
         main = "Inactive")
  qqline(temp[activ == "no"])
})

# And we can change for active period
# = "yes"
with(transformed_beaver_data, {
  qqnorm(temp[activ == "yes"], 
         main = "Active")
  qqline(temp[activ == "yes"])
})

# Formal test of normality
# provided through widely used Shapiro-Wilks test
normality_test <- shapiro.test(transform.beaver$temp)
normality_test$p.value
# p-value tells us the chances that the sample comes 
# from a normal distribution 
# In this example, p-value is clearly lower than 0.05
# so not normally distributed

# We can check the normality in each variable
# using the tapply() function
with(transformed_beaver_data, tapply(temp, activ, shapiro.test))

# Comparing 2 samples - most widely used test
# eg comparing mileage in cars with manual and auto gearboxes
# R provides 2 tests for comparing numerical data
# the t-test and the Wilcoxon test
# Wilcoxon test does not require normally distributed data

# Carrying out a t-test
# Normally we can only carry out t-test on samples where variances
# are equal. Applying Welch variation allows for unequal variances

# In this test we are evaluating temp within groups determined by activ
t.test(temp ~ activ, data = transformed_beaver_data)

# t = test statistic
# df = degrees of freedom
# p = p value. Small p = means of both samples differ significantly
# Alternative hypothesis = what you can conclude if the p-value 
# is lower than the limit for significance (<0.05)
# This shows us that the true mean of the difference is not 0 
# ie that we reject the null hypothesis
# 95 percent confidence interval contains the difference between the means
# with 95% probability, in this case the difference between the means lies probably
# between 0.72 and 0.89

# We can also use two separate vectors for the samples you want to compare
# and pass both to the function
with(transformed_beaver_data,
     t.test(temp[activ == "yes"],
            temp[activ == "no"]))

# We can use the Wilcox.text() function 
# for data that deviates from normality
# In this test we get the test statistic (W)
# as well as the p value
# This test examines whether the centre of the data
# differs for both samples
wilcox.test(temp ~ activ, data = transformed_beaver_data)
# This test is the same as the Mann-Whitney U test
# so R doent have a separate test for Mann-Whitney

t.test(extra ~ group, data = sleep, paired = TRUE)

# Comparing paired data
# Paired data occurs when 2 different treatments are
# given to the same subjects

# Sleep dataset contains data from 10 participants
# who are given 2 types of sleep medicine
# Researchers record difference in sleep for
# each person with and without drugs
str(sleep)
# Extra = extra hours of sleep after medication
# Group = which variant each participant took
# Id = participant ID

# Each person gets both variants - data is therefore paired
# We want to know if both types of sleep medicine had an effect
# on length of sleep

# t test and wilcox test have paired argument
t.test(extra ~ group, data = sleep, paired = TRUE)
# We only get 1 mean instead of 2 this time

# Testing counts and proportions
# Counts are summarised in tables

# Example of patients involved in car accidents
# whether they wore a seat belt or not
survivors <- matrix(c(1781, 1443, 135, 47), ncol = 2)
colnames(survivors) <- c("survived", "died")
rownames(survivors) <- c("no seat belt", "seat belt")
survivors

# A proportion test can examine the probability
# that both proportions are the same
result_prop_test <- prop.test(survivors)
result_prop_test
# Results are almost identical to t.test function
# p value tells us how likely it is that both 
# proportions are equal.
# p = 0.0000008105 - null hypothesis not true
# therefore dying in a hospital after a crash is lower 
# if you’re wearing a seat belt at the time of the crash

# We can use the prop.test() function with 2 columns
# Alternatively we can use the chisq.test()function 
# to examine this same seat belt data
chisq.test(survivors)
# Results from both tests are the same - as expected

# These tests can also be used with more than 2 columns
# We'll use HairEyeColor to proove this

str(HairEyeColor)
class(HairEyeColor)
# Contains 3 dimensions - hair, eye colour, and sex
# We can use dimnames to collapse to extract dimension names
# See help for more details
dimnames(HairEyeColor)

# Check if hair colour and eye colour are related
# we have to collapse the table first using the margin.table() function
# The margin argumant is an index number, so
# we're building the table firstly by hair and then organise
# data by eye colour
hair_eye_margin <- margin.table(HairEyeColor, margin = c(1, 2))
hair_eye_margin

# Once this is built then we can simply check
# whether hair and eye colour are related
chisq.test(hair_eye_margin)
# The p-value tells us some combinations of hair and eye color are more
# common than others.