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
transformed_beaver_data

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


# Example of patients involved in car accidents
# whether they wore a seat belt or not
survivors <- matrix(c(1781, 1443, 135, 47), ncol = 2)
colnames(survivors) <- c("survived", "died")
rownames(survivors) <- c("no seat belt", "seat belt")
survivors


result_prop_test <- prop.test(survivors)
result_prop_test
