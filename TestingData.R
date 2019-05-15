# Example using mobile phone usage while driving

# Comparing 2 samples (with and without phone)
# and comparing mean reaction times f both groups
# so we'll use a paired t-test

install.packages("pwr")
library(pwr)
power_information <- pwr.t.test(d=0.8, sig.level = 0.05, power = 0.90,
                                type = "two.sample", alternative = "two.sided")
power_information

# Results suggest that we need 34 participants in each group (34 with and 34 without phone) to have an
# effect sze of 0.8 with 90% certainty and no more than 5% chance of erronously concluding that a 
# difference exists when it doesn't.

power_information2 <- pwr.t.test(d=0.8, sig.level = 0.01, power = 0.90,
                                type = "two.sample", alternative = "two.sided")
power_information2

# Changing the significant lever to less than 1% means we need 48 people.

plot(power_information2)


# Cohen describes the effect size as "The degree to which the null hypothesis is false".

h_value <- ES.h(p1 = 0.75, p2 = 0.50)
heads_or_tails <- pwr.p.test(h = h_value, sig.level = 0.05, power = 0.90)
heads_or_tails

cohen.ES(test = "r", size = "medium")
cohen.ES(test = "r", size = "large")
cohen.ES(test = "r", size = "small")
