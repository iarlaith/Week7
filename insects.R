#We'll use the InsectSprays built-in R dataset.

InsectSprays

#Optional Test for Normality (there's more than one)
library(lattice)
histogram(~count | spray, data = InsectSprays)

with(InsectSprays, tapply(count, spray, shapiro.test))

#we want to model the means of variable count as a function
#of the varuable spray.
#the formula "count ~ spray" reads as "count A  function of spray"
aov_model <- aov(count ~ spray, data = InsectSprays)
aov_model

summary(aov_model)
# p< provides evidence that the 6 sprays 
# are not all the same.

# model.tables() functions examins the individual levels of 
# factors. Creates 2 tables
model.tables(aov_model, type = 'effects')

#spray E on average had 6 bugs fewer than average over all fields.
#On fields where A was used, farmers found on average 5 bugs more compared to the 
#overall mean.

#Test the pair-wise differences between sprays. Pairwiese comparison tests can be used to 
#determine which group differences are statistically significant.

comparisons <- TukeyHSD(aov_model)
#Model now conatins a list of where each elements is named after one factor in the model.
comparisons$spray['F-C',]

plot(comparisons, las = 1)

