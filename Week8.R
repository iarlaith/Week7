install.packages('multcomp')
library(multcomp)
str(cholesterol)
attach(cholesterol)
aov_model <- aov(response ~ trt)
summary(aov_model)
#p value less than 0.001 proves that the 5 treatments aren't all equally effective
detach(cholesterol)

install.packages('gplots')
library(gplots)
attach(cholesterol)
plotmeans(response ~ trt, xlab = "Treatment",
         ylab = "Response",
         main = "Mean Plot\nwith 95% confidence interval")

# Let's examine the output from the TukeyHSD() function for pairwise differences
# between group means. 
TukeyHSD(aov_model)
# The mean cholesterol reductions for 1 time and 2 times aren't significantlf difference from one acnother (P = 0.138)
# whereas the difference between 1 time and 4 times is signiicantly different (P<0.0001).
par(mar=c(10,10,5,5))
plot(TukeyHSD(aov_model), las = 2)

library(car)
qqPlot(lm(response ~ trt, data = cholesterol), simulate = TRUE,
     main = "Q-Q Plot", labels = FALSE)
# dotted line = 95% confidence boundary, suggesting that the normality 
# assumption hs been met. 

# Anova assumps that variances are equal accross groups or samples 
# The Bartlet test can verify this assumption.
bartlett.test(response ~ trt, data = cholesterol)

#anova is sensitive to outliers
outlierTest(aov_model)
