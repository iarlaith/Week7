# demo uses the data set "women" containing height and weight of 15 women

simple_linear_model <- lm(weight ~ height, data = women)
simple_linear_model
# weight = -87.52 + 3.45 X height

plot(women$height, women$weight, 
     xlab = "Height (inches)", 
     ylab = "Weight (pounds)", 
     main = "Scatter Plot showing regression line for weight predicted from the height")

abline(simple_linear_model)

summary(simple_linear_model)

# residuals - provide a quick view of the distribution of the residuals, which by definition
# have a mean zero. Therefore the median should not be far from zero, and the min and max
# should be roughly equal in absolute value.
# Residual Standard Error (RSE), R^2 and F-Statistic are metrics that are used to determine 
# how well the model fits the data.
# The Standard Error (SE) finds the accuracy of the beta coefficients. For a given beta
# coefficient, the SE reflects how the coefficient varies under the repeat sampling.
# It can be used to compute the CI and T-Statistic.

# The correlation coefficient measures the leel of association between 2 variables and ranges
# from -1 (perfect neg. correlation) to +1 (perfectly positively correlated). A value close to
# zero is a weak relationship. A low correlation is (-0.2 < x < 0.2) and idicates a lot of the 
# variation of the outcome (y) against predictor (x) is unexplained and we should then look 
# for better predictor variables.

cor(women$height, women$weight)
#very strong correlation

confint(simple_linear_model)


###################################################################################################
###################################################################################################

install.packages("CARS")
library(cars)
head(cars)
scatter.smooth(x = cars$speed, y = cars$dist,
               main = "Distance ~ Speed",
               xlab = "Car Speed",
               ylab = "Stopping Distance")

# Boxplots will show outliers in the data
par(mfrow = c(1,2)) # divide graph area into 2 columns
boxplot(x = cars$speed, main = "Speed", 
        sub = paste("Outlier rows : ", boxplot.stats(cars$speed)$out))

boxplot(x = cars$dist, main = "Distance", 
        sub = paste("Outlier rows : ", boxplot.stats(cars$dist)$out))

# Skewness fuction to examine the normaility
install.packages("e1071")
library(e1071)
# Density plot for speed 
# Skewness < -1 or > 1 = highly skewed
# -1 to -.05 and 0.5 to 1 = moderate skewness
# -0.5 to 0.5 = approximately symmetric

plot(density(cars$speed), main = "Density Plot: Speed",
     ylab = "Frequency",
     sub = paste("Skewness : ", round(e1071::skewness(cars$speed), 2)))

# Fill the area within the density plot to red
polygon(density(cars$speed), col = "red")

plot(density(cars$dist), main = "Density Plot: Dist",
     ylab = "Frequency",
     sub = paste("Skewness : ", round(e1071::skewness(cars$dist), 2)),)
polygon(density(cars$dist), col = "blue")

cor(cars$speed, cars$dist)

# Built the model on full data
linear_model <- lm(dist ~ speed, data = cars)
linear_model
summary(linear_model)

set.seed(200)
# this is so we all get the same values when we randomize

# Choose a random sample from 1:all records in cars DS, 80% of rows.
random_sample <- sample(1:nrow(cars), 0.8 * nrow(cars))

#model trainign data
training_data <- cars[random_sample,]

#model testing data
testing_data <- cars[-random_sample,]

nrow(cars)
nrow(training_data)
nrow(testing_data)
head(cars)

# Built the model on the training data 
lr_model <- lm(dist ~ speed, data = training_data)
lr_model
summary(lr_model)

distance_predicted <- predict(lr_model, testing_data)
distance_predicted

actuals_predicted <- data.frame(cbind(actuals = testing_data$dist, 
                                      predicted = distance_predicted ))
actuals_predicted

correlation_accuracy <- cor(actuals_predicted)
correlation_accuracy

# min - max accutracy 
min_max_accuracy <- mean(apply(actuals_predicted, 1, min) / apply(actuals_predicted, 1, max))

# MAPE
mape <- mean(abs((actuals_predicted$predicted - actuals_predicted$actuals)) /
               actuals_predicted$actuals)

min_max_accuracy
mape

################################## Multiple Linear Regression ########################################################

# Use the state.x77 dataset to explore the relationships between murder rate and other characteristics including
# populaion, illeteracy, income and frost (weather). The first step in multiple regression is to examine the 
# relationships among the variables 2 at a time. 

# The scatterplot matrix uses the car package.

states <- as.data.frame((state.x77[,c("Murder", "Populaton", "Illiteracy", "Income", "Frost")]))
car(states)
library(car)
scatterplotMatrix(states,
                  spread = FALSE,
                  smoother.args = list(lty = 2),
                  )