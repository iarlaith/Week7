# Descriptive statistics ----------------------------------------------
# we’ll look at measures of central tendency, variability
# and distribution shape for continuous variables
my_variables <- c("mpg", "hp", "wt")
head(mtcars[my_variables])

# Summary statistics
# provides the minimum, maximum, quartiles, and 
# mean for numerical variables and frequencies 
# for factors and logical vectors
my_variables <- c("mpg", "hp", "wt")
summary(mtcars[my_variables])

# Descriptive statistics via sapply()
# myvars lists our variable of interest from mtcars
my_variables <- c("mpg", "hp", "wt")

# Our function that accepts in data (x)
# and outputs it as mean, no of values, SD
# skew and Kurtosis value
# Use sapply(mtcars[myvars], mystats, na.omit=TRUE)
# if you want to omit missing values

# Note - see notes for explanation on 
# skewness and Kurtosis
my_stats <- function(x, na.omit = FALSE) {
  if (na.omit)
    x <- x[!is.na(x)] # omit missing values
  m <- mean(x)
  n <- length(x) #no of values in x
  s <- sd(x) #SD of all values in each column
  skew <- sum((x - m) ^ 3 / s ^ 3) / n
  kurt <- sum((x - m) ^ 4 / s ^ 4) / n - 3
  return(c(n = n, mean = m, stdev = s, skew = skew, kurtosis = kurt))
}


# sapply(x, FUN, options) where x is the 
# data frame (or matrix) and FUN is an arbitrary function
head(mtcars)
sapply(mtcars[my_variables], my_stats)

# Results show mean mpg is 20.1, with a SD of 6.0. 
# The distribution is skewed to the right(+0.61) 
# and is somewhat flatter than a normal distribution (–0.37).
# See notes for details on skew and Kurtosis

# Show mpg graphically
d <- density(mtcars$mpg)
plot(d, main = "Chart showing MPG ")
# Show mean mpg
abline(v = 20, lty = 2, col = "blue")


install.packages("Hmisc") # note it is case sensitive
library(Hmisc)
my_variables <- c("mpg", "hp", "wt")
# describe() function in the Hmisc package returns 
# the number of variables and observations
# the number of missing and unique values
# the mean, quantiles, and the five highest and lowest values.
describe(mtcars[my_variables])
is.na(mtcars[my_variables])


# pastecs package includes a function 
# named stat.desc() that provides a wide
# range of descriptive statistics.
install.packages("pastecs")
library(pastecs)
my_variables <- c("mpg", "hp", "wt")
# the number of values, null values, missing values, 
# minimum, maximum, range, and sum are provided

# also show skewness, Kurtosis, Shapiro-Wilk test
# of normality
stat.desc(mtcars[my_variables], norm = TRUE)
# See help for more info



# the psych package also has a function called describe() that
# provides the number of nonmissing observations, mean, sd, median,
# trimmed mean, median absolute deviation (mad), minimum, maximum, 
# range, skew, kurtosis, and standard error of the mean

install.packages("psych")
library(psych)
my_variables <- c("mpg", "hp", "wt")
describe(mtcars[my_variables])

# Descriptive statistics by group using aggregate() ---------------
# When comparing groups of individuals or observations
# the focus is usually on the descriptive statistics of each group
# rather than the total sample

my_variables <- c("mpg", "hp", "wt")
# Note the use of list(am=mtcars$am)
# If you used list(mtcars$am), the am column
# would be labeled Group.1 rather than am.
aggregate(mtcars[my_variables], by = list(am = mtcars$am), mean)

# Aggregate won’t return several statistics at once
# single-value functions only eg mean, sd
# We use the by() function for several outputs
# using a function that operates on all columns of
# a data frame
dstats <- function(x) sapply(x, my_stats)
my_variables <- c("mpg", "hp", "wt")
# Here we're applying the mystats function
# to each column of the data frame.
# Placing it in the by() function gives 
# you summary statistics for each level of am.
# Remember - am  = 0 for automatic
# or 1 for manual transmission
by(mtcars[my_variables], mtcars$am, dstats)

# doBy - -------------------------------------------

# doBy provide functions for descriptive statistics by group
install.packages("doBy")
library(doBy)
# Variables on the left of the ~ are the numeric variables 
# to be analysed, and variables on the right are categorical 
# grouping variables
# Uses the mystats function defined earlier
summaryBy(mpg + hp + wt ~ am, data = mtcars, FUN = my_stats)

# frequency and contingency tables from categorical variables

# Contingency tables tell you the frequency or proportions
# of cases for each combination of the variables that make up the table

# describeBy() - ---------------------------------------
library(psych)
my_variables <- c("mpg", "hp", "wt")
# DescribeBy() doesn’t allow you to specify
# an arbitrary function, so it ’s less generally applicable
describeBy(mtcars[my_variables], list(am = mtcars$am))

# Frequency and contingency tables ---------------------
library(vcd)
# Show top of structure
head(Arthritis)
# Treatment, sex, and improved are categorical factors
str(Arthritis)

# one-way table ----------------------------------------
# simple frequency counts using the table() function
mytable <- with(Arthritis, table(Improved))
mytable

# Turn these frequencies into proportions
# Expresses table entries as fractions of the 
# marginal table defined by the margins
# In this example we dont have a representation for margins
prop.table(mytable)

# Expressed as percentages
# 50% of study participants had some or marked improvement
prop.table(mytable) * 100

# two-way tables -------------------------------------------
# use syntax mytable <- table(A, B)
# where A is the row variable and B is the column variable

# Alternatively use xtabs() ---------------------------------
# the variables to be cross-classified appear on the right 
# of the formula (that is, to the right of the ~) separated by + signs
# If variable is included on the left side of the formula, 
# it’s assumed to be a vector of frequencies 
# (useful if the data have already been tabulated).
mytable <- xtabs(~Treatment + Improved, data = Arthritis)
mytable

# generate marginal frequencies
# for mytable
# The index (1) refers to the first variable 
# in the table() statement
# This means proportional table by index (1)
# examines proportions by treatment
# and index (2) examines improved proportions
margin.table(mytable, 1)

# Looking at the table, we can see that 51% of treated 
# individuals had marked improvement, compared to 16% 
# of those receiving a placebo.
prop.table(mytable, 1)

# For column sums and proportions we use the same
# commands but examine the second variable in
# the table() statement
# Note - 1st variable = rows, 2nd variable = columns
# Here, the index (2) refers to the second 
# variable in the table() statement.
margin.table(mytable, 2)
prop.table(mytable, 2)
# Cell proportions obtained using this statement
prop.table(mytable)

# addmargins() function to add marginal sums 
# to these tables. For example, the following 
# code adds a Sum row and column:
addmargins(mytable)

# Add marginal sums to proportion table
addmargins(prop.table(mytable))
mytable
# default is to create sum margins for all 
# variables in a table. 
# The following code adds a Sum column alone
addmargins(prop.table(mytable, 1), 2)
# Similarly, this code adds a Sum row:
addmargins(prop.table(mytable, 2), 1)
# 25% of those patients with marked improvement received a placebo


# CrossTable() function -------------------------

install.packages("gmodels")
library(gmodels)
# options to report percentages (row, column, and cell) 
# specify decimal places; produce chi-square, Fisher, 
# and McNemar tests of independence; report expected 
# and residual values (Pearson, standardised, and adjusted 
# standardised) include missing values as valid; annotate 
# with row and column titles; and format as SAS or SPSS style output
help(CrossTable)
CrossTable(Arthritis$Treatment, Arthritis$Improved)

# Multidimensional tables ---------------------------------------
# This code produces cell frequencies for the three-way 
# classification. 
# Treatment and Sex are shown in 2 dimensional table
# for each state of Improved
mytable <- xtabs(~Treatment + Sex + Improved, data = Arthritis)
mytable
help(xtabs)

# This code demonstrates how the ftable()
# function can be used to print a more compact
# version of the table.
ftable(mytable)

# The code in this section produces the marginal frequencies for Treatment, Sex, and
# Improved. Because you created the table with the formula ~ Treatment + Sex +
# Improved, Treatment is referred to by index 1
# Sex is referred to by index 2, and Improved is referred to by index 3.

# Marginal frequencies by treatment
margin.table(mytable, 1)

# marginal frequencies by sex
margin.table(mytable, 2)

# marginal frequencies by Improved
margin.table(mytable, 3)

# This code produces the marginal frequencies 
# for the Treatment x Improved
# classification, summed over Sex.
# See earlier ftable() output for 
# explanation of results
margin.table(mytable, c(1, 3))

# The proportion of patients with None, Some, and
# Marked improvement for each Treatment × Sex 
# combination is provided
ftable(prop.table(mytable, c(1, 2)))

# 36% of treated males had marked improvement, compared to 59% of
# treated females. In general, the proportions will add to 1 over the indices not
# included in the prop.table() call(the third index, or Improved in this case) . You can
# see this in the last example, where you add a sum margin over the third index (Improved)
ftable(addmargins(prop.table(mytable, c(1, 2)), 3))

# percentages instead of proportions, you can multiply the resulting
# table by 100. For example, this statement
ftable(addmargins(prop.table(mytable, c(1, 2)), 3)) * 100
