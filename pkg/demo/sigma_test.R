# Example of test and confidence interval of sigma for one normal population
# Load data "sweetpotato" selecting from Rcmdr menu:
# "Data" -> "Data in packages" -> "Read data set from an attached package..."
# then double-click on "randtests", click on "sweetpotato" and on "OK".
# Rcmdr reply with the following command in source pane (R Script)
data(sweetpotato, package="randtests")
sweetpotato <- as.data.frame(sweetpotato)
# To make test and confidence interval for sigma var from variable "yield", select from Rcmdr menu:
# "Statistics" -> "Nonparametric tests" -> "Single-sample Chi-square-test..."
# select "yield" and "OK"
# Rcmdr reply with the following command in source pane (R Script)
with(sweetpotato, sigma.test(yield))
#	One sample Chi-squared test for variance
#
# data:  yield
# X-squared = 6514.8, df = 69, p-value < 2.2e-16
# alternative hypothesis: true variance is not equal to 1
# 95 percent confidence interval:
#  69.41231 135.93966
# sample estimates:
# var of yield 
#    94.41731 
