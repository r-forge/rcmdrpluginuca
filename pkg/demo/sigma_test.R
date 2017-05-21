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
