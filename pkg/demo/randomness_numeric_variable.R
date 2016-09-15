# Example of randomness test for a numeric variable
# Load data "sweetpotato" selecting from Rcmdr menu:
# "Data" -> "Data in packages" -> "Read data set from an attached package..."
# then double-click on "randtests", click on "sweetpotato" and on "OK".
# Rcmdr reply with the following command in source pane (R Script)
data(sweetpotato, package="randtests")
sweetpotato <- as.data.frame(sweetpotato)
# To make randomness test on variable "yield", select from Rcmdr menu:
# "Statistics" -> "Nonparametric tests" -> "Randomness test for numeric variable..."
# select "yield" and "OK"
# Rcmdr reply with the following command in source pane (R Script)
with(sweetpotato, numeric.runs.test(yield))
