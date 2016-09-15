# Example of randomness test for a two level factor
# Load data "AMSsurvey" selecting from Rcmdr menu:
# "Data" -> "Data in packages" -> "Read data set from an attached package..."
# then double-click on "car", click on "AMSsurvey" and on "OK".
# Rcmdr reply with the following command in source pane (R Script)
data(AMSsurvey, package="car")
# To make randomness test on variable "yield", select from Rcmdr menu:
# "Statistics" -> "Nonparametric tests" -> "Randomness test for two level factor..."
# select "sex" and "OK"
# Rcmdr reply with the following command in source pane (R Script)
with(AMSsurvey, twolevelfactor.runs.test(sex))
