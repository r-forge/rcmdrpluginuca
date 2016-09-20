\name{RcmdrPlugin.UCA-package}
\title{RcmdrPlugin.UCA: Some customization to Rcmdr made by R-UCA project}
\alias{RcmdrPlugin.UCA-package}
\alias{RcmdrPlugin.UCA}
\docType{package}
\encoding{UTF-8}
\description{
  Some extension to Rcmdr (R Commander), randomness test and predictions using active model, to teach statistics in a first
  university course made by R-UCA project and used at University of
  Cadiz (UCA).

  All the customizations provides by this package will be propose to be
  part of the Rcmdr package. 
  All the customization included in the Rcmdr package will be dropped from here.  
}
\details{
\tabular{ll}{
Package: \tab RcmdrPlugin.UCA\cr
Type: \tab Package\cr
Version: \tab 3.1-2\cr
Date: \tab 2016-09-20\cr
License: \tab GPL version 3 or newer\cr
}
}

\author{
Manuel Munoz-Marquez <manuel.munoz@uca.es>
}

\keyword{package}
\seealso{
For more information see \code{\link[Rcmdr]{Rcmdr-package}}. 

Para ayuda en \enc{español}{espanol}, \enc{véase}{vease}
\link{RcmdrPlugin.UCA.es}. (For Spanish help see \link{RcmdrPlugin.UCA.es}.) 
}

\examples{
# Example of randomness test for a two level factor
# Load data "AMSsurvey" selecting from Rcmdr menu:
# "Data" -> "Data in packages" -> "Read data set from an attached package..."
# then double-click on "car", click on "AMSsurvey" and on "OK".
# Rcmdr reply with the following command in source pane (R Script)
### data(AMSsurvey, package="car")
# To make randomness test on variable "yield", select from Rcmdr menu:
# "Statistics" -> "Nonparametric tests" -> "Randomness test for two level factor..."
# select "sex" and "OK"
# Rcmdr reply with the following command in source pane (R Script)
### with(AMSsurvey, twolevelfactor.runs.test(sex))
#
#
# Example of randomness test for a numeric variable
# Load data "sweetpotato" selecting from Rcmdr menu:
# "Data" -> "Data in packages" -> "Read data set from an attached package..."
# then double-click on "randtests", click on "sweetpotato" and on "OK".
# Rcmdr reply with the following command in source pane (R Script)
### data(sweetpotato, package="randtests")
### sweetpotato <- as.data.frame(sweetpotato)
# To make randomness test on variable "yield", select from Rcmdr menu:
# "Statistics" -> "Nonparametric tests" -> "Randomness test for numeric variable..."
# select "yield" and "OK"
# Rcmdr reply with the following command in source pane (R Script)
### with(sweetpotato, numeric.runs.test(yield))
#
#
# Example of "Input data and predict"
# Load data "Chile" selecting from Rcmdr menu:
# "Data" -> "Data in packages" -> "Read data set from an attached package..."
# then double-click on "car", click on "Chile" and on "OK".
# Rcmdr reply with the following command in source pane (R Script)
### data(Chile, package="car")
# To build a model select from Rcmdr menu:
# "Statistics" -> "Model fit" -> "Linear Regresion..."
# As "Response variable" select income and age as "Explanatory variables" and click on "OK"
# Rcmdr reply with the following command in source pane (R Script)
### RegModel.1 <- lm(income~age, data=Chile)
### summary(RegModel.1)
# Note that the active model is set to RegModel.1
# So if you want to predict a new value for a 35 and 40 age person
# Select from Rcmdr menu:
# "Models" -> "Predict using active model" -> "Input data and predict"
# In age column input 35 and 40 and then close the editor.
# Rcmdr reply with the following command in source pane (R Script)
### .data <- edit(Chile[0,])
### .data
### predict(RegModel.1, .data)
### remove(.data)
# And output the predicted value of income for that age using active model (RegModel.1)
#
#
# Example of "Add predictions to existing dataset..."
# Load data "Chile" selecting from Rcmdr menu:
# "Data" -> "Data in packages" -> "Read data set from an attached package..."
# then double-click on "car", click on "Chile" and "OK".
# Rcmdr reply with the following command in source pane (R Script)
### data(Chile, package="car")
# To build a model select from Rcmdr menu:
# "Statistics" -> "Model fit" -> "Linear Regresion..."
# As "Variable explicada" select income and age as "Variables explicativas" and click on "OK"
# Rcmdr reply with the following command in source pane (R Script)
### RegModel.1 <- lm(income~age, data=Chile)
### summary(RegModel.1)
# Note that the active model is set to RegModel.1
# So if you want to predict the values for income for age data in Chile dataset
# Select from Rcmdr menu:
# "Models" -> "Predict using active model" -> "Add predictions to existing dataset..."
# In the dialog select a compatible dataset with the model
# In this case select Chile
# Rcmdr reply with the following command in source pane (R Script)
### Chile$fitted <- predict(RegModel.1, Chile)
# The predicted value of income has been saved as fitted in the selected dataset (Chile)
}

\references{
[1] \url{http://knuth.uca.es/RcmdrPlugin.UCA}
}

