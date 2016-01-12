\name{RcmdrPlugin.UCA-package}
\title{RcmdrPlugin.UCA: Some customization to Rcmdr made by R-UCA project}
\alias{RcmdrPlugin.UCA-package}
\alias{RcmdrPlugin.UCA}
\docType{package}
\encoding{UTF-8}
\description{
  Some extension to Rcmdr (R Commander) made by R-UCA project and used in teaching statistics at University of Cadiz (UCA).

  All the customizations provides by this package will be propouse to be
  part of the Rcmdr package. All the customization included in the Rcmdr
  package will be droped from here.
}
\details{
\tabular{ll}{
Package: \tab RcmdrPlugin.UCA\cr
Type: \tab Package\cr
Version: \tab 2.0-4\cr
Date: \tab 2015-06-10\cr
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
# Example for two level factor
# Load data "AMSsurvey" selecting from Rcmdr menu:
# "Data" -> "Data in packages" -> "Read data set from an attached package..."
# then double-click on "car", click on "AMSsurvey" and "OK".
data(AMSsurvey)
# To make randomness test on variable "yield", select from Rcmdr menu:
# "Statistics" -> "Nonparametric tests" -> "Randomness test for two level factor..."
# select "sex" and "OK"
with(AMSsurvey, tseries::runs.test(sex))
#
#
# Example for numeric variable
# Load data "sweetpotato" selecting from Rcmdr menu:
# "Data" -> "Data in packages" -> "Read data set from an attached package..."
# then double-click on "randtests", click on "sweetpotato" and "OK".
data(sweetpotato, package="randtests")
# To make randomness test on variable "yield", select from Rcmdr menu:
# "Statistics" -> "Nonparametric tests" -> "Randomness test for numeric variable..."
# select "yield" and "OK"
with(sweetpotato, randtests::runs.test(yield))
}



\references{
[1] \url{http://knuth.uca.es/RcmdrPlugin.UCA}
}

