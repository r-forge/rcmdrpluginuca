\name{sigmaMenu}
\title{Variance test}
\alias{Variance test}
\docType{package}
\encoding{UTF-8}
\description{
  Within the "Variances" menu, a new entry is provided for calculate
  confidence intervals and make contrasts on variance in a normal
  population. This option uses the function \code{sigma.test} of the
  package \code{TeachingDemos}. For more information see \code{\link[TeachingDemos]{sigma.test}}.
}

\details{
  Here is an example of "Single-sample Chi-square-test..." menu entry.

  Load data "sweetpotato" selecting from Rcmdr menu: "Data" ->
  "Data in packages" -> "Read data set from an attached package..." then
  double-click on "randtests", click on "sweetpotato" and on "OK". Rcmdr
  reply with the following command in source pane (R Script)

  \code{data(sweetpotato, package="randtests")}

  \code{sweetpotato <- as.data.frame(sweetpotato)}

  To build a confidence interval for sigma on variable "yield", select from Rcmdr menu: "Statistics" -> "Variances" -> "Single-sample Chi-square-test..." select "yield" and
  "OK". Rcmdr reply with the following command in source pane (R Script)

  \code{with(sweetpotato, sigma.test(yield))}
  }

\author{
Manuel Munoz-Marquez <manuel.munoz@uca.es>
}

\keyword{package}

\seealso{
For more information see \code{\link[Rcmdr]{Rcmdr-package}}. 

Para ayuda en \enc{español}{espanol}, \enc{véase}{vease}
\code{\link{Test de varianza (es)}}. (For Spanish help see \code{\link{Test de varianza (es)}}.) 
}
