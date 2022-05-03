\name{ksMenu}
\title{Kolmogorov-Smirnov test}
\alias{Kolmogorov-Smirnov test}
\docType{package}
\encoding{UTF-8}
\description{
  Within the "Statistics" -> "Non-parametric test" menu, a new entry is
  provided "Kolmogorov-Smirnov test for two samples..." to carry out a
  Kolmogorov-Smirnov test, a two-sample test of the null hypothesis that
  both samples were drawn from the same continuous distribution. This
  option uses the function \code{ks.test} of the package
  \code{stats}. For more information see \code{\link[stats]{ks.test}}.
}

\details{
  Here is an example of "Kolmogorov-Smirnov test for two samples..." menu entry.

  Load data "Baumann" selecting from Rcmdr menu: "Data" ->
  "Data in packages" -> "Read data set from an attached package..." then
  double-click on "carData", click on "Baumann" and on "OK". Rcmdr reply
  with the following command in source pane (R Script)

  \code{data(Baumann, package="carData")}

  To compare the distributions of "pretest.1" with "post.test.1", select
  from Rcmdr menu: "Statistics" -> "Non-parametric test" ->
  "Kolmogorov-Smirnov test for two samples..." select "pretest.1" and
  "post.test.1", then "OK". Rcmdr reply with the following command in
  source pane (R Script)
  
  \code{with(Baumann, ks.test(pretest.1, post.test.1))}
  }

\author{
Manuel Munoz-Marquez <manuel.munoz@uca.es>
}

\keyword{package}

\seealso{
For more information see \code{\link[Rcmdr]{Rcmdr-package}}. 

Para ayuda en \enc{español}{espanol}, \enc{véase}{vease}
\code{\link{Test de Kolmogorov-Smirnov (es)}}. (For Spanish help see \code{\link{Test de Kolmogorov-Smirnov (es)}}.) 
}
