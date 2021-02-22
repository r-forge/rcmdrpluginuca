\name{paretochartMenu}
\title{Menu option to make a Pareto chart}
\alias{paretochartMenu}
\alias{Pareto chart}
\docType{package}
\encoding{UTF-8}
\description{
  Within the menu "Quality Control" -> "Pareto Chart", a new entry is
  provided to make a Pareto chart in which a factor type variable is
  represented in decreasing order of frequencies.
  This graph is often used to identify the most common causes of a failure.
  This option uses the \code{paretochart} function from the \code{qicharts2} package.
  For more information see \code{\link[qicharts2]{paretochart}}.
}

\details{
  Here is an example of "Pareto chart" menu entry.

  Load data "hospital_infections" selecting from Rcmdr menu: "Data" ->
  "Data in packages" -> "Read data set from an attached package..." then
  double-click on "quicharts2", click on "hospital_infections" and on "OK". Rcmdr
  reply with the following command in source pane (R Script)

  \code{data(hospital_infections, package="qicharts2")}

  To build a Pareto chart on variable "infection", select from Rcmdr
  menu: "Quality Control" -> "Pareto chart" select "infection" and  "OK". Rcmdr reply with the following command in source pane (R Script)

  \code{with(hospital_infections, paretochart(infection))}

  and the graph is plotted in another window.
}


\author{
  Manuel Munoz-Marquez <manuel.munoz@uca.es>
}

\keyword{package}

\seealso{
  For more information see \code{\link[Rcmdr]{Rcmdr-package}}. 

  Para ayuda en \enc{español}{espanol}, \enc{véase}{vease}
  \code{\link{Gráfico de Pareto (es)}}. (For Spanish help see
  \code{\link{Gráfico de Pareto (es)}}.) 
}
